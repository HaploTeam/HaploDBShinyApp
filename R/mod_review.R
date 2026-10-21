#' Review Entries Module
#'
#' Admin-only tab for reviewing pending submissions.
#' Supports approve (insert into main DB tables + delete from pending) and reject (delete from pending).
#' Admin cannot approve their own submissions (server-side enforced).
#'
#' @param id Module namespace ID
#' @param db_conn DB connection
#' @param user_info Reactive returning logged-in user data.frame

review_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Review Pending Entries",
        actionButton(ns("refresh_btn"), "Refresh", icon = icon("sync"),
                     class = "btn-sm btn-outline-primary")
      ),
      layout_columns(
        col_widths = 12,
        selectInput(ns("pending_table"), "Pending Table",
                    choices = c("YJS Samples" = "pending_yjs",
                                "Strains" = "pending_strains"),
                    width = "300px")
      ),
      tags$div(
        class = "mb-3",
        actionButton(ns("select_all_btn"), "Select All",
                     class = "btn-sm btn-outline-secondary me-2"),
        actionButton(ns("unselect_all_btn"), "Unselect All",
                     class = "btn-sm btn-outline-secondary me-3"),
        actionButton(ns("approve_btn"), "Approve Selected",
                     class = "btn-success me-2", icon = icon("check")),
        actionButton(ns("reject_btn"), "Reject Selected",
                     class = "btn-danger", icon = icon("times"))
      ),
      uiOutput(ns("review_feedback")),
      DT::dataTableOutput(ns("pending_dt"))
    )
  )
}

review_server <- function(id, db_conn, user_info) {
  moduleServer(id, function(input, output, session) {

    pending_data <- reactiveVal(NULL)

    review_count <- reactiveVal(0)

    get_pending_counts <- function() {

      current_user <- user_info()$username

      yjs_count <- DBI::dbGetQuery(
        db_conn,
        "SELECT COUNT(*) AS n FROM pending_yjs WHERE submitted_by != $1",
        params = list(current_user)
      )

      strain_count <- DBI::dbGetQuery(
        db_conn,
        "SELECT COUNT(*) AS n FROM pending_strains WHERE submitted_by != $1",
        params = list(current_user)
      )

      list(
        yjs = if (nrow(yjs_count) > 0) as.integer(yjs_count$n[[1]]) else 0L,
        strains = if (nrow(strain_count) > 0) as.integer(strain_count$n[[1]]) else 0L
      )
    }

    choose_default_table <- function() {
      counts <- get_pending_counts()

      if (counts$yjs == 0 && counts$strains > 0) {
        "pending_strains"
      } else {
        "pending_yjs"
      }
    }

    update_dropdown_counts <- function() {

      counts <- get_pending_counts()

      review_count(counts$yjs + counts$strains)

      choices <- setNames(
        c("pending_yjs", "pending_strains"),
        c(
          sprintf("YJS Samples (%d)", counts$yjs),
          sprintf("Strains (%d)", counts$strains)
        )
      )

      updateSelectInput(
        session,
        "pending_table",
        choices = choices,
        selected = input$pending_table
      )
    }

    load_pending <- function() {
      req(is_admin(user_info()))

      current_user <- user_info()$username

      req(input$pending_table)
      table_name <- input$pending_table

      df <- DBI::dbGetQuery(
        db_conn,
        sprintf(
          "SELECT * FROM %s WHERE submitted_by != $1 ORDER BY submitted_at DESC",
          table_name
        ),
        params = list(current_user)
      )

      pending_data(df)
      update_dropdown_counts()
    }

    observe({

      req(user_info())
      req(user_info()$username)

      updateSelectInput(
        session,
        "pending_table",
        selected = choose_default_table()  )

    }) |> bindEvent(user_info(), once = TRUE)

    observeEvent(input$pending_table, { load_pending() })
    observeEvent(input$refresh_btn, { load_pending() })

    dt_proxy <- DT::dataTableProxy("pending_dt")

    output$pending_dt <- DT::renderDataTable({
      req(pending_data())
      df <- pending_data()
      if (nrow(df) == 0) {
        return(DT::datatable(df, rownames = FALSE,
                             options = list(dom = "t", language = list(emptyTable = "No pending entries."))))
      }
      DT::datatable(df, selection = "multiple", rownames = FALSE,
                    options = list(scrollX = TRUE, scrollY = "calc(100vh - 300px)", paging = FALSE, dom = "ti"))
    })

    observeEvent(input$select_all_btn, {
      req(pending_data(), nrow(pending_data()) > 0)
      DT::selectRows(dt_proxy, seq_len(nrow(pending_data())))
    })

    observeEvent(input$unselect_all_btn, {
      DT::selectRows(dt_proxy, NULL)
    })

    # Approve selected rows
    observeEvent(input$approve_btn, {
      req(is_admin(user_info()))
      selected <- input$pending_dt_rows_selected
      if (length(selected) == 0) {
        output$review_feedback <- renderUI(
          tags$div(class = "alert alert-warning mt-2", "No rows selected.")
        )
        return()
      }

      df <- pending_data()
      rows <- df[selected, , drop = FALSE]
      current_user <- user_info()$username
      table_name <- input$pending_table

      # Self-approval check
      own_rows <- rows$submitted_by == current_user
      if (any(own_rows)) {
        output$review_feedback <- renderUI(
          tags$div(class = "alert alert-danger mt-2",
                   sprintf("Cannot approve your own submissions. %d row(s) blocked (submitted by '%s').",
                           sum(own_rows), current_user))
        )
        return()
      }

      approved <- 0
      failed <- character(0)
      processed_groups <- character(0)
      total_linked_strains <- 0
      total_linked_yjs <- 0

      for (i in seq_len(nrow(rows))) {
        row <- rows[i, , drop = FALSE]
        row_id <- row$id

        tryCatch({
          if (
            "submission_group" %in% names(row) &&
            !is.na(row$submission_group) &&
            nzchar(row$submission_group)
          ) {

            if (row$submission_group %in% processed_groups) {
              next
            }

            combined_result <- approve_combined_submission(
              row$submission_group,
              db_conn,
              current_user
            )

            processed_groups <- c(
              processed_groups,
              row$submission_group
            )

            total_linked_strains <- total_linked_strains +
              combined_result$strain_count

            total_linked_yjs <- total_linked_yjs +
              combined_result$yjs_count

          } else if (table_name == "pending_yjs") {

            assigned_yjs <- approve_yjs_row(
              row,
              db_conn
            )

            create_notification(
              db_conn,
              row$submitted_by,
              "YJS Sample",
              row$sample_name,
              assigned_yjs,
              "approved",
              current_user,
              box = row$box_number,
              box_row = row$box_row,
              box_col = row$box_col,
              plate = row$plate,
              plate_row = row$plate_row,
              plate_col = row$plate_col
            )

          } else if (table_name == "pending_strains") {

            approve_strain_row(
              row,
              db_conn
            )

            create_notification(
              db_conn,
              row$submitted_by,
              "Strain",
              row$original_name,
              row$strain,
              "approved",
              current_user
            )

          }

          approved <- approved + 1
        }, error = function(e) {
          failed <<- c(failed, sprintf("Row %s: %s", as.character(row_id), conditionMessage(e)))
        })
      }

      msg_parts <- c(
        sprintf("%d approval operation(s) completed.", approved)
      )

      if (total_linked_strains > 0 || total_linked_yjs > 0) {

        msg_parts <- c(
          msg_parts,
          sprintf(
            "Approved %d strain(s) and %d YJS sample(s) from linked submission(s).",
            total_linked_strains,
            total_linked_yjs
          )
        )

      }

      msg <- paste(
        msg_parts,
        collapse = "<br>"
      )

      if (length(failed) > 0) {
        msg <- paste(msg, "Failures:", paste(failed, collapse = "; "))
      }

      output$review_feedback <- renderUI(
        tags$div(class = if (length(failed) > 0) "alert alert-warning mt-2" else "alert alert-success mt-2",
                 HTML(msg))
      )
      load_pending()
    })

    # Reject selected rows
    observeEvent(input$reject_btn, {
      req(is_admin(user_info()))
      selected <- input$pending_dt_rows_selected
      if (length(selected) == 0) {
        output$review_feedback <- renderUI(
          tags$div(class = "alert alert-warning mt-2", "No rows selected.")
        )
        return()
      }

      df <- pending_data()
      table_name <- input$pending_table

      rows <- df[selected, , drop = FALSE]
      current_user <- user_info()$username

      processed_groups <- character(0)
      reject_messages <- character(0)
      ids_to_delete <- integer(0)
      total_linked_strains_rejected <- 0
      total_linked_yjs_rejected <- 0
      rejected <- 0

      for (i in seq_len(nrow(rows))) {
        row <- rows[i, , drop = FALSE]
        entry_name <- if (table_name == "pending_yjs") row$sample_name else row$original_name
        entry_type <- if (table_name == "pending_yjs") "YJS Sample" else "Strain"

        if (
          "submission_group" %in% names(row) &&
          !is.na(row$submission_group) &&
          nzchar(row$submission_group)
        ) {

          if (row$submission_group %in% processed_groups) {
            next
          }

          result <- reject_combined_submission(
            row$submission_group,
            db_conn,
            current_user
          )

          processed_groups <- c(
            processed_groups,
            row$submission_group
          )

          total_linked_strains_rejected <-
            total_linked_strains_rejected +
            result$strain_count

          total_linked_yjs_rejected <-
            total_linked_yjs_rejected +
            result$yjs_count

          rejected <- rejected + 1

          next

        } else if (table_name == "pending_yjs") {

          ids_to_delete <- c(
            ids_to_delete,
            row$id
          )

          create_notification(
            db_conn, row$submitted_by, entry_type,
            entry_name, NA_character_, "rejected", current_user,
            box = row$box_number, box_row = row$box_row, box_col = row$box_col,
            plate = row$plate, plate_row = row$plate_row, plate_col = row$plate_col
          )

          rejected <- rejected + 1

        } else if (table_name == "pending_strains") {


          ids_to_delete <- c(
            ids_to_delete,
            row$id
          )
          
          create_notification(
            db_conn, row$submitted_by, entry_type,
            entry_name, NA_character_, "rejected", current_user
          )

          rejected <- rejected + 1
        }
      }

      if (length(ids_to_delete) > 0) {

        placeholders <- paste0(
          "$",
          seq_along(ids_to_delete),
          collapse = ","
        )

        DBI::dbExecute(
          db_conn,
          sprintf(
            "DELETE FROM %s WHERE id IN (%s)",
            table_name,
            placeholders
          ),
          params = as.list(ids_to_delete)
        )

      }

      msg_parts <- c(
        sprintf(
          "%d rejection operation(s) completed.",
          rejected
        )
      )

      if (
        total_linked_strains_rejected > 0 ||
        total_linked_yjs_rejected > 0
      ) {

        msg_parts <- c(
          msg_parts,
          sprintf(
            "Rejected %d strain(s) and %d YJS sample(s) from linked submission(s).",
            total_linked_strains_rejected,
            total_linked_yjs_rejected
          )
        )

      }

      msg <- paste(
        msg_parts,
        collapse = "<br>"
      )

      output$review_feedback <- renderUI(
        tags$div(
          class = "alert alert-info mt-2",
          HTML(msg)
        )
      )

      load_pending()
    })

    return(review_count)
  })
}

# -- Approval helpers --

#' Approve a YJS row: auto-assign YJS number, insert into main DB, delete from pending
#' @return The assigned YJS number
approve_yjs_row <- function(row, db_conn) {
  assigned_yjs <- get_next_yjs_number(db_conn, 1)

  db_yjs_cols <- c("yjs_number", "sample_name", "species", "mating_type", "ploidy",
                   "genotype", "sporulation", "external_origin", "eco_origin",
                   "comments_origin", "parental_origin", "publication", "strains_group",
                   "old_box", "box_number", "box_row", "box_col",
                   "plate", "plate_row", "plate_col",
                   "notes", "stocked_by", "comments", "id_strain",
                   "sample_type", "collection")

  insert_data <- row[, db_yjs_cols, drop = FALSE]
  names(insert_data) <- db_yjs_cols
  insert_data$yjs_number <- assigned_yjs
  placeholders <- paste0("$", seq_along(db_yjs_cols), collapse = ", ")
  col_names <- paste(db_yjs_cols, collapse = ", ")
  sql <- sprintf("INSERT INTO yjs_numbers (%s) VALUES (%s)", col_names, placeholders)

  DBI::dbExecute(
    db_conn,
    sql,
    params = unname(as.list(insert_data))
  )

  DBI::dbExecute(
    db_conn,
    "DELETE FROM pending_yjs WHERE id = $1",
    params = list(row$id)
  )

  assigned_yjs
}

#' Approve a Strain row: insert into main DB Strains + alt_names, delete from pending
approve_strain_row <- function(row, db_conn) {
  strain_cols <- c("strain", "isolation", "eco_origin", "geo_origin",
                   "continent", "country", "clade", "srr_id", "species")
  db_strain_cols <- c("strain", "isolation", "eco_origin", "geo_origin",
                      "continent", "country", "clade", "srr_id", "species")

  insert_data <- row[, strain_cols, drop = FALSE]
  names(insert_data) <- db_strain_cols
  placeholders <- paste0("$", seq_along(strain_cols), collapse = ", ")
  col_names <- paste(db_strain_cols, collapse = ", ")
  sql <- sprintf("INSERT INTO strains (%s) VALUES (%s)", col_names, placeholders)

  DBI::dbExecute(
    db_conn,
    sql,
    params = unname(as.list(insert_data))
  )

  if (!is.na(row$original_name) &&
      nchar(trimws(row$original_name)) > 0) {

    DBI::dbExecute(
      db_conn,
      "INSERT INTO alt_names (strain, alt_name) VALUES ($1, $2)",
      params = list(row$strain, row$original_name)
    )
  }

  DBI::dbExecute(
    db_conn,
    "DELETE FROM pending_strains WHERE id = $1",
    params = list(row$id)
  )
}

get_linked_pending_yjs <- function(
  submission_group,
  db_conn
) {

  DBI::dbGetQuery(
    db_conn,
    "
    SELECT *
    FROM pending_yjs
    WHERE submission_group = $1
    ",
    params = list(submission_group)
  )

}

approve_combined_submission <- function(
  submission_group,
  db_conn, 
  reviewer
) {

  linked_strain <- DBI::dbGetQuery(
    db_conn,
    "
    SELECT *
    FROM pending_strains
    WHERE submission_group = $1
    ",
    params = list(submission_group)
  )

  linked_yjs <- get_linked_pending_yjs(
    submission_group,
    db_conn
  )

  if (nrow(linked_strain) == 0) {
    stop(
      sprintf(
        "No linked strain found for submission_group '%s'",
        submission_group
      )
    )
  }

  if (nrow(linked_strain) > 1) {
    stop(
      sprintf(
        "Multiple linked strains found for submission_group '%s'",
        submission_group
      )
    )
  }

  assigned_yjs <- character(nrow(linked_yjs))

  pool::poolWithTransaction(
    db_conn,
    function(conn) {

      approve_strain_row(
        linked_strain[1, , drop = FALSE],
        conn
      )

      for (i in seq_len(nrow(linked_yjs))) {

        assigned_yjs[i] <<- approve_yjs_row(
          linked_yjs[i, , drop = FALSE],
          conn
        )

      }

    }
  )

  create_notification(
    db_conn,
    linked_strain$submitted_by[1],
    "Strain",
    linked_strain$original_name[1],
    linked_strain$strain[1],
    "approved",
    reviewer
  )

  for (i in seq_len(nrow(linked_yjs))) {

    create_notification(
      db_conn,
      linked_yjs$submitted_by[i],
      "YJS Sample",
      linked_yjs$sample_name[i],
      assigned_yjs[i],
      "approved",
      reviewer
    )

  }

  list(
    strain_count = nrow(linked_strain),
    yjs_count = nrow(linked_yjs)
  )
}

reject_combined_submission <- function(
  submission_group,
  db_conn,
  reviewer
) {

  linked_strain <- DBI::dbGetQuery(
    db_conn,
    "
    SELECT *
    FROM pending_strains
    WHERE submission_group = $1
    ",
    params = list(submission_group)
  )

  linked_yjs <- DBI::dbGetQuery(
    db_conn,
    "
    SELECT *
    FROM pending_yjs
    WHERE submission_group = $1
    ",
    params = list(submission_group)
  )

  for (i in seq_len(nrow(linked_strain))) {

    create_notification(
      db_conn,
      linked_strain$submitted_by[i],
      "Strain",
      linked_strain$original_name[i],
      NA_character_,
      "rejected",
      reviewer
    )

  }

  for (i in seq_len(nrow(linked_yjs))) {

    create_notification(
      db_conn,
      linked_yjs$submitted_by[i],
      "YJS Sample",
      linked_yjs$sample_name[i],
      NA_character_,
      "rejected",
      reviewer,
      box = linked_yjs$box_number[i],
      box_row = linked_yjs$box_row[i],
      box_col = linked_yjs$box_col[i],
      plate = linked_yjs$plate[i],
      plate_row = linked_yjs$plate_row[i],
      plate_col = linked_yjs$plate_col[i]
    )

  }

  pool::poolWithTransaction(
    db_conn,
    function(conn) {

      DBI::dbExecute(
        conn,
        "
        DELETE FROM pending_strains
        WHERE submission_group = $1
        ",
        params = list(submission_group)
      )

      DBI::dbExecute(
        conn,
        "
        DELETE FROM pending_yjs
        WHERE submission_group = $1
        ",
        params = list(submission_group)
      )

    }
  )

  list(
    strain_count = nrow(linked_strain),
    yjs_count = nrow(linked_yjs)
  )

}