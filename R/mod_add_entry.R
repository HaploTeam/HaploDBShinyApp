#' Add Entry Module
#'
#' Provides sub-tabs for adding new entries to the database.
#' v1: YJS Samples and Strains. Rows go to DB pending tables for admin review.
#'
#' @param id Module namespace ID
#' @param db_conn Database connection
#' @param user_info Reactive returning logged-in user data.frame
#' @param species_list Character vector of allowed species

# -- Validation helpers (pure functions, testable) --

#' Validate YJS sample rows
#'
#' @param df Data frame of YJS rows to validate
#' @param db_conn Database connection
#' @return List with valid (logical) and errors (character vector)
validate_yjs <- function(df, db_conn) {
  errors <- character(0)

  existing_strains <- DBI::dbGetQuery(
    db_conn, "SELECT strain FROM strains"
  )$strain
  pending_s <- DBI::dbGetQuery(
    db_conn, "SELECT strain FROM pending_strains"
  )$strain
  all_strains <- c(existing_strains, pending_s)

  names(df) <- tolower(names(df))
  for (i in seq_len(nrow(df))) {
    row_prefix <- sprintf("Row %d: ", i)

    if (is.na(df$sample_name[i]) ||
        nchar(trimws(as.character(df$sample_name[i]))) == 0) {
      errors <- c(errors,
        paste0(row_prefix, "sample_name is required."))
    }

    has_box <- !is.na(df$box_number[i])
    has_plate <- !is.na(df$plate[i]) &&
                 !is.na(df$plate_row[i]) &&
                 !is.na(df$plate_col[i])
    if (!has_box && !has_plate) {
      errors <- c(errors,
        paste0(row_prefix,
          "At least box_number or plate location ",
          "(plate + plate_row + plate_col) is required."))
    }

    if (is.na(df$stocked_by[i]) ||
        nchar(trimws(as.character(df$stocked_by[i]))) == 0) {
      errors <- c(errors,
        paste0(row_prefix, "stocked_by is required."))
    }

    if (!is.na(df$ploidy[i]) &&
        nchar(trimws(df$ploidy[i])) > 0 &&
        !grepl("^\\d+n$", df$ploidy[i])) {
      errors <- c(errors,
        paste0(row_prefix, "Invalid ploidy '",
               df$ploidy[i], "'. Expected format: <int>n"))
    }

    if (!is.na(df$id_strain[i]) &&
        nchar(trimws(df$id_strain[i])) > 0 &&
        !(df$id_strain[i] %in% all_strains)) {
      errors <- c(errors,
        paste0(row_prefix, "id_strain '",
               df$id_strain[i], "' not found."))
    }
  }

  list(valid = length(errors) == 0, errors = errors)
}

#' Validate strain rows
#'
#' @param df Data frame of strain rows to validate
#' @param db_conn Database connection
#' @return List with valid (logical) and errors (character vector)
validate_strains <- function(df, db_conn) {
  errors <- character(0)

  existing_strains <- DBI::dbGetQuery(
    db_conn, "SELECT strain FROM strains"
  )$strain
  pending_s <- DBI::dbGetQuery(
    db_conn, "SELECT strain FROM pending_strains"
  )$strain
  all_strains <- c(existing_strains, pending_s)

  for (i in seq_len(nrow(df))) {
    row_prefix <- sprintf("Row %d: ", i)

    if (df$strain[i] %in% all_strains) {
      errors <- c(errors,
        paste0(row_prefix, "strain '",
               df$strain[i], "' already exists."))
    }
  }

  list(valid = length(errors) == 0, errors = errors)
}

#' Validate YJS strain combined rows
#'
#' @param strain_df Data frame of strain rows to validate
#' @param yjs_df Data frame of yjs rows to validate
#' @param db_conn Database connection
#' @return List with valid (logical) and errors (character vector)
validate_combined <- function(
  strain_df,
  yjs_df,
  db_conn, 
  check_strains = TRUE
) {

  if (check_strains) {

    strain_result <- validate_strains(
      strain_df,
      db_conn
    )

    errors <- strain_result$errors

  } else {

    errors <- character(0)

  }

  names(yjs_df) <- tolower(names(yjs_df))

  for (i in seq_len(nrow(yjs_df))) {

    row_prefix <- sprintf("Row %d: ", i)

    if (is.na(yjs_df$sample_name[i]) ||
        nchar(trimws(as.character(yjs_df$sample_name[i]))) == 0) {
      errors <- c(
        errors,
        paste0(row_prefix, "sample_name is required.")
      )
    }

    has_box <- !is.na(yjs_df$box_number[i])

    has_plate <- !is.na(yjs_df$plate[i]) &&
      !is.na(yjs_df$plate_row[i]) &&
      !is.na(yjs_df$plate_col[i])

    if (!has_box && !has_plate) {
      errors <- c(
        errors,
        paste0(
          row_prefix,
          "At least box_number or plate location ",
          "(plate + plate_row + plate_col) is required."
        )
      )
    }

    if (is.na(yjs_df$stocked_by[i]) ||
        nchar(trimws(as.character(yjs_df$stocked_by[i]))) == 0) {

      errors <- c(
        errors,
        paste0(row_prefix, "stocked_by is required.")
      )
    }

    if (!is.na(yjs_df$ploidy[i]) &&
        nchar(trimws(yjs_df$ploidy[i])) > 0 &&
        !grepl("^\\d+n$", yjs_df$ploidy[i])) {

      errors <- c(
        errors,
        paste0(
          row_prefix,
          "Invalid ploidy '",
          yjs_df$ploidy[i],
          "'. Expected format: <int>n"
        )
      )
    }

  }

  list(
    valid = length(errors) == 0,
    errors = errors
  )
}

#' Get next XTRA_ strain name by incrementing the 3-letter code
#'
#' @param db_conn Database connection
#' @param count Number of names to generate
#' @return Character vector of XTRA_XXX names
next_xtra_names <- function(db_conn, count = 1, extra_names = character(0)) {
  db_last <- DBI::dbGetQuery(
    db_conn,
    "SELECT strain FROM strains WHERE strain LIKE 'XTRA_%' ORDER BY strain DESC LIMIT 1"
  )$strain

  pending_last <- DBI::dbGetQuery(
    db_conn,
    "SELECT strain FROM pending_strains WHERE strain LIKE 'XTRA_%' ORDER BY strain DESC LIMIT 1"
  )$strain

  candidates <- c(db_last, pending_last, extra_names)
  if (length(candidates) == 0) {
    last_name <- "XTRA_AAA"
    idx1 <- 1; idx2 <- 1; idx3 <- 0
  } else {
    last_name <- sort(candidates, decreasing = TRUE)[1]
    idx1 <- match(substr(last_name, 6, 6), LETTERS)
    idx2 <- match(substr(last_name, 7, 7), LETTERS)
    idx3 <- match(substr(last_name, 8, 8), LETTERS)
  }

  names <- character(count)
  for (i in seq_len(count)) {
    if (idx3 < 26) {
      idx3 <- idx3 + 1
    } else if (idx2 < 26) {
      idx3 <- 1
      idx2 <- idx2 + 1
    } else {
      idx3 <- 1
      idx2 <- 1
      idx1 <- idx1 + 1
    }
    names[i] <- paste0("XTRA_", LETTERS[idx1], LETTERS[idx2], LETTERS[idx3])
  }
  names
}

# YJS column definitions for the form and template
yjs_columns <- function() {
  c("sample_name", "species", "mating_type", "ploidy",
    "genotype", "sporulation", "external_origin", "eco_origin",
    "comments_origin", "parental_origin", "publication",
    "strains_group", "old_box", "box_number", "box_row", "box_col",
    "plate", "plate_row", "plate_col",
    "notes", "stocked_by", "comments", "id_strain",
    "sample_type", "collection")
}

strain_columns <- function() {
  c("strain_name", "isolation", "eco_origin", "geo_origin",
    "continent", "country", "clade", "srr_id", "species")
}

combined_columns <- function() {

  c(
    # Name and species
    "sample_name",
    "species",

    # Storage info
    "stocked_by",
    "box_number",
    "box_row",
    "box_col",
    "plate",
    "plate_row",
    "plate_col",

    # Origin info
    "eco_origin",
    "geo_origin",
    "continent",
    "country",
    "isolation",

    # Sequencing info
    "srr_id",
    "clade",

    # YJS optional fields
    "mating_type",
    "ploidy",
    "genotype",
    "sporulation",
    "external_origin",
    "comments_origin",
    "parental_origin",
    "publication",
    "strains_group",
    "old_box",
    "notes",
    "comments",
    "sample_type",
    "collection"
  )

}

# -- UI --

add_entry_ui <- function(id) {
  ns <- NS(id)
  navset_card_tab(
    id = ns("subtab"),
    title = "Add Entries",

    nav_panel(
      "Linked YJS Samples + Strains",
      value = "combined",

      layout_columns(
        col_widths = c(4, 8),

        card(
          card_header("Upload Strains and YJS Samples"),

          tags$div(
            class = "alert alert-info",
            icon("info-circle"),
            "Upload a file containing both strain and YJS sample information. ",
            "A provisional XTRA strain ID will be generated automatically and ",
            "associated with the uploaded YJS sample."
          ),

          fileInput(
            ns("combined_file"),
            "Upload TSV/CSV file",
            accept = c(".tsv", ".csv", ".txt")
          ),

          tags$small(
            class = "text-muted",
            "One row corresponds to one strain and one linked YJS sample."
          ),

          tags$hr(),

          actionButton(
            ns("combined_preview_btn"),
            "Preview",
            class = "btn-outline-primary me-2"
          ),

          actionButton(
            ns("combined_submit_btn"),
            "Submit",
            class = "btn-primary"
          ),

          tags$div(
            class = "mt-2",
            downloadButton(
              ns("combined_template"),
              "Download Template",
              class = "btn-sm btn-outline-secondary"
            )
          )
        ),

        card(
          card_header("Submission Preview"),

          uiOutput(
            ns("combined_feedback")
          ),

          DT::dataTableOutput(
            ns("combined_preview_table")
          )
        )
      )
    ),

    nav_panel("YJS Samples only", value = "yjs",
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Add YJS Sample"),
          radioButtons(ns("yjs_mode"), "Input Mode",
                       choices = c("Manual" = "manual", "File Upload" = "file"),
                       inline = TRUE),
          conditionalPanel(
            sprintf("input['%s'] == 'manual'", ns("yjs_mode")),
            tags$div(
              class = "alert alert-info py-2 mb-3",
              icon("info-circle"),
              "YJS numbers are automatically assigned upon approval."
            ),
            textInput(ns("yjs_sample_name"), "Sample Name *"),
            textInput(ns("yjs_stocked_by"), "Stocked By *"),
            tags$p(
              class = "info-text-blue",
              style = "margin-bottom: 0.5rem;",
              "At least box number or plate location (Plate + Row + Col) must be filled."
            ),
            textInput(ns("yjs_box_number"), "Box Number"),
            textInput(ns("yjs_box_row"), "Box Row"),
            textInput(ns("yjs_box_col"), "Box Column"),
            textInput(ns("yjs_plate"), "Plate"),
            textInput(ns("yjs_plate_row"), "Plate Row"),
            textInput(ns("yjs_plate_col"), "Plate Column"),
            selectizeInput(
              ns("yjs_species"), "Species", choices = c(""),
              options = list(
                create = TRUE,
                render = I("{
                  option: function(item, escape) {
                    return '<div><em>' + escape(item.label) + '</em></div>';
                  },
                  item: function(item, escape) {
                    return '<div><em>' + escape(item.label) + '</em></div>';
                  }
                }")
              )
            ),
            selectizeInput(
              ns("yjs_mating_type"), "Mating Type",
              choices = c("", "MATa/alpha", "MATa", "MATalpha"),
              options = list(create = TRUE)
            ),
            selectizeInput(
              ns("yjs_ploidy"), HTML("Ploidy <small class='text-muted'>(format: Nn, e.g. 2n)</small>"),
              choices = c("", "1n", "2n", "3n", "4n", "5n", "6n"),
              options = list(create = TRUE)
            ),
            textInput(ns("yjs_genotype"), "Genotype"),
            textInput(ns("yjs_sporulation"), "Sporulation"),
            textInput(ns("yjs_external_origin"), "External Origin"),
            textInput(ns("yjs_eco_origin"), "Ecological Origin"),
            textInput(ns("yjs_comments_origin"), "Comments Origin"),
            textInput(ns("yjs_parental_origin"), "Parental Origin"),
            textInput(ns("yjs_publication"), "Publication"),
            textInput(ns("yjs_strains_group"), "Strains Group"),
            textInput(ns("yjs_old_box"), "Old Box"),
            textInput(ns("yjs_notes"), "Notes"),
            textInput(ns("yjs_comments"), "Comments"),
            textInput(ns("yjs_id_strain"), "ID Strain (must exist)"),
            textInput(ns("yjs_sample_type"), "Sample Type"),
            selectizeInput(
              ns("yjs_collection"), "Collection", choices = c(""),
              options = list(create = TRUE)
            )
          ),
          conditionalPanel(
            sprintf("input['%s'] == 'file'", ns("yjs_mode")),
            fileInput(ns("yjs_file"), "Upload TSV/CSV file",
                      accept = c(".tsv", ".csv", ".txt")),
            tags$small(
              class = "text-muted",
              "YJS numbers are auto-assigned on approval.",
              "Any yjs_number column in the file will be ignored."
            )
          ),
          tags$hr(),
          conditionalPanel(
            sprintf("input['%s'] == 'manual'", ns("yjs_mode")),
            actionButton(ns("yjs_add_btn"), "Add to List", class = "btn-outline-primary me-2"),
            actionButton(ns("yjs_update_btn"), "Update Selected", class = "btn-outline-warning me-2")
          ),
          conditionalPanel(
            sprintf("input['%s'] == 'file'", ns("yjs_mode")),
            actionButton(ns("yjs_preview_btn"), "Preview", class = "btn-outline-primary me-2")
          ),
          actionButton(ns("yjs_submit_btn"), "Submit", class = "btn-primary"),
          tags$div(class = "mt-2",
            downloadButton(ns("yjs_template"), "Download Template", class = "btn-sm btn-outline-secondary")
          )
        ),
        card(
          card_header("Submission List"),
          uiOutput(ns("yjs_feedback")),
          DT::dataTableOutput(ns("yjs_preview_table")),
          conditionalPanel(
            sprintf("input['%s'] == 'manual'", ns("yjs_mode")),
            tags$div(class = "mt-2",
              actionButton(ns("yjs_remove_btn"), "Remove Selected", class = "btn-outline-danger btn-sm")
            )
          )
        )
      )
    ),

    nav_panel("Strains only", value = "strains",
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Add Strain"),
          radioButtons(ns("strain_mode"), "Input Mode",
                       choices = c("Manual" = "manual", "File Upload" = "file"),
                       inline = TRUE),
          conditionalPanel(
            sprintf("input['%s'] == 'manual'", ns("strain_mode")),
            textInput(ns("strain_name"), HTML("Strain Name (e.g., RM11) &mdash; Standardized ID will be automatically generated *")),
            textInput(ns("strain_isolation"), "Isolation"),
            textInput(ns("strain_eco_origin"), "Ecological Origin"),
            textInput(ns("strain_geo_origin"), "Geographic Origin"),
            textInput(ns("strain_continent"), "Continent"),
            textInput(ns("strain_country"), "Country"),
            textInput(ns("strain_clade"), "Clade"),
            textInput(ns("strain_srr_id"), "SRR ID"),
            selectizeInput(
              ns("strain_species"), "Species", choices = c(""),
              options = list(
                create = TRUE,
                render = I("{
                  option: function(item, escape) {
                    return '<div><em>' + escape(item.label) + '</em></div>';
                  },
                  item: function(item, escape) {
                    return '<div><em>' + escape(item.label) + '</em></div>';
                  }
                }")
              )
            )
          ),
          conditionalPanel(
            sprintf("input['%s'] == 'file'", ns("strain_mode")),
            fileInput(ns("strain_file"), "Upload TSV/CSV file",
                      accept = c(".tsv", ".csv", ".txt")),
            tags$small(class = "text-muted",
                       "File must have a strain_name column. XTRA_XXX names are auto-assigned.")
          ),
          tags$hr(),
          conditionalPanel(
            sprintf("input['%s'] == 'manual'", ns("strain_mode")),
            actionButton(ns("strain_add_btn"), "Add to List", class = "btn-outline-primary me-2"),
            actionButton(ns("strain_update_btn"), "Update Selected", class = "btn-outline-warning me-2")
          ),
          conditionalPanel(
            sprintf("input['%s'] == 'file'", ns("strain_mode")),
            actionButton(ns("strain_preview_btn"), "Preview", class = "btn-outline-primary me-2")
          ),
          actionButton(ns("strain_submit_btn"), "Submit", class = "btn-primary"),
          tags$div(class = "mt-2",
            downloadButton(ns("strain_template"), "Download Template", class = "btn-sm btn-outline-secondary")
          )
        ),
        card(
          card_header("Submission List"),
          uiOutput(ns("strain_feedback")),
          DT::dataTableOutput(ns("strain_preview_table")),
          conditionalPanel(
            sprintf("input['%s'] == 'manual'", ns("strain_mode")),
            tags$div(class = "mt-2",
              actionButton(ns("strain_remove_btn"), "Remove Selected", class = "btn-outline-danger btn-sm")
            )
          )
        )
      )
    )

  )
}

# -- Server --

add_entry_server <- function(id, db_conn, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Dynamic choices for species, ploidy, mating type
    get_species <- function() {
      c("", get_field_choices(db_conn, "species"))
    }
    get_ploidy <- function() {
      c("", get_field_choices(db_conn, "ploidy"))
    }
    get_mating <- function() {
      c("", get_field_choices(db_conn, "mating_type"))
    }
    get_collection <- function() {
      c("", get_field_choices(db_conn, "collection"))
    }

    refresh_choices <- function() {
      updateSelectizeInput(
        session, "yjs_species",
        choices = get_species(), server = FALSE
      )
      updateSelectizeInput(
        session, "yjs_ploidy",
        choices = get_ploidy(), server = FALSE
      )
      updateSelectizeInput(
        session, "yjs_mating_type",
        choices = get_mating(), server = FALSE
      )
      updateSelectizeInput(
        session, "yjs_collection",
        choices = get_collection(), server = FALSE
      )
      updateSelectizeInput(
        session, "strain_species",
        choices = get_species(), server = FALSE
      )
    }

    observe({
      req(input$subtab)
      refresh_choices()
    })

    # ---- Combined YJS + Strain ----

    combined_preview_data <- reactiveVal(NULL)

    build_combined_from_file <- function() {

      req(input$combined_file)

      file_path <- input$combined_file$datapath
      ext <- tools::file_ext(input$combined_file$name)

      df <- if (ext %in% c("tsv", "txt")) {
        read.delim(
          file_path,
          stringsAsFactors = FALSE,
          na.strings = c("", "NA")
        )
      } else {
        read.csv(
          file_path,
          stringsAsFactors = FALSE,
          na.strings = c("", "NA")
        )
      }

      expected <- combined_columns()

      for (col in expected) {
        if (!col %in% names(df)) {
          df[[col]] <- NA
        }
      }

      df[, expected, drop = FALSE]

    }

    combined_to_strain <- function(df) {

      data.frame(
        strain_name = df$sample_name,
        isolation = df$isolation,
        eco_origin = df$eco_origin,
        geo_origin = df$geo_origin,
        continent = df$continent,
        country = df$country,
        clade = df$clade,
        srr_id = df$srr_id,
        species = df$species,
        stringsAsFactors = FALSE
      )

    }

    combined_to_yjs <- function(df) {

      data.frame(
        sample_name = df$sample_name,
        species = df$species,
        mating_type = df$mating_type,
        ploidy = df$ploidy,
        genotype = df$genotype,
        sporulation = df$sporulation,
        external_origin = df$external_origin,
        eco_origin = df$eco_origin,
        comments_origin = df$comments_origin,
        parental_origin = df$parental_origin,
        publication = df$publication,
        strains_group = df$strains_group,
        old_box = df$old_box,
        box_number = df$box_number,
        box_row = df$box_row,
        box_col = df$box_col,
        plate = df$plate,
        plate_row = df$plate_row,
        plate_col = df$plate_col,
        notes = df$notes,
        stocked_by = df$stocked_by,
        comments = df$comments,
        sample_type = df$sample_type,
        collection = df$collection,
        stringsAsFactors = FALSE
      )

    }

    # Preview from file upload
    observeEvent(input$combined_preview_btn, {

      tryCatch({

        combined_df <- build_combined_from_file()

        strain_df <- combined_to_strain(combined_df)

        yjs_df <- combined_to_yjs(combined_df)

        validation <- validate_combined(
          strain_df,
          yjs_df,
          db_conn, 
          check_strains = FALSE
        )

        combined_preview_data(
          combined_df
        )

        if (!validation$valid) {

          output$combined_feedback <- renderUI(
            tags$div(
              class = "alert alert-danger",
              tags$strong("Validation errors:"),
              tags$ul(
                lapply(validation$errors, tags$li)
              )
            )
          )

        } else {

          output$combined_feedback <- renderUI(
            tags$div(
              class = "alert alert-success",
              sprintf(
                "%d linked strain/sample pair(s) ready.",
                nrow(combined_df)
              )
            )
          )

        }

      }, error = function(e) {

        output$combined_feedback <- renderUI(
          tags$div(
            class = "alert alert-danger",
            paste("Error:", e$message)
          )
        )

      })

    })

    output$combined_preview_table <- DT::renderDataTable({

      req(combined_preview_data())

      DT::datatable(
        combined_preview_data(),
        rownames = FALSE,
        selection = "none",
        options = list(
          scrollX = TRUE,
          scrollY = "400px",
          paging = FALSE,
          dom = "ti"
        )
      )

    })

    observeEvent(input$combined_submit_btn, {

      req(user_info())
      req(combined_preview_data())

      combined_df <- combined_preview_data()

      strain_df <- combined_to_strain(combined_df)

      strain_df <- assign_xtra_names(strain_df)

      yjs_df <- combined_to_yjs(combined_df)

      yjs_df$id_strain <- strain_df$strain

      validation <- validate_combined(
        strain_df,
        yjs_df,
        db_conn
      )

      if (!validation$valid) {

        output$combined_feedback <- renderUI(
          tags$div(
            class = "alert alert-danger",
            tags$strong("Cannot submit — validation errors:"),
            tags$ul(
              lapply(validation$errors, tags$li)
            )
          )
        )

        return()

      }

      batch_id <- paste0(
        format(Sys.time(), "%Y%m%d%H%M%S"),
        "_",
        sample(100000:999999, 1)
      )

      submission_groups <- sprintf(
        "%s_%d",
        batch_id,
        seq_len(nrow(combined_df))
      )

      strain_df$submission_group <- submission_groups
      yjs_df$submission_group <- submission_groups

      strain_df$submitted_by <- user_info()$username
      strain_df$submitted_at <- format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      )

      yjs_df$submitted_by <- user_info()$username
      yjs_df$submitted_at <- format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      )

      yjs_df$yjs_number <- "PENDING"

      tryCatch({

        DBI::dbWriteTable(
          db_conn,
          "pending_strains",
          strain_df,
          append = TRUE,
          row.names = FALSE
        )

        DBI::dbWriteTable(
          db_conn,
          "pending_yjs",
          yjs_df,
          append = TRUE,
          row.names = FALSE
        )

        output$combined_feedback <- renderUI(
          tags$div(
            class = "alert alert-success",
            sprintf(
              "%d linked strain/sample pair(s) submitted.",
              nrow(strain_df)
            )
          )
        )

        combined_preview_data(NULL)

      }, error = function(e) {

        output$combined_feedback <- renderUI(
          tags$div(
            class = "alert alert-danger",
            paste("Submission error:", e$message)
          )
        )

      })

    })

    output$combined_template <- downloadHandler(
      filename = function() {
        "combined_template.tsv"
      },

      content = function(file) {

        template <- data.frame(
          matrix(
            ncol = length(combined_columns()),
            nrow = 0
          )
        )

        colnames(template) <- combined_columns()

        write.table(
          template,
          file,
          sep = "\t",
          row.names = FALSE,
          quote = FALSE
        )
      }
    )

    # ---- YJS Samples ----

    yjs_preview_data <- reactiveVal(NULL)
    yjs_list_data <- reactiveVal(data.frame())

    # Helper to parse text input as integer (NA if empty or non-numeric)
    parse_int <- function(x) {
      if (is.null(x) || trimws(x) == "") return(NA_integer_)
      val <- suppressWarnings(as.integer(x))
      val
    }

    build_yjs_manual <- function() {
      data.frame(
        sample_name = trimws(input$yjs_sample_name),
        species = na_if_empty(input$yjs_species),
        mating_type = na_if_empty(input$yjs_mating_type),
        ploidy = na_if_empty(input$yjs_ploidy),
        genotype = na_if_empty(input$yjs_genotype),
        sporulation = na_if_empty(input$yjs_sporulation),
        external_origin = na_if_empty(input$yjs_external_origin),
        eco_origin = na_if_empty(input$yjs_eco_origin),
        comments_origin = na_if_empty(input$yjs_comments_origin),
        parental_origin = na_if_empty(input$yjs_parental_origin),
        publication = na_if_empty(input$yjs_publication),
        strains_group = na_if_empty(input$yjs_strains_group),
        old_box = parse_int(input$yjs_old_box),
        box_number = parse_int(input$yjs_box_number),
        box_row = parse_int(input$yjs_box_row),
        box_col = parse_int(input$yjs_box_col),
        plate = parse_int(input$yjs_plate),
        plate_row = parse_int(input$yjs_plate_row),
        plate_col = parse_int(input$yjs_plate_col),
        notes = na_if_empty(input$yjs_notes),
        stocked_by = trimws(input$yjs_stocked_by),
        comments = na_if_empty(input$yjs_comments),
        id_strain = na_if_empty(input$yjs_id_strain),
        sample_type = na_if_empty(input$yjs_sample_type),
        collection = na_if_empty(input$yjs_collection),
        stringsAsFactors = FALSE
      )
    }

    reset_yjs_form <- function() {
      updateTextInput(session, "yjs_sample_name", value = "")
      updateSelectizeInput(
        session, "yjs_species",
        choices = get_species(), selected = "", server = FALSE
      )
      updateSelectizeInput(
        session, "yjs_mating_type",
        choices = get_mating(), selected = "", server = FALSE
      )
      updateSelectizeInput(
        session, "yjs_ploidy",
        choices = get_ploidy(), selected = "", server = FALSE
      )
      updateTextInput(session, "yjs_genotype", value = "")
      updateTextInput(session, "yjs_sporulation", value = "")
      updateTextInput(session, "yjs_external_origin", value = "")
      updateTextInput(session, "yjs_eco_origin", value = "")
      updateTextInput(session, "yjs_comments_origin", value = "")
      updateTextInput(session, "yjs_parental_origin", value = "")
      updateTextInput(session, "yjs_publication", value = "")
      updateTextInput(session, "yjs_strains_group", value = "")
      updateTextInput(session, "yjs_old_box", value = "")
      updateTextInput(session, "yjs_box_number", value = "")
      updateTextInput(session, "yjs_box_row", value = "")
      updateTextInput(session, "yjs_box_col", value = "")
      updateTextInput(session, "yjs_plate", value = "")
      updateTextInput(session, "yjs_plate_row", value = "")
      updateTextInput(session, "yjs_plate_col", value = "")
      updateTextInput(session, "yjs_notes", value = "")
      updateTextInput(session, "yjs_stocked_by", value = "")
      updateTextInput(session, "yjs_comments", value = "")
      updateTextInput(session, "yjs_id_strain", value = "")
      updateTextInput(session, "yjs_sample_type", value = "")
      updateSelectizeInput(
        session, "yjs_collection",
        choices = get_collection(), selected = "", server = FALSE
      )
    }

    fill_yjs_form <- function(row) {
      updateTextInput(session, "yjs_sample_name",
                      value = na_to_empty(row$sample_name))
      sp <- get_species()
      sel_sp <- na_to_empty(row$species)
      if (nchar(sel_sp) > 0 && !(sel_sp %in% sp)) sp <- c(sp, sel_sp)
      updateSelectizeInput(session, "yjs_species",
                           choices = sp, selected = sel_sp,
                           server = FALSE)
      mt <- get_mating()
      sel_mt <- na_to_empty(row$mating_type)
      if (nchar(sel_mt) > 0 && !(sel_mt %in% mt)) mt <- c(mt, sel_mt)
      updateSelectizeInput(session, "yjs_mating_type",
                           choices = mt, selected = sel_mt,
                           server = FALSE)
      pl <- get_ploidy()
      sel_pl <- na_to_empty(row$ploidy)
      if (nchar(sel_pl) > 0 && !(sel_pl %in% pl)) pl <- c(pl, sel_pl)
      updateSelectizeInput(session, "yjs_ploidy",
                           choices = pl, selected = sel_pl,
                           server = FALSE)
      updateTextInput(session, "yjs_genotype", value = na_to_empty(row$genotype))
      updateTextInput(session, "yjs_sporulation", value = na_to_empty(row$sporulation))
      updateTextInput(session, "yjs_external_origin", value = na_to_empty(row$external_origin))
      updateTextInput(session, "yjs_eco_origin", value = na_to_empty(row$eco_origin))
      updateTextInput(session, "yjs_comments_origin", value = na_to_empty(row$comments_origin))
      updateTextInput(session, "yjs_parental_origin", value = na_to_empty(row$parental_origin))
      updateTextInput(session, "yjs_publication", value = na_to_empty(row$publication))
      updateTextInput(session, "yjs_strains_group", value = na_to_empty(row$strains_group))
      updateTextInput(session, "yjs_old_box", value = na_to_empty(row$old_box))
      updateTextInput(session, "yjs_box_number", value = na_to_empty(row$box_number))
      updateTextInput(session, "yjs_box_row", value = na_to_empty(row$box_row))
      updateTextInput(session, "yjs_box_col", value = na_to_empty(row$box_col))
      updateTextInput(session, "yjs_plate", value = na_to_empty(row$plate))
      updateTextInput(session, "yjs_plate_row", value = na_to_empty(row$plate_row))
      updateTextInput(session, "yjs_plate_col", value = na_to_empty(row$plate_col))
      updateTextInput(session, "yjs_notes", value = na_to_empty(row$notes))
      updateTextInput(session, "yjs_stocked_by", value = na_to_empty(row$stocked_by))
      updateTextInput(session, "yjs_comments", value = na_to_empty(row$comments))
      updateTextInput(session, "yjs_id_strain", value = na_to_empty(row$id_strain))
      updateTextInput(session, "yjs_sample_type", value = na_to_empty(row$sample_type))
      cl <- get_collection()
      sel_cl <- na_to_empty(row$collection)
      if (nchar(sel_cl) > 0 && !(sel_cl %in% cl)) cl <- c(cl, sel_cl)
      updateSelectizeInput(session, "yjs_collection",
                           choices = cl, selected = sel_cl,
                           server = FALSE)
    }

    build_yjs_from_file <- function() {
      req(input$yjs_file)
      file_path <- input$yjs_file$datapath
      ext <- tools::file_ext(input$yjs_file$name)

      df <- if (ext == "tsv" || ext == "txt") {
        read.delim(file_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
      } else {
        read.csv(file_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
      }

      expected <- yjs_columns()
      # Allow strain_name as alias for id_strain
      if ("strain_name" %in% names(df) && !("id_strain" %in% names(df))) {
        names(df)[names(df) == "strain_name"] <- "id_strain"
      }

      # Drop yjs_number if present (auto-assigned on approval)
      if ("yjs_number" %in% names(df)) df$yjs_number <- NULL

      # Add missing columns as NA
      for (col in expected) {
        if (!col %in% names(df)) df[[col]] <- NA
      }
      df <- df[, expected, drop = FALSE]

      df
    }

    # Fill form when a row is selected in the table
    observeEvent(input$yjs_preview_table_rows_selected, {
      sel <- input$yjs_preview_table_rows_selected
      if (length(sel) == 1 && input$yjs_mode == "manual") {
        df <- yjs_list_data()
        if (sel >= 1 && sel <= nrow(df)) {
          fill_yjs_form(df[sel, ])
        }
      }
    })

    # Add to list (manual mode)
    observeEvent(input$yjs_add_btn, {
      tryCatch({
        df <- build_yjs_manual()
        result <- validate_yjs(df, db_conn)

        if (!result$valid) {
          output$yjs_feedback <- renderUI(
            tags$div(class = "alert alert-danger",
                     tags$strong("Validation errors:"),
                     tags$ul(lapply(result$errors, tags$li)))
          )
          return()
        }

        current <- yjs_list_data()
        yjs_list_data(rbind(current, df))
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-success",
                   sprintf("Added '%s' to list. %d entry(ies) ready.",
                           df$sample_name[1], nrow(current) + 1))
        )
        reset_yjs_form()
      }, error = function(e) {
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-danger", paste("Error:", e$message))
        )
      })
    })

    # Update selected entry (manual mode)
    observeEvent(input$yjs_update_btn, {
      sel <- input$yjs_preview_table_rows_selected
      if (length(sel) != 1) {
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-warning", "Select a row in the list to update.")
        )
        return()
      }

      tryCatch({
        df <- build_yjs_manual()
        current <- yjs_list_data()

        result <- validate_yjs(df, db_conn)
        if (!result$valid) {
          output$yjs_feedback <- renderUI(
            tags$div(class = "alert alert-danger",
                     tags$strong("Validation errors:"),
                     tags$ul(lapply(result$errors, tags$li)))
          )
          return()
        }

        current[sel, ] <- df
        yjs_list_data(current)
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-success",
                   sprintf("Updated %s in list.", df$sample_name[1]))
        )
        reset_yjs_form()
      }, error = function(e) {
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-danger", paste("Error:", e$message))
        )
      })
    })

    # Remove selected entry from YJS list
    observeEvent(input$yjs_remove_btn, {
      sel <- input$yjs_preview_table_rows_selected
      if (length(sel) != 1) {
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-warning", "Select a row in the list to remove.")
        )
        return()
      }
      current <- yjs_list_data()
      if (sel >= 1 && sel <= nrow(current)) {
        yjs_list_data(current[-sel, , drop = FALSE])
        reset_yjs_form()
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-info",
                   sprintf("Removed entry. %d entry(ies) in list.", nrow(current) - 1))
        )
      }
    })

    # Preview from file upload
    observeEvent(input$yjs_preview_btn, {
      tryCatch({
        df <- build_yjs_from_file()
        result <- validate_yjs(df, db_conn)
        yjs_preview_data(df)

        if (!result$valid) {
          output$yjs_feedback <- renderUI(
            tags$div(class = "alert alert-danger",
                     tags$strong("Validation errors:"),
                     tags$ul(lapply(result$errors, tags$li)))
          )
        } else {
          output$yjs_feedback <- renderUI(
            tags$div(class = "alert alert-success",
                     sprintf("%d row(s) passed validation. Ready to submit.", nrow(df)))
          )
        }
      }, error = function(e) {
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-danger", paste("Error:", e$message))
        )
      })
    })

    output$yjs_preview_table <- DT::renderDataTable({
      if (input$yjs_mode == "manual") {
        df <- yjs_list_data()
        req(nrow(df) > 0)
        DT::datatable(df, rownames = FALSE,
                      selection = list(mode = "single"),
                      options = list(scrollX = TRUE, scrollY = "400px", paging = FALSE, dom = "ti"))
      } else {
        req(yjs_preview_data())
        DT::datatable(yjs_preview_data(), rownames = FALSE,
                      selection = "none",
                      options = list(scrollX = TRUE, scrollY = "400px", paging = FALSE, dom = "ti"))
      }
    })

    observeEvent(input$yjs_submit_btn, {
      req(user_info())

      df <- if (input$yjs_mode == "manual") {
        yjs_list_data()
      } else {
        yjs_preview_data()
      }
      req(nrow(df) > 0)

      result <- validate_yjs(df, db_conn)

      if (!result$valid) {
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-danger",
                   tags$strong("Cannot submit — validation errors:"),
                   tags$ul(lapply(result$errors, tags$li)))
        )
        return()
      }

      df$yjs_number <- "PENDING"
      df$submitted_by <- user_info()$username
      df$submitted_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

      tryCatch({
        # Save any new custom options
        for (val in unique(df$species[!is.na(df$species) & df$species != ""])) {
          save_custom_option(db_conn, "species", val)
        }
        for (val in unique(df$ploidy[!is.na(df$ploidy) & df$ploidy != ""])) {
          save_custom_option(db_conn, "ploidy", val)
        }
        for (val in unique(df$mating_type[!is.na(df$mating_type) & df$mating_type != ""])) {
          save_custom_option(db_conn, "mating_type", val)
        }
        for (val in unique(df$collection[!is.na(df$collection) & df$collection != ""])) {
          save_custom_option(db_conn, "collection", val)
        }

        DBI::dbWriteTable(db_conn, "pending_yjs", df, append = TRUE, row.names = FALSE)
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-success",
                   sprintf("%d YJS sample(s) submitted for review.", nrow(df)))
        )
        yjs_preview_data(NULL)
        yjs_list_data(data.frame())
        reset_yjs_form()
        refresh_choices()
      }, error = function(e) {
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-danger", paste("Submission error:", e$message))
        )
      })
    })

    output$yjs_template <- downloadHandler(
      filename = function() "yjs_template.tsv",
      content = function(file) {
        template <- data.frame(matrix(ncol = length(yjs_columns()), nrow = 0))
        colnames(template) <- yjs_columns()
        write.table(template, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )

    # ---- Strains ----

    strain_preview_data <- reactiveVal(NULL)
    strain_list_data <- reactiveVal(data.frame())

    build_strain_manual <- function() {
      data.frame(
        strain_name = trimws(input$strain_name),
        isolation = na_if_empty(input$strain_isolation),
        eco_origin = na_if_empty(input$strain_eco_origin),
        geo_origin = na_if_empty(input$strain_geo_origin),
        continent = na_if_empty(input$strain_continent),
        country = na_if_empty(input$strain_country),
        clade = na_if_empty(input$strain_clade),
        srr_id = na_if_empty(input$strain_srr_id),
        species = na_if_empty(input$strain_species),
        stringsAsFactors = FALSE
      )
    }

    reset_strain_form <- function() {
      updateTextInput(session, "strain_name", value = "")
      updateTextInput(session, "strain_isolation", value = "")
      updateTextInput(session, "strain_eco_origin", value = "")
      updateTextInput(session, "strain_geo_origin", value = "")
      updateTextInput(session, "strain_continent", value = "")
      updateTextInput(session, "strain_country", value = "")
      updateTextInput(session, "strain_clade", value = "")
      updateTextInput(session, "strain_srr_id", value = "")
      updateSelectizeInput(
        session, "strain_species",
        choices = get_species(), selected = "", server = FALSE
      )
    }

    fill_strain_form <- function(row) {
      updateTextInput(session, "strain_name", value = na_to_empty(row$original_name))
      updateTextInput(session, "strain_isolation", value = na_to_empty(row$isolation))
      updateTextInput(session, "strain_eco_origin", value = na_to_empty(row$eco_origin))
      updateTextInput(session, "strain_geo_origin", value = na_to_empty(row$geo_origin))
      updateTextInput(session, "strain_continent", value = na_to_empty(row$continent))
      updateTextInput(session, "strain_country", value = na_to_empty(row$country))
      updateTextInput(session, "strain_clade", value = na_to_empty(row$clade))
      updateTextInput(session, "strain_srr_id", value = na_to_empty(row$srr_id))
      sp <- get_species()
      sel_sp <- na_to_empty(row$species)
      if (nchar(sel_sp) > 0 && !(sel_sp %in% sp)) sp <- c(sp, sel_sp)
      updateSelectizeInput(session, "strain_species",
                           choices = sp, selected = sel_sp,
                           server = FALSE)
    }

    build_strain_from_file <- function() {
      req(input$strain_file)
      file_path <- input$strain_file$datapath
      ext <- tools::file_ext(input$strain_file$name)

      df <- if (ext == "tsv" || ext == "txt") {
        read.delim(file_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
      } else {
        read.csv(file_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
      }

      expected <- strain_columns()
      for (col in expected) {
        if (!col %in% names(df)) df[[col]] <- NA
      }
      df <- df[, expected, drop = FALSE]
      df
    }

    assign_xtra_names <- function(df) {

      df$strain <- next_xtra_names(
        db_conn,
        nrow(df)
      )

      df$original_name <- df$strain_name
      df$strain_name <- NULL

      df
    }

    # Fill form when a row is selected in the table
    observeEvent(input$strain_preview_table_rows_selected, {
      sel <- input$strain_preview_table_rows_selected
      if (length(sel) == 1 && input$strain_mode == "manual") {
        df <- strain_list_data()
        if (sel >= 1 && sel <= nrow(df)) {
          fill_strain_form(df[sel, ])
        }
      }
    })

    # Add to list (manual mode)
    observeEvent(input$strain_add_btn, {
      tryCatch({
        df <- build_strain_manual()

        if (nrow(df) == 0 || all(is.na(df$strain_name) | trimws(df$strain_name) == "")) {
          output$strain_feedback <- renderUI(
            tags$div(class = "alert alert-danger", "Strain name is required.")
          )
          return()
        }

        df_with_names <- assign_xtra_names(df)
        result <- validate_strains(df_with_names, db_conn)

        if (!result$valid) {
          output$strain_feedback <- renderUI(
            tags$div(class = "alert alert-danger",
                     tags$strong("Validation errors:"),
                     tags$ul(lapply(result$errors, tags$li)))
          )
          return()
        }

        current <- strain_list_data()
        strain_list_data(rbind(current, df_with_names))
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-success",
                   sprintf("Added %s (%s) to list. %d entry(ies) ready to submit.",
                           df_with_names$strain[1], df_with_names$original_name[1],
                           nrow(current) + 1))
        )
        reset_strain_form()
      }, error = function(e) {
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-danger", paste("Error:", e$message))
        )
      })
    })

    # Update selected entry (manual mode)
    observeEvent(input$strain_update_btn, {
      sel <- input$strain_preview_table_rows_selected
      if (length(sel) != 1) {
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-warning", "Select a row in the list to update.")
        )
        return()
      }

      tryCatch({
        df <- build_strain_manual()

        if (nrow(df) == 0 || all(is.na(df$strain_name) | trimws(df$strain_name) == "")) {
          output$strain_feedback <- renderUI(
            tags$div(class = "alert alert-danger", "Strain name is required.")
          )
          return()
        }

        current <- strain_list_data()
        # Keep the existing strain (XTRA_XXX) name for the updated row
        existing_strain <- current$strain[sel]
        df_updated <- df
        df_updated$strain <- existing_strain
        df_updated$original_name <- df_updated$strain_name
        df_updated$strain_name <- NULL

        result <- validate_strains(df_updated, db_conn)
        if (!result$valid) {
          output$strain_feedback <- renderUI(
            tags$div(class = "alert alert-danger",
                     tags$strong("Validation errors:"),
                     tags$ul(lapply(result$errors, tags$li)))
          )
          return()
        }

        current[sel, ] <- df_updated
        strain_list_data(current)
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-success",
                   sprintf("Updated %s in list.", existing_strain))
        )
        reset_strain_form()
      }, error = function(e) {
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-danger", paste("Error:", e$message))
        )
      })
    })

    # Remove selected entry from strain list
    observeEvent(input$strain_remove_btn, {
      sel <- input$strain_preview_table_rows_selected
      if (length(sel) != 1) {
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-warning", "Select a row in the list to remove.")
        )
        return()
      }
      current <- strain_list_data()
      if (sel >= 1 && sel <= nrow(current)) {
        strain_list_data(current[-sel, , drop = FALSE])
        reset_strain_form()
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-info",
                   sprintf("Removed entry. %d entry(ies) in list.", nrow(current) - 1))
        )
      }
    })

    # Preview from file upload
    observeEvent(input$strain_preview_btn, {
      tryCatch({
        df <- build_strain_from_file()

        if (nrow(df) == 0 || all(is.na(df$strain_name) | trimws(df$strain_name) == "")) {
          output$strain_feedback <- renderUI(
            tags$div(class = "alert alert-danger", "At least one strain name is required.")
          )
          return()
        }

        df_with_names <- assign_xtra_names(df)
        result <- validate_strains(df_with_names, db_conn)
        strain_preview_data(df_with_names)

        if (!result$valid) {
          output$strain_feedback <- renderUI(
            tags$div(class = "alert alert-danger",
                     tags$strong("Validation errors:"),
                     tags$ul(lapply(result$errors, tags$li)))
          )
        } else {
          output$strain_feedback <- renderUI(
            tags$div(class = "alert alert-success",
                     sprintf("%d strain(s) passed validation. Names assigned: %s",
                             nrow(df_with_names),
                             paste(df_with_names$strain, collapse = ", ")))
          )
        }
      }, error = function(e) {
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-danger", paste("Error:", e$message))
        )
      })
    })

    output$strain_preview_table <- DT::renderDataTable({
      if (input$strain_mode == "manual") {
        df <- strain_list_data()
        req(nrow(df) > 0)
        DT::datatable(df, rownames = FALSE,
                      selection = list(mode = "single"),
                      options = list(scrollX = TRUE, scrollY = "400px", paging = FALSE, dom = "ti"))
      } else {
        req(strain_preview_data())
        DT::datatable(strain_preview_data(), rownames = FALSE,
                      selection = "none",
                      options = list(scrollX = TRUE, scrollY = "400px", paging = FALSE, dom = "ti"))
      }
    })

    observeEvent(input$strain_submit_btn, {
      req(user_info())

      df <- if (input$strain_mode == "manual") {
        strain_list_data()
      } else {
        strain_preview_data()
      }
      req(nrow(df) > 0)

      result <- validate_strains(df, db_conn)

      if (!result$valid) {
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-danger",
                   tags$strong("Cannot submit — validation errors:"),
                   tags$ul(lapply(result$errors, tags$li)))
        )
        return()
      }

      df$submitted_by <- user_info()$username
      df$submitted_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

      tryCatch({
        # Save any new custom species options
        for (val in unique(df$species[!is.na(df$species) & df$species != ""])) {
          save_custom_option(db_conn, "species", val)
        }

        DBI::dbWriteTable(db_conn, "pending_strains", df, append = TRUE, row.names = FALSE)
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-success",
                   sprintf("%d strain(s) submitted for review.", nrow(df)))
        )
        strain_preview_data(NULL)
        strain_list_data(data.frame())
        reset_strain_form()
        refresh_choices()
      }, error = function(e) {
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-danger", paste("Submission error:", e$message))
        )
      })
    })

    output$strain_template <- downloadHandler(
      filename = function() "strains_template.tsv",
      content = function(file) {
        template <- data.frame(matrix(ncol = length(strain_columns()), nrow = 0))
        colnames(template) <- strain_columns()
        write.table(template, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )
  })
}

# Helper: convert empty string to NA
na_if_empty <- function(x) {
  if (is.null(x) || length(x) == 0 || (is.character(x) && trimws(x) == "")) NA else x
}

# Helper: convert NA to empty string (for filling form fields)
na_to_empty <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) "" else as.character(x)
}
