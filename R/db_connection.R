# PostgreSQL connection utilities

create_postgres_conn <- function() {

  pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = Sys.getenv("DB_PORT"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    sslmode = "require",
    minSize = 0,
    maxSize = 10
  )

}