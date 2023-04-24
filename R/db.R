#' Provide a connection to the relevant Oracle database
#'
#' @param schema the Oracle DB schema to connect to.
#'               Note: this is more the prefix of the environment variable
#'               where the credentials are stored, like `<schema>_USR`,
#'               `<schema>_PWD` and `<schema>_DBNAME`.
#'               Possible values are `PRU_PROD`, `PRU_DEV`, `PRU_TEST`, ...
#'
#' @return DB connection
#' @export
#'
#' @examples
#' \dontrun{
#' con <- db_connection()
#' }
db_connection <- function(schema = "PRU_PROD") {
  withr::local_envvar(c(
    "TZ" = "UTC",
    "ORA_SDTZ" = "UTC",
    "NLS_LANG" = ".AL32UTF8"
  ))

  drv <- ROracle::Oracle()

  USR <- Sys.getenv(paste0(schema, "_USR"))
  PWD <- Sys.getenv(paste0(schema, "_PWD"))
  DBN <- Sys.getenv(paste0(schema, "_DBNAME"))

  con <- DBI::dbConnect(
    drv = drv,
    username = USR,
    password = PWD,
    dbname = DBN
  )

  con
}


#' Return a reference to the Airlines table
#'
#' The returned `tbl` is referencing the airline table in PRISME.
#' You can use `dplyr`/`dbplyr` verbs to filter, join, ... with other
#' datasets.
#' NOTE: you need access to PRU_DEV
#'
#' @return a tbl referencing the Oracle table for airlines
#' @export
#'
#' @examples
#' \dontrun{
#' arl <- airlines_tbl()
#' }
airlines_tbl <- function() {
  con <- db_connection(schema = "PRU_DEV")
  arl <- dplyr::tbl(con, dbplyr::in_schema("LDW_ACC", "AO_GROUPS_ASSOCIATION"))
  arl
}
