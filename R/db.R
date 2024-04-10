#' Provide a connection to the relevant Oracle database
#'
#' # Note
#' The `schema` is in fact the prefix of the environment variables
#' where the credentials are stored, like `<schema>_USR`,
#' `<schema>_PWD` and `<schema>_DBNAME`.
#' Possible values for `schema` are `PRU_PROD`, `PRU_DEV`,
#' `PRU_TEST`, ...
#'
#' @param schema the Oracle DB schema to connect to.
#'
#'
#' @return A connection to a database (specifically an implementation of [DBI::DBIConnection-class]
#'         for an Oracle database.)
#' @export
#'
#' @examples
#' \dontrun{
#' conn <- db_connection()
#' # ... perform other API operations re-using the same connection
#' # ...
#' DBI::dbDisconnect(conn)
#' }
db_connection <- function(schema = "PRU_PROD") {

  USR <- Sys.getenv(paste0(schema, "_USR"))
  PWD <- Sys.getenv(paste0(schema, "_PWD"))
  DBN <- Sys.getenv(paste0(schema, "_DBNAME"))

  withr::local_envvar(c(
    "TZ" = "UTC",
    "ORA_SDTZ" = "UTC",
    "NLS_LANG" = ".AL32UTF8"
  ))
  conn <- DBI::dbConnect(
    drv      = DBI::dbDriver("Oracle"),
    username = USR,
    password = PWD,
    dbname   = DBN,
    timezone = "UTC"
  )

  conn
}

