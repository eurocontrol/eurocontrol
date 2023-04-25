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


#' Return a reference to the Flights table
#'
#' The returned `tbl` is referencing the flight table in PRISME.
#' You can use `dplyr`/`dbplyr` verbs to filter, join, ... with other
#' datasets.
#' NOTE: you need access to PRU_DEV
#'
#' @return a tbl referencing the Oracle table for flight
#' @export
#'
#' @examples
#' \dontrun{
#' arl <- flights_tbl()
#' }
flights_tbl <- function() {
  con <- db_connection(schema = "PRU_DEV")
  flt <- dplyr::tbl(con, dbplyr::in_schema("SWH_FCT", "FAC_FLIGHT"))
  flt
}

#' Extract useful flights list from NM flight table
#'
#' @param wef With EFfect date (included)
#' @param til TILl date (excluded)
#' @param .cols either ALL, USEFUL or a character vector of column names [default: USEFUL]
#'              USEFUL is a subset of typically used column names.
#'
#' @return a data frame with
#' * flight unique id,
#' * lobt, iobt, actualt OBT, day
#' * arrival datetime
#' * aircraft id (also CRCO and ACARS ones)
#' * aircraft registration (also from CRCO and ACARS)
#' * ...
#' * aircraft type
#' * aircraft operator
#' * aircraft group
#' * market segment
#'
#' @examples
#' \dontrun{
#' my_flts <- flights_tidy(wef = "2023-01-01", til = "2023-04-01")
#' }
flights_tidy <- function(wef, til, .cols = "USEFUL") {
  stopifnot(is.character(.cols))
  if (length(.cols) == 1L) {
    if (toupper(.cols) == "USEFUL") {
      columns <- c(
        "FLT_UID",
        "LOBT",
        "IOBT",
        "AIRCRAFT_ID",
        "CRCO_FLT_ID",
        "ACARS_CALLSIGN",
        "REGISTRATION",
        "CRCO_REGISTRATION",
        "ACARS_REGISTRATION",
        "AIRCRAFT_TYPE_ICAO_ID",
        "FLT_RULES",
        "ICAO_FLT_TYPE",
        "CRCO_ICAO_AIRCRAFT_TYPE",
        "WK_TBL_CAT",
        "AIRCRAFT_OPERATOR",
        "CRCO_USERNAME",
        "AIRCRAFT_ADDRESS",
        "CRCO_AIRCRAFT_ADDRESS",
        "LAST_FPL_ARCADDR",
        "ADEP",
        "ADES",
        "ID",
        "SENSITIVE",
        "EOBT_1",
        "ARVT_1",
        "TAXI_TIME_1",
        "AOBT_3",
        "ARVT_3",
        "TAXI_TIME_3",
        # Market Segment
        "RULE_NAME",
        # airline group
        "AO_GRP_CODE",
        "AO_GRP_NAME",
        NULL)
    } else if (toupper(.cols) == "ALL" || .cols == "") {
      # GET the list f all cols
    } else {
      stop(".cols can take ALL, USEFUL or a vector of column names.")
    }
  }

  con <- db_connection(schema = "PRU_DEV")

  flt <- dplyr::tbl(con, dbplyr::in_schema("SWH_FCT", "FAC_FLIGHT"))
  frl <- dplyr::tbl(con, dbplyr::in_schema("SWH_FCT", "DIM_FLIGHT_TYPE_RULE"))
  aog <- dplyr::tbl(con, dbplyr::in_schema("PRUDEV", "V_COVID_DIM_AO"))

  wef <- (lubridate::ymd(wef, tz = "UTC") - lubridate::hours(28)) |> format("%Y-%m-%d %H:%M:%S")
  til <- (lubridate::ymd(til, tz = "UTC") + lubridate::hours(24)) |> format("%Y-%m-%d %H:%M:%S")

  fff <- flt |>
    dplyr::filter(to_date(wef, "yyyy-mm-dd hh24:mi:ss") <= LOBT,
                  LOBT < to_date(til, "yyyy-mm-dd hh24:mi:ss")) |>
    dplyr::left_join(frl) |>
    dplyr::left_join(aog, by = c("AIRCRAFT_OPERATOR" = "AO_CODE")) |>
    dplyr::select(dplyr::all_of(columns))
}
