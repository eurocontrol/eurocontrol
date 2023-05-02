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



#' Extract a clean flights list in an interval
#'
#' The returned [tbl] includes scheduled and non-scheduled flight
#' in the right-opened interval `[wef, til)`.
#' General aviation, State, military and sensitive flight are excluded.
#'
#' *NOTE*: you need access to PRU_DEV
#'
#' @param wef **W**ith **EF**fect date (included) at Zulu time
#'            in a format recognized by [as_datetime()]
#' @param til **TIL**l date (excluded) at Zulu time
#'            in a format recognized by [as_datetime()]
#'
#' @return a [tbl] with the following columns
#'
#' * FLT_UID: the flight unique id
#' * LOBT: **L**ast received **O**ff-**B**lock **T**ime
#' * IOBT: **I**nitial **O**ff-**B**lock **T**ime
#' * AIRCRAFT_ID
#' * REGISTRATION: the registration
#' * AIRCRAFT_TYPE_ICAO_ID
#' * FLT_RULES (see [FPL Item 8](https://www.skybrary.aero/articles/flight-plan-completion)):
#'   which sets of regulations the flight is operated under.
#'   Possible values are:
#'
#'   - `I` for IFR
#'   - `V` for VFR,
#'   - `Y` first IFR thereafter VFR,
#'   - `Z` first VFR thereafter IFR
#'
#' * ICAO_FLT_TYPE (see [FPL Item 8](https://www.skybrary.aero/articles/flight-plan-completion)):
#'   flight type. Possible values (but G, M aand X shouldn't appear):
#'
#'   - `S` for scheduled air service
#'   - `N` for non-scheduled air service
#'   - `G` for general aviation
#'   - `M` for military
#'   - `X` for other than the preceding categories
#'
#'
#' * WK_TBL_CAT (see [FPL Item 9](https://www.skybrary.aero/articles/flight-plan-completion)): wake turbulence category, cab be
#'   - `L` LIGHT, i.e. maximum certificated takeoff mass of 7000 kg (15_500 lbs) or less.
#'   - `M` MEDIUM, i.e maximum certificated takeoff mass less than 136_000 kg (300_000 lbs),
#'     but more than 7_000 kg (15_500 lbs)
#'   - `H` HEAVY, i.e. maximum certificated takeoff mass of 136_000 kg (300_000 lbs) or more
#'     (except those specified as `J`)
#'   - `J` SUPER, presently the only the AIRBUS A-380-800
#'
#' * AIRCRAFT_OPERATOR:
#' * AIRCRAFT_ADDRESS: the ICAO 24-bit address of the airframe for ADS-B/Mode S broadcasting
#' * ADEP: ([ICAO code](https://observablehq.com/@openaviation/airports) of the) **A**erodrome of **DEP**arture
#' * ADES: ([ICAO code](https://observablehq.com/@openaviation/airports) of the) **A**erodrome of **DES**tination
#' * ID: ETFMS ID (?)
#' * EOBT_1: **E**stimated **O**ff-**B**lock **T**ime for FPL-based (M1) trajectory
#' * ARVT_1: **AR**ri**V**al **T**ime for FPL-based (M1) trajectory
#' * TAXI_TIME_1: Taxi time for FPL-based (M1) trajectory
#' * AOBT_3: **A**ctual **O**ff-**B**lock **T**ime for flown (M3) trajectory
#' * ARVT_3: **ARV**ival **T**ime for flown (M3) trajectory
#' * TAXI_TIME_3: Taxi time for flown (M3) trajectory
#' * RULE_NAME: market segment type
#' * AO_GRP_CODE: Aircraft Operator group (code)
#' * AO_GRP_NAME: : Aircraft Operator group (name)
#' * RTE_LEN_1: route length (NM) for FPL-based (M1) trajectory
#' * RTE_LEN_3: route length (NM) for for flown (M3) trajectory
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my_flts <- flights_tidy(wef = "2023-01-01", til = "2023-04-01")
#' }
flights_tidy <- function(wef, til) {
  columns <- c(
    "FLT_UID",
    "LOBT",
    "IOBT",
    "AIRCRAFT_ID",
    # "CRCO_FLT_ID",
    # "ACARS_CALLSIGN",
    "REGISTRATION",
    # "CRCO_REGISTRATION",
    # "ACARS_REGISTRATION",
    "AIRCRAFT_TYPE_ICAO_ID",
    "FLT_RULES",
    "ICAO_FLT_TYPE",
    # "SK_FLT_TYPE_RULE_ID",
    # "CRCO_ICAO_AIRCRAFT_TYPE",
    "WK_TBL_CAT",
    "AIRCRAFT_OPERATOR",
    # "CRCO_USERNAME",
    "AIRCRAFT_ADDRESS",
    # "CRCO_AIRCRAFT_ADDRESS",
    # "LAST_FPL_ARCADDR",
    "ADEP",
    "ADES",
    "ID",
    # "SENSITIVE",
    # "EXMP_RSN_LH",
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
    # route length
    "RTE_LEN_1",
    "RTE_LEN_3",
    NULL)

  con <- db_connection(schema = "PRU_DEV")

  flt <- dplyr::tbl(con, dbplyr::in_schema("SWH_FCT", "FAC_FLIGHT"))
  frl <- dplyr::tbl(con, dbplyr::in_schema("SWH_FCT", "DIM_FLIGHT_TYPE_RULE"))
  aog <- dplyr::tbl(con, dbplyr::in_schema("PRUDEV", "V_COVID_DIM_AO"))

  wef <- lubridate::as_datetime(wef, tz = "UTC") |> format("%Y-%m-%d %H:%M:%S")
  til <- lubridate::as_datetime(til, tz = "UTC") |> format("%Y-%m-%d %H:%M:%S")

  fff <- flt |>
    dplyr::filter(to_date(wef, "yyyy-mm-dd hh24:mi:ss") <= LOBT,
                  LOBT < to_date(til, "yyyy-mm-dd hh24:mi:ss")) |>
    dplyr::left_join(frl, by = "SK_FLT_TYPE_RULE_ID") |>
    dplyr::left_join(aog, by = c("AIRCRAFT_OPERATOR" = "AO_CODE")) |>
    dplyr::filter(
      # only commercial flights (scheduled and non-scheduled), i.e. General Aviation, Military and Other excluded
      ICAO_FLT_TYPE %in% c('S', 'N'),
      # make sure military flights are excluded
      SK_FLT_TYPE_RULE_ID != 1L,
      # exclude sensitive flights
      SENSITIVE != 'Y',
      # exclude "Head of State" flights, might be redundant with the "SENSITIVE" flag
      EXMP_RSN_LH != 'HEAD') |>
    dplyr::select(dplyr::all_of(columns))
}
