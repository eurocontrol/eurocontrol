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
    drv = DBI::dbDriver("Oracle"),
    username = USR,
    password = PWD,
    dbname = DBN,
    timezone = "UTC"
  )

  conn
}


#' Return a reference to the Airlines table
#'
#' @description
#' The returned [dplyr::tbl()] is referencing the airlines table in PRISME.
#' You can use `dplyr`/`dbplyr` verbs to filter, join, ... with other
#' datasets.
#'
#' # Note
#' You need to either provide a connection `conn`
#' that has access to `PRU_DEV.V_COVID_DIM_AO` or go with the
#' default which uses PRU_DEV to establish a [db_connection()].
#'
#' @param conn Database connection or instantiate the default one.
#'
#' @return a [dplyr::tbl()] referencing the Oracle table for airlines.
#' @export
#'
#' @examples
#' \dontrun{
#' arl <- airlines_tbl()
#' }
airlines_tbl <- function(conn = NULL) {
  if (is.null(conn)) {
    conn <- db_connection(schema = "PRU_DEV")
  }
  arl <- dplyr::tbl(conn, "V_COVID_DIM_AO")
  arl
}


#' Return a reference to the Flights table
#'
#' @description
#' The returned [dplyr::tbl()] is referencing the flights table in PRISME.
#' You can use `dplyr`/`dbplyr` verbs to filter, join, ... with other
#' datasets.
#'
#' # Note
#' You need to either provide a connection `conn` that has access to `SWH_FCT.FAC_FLIGHT` or
#' go with the default which uses PRU_DEV to establish a [db_connection()].
#'
#' @inheritParams airlines_tbl
#'
#' @return a [dplyr::tbl()] referencing the Oracle table for flights.
#' @export
#'
#' @examples
#' \dontrun{
#' arl <- flights_tbl()
#' }
#'
flights_tbl <- function(conn = NULL) {
  if (is.null(conn)) {
    conn <- db_connection(schema = "PRU_DEV")
  }
  flt <- dplyr::tbl(conn, dbplyr::in_schema("SWH_FCT", "FAC_FLIGHT"))
  flt
}





#' Extract a clean flights list in an interval
#'
#' @description
#' The returned [dplyr::tbl()] includes scheduled and non-scheduled flight
#' departing in the right-opened interval `[wef, til)`.
#'
#' General aviation, State, military and sensitive flight are excluded.
#'
#' # Note
#' You need to either provide a connection `conn` that has access to `SWH_FCT.DIM_FLIGHT_TYPE_RULE`,
#' `PRUDEV.V_COVID_DIM_AO` and `SWH_FCT.FAC_FLIGHT` or go with the default which uses
#' PRU_DEV to establish a [db_connection()].
#'
#' @inheritParams airlines_tbl
#'
#' @param wef **W**ith **EF**fect date (included) at Zulu time
#'            in a format recognized by [lubridate::as_datetime()]
#' @param til un**TIL**l date (excluded) at Zulu time
#'            in a format recognized by [lubridate::as_datetime()]
#'
#' @return A [dplyr::tbl()] with the following columns:
#'
#' * FLT_UID: the flight unique id.
#' * LOBT: **L**ast received **O**ff-**B**lock **T**ime.
#' * IOBT: **I**nitial **O**ff-**B**lock **T**ime.
#' * AIRCRAFT_ID: the [callsign](https://www.skybrary.aero/articles/aircraft-call-sign)
#'                of the relevant flight, e.g. BAW6VB.
#' * REGISTRATION: the [aircraft registration](https://en.wikipedia.org/wiki/Aircraft_registration)
#'   (with spaces, dashes, ... stripped), e.g. GEUUU.
#' * AIRCRAFT_TYPE_ICAO_ID: the [ICAO code for the aircraft type](https://www.icao.int/publications/doc8643/pages/search.aspx), for example A30B for an
#'   Airbus A-300B2-200.
#' * FLT_RULES (see [FPL Item 8](https://www.skybrary.aero/articles/flight-plan-completion)):
#'   which sets of regulations the flight is operated under.
#'   Possible values are:
#'   - `I` for IFR
#'   - `V` for VFR
#'   - `Y` first IFR thereafter VFR
#'   - `Z` first VFR thereafter IFR
#'
#' * ICAO_FLT_TYPE (see [FPL Item 8](https://www.skybrary.aero/articles/flight-plan-completion)):
#'   flight type. Possible values (but G, M aand X shouldn't appear):
#'   - `S` for scheduled air service
#'   - `N` for non-scheduled air service
#'   - `G` for general aviation (note: filtered out)
#'   - `M` for military (note: filtered out)
#'   - `X` for other than the preceding categories
#'
#' * WK_TBL_CAT (see [FPL Item 9](https://www.skybrary.aero/articles/flight-plan-completion)): wake turbulence category, can be
#'   - `L` LIGHT, i.e. maximum certificated takeoff mass of 7000 kg (15_500 lbs) or less.
#'   - `M` MEDIUM, i.e maximum certificated takeoff mass less than 136_000 kg (300_000 lbs),
#'     but more than 7_000 kg (15_500 lbs)
#'   - `H` HEAVY, i.e. maximum certificated takeoff mass of 136_000 kg (300_000 lbs) or more
#'     (except those specified as `J`)
#'   - `J` SUPER, presently the only the AIRBUS A-380-800
#'
#' * AIRCRAFT_OPERATOR: the [ICAO Airline Designator](https://en.wikipedia.org/wiki/List_of_airline_codes),
#'   i.e. `OAL` for `Olympic`
#' * AIRCRAFT_ADDRESS: the [ICAO 24-bit address](https://en.wikipedia.org/wiki/Aviation_transponder_interrogation_modes#ICAO_24-bit_address)
#'   of the airframe for ADS-B/Mode S broadcasting.
#' * ADEP: [ICAO code](https://observablehq.com/@openaviation/airports) of the **A**erodrome of **DEP**arture
#' * NAME_ADEP: the (AIU) name of the departing airport
#' * COUNTRY_CODE_ADEP: the ISO 2-alpha country code for the ADEP
#' * COUNTRY_NAME_ADEP: the country name for the ADEP
#' * ADES: [ICAO code](https://observablehq.com/@openaviation/airports) of the **A**erodrome of **DES**tination
#' * NAME_ADES: the (AIU) name of the airport
#' * COUNTRY_CODE_ADES: the ISO 2-alpha country code for the ADES
#' * COUNTRY_NAME_ADES: the country name for the ADES
#' * ID: the so called `SAM ID`, used internally by PRISME
#' * EOBT_1: **E**stimated **O**ff-**B**lock **T**ime for FPL-based (M1) trajectory
#' * ARVT_1: **AR**ri**V**al **T**ime for FPL-based (M1) trajectory
#' * TAXI_TIME_1: Taxi time for FPL-based (M1) trajectory
#' * AOBT_3: **A**ctual **O**ff-**B**lock **T**ime for flown (M3) trajectory
#' * ARVT_3: **ARV**ival **T**ime for flown (M3) trajectory
#' * TAXI_TIME_3: Taxi time for flown (M3) trajectory
#' * RULE_NAME: market segment type as defined on the
#'   [Market Segment Rules](https://www.eurocontrol.int/publication/market-segment-rules),
#'   it can be:
#'   - “Mainline”
#'   - “Regional”
#'   - “Low-Cost”
#'   - “Business Aviation”
#'   - “All-Cargo”
#'   - “Charter” (Non-Scheduled)
#'   - “Military”
#'   - “Other”
#'
#' * AO_GRP_CODE: Aircraft Operator group (code), i.e.
#' * AO_GRP_NAME: : Aircraft Operator group (name), i.e. AEGEAN Group
#' * RTE_LEN_1: route length (in Nautical Miles) for FPL-based (M1) trajectory
#' * RTE_LEN_3: route length (in Nautical Miles) for for flown (M3) trajectory
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my_flts <- flights_tidy(wef = "2023-01-01", til = "2023-04-01")
#' }
flights_tidy <- function(conn = NULL, wef, til) {

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
    "NAME_ADEP",
    "COUNTRY_CODE_ADEP",
    "COUNTRY_NAME_ADEP",
    "ADES",
    "NAME_ADES",
    "COUNTRY_CODE_ADES",
    "COUNTRY_NAME_ADES",
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
    "AO_ISO_CTRY_CODE",
    # route length
    "RTE_LEN_1",
    "RTE_LEN_3",
    NULL)

  if (is.null(conn)) {
    conn <- db_connection(schema = "PRU_DEV")
  }

  flt <- flights_tbl(conn)
  frl <- dplyr::tbl(conn, dbplyr::in_schema("SWH_FCT", "DIM_FLIGHT_TYPE_RULE"))
  aog <- dplyr::tbl(conn, dbplyr::in_schema("PRUDEV", "V_COVID_DIM_AO"))
  apt <- dplyr::tbl(conn, dbplyr::in_schema("PRUDEV", "V_COVID_REL_AIRPORT_AREA"))
  # |>
  #   dplyr::select(APT_CODE = CFMU_AP_CODE,
  #          APT_NAME = PRU_DASHBOARD_AP_NAME,
  #          COUNTRY_CODE,
  #          COUNTRY_NAME)

  wef <- lubridate::as_datetime(wef, tz = "UTC") |> format("%Y-%m-%d %H:%M:%S")
  til <- lubridate::as_datetime(til, tz = "UTC") |> format("%Y-%m-%d %H:%M:%S")

  fff <- flt |>
    dplyr::filter(
      TO_DATE(wef, "yyyy-mm-dd hh24:mi:ss") <= .data$LOBT,
      .data$LOBT < TO_DATE(til, "yyyy-mm-dd hh24:mi:ss"),
      # only commercial flights (scheduled and non-scheduled), i.e. General Aviation, Military and Other excluded
      .data$ICAO_FLT_TYPE %in% c('S', 'N'),
      # make sure military flights are excluded
      .data$SK_FLT_TYPE_RULE_ID != 1L,
      # exclude sensitive flights
      .data$SENSITIVE != 'Y',
      # exclude "Head of State" flights, might be redundant with the "SENSITIVE" flag
      .data$EXMP_RSN_LH != 'HEAD'
      ) |>
    dplyr::left_join(frl, by = "SK_FLT_TYPE_RULE_ID") |>
    dplyr::left_join(aog, by = c("AIRCRAFT_OPERATOR" = "AO_CODE")) |>
    dplyr::left_join(apt, by = c("ADEP" = "CFMU_AP_CODE")) |>
    dplyr::rename(
      NAME_ADEP = .data$PRU_DASHBOARD_AP_NAME,
      COUNTRY_CODE_ADEP = .data$COUNTRY_CODE,
      COUNTRY_NAME_ADEP = .data$COUNTRY_NAME) |>
    dplyr::left_join(apt, by = c("ADES" = "CFMU_AP_CODE")) |>
    dplyr::rename(
      NAME_ADES = .data$PRU_DASHBOARD_AP_NAME,
      COUNTRY_CODE_ADES = .data$COUNTRY_CODE,
      COUNTRY_NAME_ADES = .data$COUNTRY_NAME) |>
    dplyr::select(dplyr::all_of(columns))
  fff
}







#' Airline info including group affiliation
#'
#' # Note
#' You need to either provide a connection `conn` that has access to `PRUDEV.V_COVID_DIM_AO`
#' or go with the default which uses PRU_DEV to establish a [db_connection()].
#'
#' @param conn Optional connection to the PRU_DEV schema.
#'
#' @return A data frame with airline info such as:
#'
#' * `AO_CODE`: the the [ICAO Airline Designator](https://en.wikipedia.org/wiki/List_of_airline_codes), i.e. OAL
#' * `AO_NAME`: the airline's name, i.e. Olympic
#' * `AO_GRP_CODE`: the airline's affiliation group code, i.e.
#' * `AO_GRP_NAME`: the airline's affiliation group, i.e. AEGEAN Group
#' * `AO_ISO_CTRY_CODE`: the ISO2C code of the airline's country, i.e. GR,
#' * `EU`: whether the airlines is in a EUROCONTROL's Member State (full, comprehensive or
#'         transition plus Kosovo), i.e. TRUE.
#' @export
#'
#' @examples
#' \dontrun{
#' airlines_tidy()
#' }
airlines_tidy <- function(conn = NULL) {
  if (is.null(conn)) {
    conn <- db_connection(schema = "PRU_DEV")
  }

  arl <- dplyr::tbl(conn, dbplyr::in_schema("PRUDEV", "V_COVID_DIM_AO")) |>
    dplyr::select(
      "AO_CODE",
      "AO_NAME",
      "AO_GRP_CODE",
      "AO_GRP_NAME",
      "AO_ISO_CTRY_CODE") |>
    dplyr::collect()


  grp_codes <- arl |> dplyr::pull(.data$AO_CODE)

  arl_non_grp <- arl |>
    dplyr::filter(!.data$AO_CODE %in% grp_codes) |>
    dplyr::distinct(.data$AO_CODE, .keep_all = TRUE)

  ect <- eurocontrol::member_state |> dplyr::pull(.data$iso2c)

  arl <- arl_non_grp |>
    dplyr::bind_rows(arl) |>
    dplyr::mutate(EU = dplyr::if_else(.data$AO_ISO_CTRY_CODE %in% ect, TRUE, FALSE))

  arl
}
