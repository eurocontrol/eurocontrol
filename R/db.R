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

  USR <- Sys.getenv(paste0(schema, "_USR"))
  PWD <- Sys.getenv(paste0(schema, "_PWD"))
  DBN <- Sys.getenv(paste0(schema, "_DBNAME"))

  withr::local_envvar(c(
    "TZ" = "UTC",
    "ORA_SDTZ" = "UTC",
    "NLS_LANG" = ".AL32UTF8"
  ))
  con <- DBI::dbConnect(
    drv = DBI::dbDriver("Oracle"),
    username = USR,
    password = PWD,
    dbname = DBN,
    timezone = "UTC"
  )

  con
}


#' Return a reference to the Airlines table
#'
#' @description
#' The returned [dplyr::tbl()] is referencing the airlines table in PRISME.
#' You can use `dplyr`/`dbplyr` verbs to filter, join, ... with other
#' datasets.
#'
#' **NOTE**: you need access to PRU_DEV schema
#'
#' @param con DB connection to be used, if NULL instantiate one.
#'
#' @return a tbl referencing the Oracle table for airlines
#' @export
#'
#' @examples
#' \dontrun{
#' arl <- airlines_tbl()
#' }
airlines_tbl <- function(con = NULL) {
  if (is.null(con)) {
    con <- db_connection(schema = "PRU_DEV")
  }
  arl <- dplyr::tbl(con, "V_COVID_DIM_AO")
  arl
}


#' Return a reference to the Flights table
#'
#' @description
#' The returned [dplyr::tbl()] is referencing the flights table in PRISME.
#' You can use `dplyr`/`dbplyr` verbs to filter, join, ... with other
#' datasets.
#'
#' **NOTE**: you need access to PRU_DEV schema
#'
#' @inheritParams airlines_tbl
#'
#' @return a tbl referencing the Oracle table for flight
#' @export
#'
#' @examples
#' \dontrun{
#' arl <- flights_tbl()
#' }
#'
flights_tbl <- function(con = NULL) {
  if (is.null(con)) {
    con <- db_connection(schema = "PRU_DEV")
  }
  flt <- dplyr::tbl(con, dbplyr::in_schema("SWH_FCT", "FAC_FLIGHT"))
  flt
}



#' Return a reference to the Airspace Profile table
#'
#' @description
#' The returned [dplyr::tbl()] is referencing the airspace profiles table in PRISME.
#' You can use `dplyr`/`dbplyr` verbs to filter, join, ... with other
#' datasets.
#'
#' **NOTE**: you need access to PRU_DEV schema
#'
#' @inheritParams airlines_tbl
#'
#' @return a tbl referencing the Oracle table for airspace profiles
#' @export
#'
#' @examples
#' \dontrun{
#' prf <- airspace_profile_tbl()
#' }
airspace_profile_tbl <- function(con = NULL) {
  if (is.null(con)) {
    con <- db_connection(schema = "PRU_DEV")
  }
  prof <- dplyr::tbl(con, dbplyr::in_schema("FSD", "ALL_FT_ASP_PROFILE"))

  prof
}


#' Extract a clean flights list in an interval
#'
#' @description
#' The returned [tbl] includes scheduled and non-scheduled flight
#' in the right-opened interval `[wef, til)`.
#' General aviation, State, military and sensitive flight are excluded.
#'
#' **NOTE**: you need access to PRU_DEV schema
#'
#' @inheritParams airspace_profiles_tidy
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
#' * ID: the so called SAM ID, used internally by PRISME
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
flights_tidy <- function(con = NULL, wef, til) {

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

  if (is.null(con)) {
    con <- db_connection(schema = "PRU_DEV")
  }

  flt <- flights_tbl(con)
  frl <- dplyr::tbl(con, dbplyr::in_schema("SWH_FCT", "DIM_FLIGHT_TYPE_RULE"))
  aog <- dplyr::tbl(con, dbplyr::in_schema("PRUDEV", "V_COVID_DIM_AO"))

  wef <- lubridate::as_datetime(wef, tz = "UTC") |> format("%Y-%m-%d %H:%M:%S")
  til <- lubridate::as_datetime(til, tz = "UTC") |> format("%Y-%m-%d %H:%M:%S")

  fff <- flt |>
    dplyr::filter(
      to_date(wef, "yyyy-mm-dd hh24:mi:ss") <= .data$LOBT,
      .data$LOBT < to_date(til, "yyyy-mm-dd hh24:mi:ss"),
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
    dplyr::select(dplyr::all_of(columns))
  fff
}


#' Provide all airspace profile segments intersecting an interval of interest
#'
#' @description
#' The returned [tbl] includes segments for scheduled and non-scheduled flights
#' temporally intersecting the right-opened interval `[wef, til)`.
#' General aviation, State, military and sensitive flight are excluded.
#'
#' **NOTE**: you need access to PRU_DEV schema
#'
#' @inheritParams airlines_tbl
#'
#' @param wef **W**ith **EF**fect date (included) at Zulu time
#'            in a format recognized by [lubridate::as_datetime()]
#' @param til u**TIL**l date (excluded) at Zulu time
#'            in a format recognized by [lubridate::as_datetime()]
#'
#' @param airspace the type of airspace (default: 'FIR'), one of:
#'  * 'FIR' ([Flight Information Region](https://observablehq.com/@openaviation/flight-information-regions))
#'  * 'NAS' (National Airspace)
#'  * 'AUA' (ATC Unit Airspace)
#'  * 'ES' (Elementary Sector)
#'
#' @param profile the [model of the trajectory](https://ansperformance.eu/definition/flight-models/)
#'  profile (default: 'CTFM'), one of:
#'  * 'FTFM'
#'  * 'RTFM'
#'  * 'CTFM'
#'  * 'CPF'
#'  * 'DCT'
#'  * 'SCR'
#'  * 'SRR'
#'  * 'SUR'
#'
#' @return a [tbl] with the following columns
#'
#' * ID: the so called SAM ID, used internally by PRISME
#' * SEQ_ID: the sequence number of the segment for the relevant airspace profile
#' * ENTRY_TIME: the time of entry into the relevant airspace
#' * ENTRY_LON:  the longitude of entry into the relevant airspace
#' * ENTRY_LAT: the latitude of entry into the relevant airspace
#' * ENTRY_FL: the flight level of entry into the relevant airspace
#' * EXIT_TIME: the time of exit out of the relevant airspace
#' * EXIT_LON: the longitude of exit out of the relevant airspace
#' * EXIT_LAT: the latitude of exit out of the relevant airspace
#' * EXIT_FL: the flight level of exit out of the relevant airspace
#' * AIRSPACE_ID: the airspace ID
#' * AIRSPACE_TYPE: the airspace type as per `airspace` input parameter
#' * MODEL_TYPE: the trajectory model as per `profile` input parameter
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' asp_profs <- airspace_profiles_tidy(con = NULL, wef = "2023-01-01", til = "2023-04-01")
#' }
airspace_profiles_tidy <- function(con = NULL, wef, til, airspace = "FIR", profile = "CTFM") {

  # magic numbers: tables are indexed on LOBT, but LOBT is not precise to
  #                capture actual flight events, so we need some buffers.
  before_hours <- 28
  after_hours  <- 24

  wef_before <- (lubridate::as_datetime(wef) - lubridate::dhours(before_hours))|>
    format("%Y-%m-%d %H:%M:%S")
  til_after  <- (lubridate::as_date(til)     + lubridate::dhours(after_hours)) |>
    format("%Y-%m-%d %H:%M:%S")

  wef <- lubridate::as_datetime(wef) |> format("%Y-%m-%d %H:%M:%S")
  til <- lubridate::as_datetime(til) |> format("%Y-%m-%d %H:%M:%S")

  if (is.null(con)) {
    con <- db_connection(schema = "PRU_DEV")
  }

  flt <- flights_tidy(con = con, wef = wef_before, til = til_after)
  ids <- flt |> dplyr::select(ID) |> dplyr::distinct()

  prf <- airspace_profile_tbl(con = con) |>
    dplyr::filter(
      to_date(wef_before, "yyyy-mm-dd hh24:mi:ss") <= LOBT,
      LOBT < to_date(til_after, "yyyy-mm-dd hh24:mi:ss"),
      MODEL_TYPE %in% profile,
      AIRSPACE_TYPE == airspace,
      # entry
      !is.na(ENTRY_LON),
      !is.na(ENTRY_LAT),
      !is.na(ENTRY_TIME),
      !is.na(ENTRY_FL),
      # exit
      !is.na(EXIT_LON),
      !is.na(EXIT_LAT),
      !is.na(EXIT_TIME),
      !is.na(EXIT_FL),
      # consider only the segments intersecting the [wef, til)
      ENTRY_TIME <= to_date(til, "yyyy-mm-dd hh24:mi:ss"),
      to_date(wef, "yyyy-mm-dd hh24:mi:ss") < EXIT_TIME
    )

  prf <- prf |>
    # dplyr::inner_join(flt, sql_on = "LHS.SAM_ID = RHS.ID AND LHS.LOBT = LHS.LOBT") |>
    dplyr::inner_join(ids, by = c("SAM_ID" = "ID")) |>
    dplyr::select(
      ID = SAM_ID,
      SEQ_ID,
      ENTRY_TIME, ENTRY_LON, ENTRY_LAT, ENTRY_FL,
      EXIT_TIME, EXIT_LON, EXIT_LAT, EXIT_FL,
      AIRSPACE_ID, AIRSPACE_TYPE, MODEL_TYPE)
  prf
}


#' Extract the flights list for the airspace profile segments intersecting an interval of interest
#'
#' @description
#' The returned [tbl] includes scheduled and non-scheduled flights whose airspace segments
#' temporally intersecting the right-opened interval `[wef, til)`.
#' General aviation, State, military and sensitive flight are excluded.
#'
#' **NOTE**: you need access to PRU_DEV schema
#'
#' @inheritParams airspace_profiles_tidy
#'
#' @return a [tbl] with the same columns as [flights_tidy]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' asp_profs <- flights_airspace_profiles_tidy(wef = "2023-01-01", til = "2023-04-01")
#' }
flights_airspace_profiles_tidy <- function(con = NULL, wef, til, airspace = "FIR", profile = "CTFM") {

  # magic numbers: tables are indexed on LOBT, but LOBT is not precise to
  #                capture actual flight events, so we need some buffers.
  before_hours <- 28
  after_hours  <- 24

  wef_before <- (lubridate::as_datetime(wef) - lubridate::dhours(before_hours))|>
    format("%Y-%m-%d %H:%M:%S")
  til_after  <- (lubridate::as_date(til)     + lubridate::dhours(after_hours)) |>
    format("%Y-%m-%d %H:%M:%S")

  prf <- airspace_profiles_tidy(con = con, wef, til, airspace = airspace, profile = profile)
  ids <- prf |> dplyr::select(ID) |> dplyr::distinct()

  # reuse the same DB connection as per the flights
  con <- prf$src$con

  flt <- flights_tidy(con = con, wef = wef_before, til = til_after)
  cols <- colnames(flt)

  flt <- flt |>
    # dplyr::inner_join(flt, sql_on = "LHS.SAM_ID = RHS.ID AND LHS.LOBT = LHS.LOBT") |>
    # dplyr::inner_join(prf, by = c("ID" = "ID"))
    dplyr::inner_join(prf, by = c("ID" = "ID")) |>
    select(cols) |>
    distinct()

  flt
}



#' Airline info including group affiliation
#'
#' A data frame with airline info such as:
#'
#'  * `operator_code`: the airline's ICAO code, i.e. OAL,
#'  * `operator_name`: the airline's name, i.e. Olympic,
#'  * `operator_group`: the airline's affiliation group, i.e. AEGEAN Group,
#'  * `iso2c`: the ISO2C code of the airline's country, i.e. GR,
#'  * `eu`: whether the airlines is in a EUROCONTROL's Member State (plus Kosovo), i.e. TRUE.
#'
#' @param con Optional connection to the PRU_DEV schema.
#'
#' @return A data frame with airline info such as:
#'         * `operator_code`: the airline's ICAO code, i.e. OAL,
#'         * `operator_name`: the airline's name, i.e. Olympic,
#'         * `operator_group`: the airline's affiliation group, i.e. AEGEAN Group,
#'         * `iso2c`: the ISO2C code of the airline's country, i.e. GR,
#'         * `eu`: whether the airlines is in a EUROCONTROL's Member State (plus Kosovo), i.e. TRUE.
#' @export
#'
#' @examples
#' \dontrun{
#' airlines_tidy()
#' }
airlines_tidy <- function(con = NULL) {
  if (is.null(con)) {
    con <- db_connection(schema = "PRU_DEV")
  }

  query <- "SELECT * FROM V_COVID_DIM_AO"

  arl <- dplyr::tbl(con, dplyr::sql(query)) |>
    dplyr::select(
      operator_code  = .data$AO_CODE,
      operator_name  = .data$AO_NAME,
      operator_group = .data$AO_GRP_NAME,
      iso2c          = .data$AO_ISO_CTRY_CODE) |>
    dplyr::collect()


  arl_grp <- arl |>
    dplyr::distinct(operator_code, .keep_all = TRUE)


  grp_codes <- arl_grp |> dplyr::pull(operator_code)

  arl_non_grp <- arl |>
    dplyr::filter(!operator_code %in% grp_codes) |>
    dplyr::distinct(operator_code, .keep_all = TRUE)

  ect <- member_states |> dplyr::pull(iso2c)

  arl <- arl_non_grp |>
    dplyr::bind_rows(arl_grp) |>
    dplyr::mutate(eu = dplyr::if_else(iso2c %in% ect, TRUE, FALSE))

  arl
}
