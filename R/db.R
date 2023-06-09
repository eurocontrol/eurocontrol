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
#' The returned [tbl] is referencing the airlines table in PRISME.
#' You can use `dplyr`/`dbplyr` verbs to filter, join, ... with other
#' datasets.
#'
#' # Note
#' You need to either provide a connection `con`
#' that has access to `PRU_DEV.V_COVID_DIM_AO` or go with the
#' default which uses PRU_DEV to establish a [db_connection()].
#'
#' @param con Database connection or instantiate the default one.
#'
#' @return a [tbl] referencing the Oracle table for airlines.
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
#' The returned [tbl] is referencing the flights table in PRISME.
#' You can use `dplyr`/`dbplyr` verbs to filter, join, ... with other
#' datasets.
#'
#' # Note
#' You need to either provide a connection `con` that has access to `SWH_FCT.FAC_FLIGHT` or
#' go with the default which uses PRU_DEV to establish a [db_connection()].
#'
#' @inheritParams airlines_tbl
#'
#' @return a [tbl] referencing the Oracle table for flights.
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
#' The returned [tbl] is referencing the airspace profiles table in PRISME.
#' You can use `dplyr`/`dbplyr` verbs to filter, join, ... with other
#' datasets.
#'
#' # Note
#' You need to either provide a connection `con` that has access to `FSD.ALL_FT_ASP_PROFILE` or
#' go with the default which uses PRU_DEV to establish a [db_connection()].
#'
#' @inheritParams airlines_tbl
#'
#' @return a [tbl] referencing the Oracle table for airspace profiles.
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
#' departing in the right-opened interval `[wef, til)`.
#'
#' General aviation, State, military and sensitive flight are excluded.
#'
#' # Note
#' You need to either provide a connection `con` that has access to `SWH_FCT.DIM_FLIGHT_TYPE_RULE`,
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
#' @return A [tbl] with the following columns:
#'
#' * FLT_UID: the flight unique id.
#' * LOBT: **L**ast received **O**ff-**B**lock **T**ime.
#' * IOBT: **I**nitial **O**ff-**B**lock **T**ime.
#' * AIRCRAFT_ID: the [callsign](https://www.skybrary.aero/articles/aircraft-call-sign)
#'                of the relevant flight, e.g. BAW6VB.
#' * REGISTRATION: the [aircraft registration](https://en.wikipedia.org/wiki/Aircraft_registration)
#'   (with spaces, dashes, ... stripped), e.g. GEUUU.
#' * AIRCRAFT_TYPE_ICAO_ID: the [ICAO code for the aircraft type](), for example A30B for an
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
#'                     of the airframe for ADS-B/Mode S broadcasting
#' * ADEP: ([ICAO code](https://observablehq.com/@openaviation/airports) of the) **A**erodrome of **DEP**arture
#' * ADES: ([ICAO code](https://observablehq.com/@openaviation/airports) of the) **A**erodrome of **DES**tination
#' * ID: the so called `SAM ID`, used internally by PRISME
#' * EOBT_1: **E**stimated **O**ff-**B**lock **T**ime for FPL-based (M1) trajectory
#' * ARVT_1: **AR**ri**V**al **T**ime for FPL-based (M1) trajectory
#' * TAXI_TIME_1: Taxi time for FPL-based (M1) trajectory
#' * AOBT_3: **A**ctual **O**ff-**B**lock **T**ime for flown (M3) trajectory
#' * ARVT_3: **ARV**ival **T**ime for flown (M3) trajectory
#' * TAXI_TIME_3: Taxi time for flown (M3) trajectory
#' * RULE_NAME: market segment type as defined on the
#'              [Market Segment Rules](https://www.eurocontrol.int/publication/market-segment-rules),
#'              it can be:
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
#'
#' General aviation, State, military and sensitive flight are excluded.
#'
#' # Note
#' You need to either provide a connection `con` that has access to as noted in
#' [airspace_profile_tbl] and [flight_tidy] or go with the default which uses
#' PRU_DEV to establish a [db_connection()].
#'
#' @inheritParams flights_tidy
#'
#' @param airspace the type of airspace (default: 'FIR'), one of:
#'  * 'FIR' ([Flight Information Region](https://observablehq.com/@openaviation/flight-information-regions))
#'  * 'NAS' (National Airspace)
#'  * 'AUA' (ATC Unit Airspace)
#'  * 'ES' (Elementary Sector)
#'
#' @param profile the [model of the trajectory](https://ansperformance.eu/definition/flight-models/)
#'  profile (default: 'CTFM'), one of:
#'  * 'FTFM', Filed Tactical Flight Model
#'  * 'RTFM', Regulated Tactical Flight Model
#'  * 'CTFM', Current Tactical Flight Model
#'  * 'CPF', Correlated Position reports for a Flight
#'  * 'DCT', Direct route
#'  * 'SCR', Shortest Constrained Route
#'  * 'SRR', Shortest RAD restrictions applied Route
#'  * 'SUR', Shortest Unconstrained Route
#'
#' @return a [tbl] with the following columns
#'
#' * ID: the so called `SAM ID`, used internally by PRISME
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
#' # Note
#' You need to either provide a connection `con` that has access to as noted in
#' [airspace_profile_tbl] and [flight_tidy] or go with the default which uses
#' PRU_DEV to establish a [db_connection()].
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
#' # Note
#' You need to either provide a connection `con` that has access to `PRUDEV.V_COVID_DIM_AO`
#' or go with the default which uses PRU_DEV to establish a [db_connection()].
#'
#' @param con Optional connection to the PRU_DEV schema.
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
airlines_tidy <- function(con = NULL) {
  if (is.null(con)) {
    con <- db_connection(schema = "PRU_DEV")
  }

  arl <- dplyr::tbl(con, dbplyr::in_schema("PRUDEV", "V_COVID_DIM_AO")) |>
    dplyr::select(
      "AO_CODE",
      "AO_NAME",
      "AO_GRP_CODE",
      "AO_GRP_NAME",
      "AO_ISO_CTRY_CODE") |>
    dplyr::collect()


  arl_grp <- arl |>
    dplyr::distinct(operator_code, .keep_all = TRUE)


  grp_codes <- arl_grp |> dplyr::pull(operator_code)

  arl_non_grp <- arl |>
    dplyr::filter(!operator_code %in% grp_codes) |>
    dplyr::distinct(operator_code, .keep_all = TRUE)

  ect <- eurocontrol::member_state |> dplyr::pull(iso2c)

  arl <- arl_non_grp |>
    dplyr::bind_rows(arl_grp) |>
    dplyr::mutate(EU = dplyr::if_else(iso2c %in% ect, TRUE, FALSE))

  arl
}
