#' Return a reference to the Flights table
#'
#' @description
#' The returned [dplyr::tbl()] is referencing the flights table in PRISME.
#' You can use `dplyr`/`dbplyr` verbs to filter, join, ... with other
#' datasets.
#'
#' # Note
#' You need to either provide a connection `conn` that has access to
#' `SWH_FCT.V_FAC_FLIGHT_MS` or
#' go with the default which uses PRU_DEV to establish a [db_connection()].
#' Market Segment is not available before 2004.
#'
#' @inheritParams airlines_tbl
#'
#' @return a [dplyr::tbl()] referencing the Oracle table for flights.
#' @export
#'
#' @examples
#' \dontrun{
#' flt <- flights_tbl()
#' # other operations on flt, i.e. filtering,
#' # followed by a collect() to retrieve the concrete data frame
#' flt_filtered <- flt |>
#'    filter(TO_DATE("2023-06-01 10:00", "YYYY-MM-DD HH24:MI") <= IOBT,
#'                   IOBT < TO_DATE("2023-06-02 10:30", "YYYY-MM-DD HH24:MI")) |>
#'    collect()
#'
#'
#' # NOTE: you can reuse the connection for other API calls
#' conn <- flt$src$con
#'
#' # other ops requiring conn
#' # ...
#'
#' # IMPORTANT: close the DB connection
#' DBI::dbDisconnect(conn)
#' }
#'
flights_tbl <- function(conn = NULL) {
  if (is.null(conn)) {
    conn <- db_connection(schema = "PRU_DEV")
  }
  flt <- dplyr::tbl(conn, dbplyr::in_schema("SWH_FCT", "V_FAC_FLIGHT_MS"))
  flt
}





#' Extract a clean flights list in an interval
#'
#' @description
#' The returned [dplyr::tbl()] includes scheduled and non-scheduled flight
#' departing in the right-opened interval `[wef, til)`.
#'
#' Defaults values will assure that General aviation, State, military and
#' sensitive flight will excluded.
#' They can be retrieved via the other function call arguments in case of need.
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
#' @param icao_flt_types the types of flights as described below in
#'                       `ICAO_FLT_TYPE`, default `c('S', 'N')`, NULL includes all
#'                       notwithstanding other argument options.
#'                       When including military via `include_military` you should
#'                       either pass NULL or make sure 'M' is included
#' @param ids list of `ID`s (aka `SAM ID`) to return, default NULL for all flights
#' @param include_sensitive include sensitive flights, default FALSE
#' @param include_military include military flights, default FALSE
#' @param include_head include Head of State flights, default FALSE
#'
#' @return A [dplyr::tbl()] with the following columns (grouped here by
#'         flight details, aircraft info, aircraft operator info and operational
#'         details):
#' ## Flight details
#' * FLT_UID: flight unique id.
#' * ID: the so called `SAM ID`, used internally by PRISME
#' * AIRCRAFT_ID: the [callsign](https://www.skybrary.aero/articles/aircraft-call-sign)
#'                of the relevant flight, e.g. BAW6VB.
#' * LOBT: **L**ast received **O**ff-**B**lock **T**ime.
#' * IOBT: **I**nitial **O**ff-**B**lock **T**ime.
#' * FLT_RULES (see [FPL Item 8](https://www.skybrary.aero/articles/flight-plan-completion)):
#'   which sets of regulations the flight is operated under.
#'   Possible values are:
#'   - `I` for IFR
#'   - `V` for VFR
#'   - `Y` first IFR thereafter VFR
#'   - `Z` first VFR thereafter IFR
#' * ICAO_FLT_TYPE (see [FPL Item 8](https://www.skybrary.aero/articles/flight-plan-completion)):
#'   flight type. Possible values:
#'   - `S` for scheduled air service
#'   - `N` for non-scheduled air service
#'   - `G` for general aviation
#'   - `M` for military (note: filtered out)
#'   - `X` for other than the preceding categories
#' * ADEP: [ICAO code](https://observablehq.com/@openaviation/airports) of the **A**erodrome of **DEP**arture
#' * NAME_ADEP: the (AIU) name of the departing airport
#' * COUNTRY_CODE_ADEP: the ISO 2-alpha country code for the ADEP
#' * COUNTRY_NAME_ADEP: the country name for the ADEP
#' * ADES: [ICAO code](https://observablehq.com/@openaviation/airports) of the **A**erodrome of **DES**tination
#' * NAME_ADES: the (AIU) name of the airport
#' * COUNTRY_CODE_ADES: the ISO 2-alpha country code for the ADES
#' * COUNTRY_NAME_ADES: the country name for the ADES
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
#'   - "Not classified"
#' * SENSITIVE: 'Y' if sensitive
#' * SPECIAL_EXEMPT: reasons for special handling by ATS.
#'   One of:
#'   - "AEAP" ATFM exemption approved
#'   - "EMER" emergency
#'   - "FIRE" fire fighting
#'   - "HEAD" flights with Head of State status
#'   - "MEDE" medical evacuation
#'   - "NEXE" not exempted
#'   - "SERE" search & rescue
#'
#' ## Aircraft info
#' * REGISTRATION: the [aircraft registration](https://en.wikipedia.org/wiki/Aircraft_registration)
#'   (with spaces, dashes, ... stripped), e.g. GEUUU.
#' * AIRCRAFT_ADDRESS: the [ICAO 24-bit address](https://en.wikipedia.org/wiki/Aviation_transponder_interrogation_modes#ICAO_24-bit_address)
#'   of the airframe for ADS-B/Mode S broadcasting.
#' * AIRCRAFT_TYPE_ICAO_ID: the [ICAO code for the aircraft type](https://www.icao.int/publications/doc8643/pages/search.aspx), for example A30B for an
#'   Airbus A-300B2-200.
#' * WK_TBL_CAT (see [FPL Item 9](https://www.skybrary.aero/articles/flight-plan-completion)): wake turbulence category, can be
#'   - `L` LIGHT, i.e. maximum certificated takeoff mass of 7000 kg (15_500 lbs) or less.
#'   - `M` MEDIUM, i.e maximum certificated takeoff mass less than 136_000 kg (300_000 lbs),
#'     but more than 7_000 kg (15_500 lbs)
#'   - `H` HEAVY, i.e. maximum certificated takeoff mass of 136_000 kg (300_000 lbs) or more
#'     (except those specified as `J`)
#'   - `J` SUPER, presently the only the AIRBUS A-380-800
#'
#' ## Aircraft operator details
#' * AIRCRAFT_OPERATOR: the [ICAO Airline Designator](https://en.wikipedia.org/wiki/List_of_airline_codes),
#'   i.e. `OAL` for `Olympic`
#' * AO_GRP_CODE: Aircraft Operator group (code), i.e. AEE_GRP
#' * AO_GRP_NAME: : Aircraft Operator group (name), i.e. AEGEAN Group
#' * AO_ISO_CTRY_CODE: ISO country code for AO
#'
#' ## Operational details
#' * EOBT_1: **E**stimated **O**ff-**B**lock **T**ime for FPL-based (M1) trajectory
#' * ARVT_1: **AR**ri**V**al **T**ime for FPL-based (M1) trajectory
#' * TAXI_TIME_1: Taxi time for FPL-based (M1) trajectory
#' * AOBT_3: **A**ctual **O**ff-**B**lock **T**ime for flown (M3) trajectory
#' * ARVT_3: **ARV**ival **T**ime for flown (M3) trajectory
#' * TAXI_TIME_3: Taxi time for flown (M3) trajectory
#' * RTE_LEN_1: route length (in Nautical Miles) for FPL-based (M1) trajectory
#' * RTE_LEN_3: route length (in Nautical Miles) for for flown (M3) trajectory
#' * FLT_DUR_1: route duration (in minutes) for FPL-based (M1) trajectory
#' * FLT_DUR_3: route length (in minutes) for flown (M3) trajectory
#' * FLT_TOW: takeoff weight
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flts <- flights_tidy(wef = "2023-01-01", til = "2023-01-05")
#' # other operations on flts, i.e. filtering,
#' # followed by a collect() to retrieve the concrete data frame
#' flts_filtered <- flts |>
#'    filter(TO_DATE("2023-06-01 10:00", "YYYY-MM-DD HH24:MI") <= IOBT,
#'                   IOBT < TO_DATE("2023-01-02 10:30", "YYYY-MM-DD HH24:MI")) |>
#'    collect()
#'
#' # NOTE: you can reuse the connection for other API calls
#' conn <- flts$src$con
#'
#' # other ops requiring conn
#' # ...
#'
#' # IMPORTANT: close the DB connection
#' DBI::dbDisconnect(conn)
#' }
flights_tidy <- function(
    conn = NULL,
    wef, til,
    icao_flt_types = c('S', 'N'),
    ids = NULL,
    include_sensitive = FALSE,
    include_military = FALSE,
    include_head = FALSE
    ) {

  columns <- c(
    #-- flight
    "ID",
    "LOBT",
    "AIRCRAFT_ID",
    "ADEP",
    "NAME_ADEP",
    "COUNTRY_CODE_ADEP",
    "COUNTRY_NAME_ADEP",
    "ADES",
    "NAME_ADES",
    "COUNTRY_CODE_ADES",
    "COUNTRY_NAME_ADES",
    "SENSITIVE",
    "SPECIAL_EXEMPT",
    "RULE_NAME", # Market Segment
    "FLT_UID",
    "IOBT",
    "FLT_RULES",
    "ICAO_FLT_TYPE",
    #-- aircraft
    "REGISTRATION",
    "AIRCRAFT_ADDRESS",
    "AIRCRAFT_TYPE_ICAO_ID",
    "WK_TBL_CAT",
    #-- aircraft operator
    "AIRCRAFT_OPERATOR",
    "AO_ISO_CTRY_CODE",
    "AO_GRP_CODE",
    "AO_GRP_NAME",
    #-- operational details
    "EOBT_1",
    "ARVT_1",
    "TAXI_TIME_1",
    "AOBT_3",
    "ARVT_3",
    "TAXI_TIME_3",
    "FLT_DUR_1",
    "FLT_DUR_3",
    "RTE_LEN_1",
    "RTE_LEN_3",
    "FLT_TOW",
    NULL)

  if (is.null(conn)) {
    conn <- db_connection(schema = "PRU_DEV")
  }

  flt <- flights_tbl(conn)
  frl <- dplyr::tbl(conn, dbplyr::in_schema("SWH_FCT", "DIM_FLIGHT_TYPE_RULE"))
  aog <- dplyr::tbl(conn, dbplyr::in_schema("PRUDEV", "V_COVID_DIM_AO"))
  apt <- dplyr::tbl(conn, dbplyr::in_schema("PRUDEV", "V_COVID_REL_AIRPORT_AREA"))

  wef <- lubridate::as_datetime(wef, tz = "UTC") |> format("%Y-%m-%d %H:%M:%S")
  til <- lubridate::as_datetime(til, tz = "UTC") |> format("%Y-%m-%d %H:%M:%S")

  fff <- flt |>
    dplyr::filter(
      TO_DATE(wef, "yyyy-mm-dd hh24:mi:ss") <= .data$LOBT,
      .data$LOBT < TO_DATE(til, "yyyy-mm-dd hh24:mi:ss")
    )

  if (!is.null(icao_flt_types)) {
    fff <- fff |>
      dplyr::filter(
        .data$ICAO_FLT_TYPE %in% icao_flt_types
      )
  }


  if (!is.null(ids)) {
    fff <- fff |>
      dplyr::filter(
        # include only certain IDs
        .data$ID %in% ids
      )
  }


  if (include_sensitive == FALSE) {
    fff <- fff |>
      dplyr::filter(
        # exclude sensitive flights
        .data$SENSITIVE != 'Y'
      )
  }

  if (include_military == FALSE) {
    fff <- fff |>
      dplyr::filter(
        # make sure military flights are excluded
        .data$SK_FLT_TYPE_RULE_ID != 1L
      )
  }

  if (include_head == FALSE) {
    fff <- fff |>
      dplyr::filter(
        # exclude "Head of State" flights, might be redundant with the "SENSITIVE" flag
        .data$EXMP_RSN_LH != 'HEAD'
      )
  }


  fff <- fff |>
    dplyr::left_join(frl, by = "SK_FLT_TYPE_RULE_ID") |>
    dplyr::left_join(aog, by = c("AIRCRAFT_OPERATOR" = "AO_CODE")) |>
    dplyr::left_join(apt, by = c("ADEP" = "CFMU_AP_CODE")) |>
    dplyr::rename(
      SPECIAL_EXEMPT = .data$EXMP_RSN_LH,
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
