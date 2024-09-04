#' Return a reference to the Airspace Profile table
#'
#' @description
#' The returned [dbplyr::tbl_dbi()] is referencing the airspace profiles table in PRISME.
#' You can use `dplyr`/`dbplyr` verbs to filter, join, ... with other
#' datasets.
#'
#' # Note
#' You need to either provide a connection `conn` that has access to `FSD.ALL_FT_ASP_PROFILE` or
#' go with the default which uses PRU_DEV to establish a [db_connection()].
#'
#' @inheritParams airlines_tbl
#'
#' @return a [dbplyr::tbl_dbi()] referencing the Oracle table for airspace profiles.
#' @export
#'
#' @examples
#' \dontrun{
#' pp <- airspace_profile_tbl()
#' # other operations on pp, i.e. filtering,
#' # followed by a collect() to retrieve the concrete data frame
#' # IMPORTANT: close the DB connection when done
#' DBI::dbDisconnect(pp$src$con)
#'
#' # if you use a DB connection for many different APIs
#' conn <- eurocontrol::db_connection("PRU_DEV")
#' pp <- airspace_profile_tbl(conn = conn)
#'
#' # ... do something else with conn
#' # ...
#' # then manually close the connection to the DB
#' DBI::dbDisconnect(conn)
#' }
airspace_profile_tbl <- function(conn = NULL) {
  if (is.null(conn)) {
    conn <- db_connection(schema = "PRU_DEV")
  }
  prof <- dplyr::tbl(conn, dbplyr::in_schema("FSD", "ALL_FT_ASP_PROFILE"))

  prof
}


#' Provide all airspace profile segments intersecting an interval of interest
#'
#' @description
#' The returned [dbplyr::tbl_dbi()] includes segments for scheduled and non-scheduled flights
#' temporally intersecting the right-opened interval `[wef, til)`.
#'
#' General aviation, State, military and sensitive flight are excluded.
#'
#' # Note
#' You need to either provide a connection `conn` that has access to as noted in
#' [airspace_profile_tbl()] and [flights_tidy()] or go with the default which uses
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
#' @return a [dbplyr::tbl_dbi()] with the following columns
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
#' ps <- airspace_profiles_tidy(wef = "2023-01-01", til = "2023-04-01")
#' # IMPORTANT: always close the DB connection when done
#' DBI::dbDisconnect(ps$src$con)
#'
#' # if you re-use DB connections
#' conn <- eurocontrol::db_connection("PRU_DEV")
#' ps <- airspace_profiles_tidy(conn = conn)
#'
#' # ... do something else with conn
#' # ...
#' # then manually close the connection to the DB
#' DBI::dbDisconnect(conn)
#' }
airspace_profiles_tidy <- function(conn = NULL, wef, til, airspace = "FIR", profile = "CTFM") {
  # magic numbers: tables are indexed on LOBT, but LOBT is not precise to
  #                capture actual flight events, so we need some buffers.
  before_hours <- 28
  after_hours <- 24

  wef <- lubridate::as_datetime(wef)
  til <- lubridate::as_datetime(til)


  wef_before <- (wef - lubridate::dhours(before_hours)) |>
    format("%Y-%m-%d %H:%M:%S")
  til_after <- (til + lubridate::dhours(after_hours)) |>
    format("%Y-%m-%d %H:%M:%S")

  wef <- wef |> format("%Y-%m-%d %H:%M:%S")
  til <- til |> format("%Y-%m-%d %H:%M:%S")

  if (is.null(conn)) {
    conn <- db_connection(schema = "PRU_DEV")
  }

  flt <- flights_tidy(conn = conn, wef = wef_before, til = til_after)
  ids <- flt |>
    dplyr::select(.data$ID) |>
    dplyr::distinct()

  prf <- airspace_profile_tbl(conn = conn) |>
    dplyr::filter(
      TO_DATE(wef_before, "yyyy-mm-dd hh24:mi:ss") <= .data$LOBT,
      .data$LOBT < TO_DATE(til_after, "yyyy-mm-dd hh24:mi:ss"),
      .data$MODEL_TYPE %in% profile,
      .data$AIRSPACE_TYPE == airspace,
      # entry
      !is.na(.data$ENTRY_LON),
      !is.na(.data$ENTRY_LAT),
      !is.na(.data$ENTRY_TIME),
      !is.na(.data$ENTRY_FL),
      # exit
      !is.na(.data$EXIT_LON),
      !is.na(.data$EXIT_LAT),
      !is.na(.data$EXIT_TIME),
      !is.na(.data$EXIT_FL),
      # consider only the segments intersecting the [wef, til)
      .data$ENTRY_TIME <= TO_DATE(til, "yyyy-mm-dd hh24:mi:ss"),
      TO_DATE(wef, "yyyy-mm-dd hh24:mi:ss") < .data$EXIT_TIME
    )

  prf <- prf |>
    # dplyr::inner_join(flt, sql_on = "LHS.SAM_ID = RHS.ID AND LHS.LOBT = LHS.LOBT") |>
    dplyr::inner_join(ids, by = c("SAM_ID" = "ID")) |>
    # dply::select(-ID) |>
    # dplyr::rename(
    #   ID = .data$SAM_ID
    # ) |>
    dplyr::select(
      .data$SAM_ID,
      .data$SEQ_ID,
      .data$ENTRY_TIME, .data$ENTRY_LON, .data$ENTRY_LAT, .data$ENTRY_FL,
      .data$EXIT_TIME, .data$EXIT_LON, .data$EXIT_LAT, .data$EXIT_FL,
      .data$AIRSPACE_ID, .data$AIRSPACE_TYPE, .data$MODEL_TYPE
    ) |>
    dplyr::rename(
      ID = .data$SAM_ID
    )

  prf
}



#' Extract the flights list for the airspace profile segments intersecting an interval of interest
#'
#' @description
#' The returned [dbplyr::tbl_dbi()] includes scheduled and non-scheduled flights whose airspace segments
#' temporally intersecting the right-opened interval `[wef, til)`.
#' General aviation, State, military and sensitive flight are excluded.
#'
#' # Note
#' You need to either provide a connection `conn` that has access to as noted in
#' [airspace_profile_tbl()] and [flights_tidy()] or go with the default which uses
#' PRU_DEV to establish a [db_connection()].
#'
#' @inheritParams airspace_profiles_tidy
#'
#' @return a [dbplyr::tbl_dbi()] with the same columns as [flights_tidy()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' aa <- flights_airspace_profiles_tidy(wef = "2023-01-01", til = "2023-04-01")
#'
#' # if you re-use DB connections
#' conn <- eurocontrol::db_connection("PRU_DEV")
#' flights_airspace_profiles_tidy(conn = conn,
#'                                wef = "2023-01-01",
#'                                til = "2023-04-01")
#'
#' # ... do something else with conn
#' # ...
#' # then manually close the connection to the DB
#' DBI::dbDisconnect(conn)
#' }
flights_airspace_profiles_tidy <- function(conn = NULL, wef, til, airspace = "FIR", profile = "CTFM") {
  # magic numbers: tables are indexed on LOBT, but LOBT is not precise to
  #                capture actual flight events, so we need some buffers.
  before_hours <- 28
  after_hours <- 24

  wef_before <- (lubridate::as_datetime(wef) - lubridate::dhours(before_hours)) |>
    format("%Y-%m-%d %H:%M:%S")
  til_after <- (lubridate::as_date(til) + lubridate::dhours(after_hours)) |>
    format("%Y-%m-%d %H:%M:%S")

  prf <- airspace_profiles_tidy(conn = conn, wef, til, airspace = airspace, profile = profile)
  ids <- prf |>
    dplyr::select(.data$ID) |>
    dplyr::distinct()

  # reuse the same DB connection as per the flights
  conn <- prf$src$con

  flt <- flights_tidy(conn = conn, wef = wef_before, til = til_after)
  cols <- colnames(flt)

  flt <- flt |>
    # dplyr::inner_join(flt, sql_on = "LHS.SAM_ID = RHS.ID AND LHS.LOBT = LHS.LOBT") |>
    # dplyr::inner_join(prf, by = c("ID" = "ID"))
    dplyr::inner_join(prf, by = c("ID" = "ID")) |>
    dplyr::select(cols) |>
    dplyr::distinct()

  flt
}



#' Return a reference to the Point Profile table
#'
#' @description
#' The returned [dbplyr::tbl_dbi()] is referencing the point profiles table in PRISME.
#' You can use `dplyr`/`dbplyr` verbs to filter, join, ... with other
#' datasets.
#'
#' # Note
#' You need to either provide a connection `conn` that has access to
#' `FSD.ALL_FT_POINT_PROFILE` or go with the default which uses
#' PRU_DEV to establish a [db_connection()].
#'
#' @inheritParams airlines_tbl
#'
#' @return a [dbplyr::tbl_dbi()] referencing the Oracle table for point profiles.
#' @export
#'
#' @examples
#' \dontrun{
#' pt <- point_profile_tbl()
#'
#' # if you re-use DB connections
#' conn <- eurocontrol::db_connection("PRU_DEV")
#' pt <- point_profile_tbl(conn = conn)
#'
#' # ... do something else with conn
#' # ...
#' # then manually close the connection to the DB
#' DBI::dbDisconnect(conn)
#' }
point_profile_tbl <- function(conn = NULL) {
  if (is.null(conn)) {
    conn <- db_connection(schema = "PRU_DEV")
  }
  prof <- dplyr::tbl(conn, dbplyr::in_schema("FSD", "ALL_FT_POINT_PROFILE"))

  prof
}

#' Export point profile from NM trajectories
#'
#' Extract NM point profile trajectories from PRISME database.
#' When a bbox is defined, we return only the (full) point profiles for
#' the flights flying thru the region.
#'
#' # Note
#' You need to either provide a connection `conn` that has access to as noted in
#' [airspace_profile_tbl()] and [flights_tidy()] or go with the default which uses
#' PRU_DEV to establish a [db_connection()].
#'
#'
#' @inheritParams airspace_profiles_tidy
#'
#' @param bbox (Optional) axis aligned bounding box
#'             (xmin, ymin, xmax, ymax)
#'
#' @return a dataframe representing a flight trajectory with the
#'         following columns:
#'
#' * FLIGHT_ID: a unique identifier for the flight
#' * TIME_OVER: the time over llon/lat
#' * LONGITUDE: the longitude
#' * LATITUDE: the latitude
#' * FLIGHT_LEVEL: the [flight level](https://ansperformance.eu/acronym/fl/)
#' * POINT_ID: the published point ID ('NO_POINT' otherwise)
#' * AIR_ROUTE: the air rout name ('DCT' otherwise)
#' * LOBT: the [last off-block time](https://ansperformance.eu/acronym/lobt/)
#' * SEQ_ID: the progressive sequence number in the trajectory points
#' * CALLSIGN: the [callsign](https://w.wiki/9XN6) of the flight
#' * REGISTRATION: the aircraft [registration](https://w.wiki/9XN7)
#' * MODEL_TYPE: the trajectory model as per `profile` input parameter
#' * AIRCRAFT_TYPE: the ICAO aircraft type
#' * AIRCRAFT_OPERATOR: the flight operator
#' * ICAO24: the ICAO 24-bit address of the aircraft
#' * ADEP: the Aerodrom of Departure
#' * ADES: the aerodrome of Destination
#'
#' @family read/export
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # export 1 day of NM (planned) trajectories
#' pf1 <- point_profiles_tidy(wef = "2019-07-14",
#'                            til = "2019-07-15",
#'                            profile = "FTFM")
#'
#' # export 2 hours of NM (flown) trajectories
#' pf2 <- point_profiles_tidy(wef = "2019-07-14 22:00",
#'                            til = "2019-07-15")
#'
#' # export 1 day of NM (flown) trajectories
#' pf3 <- point_profiles_tidy(wef = "2019-07-14",
#'                            til = "2019-07-15",
#'                            profile = "CTFM")
#'
#' # export all CTFM trajectories within a bounding box 40 NM around EDDF
#' bb <- c(xmin = 7.536746, xmax = 9.604390, ymin = 49.36732, ymax = 50.69920)
#' pf4 <- point_profiles_tidy(wef = "2019-01-01 00:00",
#'                            til = "2019-01-02 00:00",
#'                            bbox = bb)
#'
#'
#' # if you re-use DB connections
#' conn <- eurocontrol::db_connection("PRU_DEV")
#' pf <- point_profiles_tidy(conn = conn,
#'                           wef = "2020-01-01",
#'                           til = "2020-01-10")
#'
#' # ... do something else with conn
#' # ...
#' # then manually close the connection to the DB
#' DBI::dbDisconnect(conn)
#' }
point_profiles_tidy <- function(
    conn = NULL,
    wef,
    til = lubridate::today(tzone = "UTC"),
    profile = "CTFM",
    bbox = NULL
    ) {
  stopifnot(profile %in% c("CPF", "CTFM", "DCT", "FTFM", "SCR", "SRR", "SUR"))
  if (!is.null(bbox)) {
    stopifnot(names(bbox) %in% c("xmin", "xmax", "ymin", "ymax"))
    stopifnot(is.numeric(bbox))
  }

  # magic numbers: tables are indexed on LOBT, but LOBT is not precise to
  #                capture actual flight events, so we need some buffers.
  before_hours <- 28
  after_hours  <- 24

  wef <- wef |> lubridate::as_datetime(tz = "UTC")
  til <- til |> lubridate::as_datetime(tz = "UTC")


  wef_before <- (wef - lubridate::dhours(before_hours)) |>
    format("%Y-%m-%d %H:%M:%S")
  til_after <- (til + lubridate::dhours(after_hours)) |>
    format("%Y-%m-%d %H:%M:%S")

  wef <- wef |> format("%Y-%m-%d %H:%M:%S")
  til <- til |> format("%Y-%m-%d %H:%M:%S")

  if (is.null(conn)) {
    conn <- db_connection(schema = "PRU_DEV")
  }

  export_model_trajectory(
    conn,
    wef, til,
    profile = profile,
    bbox = bbox
    )

}


#' Export point trajectories for different flight models.
#'
#' Export the full trajectory.
#' When a bbox is defined we return only the trajectories of the
#' the flights flying thru the region.
#'
#'
#' @description
#' The returned [dbplyr::tbl_dbi()] includes point profiles for flights
#' departing in the right-opened interval `[wef, til)`.
#'
#' # Note
#' You need to either provide a connection `conn` that has access to `SWH_FCT.DIM_FLIGHT_TYPE_RULE`,
#' `PRUDEV.V_COVID_DIM_AO` and `SWH_FCT.FAC_FLIGHT` or go with the default which uses
#' PRU_DEV to establish a [db_connection()].
#'
#' @inheritParams airspace_profiles_tidy
#' @param bbox (Optional) axis aligned bounding box (xmin, ymin, xmax, ymax)
#' @param lobt_buffer (Optional) (portion of) hours before/after
#'             `wef` and `til` (before, after) used to query LOBT.
#'             This is to cater for flights crossing `wef` and `til`.
#'             For example `c(before = 24, after = 2.25)` allows to retrieve flights with LOBT
#'             24H before `wef` and 1H15M after `til`.
#' @param timeover_buffer (Optional) (portion of) hours before (after) `wef` (`til`).
#'                        This is to cater for flights crossing `wef` and `til`.
#'                        For example `c(before = 2, after = 0.25)` allows to retrieve
#'                        points whose TIME_OVER is 2H before `wef` and 15M after `til`.
#'
#' @return a `tbl`
#' @noRd
#'
export_model_trajectory <- function(
    conn,
    wef, til, profile = "CTFM",
    bbox = NULL,
    lobt_buffer = c(before = 28, after = 24),
    timeover_buffer = NULL) {


  wef <- wef |> lubridate::as_datetime(tz = "UTC")
  til <- til |> lubridate::as_datetime(tz = "UTC")
  wef <- format(wef, "%Y-%m-%dT%H:%M:%SZ")
  til <- format(til, "%Y-%m-%dT%H:%M:%SZ")

  where_bbox <- ""
  where_timeover_buffer <- ""
  lobt_before <- 0
  lobt_after  <- 0

  stopifnot(profile %in% c("CPF", "CTFM", "DCT", "FTFM", "SCR", "SRR", "SUR"))

  if (!is.null(bbox)) {
    stopifnot(names(bbox) %in% c("xmin", "xmax", "ymin", "ymax"))
    stopifnot(is.numeric(bbox))

    where_bbox <- stringr::str_glue(
      "AND (({lon_min} <= p.LON AND p.LON <={lon_max}) AND ({lat_min} <= p.LAT AND p.LAT <={lat_max}))",
      lon_min = bbox["xmin"],
      lon_max = bbox["xmax"],
      lat_min = bbox["ymin"],
      lat_max = bbox["ymax"])
  }

  if (!is.null(lobt_buffer)) {
    stopifnot(names(lobt_buffer) %in% c("before", "after"))
    stopifnot(is.numeric(lobt_buffer))

    lobt_before <- lobt_buffer["before"]
    lobt_after  <- lobt_buffer["after"]
  }

  if (!is.null(timeover_buffer)) {
    stopifnot(names(timeover_buffer) %in% c("before", "after"))
    stopifnot(is.numeric(timeover_buffer))

    timeover_before <- timeover_buffer["before"]
    timeover_after  <- timeover_buffer["after"]

    where_timeover_buffer <- stringr::str_glue(
      "AND (((SELECT LOBT_WEF FROM ARGS) - ({before} / 24) <= p.TIME_OVER) AND (p.TIME_OVER < (SELECT LOBT_TIL FROM ARGS) + ({after} / 24)))",
      before = timeover_before,
      after  = timeover_after)
  }

  # NOTE: to be set before you create your ROracle connection!
  # See http://www.oralytics.com/2015/05/r-roracle-and-oracle-date-formats_27.html
  withr::local_envvar(c("TZ" = "UTC",
                        "ORA_SDTZ" = "UTC"))
  withr::local_namespace("ROracle")
  con <- conn
  query <- "
    WITH
        ARGS
        AS
            (SELECT TO_DATE (?WEF,
                             'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
                        LOBT_WEF,
                    TO_DATE (?TIL,
                             'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"')
                        LOBT_TIL
               FROM DUAL),
        -- Flight IDs for the time and BBOX interval of interest,
        -- i.e.take all IDs where TIME_OVER and POSITION (LON, LAT) fit
        -- WEF / TIL and BBOX respectively
        -- NOTE: be slack with LOBT due to data hydiorincrasies
        FIDS
        AS (
          SELECT DISTINCT P.SAM_ID AS FLIGHT_ID
          FROM FSD.ALL_FT_POINT_PROFILE  P
          JOIN FLX.FLIGHT F ON (F.ID = P.SAM_ID AND F.LOBT = P.LOBT)
          WHERE     F.LOBT >= (SELECT LOBT_WEF FROM ARGS) - ({BEFORE} / 24)
                AND F.LOBT <  (SELECT LOBT_TIL FROM ARGS) + ({AFTER} / 24)
                AND P.LOBT >= (SELECT LOBT_WEF FROM ARGS) - ({BEFORE} / 24)
                AND P.LOBT <  (SELECT LOBT_TIL FROM ARGS) + ({AFTER} / 24)
                AND P.MODEL_TYPE = ?MODEL
               -- it can happen when ADEP/ADES are unknown, i.e. 'ZZZ'
               AND P.LON IS NOT NULL
               AND P.LAT IS NOT NULL
               AND P.TIME_OVER IS NOT NULL
               {WHERE_BBOX}
               AND (((SELECT LOBT_WEF FROM ARGS) <= P.TIME_OVER) AND (P.TIME_OVER < (SELECT LOBT_TIL FROM ARGS) ))
               )
    SELECT
      P.SAM_ID                 AS FLIGHT_ID,
      P.TIME_OVER,
      P.LON                    AS LONGITUDE,
      P.LAT                    AS LATITUDE,
      P.FLIGHT_LEVEL,
      P.POINT_ID,
      P.AIR_ROUTE,
      P.LOBT,
      P.SEQ_ID,
      F.AIRCRAFT_ID            AS CALLSIGN,
      F.REGISTRATION,
      P.MODEL_TYPE,
      F.AIRCRAFT_TYPE_ICAO_ID  AS AIRCRAFT_TYPE,
      F.AIRCRAFT_OPERATOR,
      F.AIRCRAFT_ADDRESS       AS ICAO24,
      F.ADEP,
      F.ADES
    FROM FSD.ALL_FT_POINT_PROFILE  P
         JOIN FLX.FLIGHT F ON (F.ID = P.SAM_ID AND F.LOBT = P.LOBT)
   WHERE     F.LOBT >= (SELECT LOBT_WEF FROM ARGS) - ({BEFORE} / 24)
         AND F.LOBT <  (SELECT LOBT_TIL FROM ARGS) + ({AFTER} / 24)
         AND P.LOBT >= (SELECT LOBT_WEF FROM ARGS) - ({BEFORE} / 24)
         AND P.LOBT <  (SELECT LOBT_TIL FROM ARGS) + ({AFTER} / 24)
         AND P.MODEL_TYPE = ?MODEL
         -- it can happen when ADEP/ADES are unknown, 'ZZZ'
         AND P.LON IS NOT NULL
         AND P.LAT IS NOT NULL
         AND P.TIME_OVER IS NOT NULL
        {WHERE_TIMEOVER_BUFFER}
  "

  query <- stringr::str_glue(query,
                             WHERE_BBOX   = where_bbox,
                             WHERE_TIMEOVER_BUFFER = where_timeover_buffer,
                             BEFORE       = lobt_before,
                             AFTER        = lobt_after)
  query <- DBI::sqlInterpolate(
    con, query,
    WEF = wef, TIL = til,
    MODEL = profile)

  fltq <- dplyr::tbl(con, dplyr::sql(query))
  pnts <- fltq |>
    dplyr::mutate(
      POINT_ID  = dplyr::if_else(is.na(.data$POINT_ID),  "NO_POINT", .data$POINT_ID),
      AIR_ROUTE = dplyr::if_else(is.na(.data$AIR_ROUTE), "NO_ROUTE", .data$AIR_ROUTE))

  pnts
}

