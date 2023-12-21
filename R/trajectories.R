#' Return a reference to the Airspace Profile table
#'
#' @description
#' The returned [dplyr::tbl()] is referencing the airspace profiles table in PRISME.
#' You can use `dplyr`/`dbplyr` verbs to filter, join, ... with other
#' datasets.
#'
#' # Note
#' You need to either provide a connection `conn` that has access to `FSD.ALL_FT_ASP_PROFILE` or
#' go with the default which uses PRU_DEV to establish a [db_connection()].
#'
#' @inheritParams airlines_tbl
#'
#' @return a [dplyr::tbl()] referencing the Oracle table for airspace profiles.
#' @export
#'
#' @examples
#' \dontrun{
#' prf <- airspace_profile_tbl()
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
#' The returned [dplyr::tbl()] includes segments for scheduled and non-scheduled flights
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
#' @return a [dplyr::tbl()] with the following columns
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
#' asp_profs <- airspace_profiles_tidy(wef = "2023-01-01", til = "2023-04-01")
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
#' The returned [dplyr::tbl()] includes scheduled and non-scheduled flights whose airspace segments
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
#' @return a [dplyr::tbl()] with the same columns as [flights_tidy()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' asp_profs <- flights_airspace_profiles_tidy(wef = "2023-01-01", til = "2023-04-01")
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
#' The returned [dplyr::tbl()] is referencing the point profiles table in PRISME.
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
#' @return a [dplyr::tbl()] referencing the Oracle table for point profiles.
#' @export
#'
#' @examples
#' \dontrun{
#' prf <- point_profile_tbl()
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
#' Extract NM point profile trajectories from PRISME database
#'
#' # Note
#' You need to either provide a connection `conn` that has access to as noted in
#' [airspace_profile_tbl()] and [flights_tidy()] or go with the default which uses
#' PRU_DEV to establish a [db_connection()].
#'
#'
#' @inheritParams flights_tidy
#'
#' @param model the model of the profile: one of
#'              CPF, CTFM, DCT, FTFM, SCR, SRR, SUR
#'              \[default: "CFTM"\].
#' @param bbox (Optional) axis aligned bounding box
#'             (xmin, ymin, xmax, ymax)
#'
#' @return a dataframe with trajectory data
#'
#' @family read/export
#'
#' @examples
#' \dontrun{
#' # export 1 day worth of NM (planned) trajectories
#' export_model_trajectory("2019-07-14", "2019-07-15", model = "FTFM")
#'
#' # export 2 hours of NM (flown) trajectories
#' export_model_trajectory("2019-07-14 22:00", "2019-07-15")
#'
#' # export 1 day of NM (flown) trajectories
#' point_profiles_tidy("2019-07-14", "2019-07-15", lobt_buffer = c(before = 24, after = 1.25))
#'
#' # export all CTFM trajectories within a bounding box 40 NM around EDDF
#' bb <- c(xmin = 7.536746, xmax = 9.604390, ymin = 49.36732, ymax = 50.69920)
#' point_profiles_tidy("2019-01-01 00:00", "2019-01-02 00:00", bbox = bb)
#' }
point_profiles_tidy <- function(
    conn = NULL,
    wef, til,
    model = "CTFM",
    bbox = NULL
    ) {
  stopifnot(model %in% c("CPF", "CTFM", "DCT", "FTFM", "SCR", "SRR", "SUR"))
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

  wef <- wef |> format("%Y-%m-%d %H:%M:%SZ")
  til <- til |> format("%Y-%m-%d %H:%M:%SZ")

  if (is.null(conn)) {
    conn <- db_connection(schema = "PRU_DEV")
  }

  p <- point_profile_tbl(conn = conn) |>
    dplyr::filter(
      TO_DATE(wef_before, "yyyy-mm-dd hh24:mi:ss") <= .data$LOBT,
      .data$LOBT < TO_DATE(til_after, "yyyy-mm-dd hh24:mi:ss"),
      .data$MODEL_TYPE %in% model,
      # # non null LON/LAT: it can happen when ADEP/ADES are unknown, i.e. 'ZZZZ'
      !is.na(.data$LON),
      !is.na(.data$LAT),
      !is.na(.data$TIME_OVER)
    )
  f <- dplyr::tbl(conn, dbplyr::in_schema("FLX", "FLIGHT")) |>
    dplyr::filter(
      TO_DATE(wef_before, "yyyy-mm-dd hh24:mi:ss") <= .data$LOBT,
      .data$LOBT < TO_DATE(til_after, "yyyy-mm-dd hh24:mi:ss")
    )


  ids <- p |>
    dplyr::inner_join(
      f,
      dplyr::join_by(
        x$SAM_ID == y$ID,
        x$LOBT == y$LOBT)) |>
    dplyr::distinct(.data$SAM_ID)

    # dplyr::select(
    #   "ID",
    #   "AIRCRAFT_ID",
    #   "REGISTRATION",
    #   "AIRCRAFT_TYPE_ICAO_ID",
    #   "AIRCRAFT_OPERATOR",
    #   "AIRCRAFT_ADDRESS",
    #   "ADEP",
    #   "ADES")


  prf <- point_profile_tbl(conn = conn) |>
    dplyr::filter(
      TO_DATE(wef_before, "yyyy-mm-dd hh24:mi:ss") <= .data$LOBT,
      .data$LOBT < TO_DATE(til_after, "yyyy-mm-dd hh24:mi:ss"),
      .data$MODEL_TYPE %in% model,
      # # non null LON/LAT: it can happen when ADEP/ADES are unknown, i.e. 'ZZZZ'
      !is.na(.data$LON),
      !is.na(.data$LAT),
      !is.na(.data$TIME_OVER)
    ) |>
    dplyr::left_join(f)




  if (!is.null(bbox)) {
    prf <- prf |>
      dplyr::filter(
        bbox["xmin"] <= .data$p.LON, .data$p.LON <=bbox["xmax"],
        bbox["ymin"] <= .data$p.LAT, .data$p.LAT <=bbox["ymax"]
      )
  }

  pp <- prf |>
    dplyr::select(
      .data$SAM_ID,
      .data$TIME_OVER,
      .data$LON,
      .data$LAT,
      .data$FLIGHT_LEVEL,
      .data$POINT_ID,
      .data$AIR_ROUTE,
      .data$LOBT,
      .data$SEQ_ID,
      .data$AIRCRAFT_ID,
      .data$REGISTRATION,
      .data$MODEL_TYPE,
      .data$AIRCRAFT_TYPE_ICAO_ID,
      .data$AIRCRAFT_OPERATOR,
      .data$AIRCRAFT_ADDRESS,
      .data$ADEP,
      .data$ADES
    ) |>
    dplyr::rename(
      FLIGHT_ID = .data$SAM_ID,
      LONGITUDE = .data$LON,
      LATITUDE  = .data$LAT,
      CALLSIGN  = .data$AIRCRAFT_ID,
      AIRCRAFT_TYPE = .data$AIRCRAFT_TYPE_ICAO_ID,
      ICAO24    = .data$AIRCRAFT_ADDRESS
    )
  pp
}
