#' Extract ADRR flight list in an interval of dates
#'
#' # Note
#' You need to either provide a connection `conn` that has access to
#' `SWH_FCT.DIM_FLIGHT_TYPE_RULE` (for `FLT_RULES`),
#' `PRUDEV.V_COVID_DIM_AO` (for aircraft and aircraft group info) and
#' `SWH_FCT.V_FAC_FLIGHT_MS` (for market segment info) or go with the default
#' which uses PRU_READ to establish a [db_connection()].
#'
#' @inheritParams flights_tidy
#'
#' @return A [dbplyr::tbl_dbi()] with the following columns (as defined in the
#' Aviation Data Repository for Research manual):
#' ## Flight details
#' * `ECTRL ID`: flight unique id.
#' * `ADEP`/`ADES`:
#' * `ADEP Latitude`/`ADES Latitude`:
#' * `ADEP Longitude`/`ADES Longitude`:
#' * `FILED OFF BLOCK TIME`:
#' * `FILED ARRIVAL TIME`:
#' * `ACTUAL OFF BLOCK TIME`:
#' * `ACTUAL ARRIVAL TIME`:
#' * `AC Type`:
#' * `AC Operator`:
#' * `AC Registration`:
#' * `ICAO Flight Type`:
#' * `STATFOR Market Segment`:
#' * `Requested FL`:
#' * `Actual Distance Flown (nm)`:
#'
#' @export
#' @examples
#' \dontrun{
#' withr::local_envvar(c(TZ = "UTC", ORA_SDTZ = "UTC", NLS_LANG = ".AL32UTF8"))
#' conn <- withr::local_db_connection(db_connection(schema = "PRU_READ"))
#' wef <- "2025-01-01"
#' til <- "2025-01-04"
#' adrr_flights_tidy(conn, wef, til)
#' }
adrr_flights_tidy <- function(conn = NULL, wef, til) {
  adrr_flights_raw(conn, wef, til) |>
    dplyr::rename(
      "ECTRL ID" = "FAC_FLIGHT.ID",
      "ADEP" = "FAC_FLIGHT.ADEP",
      "ADEP Latitude" = "DIM_AIRPORT.LATITUDE_DEP",
      "ADEP Longitude" = "DIM_AIRPORT.LONGITUDE_DEP",
      "ADES" = "FAC_FLIGHT.ADES",
      "ADES Latitude" = "DIM_AIRPORT.LATITUDE_DES",
      "ADES Longitude" = "DIM_AIRPORT.LONGITUDE_DES",
      "FILED OFF BLOCK TIME" = "FAC_FLIGHT.LOBT",
      "FILED ARRIVAL TIME" = "FAC_FLIGHT.ARVT_1",
      "ACTUAL OFF BLOCK TIME" = "FAC_FLIGHT.AOBT_3",
      "ACTUAL ARRIVAL TIME" = "FAC_FLIGHT.ARVT_3",
      "AC Type" = "F_FLIGHT.AIRCRAFT_TYPE_ICAO_ID",
      "AC Operator" = "DIM_OPERATOR.ICAO_OP_CODE",
      "AC Registration" = "FAC_FLIGHT.CORR_REGISTRATION",
      "ICAO Flight Type" = "FAC_FLIGHT.ICAO_FLT_TYPE",
      "STATFOR Market Segment" = "DIM_FTR.RULE_DESCRIPTION",
      "Requested FL" = "FAC_FLIGHT.FL_REQ",
      "Actual Distance Flown (nm)" = "FAC_FLIGHT.RTE_LEN_3",
      NULL
    )
}


adrr_flights_raw <- function(conn, wef, til) {
  f <- flights_tbl(conn)
  # reuse the automatic connection is not passed in the initial args
  conn <- f$src$con

  wef <- wef |> lubridate::as_date() |> format("%Y-%m-%d")
  til <- til |> lubridate::as_date() |> format("%Y-%m-%d")

  r <- dplyr::tbl(conn, dbplyr::in_schema("SWH_FCT", "DIM_FLIGHT_TYPE_RULE"))
  ao <- dplyr::tbl(conn, dbplyr::in_schema("SWH_FCT", "DIM_OPERATOR"))
  ad <- dplyr::tbl(conn, dbplyr::in_schema("SWH_FCT", "DIM_AIRPORT"))
  aa <- dplyr::tbl(conn, dbplyr::in_schema("SWH_FCT", "DIM_AIRPORT"))

  f |>
    dplyr::inner_join(
      r,
      dplyr::join_by("SK_FLT_TYPE_RULE_ID"),
      suffix = c(".f", ".r")
    ) |>
    dplyr::left_join(ao, dplyr::join_by("SK_OP_ID"), suffix = c("", ".ao")) |>
    dplyr::left_join(
      ad,
      dplyr::join_by(
        x$ADEP == y$EC_AP_CODE,
        x$LOBT >= y$VALID_FROM,
        x$LOBT < y$VALID_TO
      ),
      suffix = c("", ".ad")
    ) |>
    dplyr::left_join(
      aa,
      dplyr::join_by(
        x$ADES == y$EC_AP_CODE,
        x$LOBT >= y$VALID_FROM,
        x$LOBT < y$VALID_TO
      ),
      suffix = c("", ".aa")
    ) |>
    # fmt: skip
    dplyr::filter(
      TO_DATE(!!wef, 'YYYY-MM-DD') <= .data$LOBT,
      .data$LOBT < TO_DATE(!!til, 'YYYY-MM-DD'),
      .data$ICAO_FLT_TYPE %in% c('S','N'),
      .data$RULE_DESCRIPTION != 'Military'
    ) |>
    dplyr::mutate(
      DIM_OPERATOR.ICAO_OP_CODE = dplyr::if_else(
        !is.na(.data$LAST_ICAO_VERSION_NUMBER),
        .data$ICAO_OP_CODE,
        'ZZZ'
      )
    ) |>
    dplyr::arrange(.data$LOBT, .data$ID) |>
    dplyr::select(
      "FAC_FLIGHT.ID" = "ID",
      "FAC_FLIGHT.ADEP" = "ADEP",
      "DIM_AIRPORT.LATITUDE_DEP" = "LATITUDE",
      "DIM_AIRPORT.LONGITUDE_DEP" = "LONGITUDE",
      "FAC_FLIGHT.ADES" = "ADES",
      "DIM_AIRPORT.LATITUDE_DES" = "LATITUDE.aa",
      "DIM_AIRPORT.LONGITUDE_DES" = "LONGITUDE.aa",
      "FAC_FLIGHT.LOBT" = "LOBT",
      "FAC_FLIGHT.ARVT_1" = "ARVT_1",
      "FAC_FLIGHT.AOBT_3" = "AOBT_3",
      "FAC_FLIGHT.ARVT_3" = "ARVT_3",
      "F_FLIGHT.AIRCRAFT_TYPE_ICAO_ID" = "AIRCRAFT_TYPE_ICAO_ID",
      "DIM_OPERATOR.ICAO_OP_CODE",
      "FAC_FLIGHT.CORR_REGISTRATION" = "CORR_REGISTRATION",
      "FAC_FLIGHT.ICAO_FLT_TYPE" = "ICAO_FLT_TYPE",
      "DIM_FTR.RULE_DESCRIPTION" = "RULE_DESCRIPTION",
      "FAC_FLIGHT.FL_REQ" = "FL_REQ",
      "FAC_FLIGHT.RTE_LEN_3" = "RTE_LEN_3",
      NULL
    )
}
