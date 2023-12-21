#' Return a reference to the Airport Operator Data Flow table
#'
#' @description
#' The returned [dplyr::tbl()] is referencing the airport operator data flow table in PRISME.
#' You can use `dplyr`/`dbplyr` verbs to filter, join, ... with other
#' datasets.
#'
#' # Note
#' You need to either provide a connection `conn` that has access to `SWH_FCT.FAC_APDS_FLIGHT_IR691` or
#' go with the default which uses PRU_ATMAP to establish a [db_connection()].
#'
#' @inheritParams airlines_tbl
#'
#' @return a [dplyr::tbl()] referencing the Oracle table for airport operator data flow.
#' @export
#'
#' @examples
#' \dontrun{
#' aodf <- aodf_tbl()
#' }
#'
aodf_tbl <- function(conn = NULL) {
  if (is.null(conn)) {
    conn <- db_connection(schema = "PRU_ATMAP")
  }
  aodf <- dplyr::tbl(conn, dbplyr::in_schema("SWH_FCT", "FAC_APDS_FLIGHT_IR691"))
  aodf
}




#' Extract a clean airport operator data flow list in an interval
#'
#' @description
#' The returned [dplyr::tbl()] includes movements information in the
#' interval `[wef, til)`.
#' **NOTE**: it can only cover ONE month at a time
#'
#' # Note
#' You need to either provide a connection `conn` that has access to `SWH_FCT.FAC_APDS_FLIGHT_IR691`,
#' or go with the default which uses PRU_ATMAP to establish a [db_connection()].
#'
#' @inheritParams airlines_tbl
#'
#' @inheritParams flights_tidy
#'
#'
#' @return A [dplyr::tbl()] with the following columns:
#'
#'  * APDS_ID: the airport operator dataflow unique record id.
#'  * AP_C_FLTID: flight identifier (aource Airport)
#'  * AP_C_FLTRUL: which sets of regulations the flight is operated under.
#'   Possible values are:
#'   - `IFR` for IFR
#'   - `VFR` for VFR
#'   - `NA` if unknown
#'  * AP_C_REG: the [aircraft registration](https://en.wikipedia.org/wiki/Aircraft_registration)
#'   (with spaces, dashes, ... stripped), e.g. GEUUU.
#'  * ADEP_ICAO: ([ICAO code](https://observablehq.com/@openaviation/airports) of the)
#'               **A**erodrome of **DEP**arture (source airport).
#'  * ADES_ICAO: ([ICAO code](https://observablehq.com/@openaviation/airports) of the)
#'               **A**erodrome of **DES**tination  (source airport).
#'  * SRC_PHASE: flight phase. `DEP`=departure, `ARR`=arrival.
#'  * MVT_TIME_UTC: (best available) movement time
#'                  (takeoff if `SRC_PHASE` = `DEP`, landing if `SRC_PHASE` = `ARR`).
#'  * BLOCK_TIME_UTC: Block time
#'                    (off-block if `SRC_PHASE` = `DEP`, in-block if `SRC_PHASE` = `ARR`).
#'  * SCHED_TIME_UTC: scheduled time
#'                    (of departure if `SRC_PHASE` = `DEP`, of arrival if `SRC_PHASE` = `ARR`; source airport).
#'  * ARCTYP: (best available) the [ICAO code for the aircraft type](https://www.icao.int/publications/doc8643/pages/search.aspx),
#'    for example A21N for Airbus A321neo.
#'  * AP_C_RWY: Runway ID (of departure if `SRC_PHASE` = `DEP`, of arrival if `SRC_PHASE` = `ARR`; source airport).
#'  * AP_C_STND: Stand ID (of departure if `SRC_PHASE` = `DEP`, of arrival if `SRC_PHASE` = `ARR`; source airport).
#'  * C40_CROSS_TIME: time of first (last) crossing at 40 NM from ARP for departure (arrival).
#'  * C40_CROSS_LAT: latitude of first (last) crossing at 40 NM from ARP for departure (arrival).
#'  * C40_CROSS_LON: longitude of first (last) crossing at 40 NM from ARP for departure (arrival).
#'  * C40_CROSS_FL: flight level of first (last) crossing at 40 NM from ARP for departure (arrival).
#'  * C40_BEARING: bearing of first (last) crossing at 40 NM from ARP for departure (arrival).
#'  * C100_CROSS_TIME: time of first (last) crossing at 100 NM from ARP for departure (arrival).
#'  * C100_CROSS_LAT: latitude of first (last) crossing at 100 NM from ARP for departure (arrival).
#'  * C100_CROSS_LON: longitude of first (last) crossing at 100 NM from ARP for departure (arrival).
#'  * C100_CROSS_FL: flight level of first (last) crossing at 100 NM from ARP for departure (arrival).
#'  * C100_BEARING: bearing of first (last) crossing at 100 NM from ARP for departure (arrival).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' my_aodf <- aodf_tidy(wef = "2023-01-01", til = "2023-01-02")
#' # ...
#' DBI::dbDisconnect(my_aodf$src$con)
#' }
aodf_tidy <- function(conn = NULL, wef, til) {
  aodf <- aodf_tbl(conn)

  wef <- lubridate::as_datetime(wef, tz = "UTC") |> format("%Y-%m-%d %H:%M:%S")
  til <- lubridate::as_datetime(til, tz = "UTC") |> format("%Y-%m-%d %H:%M:%S")

  aa <- aodf |>
    dplyr::filter(
      TO_DATE(wef, "yyyy-mm-dd hh24:mi:ss") <= .data$MVT_TIME_UTC,
      .data$MVT_TIME_UTC < TO_DATE(til, "yyyy-mm-dd hh24:mi:ss"),
      TO_DATE(wef, "yyyy-mm-dd hh24:mi:ss") <= .data$SRC_DATE_FROM,
      .data$SRC_DATE_FROM < TO_DATE(til, "yyyy-mm-dd hh24:mi:ss")
    ) |>
    dplyr::select(
      "APDS_ID",
      "AP_C_FLTID",
      "AP_C_FLTRUL",
      "AP_C_REG",
      dplyr::ends_with("ICAO"),
      "SRC_PHASE",
      "MVT_TIME_UTC",
      "BLOCK_TIME_UTC",
      "SCHED_TIME_UTC",
      "ARCTYP",
      "AP_C_RWY",
      "AP_C_STND",
      dplyr::starts_with("C40_"),
      dplyr::starts_with("C100_")
    )  |>
    dplyr::select(
      -dplyr::ends_with("_MIN"),
      -dplyr::ends_with("_IN_FRONT"),
      -dplyr::ends_with("_CTFM"),
      -dplyr::ends_with("_CPF"),
      -dplyr::contains("TRANSIT"))
  return(aa)
}


