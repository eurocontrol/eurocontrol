#' Return a reference to the Airlines table
#'
#' @description
#' The returned [dbplyr::tbl_dbi()] is referencing the airlines table in PRISME.
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
#' @return a [dbplyr::tbl_dbi()] referencing the Oracle table for airlines.
#' @export
#'
#' @examples
#' \dontrun{
#' arl <- airlines_tbl()
#' # other operations on arl, i.e. filtering,
#' # followed by a collect() to retrieve the concrete data frame
#' arl_filtered <- arl |>
#'    dplyr::filter(AO_ISO_CTRY_CODE == "IT") |>
#'    collect()
#'
#' # NOTE: you can reuse the connection for other API calls
#' conn <- arl$src$con
#'
#' # other ops requiring conn
#' # ...
#'
#' # IMPORTANT: close the DB connection
#' DBI::dbDisconnect(conn)
#' }
airlines_tbl <- function(conn = NULL) {
  if (is.null(conn)) {
    conn <- db_connection(schema = "PRU_DEV")
  }
  arl <- dplyr::tbl(conn, "V_COVID_DIM_AO")
  arl
}


#' Airline info including group affiliation
#'
#' # Note
#' You need to either provide a connection `conn` that has access to `PRUDEV.V_COVID_DIM_AO`
#' or go with the default which uses PRU_DEV to establish a [db_connection()].
#'
#' @param conn Optional connection to the PRU_DEV schema.
#'
#' @return A [dbplyr::tbl_dbi()] with the following columns:
#'
#' * `AO_CODE`: the the [ICAO Airline Designator](https://en.wikipedia.org/wiki/List_of_airline_codes), i.e. 'OAL'
#' * `AO_NAME`: the airline's name, i.e. 'Olympic'
#' * `AO_GRP_CODE`: the airline's affiliation group code, i.e. 'AEE_GRP'
#' * `AO_GRP_NAME`: the airline's affiliation group, i.e. 'AEGEAN Group'
#' * `AO_ISO_CTRY_CODE`: the ISO2C code of the airline's country, i.e. 'GR'
#' * `EU`: (a character) whether the airlines is in a EUROCONTROL's Member State
#'         (full, comprehensive or transition plus Kosovo), i.e. 'TRUE'
#' @export
#'
#' @examples
#' \dontrun{
#' arls <- airlines_tidy()
#' # other operations on arls, i.e. filtering,
#' # followed by a collect() to retrieve the concrete data frame
#' arls_filtered <- arls |>
#'    filter(stringr::str_starts("A")) |>
#'    collect()
#'
#' # NOTE: you can reuse the connection for other API calls
#' conn <- arls$src$con
#'
#' # other ops requiring conn
#' # ...
#'
#' # IMPORTANT: close the DB connection
#' DBI::dbDisconnect(conn)
#' }
airlines_tidy <- function(conn = NULL) {
  if (is.null(conn)) {
    conn <- db_connection(schema = "PRU_DEV")
  }

  ect <- eurocontrol::member_state |> dplyr::pull(.data$iso2c)

  arl <- airlines_tbl(conn) |>
    dplyr::select(
      "AO_CODE",
      "AO_NAME",
      "AO_GRP_CODE",
      "AO_GRP_NAME",
      "AO_ISO_CTRY_CODE") |>
    # default to NA and then set what is TRUE
    dplyr::mutate(EU = dplyr::if_else(
      .data$AO_ISO_CTRY_CODE %in% ect,
      "TRUE",
      "FALSE"))

  arl
}



