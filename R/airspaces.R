#' Retrieve ACC/OAC airspace geometries for a given AIRAC cycle
#'
#' @description
#' Returns ACC (Area Control Centre) and OAC (Oceanic Area Control Centre)
#' airspace geometries as an `sf` object for the specified CFMU AIRAC cycle.
#'
#' # Note
#' You need to either provide a connection `conn` that has access to
#' `ENV_SP.AIRSPACE`, `PRU_STAT_AUA` and `SWH_MAP` or go with the default
#' which uses PRU_DEV to establish a [db_connection()].
#'
#' The `sf` package is required but only suggested; install it if not available.
#'
#' @inheritParams airlines_tbl
#'
#' @param cfmu_airac the AIRAC cycle number (CFMU format), e.g. `"517"` or
#'   `517`. Both character and numeric inputs are accepted.
#'   See [airac::cfmu_airac()] to look up the cycle number for a given date.
#'
#' @seealso [airac::cfmu_airac()] from the
#'   \href{https://github.com/eurocontrol/airac}{airac} package.
#'
#' @return An `sf` object with the following columns:
#' * `AC_ID`: the AIRAC cycle id
#' * `AV_AIRSPACE_ID`: the airspace identifier
#' * `MIN_FLIGHT_LEVEL`: the minimum flight level
#' * `MAX_FLIGHT_LEVEL`: the maximum flight level
#' * `NAME`: the airspace name
#' * `CODE`: the airspace code
#' * `AIRSPACE_TYPE`: the airspace type (`ACC` or `OAC`)
#' * `geometry`: the airspace polygon geometry
#'
#' @export
#'
#' @examples
#' \dontrun{
#' acc <- acc_sf(cfmu_airac = "517")
#'
#' # write to GeoJSON
#' sf::st_write(acc, "acc-517.geojson", driver = "GeoJSON")
#'
#' # if you re-use DB connections
#' conn <- eurocontrol::db_connection("PRU_DEV")
#' acc <- acc_sf(conn = conn, cfmu_airac = "517")
#'
#' # ... do something else with conn
#' # ...
#' # then manually close the connection to the DB
#' DBI::dbDisconnect(conn)
#' }
acc_sf <- function(conn = NULL, cfmu_airac) {
  cfmu_airac <- as.character(cfmu_airac)
  rlang::check_installed("sf", reason = "to parse GeoJSON airspace geometries")

  if (is.null(conn)) {
    conn <- db_connection(schema = "PRU_DEV")
  }

  withr::local_envvar(c(
    "TZ" = "UTC",
    "ORA_SDTZ" = "UTC",
    "NLS_LANG" = ".AL32UTF8"
  ))
  withr::local_namespace("ROracle")

  sqlq <- "
    WITH
      AIRSPACE_NAMES AS (
        SELECT
          ID,
          CODE,
          NAME,
          PRU_ATC_TYPE AS AIRSPACE_TYPE
        FROM
          PRU_STAT_AUA
        WHERE
          PRU_ATC_TYPE IN ('ACC', 'OAC')
      )
    SELECT
      '{ \"type\": \"FeatureCollection\", \"features\": [' ||
      rtrim(SWH_MAP.clobagg('{ \"type\": \"Feature\", \"geometry\": '
      || SWH_MAP.SDO2GEOJSON(SHAPE,3,0,0)
      || ', \"properties\": {'
      || '\"AC_ID\": '               || A.AC_ID                 || ', '
      || '\"AV_AIRSPACE_ID\": \"'    || A.AIRSPACE_ID           || '\", '
      || '\"MIN_FLIGHT_LEVEL\": '    || A.MIN_FLIGHT_LEVEL      || ', '
      || '\"MAX_FLIGHT_LEVEL\": '    || A.MAX_FLIGHT_LEVEL      || ', '
      || '\"NAME\": \"'              || B.NAME                  || '\", '
      || '\"CODE\": \"'              || B.CODE                  || '\", '
      || '\"AIRSPACE_TYPE\": \"'     || B.AIRSPACE_TYPE         || '\"'
      || '}}' || ',' || chr(13)),',' || chr(13))                || ']}'
    FROM
      ENV_SP.AIRSPACE A
      INNER JOIN AIRSPACE_NAMES B
        ON (
          A.AIRSPACE_ID = B.ID
          AND AIRSPACE_KIND = 'STAT_AUA'
        )
    WHERE
      A.AC_ID = ?CFMU_AIRAC
      AND A.SHAPE IS NOT NULL
  "

  query <- DBI::sqlInterpolate(conn, sqlq, CFMU_AIRAC = cfmu_airac)
  res <- DBI::dbSendQuery(conn, query)
  data <- DBI::fetch(res, n = -1)
  DBI::dbClearResult(res)

  data |>
    dplyr::first() |>
    sf::st_read(quiet = TRUE)
}


#' Retrieve ACE ANSP airspace geometries for a given AIRAC cycle
#'
#' @description
#' Returns ANSP (Air Navigation Service Provider) airspace geometries from the
#' ACE (ATM Cost-Effectiveness) benchmarking dataset as an `sf` object for the
#' specified CFMU AIRAC cycle.
#'
#' Airspaces with code `AIRPORT`, `UNKNOWN` or `MILITARY` are excluded.
#'
#' # Note
#' You need to either provide a connection `conn` that has access to
#' `ENV_SP.AIRSPACE`, `PRU_CFMU_ANSP` and `SWH_MAP` or go with the default
#' which uses PRU_DEV to establish a [db_connection()].
#'
#' The `sf` package is required but only suggested; install it if not available.
#'
#' @inheritParams acc_sf
#'
#' @seealso [airac::cfmu_airac()] from the
#'   \href{https://github.com/eurocontrol/airac}{airac} package.
#'
#' @return An `sf` object with the following columns:
#' * `airac_cfmu`: the AIRAC cycle id
#' * `id`: the airspace identifier
#' * `code`: the ANSP code
#' * `name`: the ANSP name
#' * `ace_code`: the ACE benchmarking code
#' * `min_fl`: the minimum flight level
#' * `max_fl`: the maximum flight level
#' * `airspace_type`: the airspace type
#' * `geometry`: the airspace polygon geometry
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ansps <- ansp_sf(cfmu_airac = "481")
#'
#' # write to GeoJSON
#' sf::st_write(ansps, "ansp_ace_481.geojson", driver = "GeoJSON")
#'
#' # if you re-use DB connections
#' conn <- eurocontrol::db_connection("PRU_DEV")
#' ansps <- ansp_sf(conn = conn, cfmu_airac = "481")
#'
#' # ... do something else with conn
#' # ...
#' # then manually close the connection to the DB
#' DBI::dbDisconnect(conn)
#' }
ansp_sf <- function(conn = NULL, cfmu_airac) {
  cfmu_airac <- as.character(cfmu_airac)
  rlang::check_installed("sf", reason = "to parse GeoJSON airspace geometries")

  if (is.null(conn)) {
    conn <- db_connection(schema = "PRU_DEV")
  }

  withr::local_envvar(c(
    "TZ" = "UTC",
    "ORA_SDTZ" = "UTC",
    "NLS_LANG" = ".AL32UTF8"
  ))
  withr::local_namespace("ROracle")

  sqlq <- "
    WITH
      AIRSPACE_NAMES AS (
        SELECT DISTINCT
          AIRSPACE_ID,
          AIRSPACE_TYPE,
          NAME,
          ACE_CODE,
          CODE
        FROM
          ENV_SP.AIRSPACE A,
          PRU_CFMU_ANSP P
        WHERE
          A.AIRSPACE_ID = P.ID
          AND A.AIRSPACE_KIND = 'ANSP'
          AND AC_ID = ?CFMU_AIRAC
      )
    SELECT
      '{ \"type\": \"FeatureCollection\", \"features\": [' ||
      rtrim(SWH_MAP.clobagg('{ \"type\": \"Feature\", \"geometry\": '
      || SWH_MAP.SDO2GEOJSON(SHAPE,3,0,0)
      || ', \"properties\": {'
      || '\"AC_ID\": '               || A.AC_ID                 || ', '
      || '\"AV_AIRSPACE_ID\": \"'    || A.AIRSPACE_ID           || '\", '
      || '\"MIN_FLIGHT_LEVEL\": '    || A.MIN_FLIGHT_LEVEL      || ', '
      || '\"MAX_FLIGHT_LEVEL\": '    || A.MAX_FLIGHT_LEVEL      || ', '
      || '\"NAME\": \"'              || AIRSPACE_NAMES.NAME     || '\", '
      || '\"ACE_CODE\": \"'          || AIRSPACE_NAMES.ACE_CODE || '\", '
      || '\"CODE\": \"'              || AIRSPACE_NAMES.CODE     || '\", '
      || '\"AIRSPACE_TYPE\": \"'     || A.AIRSPACE_TYPE         || '\"'
      || '}}' || ',' || chr(13)),',' || chr(13))                || ']}'
    FROM
      ENV_SP.AIRSPACE A
      LEFT JOIN AIRSPACE_NAMES
        ON (
          A.AIRSPACE_ID = AIRSPACE_NAMES.AIRSPACE_ID
          AND A.AIRSPACE_TYPE = AIRSPACE_NAMES.AIRSPACE_TYPE
        )
    WHERE
      A.AC_ID = ?CFMU_AIRAC
      AND A.AIRSPACE_TYPE = 'ANSP'
      AND A.SHAPE IS NOT NULL
  "

  query <- DBI::sqlInterpolate(conn, sqlq, CFMU_AIRAC = cfmu_airac)
  res <- DBI::dbSendQuery(conn, query)
  data <- DBI::fetch(res, n = -1)
  DBI::dbClearResult(res)

  data |>
    dplyr::first() |>
    sf::st_read(quiet = TRUE) |>
    dplyr::filter(
      !.data$CODE %in% c("AIRPORT", "UNKNOWN", "MILITARY")
    ) |>
    dplyr::rename(
      airac_cfmu    = "AC_ID",
      id            = "AV_AIRSPACE_ID",
      code          = "CODE",
      min_fl        = "MIN_FLIGHT_LEVEL",
      max_fl        = "MAX_FLIGHT_LEVEL",
      name          = "NAME",
      ace_code      = "ACE_CODE",
      airspace_type = "AIRSPACE_TYPE",
      NULL
    ) |>
    dplyr::select(
      "airac_cfmu",
      "id",
      "code",
      "name",
      "ace_code",
      "min_fl",
      "max_fl",
      "airspace_type",
      NULL
    )
}


#' Retrieve Elementary Sector airspace geometries for a given AIRAC cycle
#'
#' @description
#' Returns ES (Elementary Sector) airspace geometries as an `sf` object for the
#' specified CFMU AIRAC cycle.
#'
#' # Note
#' You need to either provide a connection `conn` that has access to
#' `ENV_SP.AIRSPACE`, `PRU_CFMU_ES` and `SWH_MAP` or go with the default
#' which uses PRU_DEV to establish a [db_connection()].
#'
#' The `sf` package is required but only suggested; install it if not available.
#'
#' @inheritParams acc_sf
#'
#' @seealso [airac::cfmu_airac()] from the
#'   \href{https://github.com/eurocontrol/airac}{airac} package.
#'
#' @return An `sf` object with the following columns:
#' * `AC_ID`: the AIRAC cycle id
#' * `AV_AIRSPACE_ID`: the airspace identifier
#' * `MIN_FLIGHT_LEVEL`: the minimum flight level
#' * `MAX_FLIGHT_LEVEL`: the maximum flight level
#' * `NAME`: the airspace name
#' * `CODE`: the airspace code
#' * `AIRSPACE_TYPE`: the airspace type (`ES`)
#' * `geometry`: the airspace polygon geometry
#'
#' @export
#'
#' @examples
#' \dontrun{
#' es <- es_sf(cfmu_airac = "517")
#'
#' # write to GeoJSON
#' sf::st_write(es, "es-517.geojson", driver = "GeoJSON")
#'
#' # if you re-use DB connections
#' conn <- eurocontrol::db_connection("PRU_DEV")
#' es <- es_sf(conn = conn, cfmu_airac = "517")
#'
#' # ... do something else with conn
#' # ...
#' # then manually close the connection to the DB
#' DBI::dbDisconnect(conn)
#' }
es_sf <- function(conn = NULL, cfmu_airac) {
  cfmu_airac <- as.character(cfmu_airac)
  rlang::check_installed("sf", reason = "to parse GeoJSON airspace geometries")

  if (is.null(conn)) {
    conn <- db_connection(schema = "PRU_DEV")
  }

  withr::local_envvar(c(
    "TZ" = "UTC",
    "ORA_SDTZ" = "UTC",
    "NLS_LANG" = ".AL32UTF8"
  ))
  withr::local_namespace("ROracle")

  sqlq <- "
    WITH
      AIRSPACE_NAMES AS (
        SELECT DISTINCT
          AIRSPACE_ID,
          AIRSPACE_TYPE,
          NAME,
          CODE
        FROM
          ENV_SP.AIRSPACE A,
          PRU_CFMU_ES P
        WHERE
          A.AIRSPACE_ID = P.ID
          AND A.AIRSPACE_KIND = 'ES'
          AND AC_ID = ?CFMU_AIRAC
      )
    SELECT
      '{ \"type\": \"FeatureCollection\", \"features\": [' ||
      rtrim(SWH_MAP.clobagg('{ \"type\": \"Feature\", \"geometry\": '
      || SWH_MAP.SDO2GEOJSON(SHAPE,3,0,0)
      || ', \"properties\": {'
      || '\"AC_ID\": '               || A.AC_ID                 || ', '
      || '\"AV_AIRSPACE_ID\": \"'    || A.AIRSPACE_ID           || '\", '
      || '\"MIN_FLIGHT_LEVEL\": '    || A.MIN_FLIGHT_LEVEL      || ', '
      || '\"MAX_FLIGHT_LEVEL\": '    || A.MAX_FLIGHT_LEVEL      || ', '
      || '\"NAME\": \"'              || AIRSPACE_NAMES.NAME     || '\", '
      || '\"CODE\": \"'              || AIRSPACE_NAMES.CODE     || '\", '
      || '\"AIRSPACE_TYPE\": \"'     || A.AIRSPACE_TYPE         || '\"'
      || '}}' || ',' || chr(13)),',' || chr(13))                || ']}'
    FROM
      ENV_SP.AIRSPACE A
      LEFT JOIN AIRSPACE_NAMES
        ON (
          A.AIRSPACE_ID = AIRSPACE_NAMES.AIRSPACE_ID
          AND A.AIRSPACE_TYPE = AIRSPACE_NAMES.AIRSPACE_TYPE
        )
    WHERE
      A.AC_ID = ?CFMU_AIRAC
      AND A.AIRSPACE_TYPE = 'ES'
      AND A.SHAPE IS NOT NULL
  "

  query <- DBI::sqlInterpolate(conn, sqlq, CFMU_AIRAC = cfmu_airac)
  res <- DBI::dbSendQuery(conn, query)
  data <- DBI::fetch(res, n = -1)
  DBI::dbClearResult(res)

  data |>
    dplyr::first() |>
    sf::st_read(quiet = TRUE)
}


#' Retrieve FIR (Flight Information Region) airspace geometries for a given AIRAC cycle
#'
#' @description
#' Returns FIR (Flight Information Region) airspace geometries as an `sf` object
#' for the specified CFMU AIRAC cycle.
#'
#' An `icao` column is derived from the first two characters of the airspace
#' `code`.
#'
#' # Note
#' You need to either provide a connection `conn` that has access to
#' `ENV_SP.AIRSPACE_VOLUME`, `PRU_CFMU_FIR` and `SWH_MAP` or go with the
#' default which uses PRU_DEV to establish a [db_connection()].
#'
#' The `sf` package is required but only suggested; install it if not available.
#'
#' @inheritParams acc_sf
#'
#' @seealso [airac::cfmu_airac()] from the
#'   \href{https://github.com/eurocontrol/airac}{airac} package.
#'
#' @return An `sf` object with the following columns:
#' * `airac_cfmu`: the AIRAC cycle id
#' * `icao`: the 2-letter ICAO prefix (derived from `code`)
#' * `id`: the airspace volume id
#' * `code`: the FIR code
#' * `name`: the FIR name
#' * `min_fl`: the minimum flight level
#' * `max_fl`: the maximum flight level
#' * `airspace_type`: the airspace type (`FIR`)
#' * `geometry`: the airspace polygon geometry
#'
#' @export
#'
#' @examples
#' \dontrun{
#' firs <- fir_sf(cfmu_airac = "517")
#'
#' # write to GeoJSON
#' sf::st_write(firs, "ir-517.geojson", driver = "GeoJSON")
#'
#' # if you re-use DB connections
#' conn <- eurocontrol::db_connection("PRU_DEV")
#' firs <- fir_sf(conn = conn, cfmu_airac = "517")
#'
#' # ... do something else with conn
#' # ...
#' # then manually close the connection to the DB
#' DBI::dbDisconnect(conn)
#' }
fir_sf <- function(conn = NULL, cfmu_airac) {
  cfmu_airac <- as.character(cfmu_airac)
  rlang::check_installed("sf", reason = "to parse GeoJSON airspace geometries")

  if (is.null(conn)) {
    conn <- db_connection(schema = "PRU_DEV")
  }

  withr::local_envvar(c(
    "TZ" = "UTC",
    "ORA_SDTZ" = "UTC",
    "NLS_LANG" = ".AL32UTF8"
  ))
  withr::local_namespace("ROracle")

  sqlq <- "
    WITH
      AIRSPACE_NAMES AS (
        SELECT DISTINCT
          AIRSPACE_ID,
          AV_TYPE AS AIRSPACE_TYPE,
          NAME,
          CODE,
          A.ID AS ID
        FROM
          ENV_SP.AIRSPACE_VOLUME A,
          PRU_CFMU_FIR P
        WHERE
          A.AIRSPACE_ID = P.CODE
          AND A.AV_TYPE = 'FIR'
          AND AC_ID = ?CFMU_AIRAC
      )
    SELECT
      '{ \"type\": \"FeatureCollection\", \"features\": [' ||
      rtrim(SWH_MAP.clobagg('{ \"type\": \"Feature\", \"geometry\": '
      || SWH_MAP.SDO2GEOJSON(SHAPE,3,0,0)
      || ', \"properties\": {'
      || '\"AC_ID\": '               || A.AC_ID                 || ', '
      || '\"AV_AIRSPACE_ID\": \"'    || A.AIRSPACE_ID           || '\", '
      || '\"MIN_FLIGHT_LEVEL\": '    || A.MIN_FLIGHT_LEVEL      || ', '
      || '\"MAX_FLIGHT_LEVEL\": '    || A.MAX_FLIGHT_LEVEL      || ', '
      || '\"NAME\": \"'              || AIRSPACE_NAMES.NAME     || '\", '
      || '\"CODE\": \"'              || AIRSPACE_NAMES.CODE     || '\", '
      || '\"ID\": \"'                || AIRSPACE_NAMES.ID       || '\", '
      || '\"AIRSPACE_TYPE\": \"'     || A.AV_TYPE               || '\"'
      || '}}' || ',' || chr(13)),',' || chr(13))                || ']}'
    FROM
      ENV_SP.AIRSPACE_VOLUME A
      LEFT JOIN AIRSPACE_NAMES
        ON (
          A.AIRSPACE_ID = AIRSPACE_NAMES.AIRSPACE_ID
          AND A.AV_TYPE = AIRSPACE_NAMES.AIRSPACE_TYPE
        )
    WHERE
      A.AC_ID = ?CFMU_AIRAC
      AND A.AV_TYPE = 'FIR'
      AND A.SHAPE IS NOT NULL
  "

  query <- DBI::sqlInterpolate(conn, sqlq, CFMU_AIRAC = cfmu_airac)
  res <- DBI::dbSendQuery(conn, query)
  data <- DBI::fetch(res, n = -1)
  DBI::dbClearResult(res)

  data |>
    dplyr::first() |>
    sf::st_read(quiet = TRUE) |>
    dplyr::rename(
      airac_cfmu    = "AC_ID",
      code          = "CODE",
      id            = "ID",
      min_fl        = "MIN_FLIGHT_LEVEL",
      max_fl        = "MAX_FLIGHT_LEVEL",
      name          = "NAME",
      airspace_type = "AIRSPACE_TYPE",
      NULL
    ) |>
    dplyr::mutate(
      icao = stringr::str_sub(.data$code, start = 1L, end = 2L)
    ) |>
    dplyr::select(
      "airac_cfmu",
      "icao",
      "id",
      "code",
      "name",
      "min_fl",
      "max_fl",
      "airspace_type",
      NULL
    )
}
