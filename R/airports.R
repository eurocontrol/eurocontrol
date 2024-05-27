# airports currently valid

# SELECT
# *
#   FROM
# SWH_FCT.DIM_AIRPORT
# WHERE
# VALID_FROM <= SYSDATE AND SYSDATE < VALID_TO;

export_airports <- function(wef, til) {
  withr::local_envvar(c(
    "TZ" = "UTC",
    "ORA_SDTZ" = "UTC",
    "NLS_LANG" = ".AL32UTF8"
  ))
  conn <- withr::local_db_connection(eurocontrol::db_connection(schema = "PRU_DEV"))

  conn |>
    dplyr::tbl(dbplyr::in_schema("SWH_FCT", "DIM_AIRPORT")) |>
    dplyr::filter(.data$VALID_FROM <= TO_DATE(wef, "YYYY-MM-DD"), TO_DATE(til, "YYYY-MM-DD") <= .data$VALID_TO)
}
