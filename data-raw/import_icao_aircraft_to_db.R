
library(eurocontrol)
library(dbplyr)
library(dplyr)
library(withr)
library(DBI)

# See data-raw/aircraft_type_icao.R for code to scrap ICAO data

con <- eurocontrol::db_connection()
# # This does not work for Oracle because there is no BEGIN TRANSACTION support
# copy_to(
#   dest      = con,
#   df        = aircraft_type,
#   name      = "ICAO_AIRCRAFT_TYPE"
# )


local_envvar(c("TZ" = "UTC", "ORA_SDTZ" = "UTC"))

DBI::dbRemoveTable(con, "ICAO_AIRCRAFT_TYPE")
DBI::dbWriteTable(
  conn      = con,
  name      = "ICAO_AIRCRAFT_TYPE",
  value     = aircraft_type,
  append    = FALSE,
  row.names = FALSE
)

DBI::dbRemoveTable(con, "ICAO_AIRCRAFT_MODEL")
DBI::dbWriteTable(
  conn      = con,
  name      = "ICAO_AIRCRAFT_MODEL",
  value     = aircraft_model,
  append    = FALSE,
  row.names = FALSE
)


icao_aircraft_type <- tbl(con, "ICAO_AIRCRAFT_TYPE") |> collect()
icao_aircraft_model <- tbl(con, "ICAO_AIRCRAFT_MODEL") |> collect()


DBI::dbDisconnect(con)


# SELECT
# *
#   -- sid, serial#, username, status
# FROM
# v$session
# WHERE
# status = 'ACTIVE'
# AND USERNAME IS NOT NULL
# AND USERNAME = 'PRUDEV'

# ALTER SYSTEM KILL SESSION 'sid,serial#' IMMEDIATE;
