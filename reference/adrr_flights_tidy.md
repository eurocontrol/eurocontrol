# Extract ADRR flight list in an interval of dates

Extract ADRR flight list in an interval of dates

## Usage

``` r
adrr_flights_tidy(conn = NULL, wef, til)
```

## Arguments

- conn:

  Database connection or instantiate the default one.

- wef:

  **W**ith **EF**fect date (included) at Zulu time in a format
  recognized by
  [`lubridate::as_datetime()`](https://lubridate.tidyverse.org/reference/as_date.html)

- til:

  un**TIL**l date (excluded) at Zulu time in a format recognized by
  [`lubridate::as_datetime()`](https://lubridate.tidyverse.org/reference/as_date.html)

## Value

A
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
with the following columns (as defined in the Aviation Data Repository
for Research manual):

### Flight details

- `ECTRL ID`: flight unique id.

- `ADEP`/`ADES`:

- `ADEP Latitude`/`ADES Latitude`:

- `ADEP Longitude`/`ADES Longitude`:

- `FILED OFF BLOCK TIME`:

- `FILED ARRIVAL TIME`:

- `ACTUAL OFF BLOCK TIME`:

- `ACTUAL ARRIVAL TIME`:

- `AC Type`:

- `AC Operator`:

- `AC Registration`:

- `ICAO Flight Type`:

- `STATFOR Market Segment`:

- `Requested FL`:

- `Actual Distance Flown (nm)`:

## Note

You need to either provide a connection `conn` that has access to
`SWH_FCT.DIM_FLIGHT_TYPE_RULE` (for `FLT_RULES`),
`PRUDEV.V_COVID_DIM_AO` (for aircraft and aircraft group info) and
`SWH_FCT.V_FAC_FLIGHT_MS` (for market segment info) or go with the
default which uses PRU_READ to establish a
[`db_connection()`](https://eurocontrol.github.io/eurocontrol/reference/db_connection.md).

## Examples

``` r
if (FALSE) { # \dontrun{
withr::local_envvar(c(TZ = "UTC", ORA_SDTZ = "UTC", NLS_LANG = ".AL32UTF8"))
conn <- withr::local_db_connection(db_connection(schema = "PRU_READ"))
wef <- "2025-01-01"
til <- "2025-01-04"
adrr_flights_tidy(conn, wef, til)
} # }
```
