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

- `ADEP`: [ICAO code](https://observablehq.com/@openaviation/airports)
  of the **A**erodrome of **DEP**arture

- `ADEP Latitude`: `ADEP` latitude (decimal degrees North)

- `ADEP Longitude`: `ADEP` longitude (decimal degrees East)

- `ADES`: [ICAO code](https://observablehq.com/@openaviation/airports)
  of the **A**erodrome of **DES**tination

- `ADES Latitude`: `ADES` latitude (decimal degrees North)

- `ADES Longitude`: `ADES` longitude (decimal degrees East)

- `FILED OFF BLOCK TIME`: (Last) filed off-block time

- `FILED ARRIVAL TIME`: (Last) filed arrival time

- `ACTUAL OFF BLOCK TIME`: actual off-block time for flown (M3)
  trajectory

- `ACTUAL ARRIVAL TIME`: arrival time for flown (M3) trajectory

- `AC Type`: the [ICAO code for the aircraft
  type](https://www.icao.int/publications/doc8643/pages/search.aspx),
  for example A30B for an Airbus A-300B2-200.

- `AC Operator`: the [ICAO Airline
  Designator](https://en.wikipedia.org/wiki/List_of_airline_codes), i.e.
  `OAL` for `Olympic`

- `AC Registration`: the [aircraft
  registration](https://en.wikipedia.org/wiki/Aircraft_registration)
  (with spaces, dashes, ... stripped), e.g. `GEUUU`

- `ICAO Flight Type`: flight type (see [FPL Item
  8](https://www.skybrary.aero/articles/flight-plan-completion)).
  Possible values:

  - `S` for scheduled air service

  - `N` for non-scheduled air service

- `STATFOR Market Segment`: market segment type as defined on the
  [Market Segment
  Rules](https://www.eurocontrol.int/publication/market-segment-rules),
  it can be:

  - “Mainline”

  - “Regional”

  - “Low-Cost”

  - “Business Aviation”

  - “All-Cargo”

  - “Charter” (Non-Scheduled)

  - “Military”

  - “Other”

  - "Not classified"

- `Requested FL`: requested cruising flight level (FL)

- `Actual Distance Flown (nm)`: route length (in Nautical Miles) for for
  flown (M3) trajectory

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
