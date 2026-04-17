# Provide all airspace profile segments intersecting an interval of interest

The returned
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
includes segments for scheduled and non-scheduled flights temporally
intersecting the right-opened interval `[wef, til)`.

General aviation, State, military and sensitive flight are excluded.

## Usage

``` r
airspace_profiles_tidy(
  conn = NULL,
  wef,
  til,
  airspace = "FIR",
  profile = "CTFM"
)
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

- airspace:

  the type of airspace (default: 'FIR'), one of:

  - 'FIR' ([Flight Information
    Region](https://observablehq.com/@openaviation/flight-information-regions))

  - 'NAS' (National Airspace)

  - 'AUA' (ATC Unit Airspace)

  - 'ES' (Elementary Sector)

- profile:

  the [model of the
  trajectory](https://ansperformance.eu/definition/flight-models/)
  profile (default: 'CTFM'), one of:

  - 'FTFM', Filed Tactical Flight Model

  - 'RTFM', Regulated Tactical Flight Model

  - 'CTFM', Current Tactical Flight Model

  - 'CPF', Correlated Position reports for a Flight

  - 'DCT', Direct route

  - 'SCR', Shortest Constrained Route

  - 'SRR', Shortest RAD restrictions applied Route

  - 'SUR', Shortest Unconstrained Route

## Value

a
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
with the following columns

- ID: the so called `SAM ID`, used internally by PRISME

- FLT_UID: flight unique id.

- SEQ_ID: the sequence number of the segment for the relevant airspace
  profile

- ENTRY_TIME: the time of entry into the relevant airspace

- ENTRY_LON: the longitude of entry into the relevant airspace

- ENTRY_LAT: the latitude of entry into the relevant airspace

- ENTRY_FL: the flight level of entry into the relevant airspace

- EXIT_TIME: the time of exit out of the relevant airspace

- EXIT_LON: the longitude of exit out of the relevant airspace

- EXIT_LAT: the latitude of exit out of the relevant airspace

- EXIT_FL: the flight level of exit out of the relevant airspace

- AIRSPACE_ID: the airspace ID

- AIRSPACE_TYPE: the airspace type as per `airspace` input parameter

- MODEL_TYPE: the trajectory model as per `profile` input parameter

## Note

You need to either provide a connection `conn` that has access to as
noted in
[`airspace_profile_tbl()`](https://eurocontrol.github.io/eurocontrol/reference/airspace_profile_tbl.md)
and
[`flights_tidy()`](https://eurocontrol.github.io/eurocontrol/reference/flights_tidy.md)
or go with the default which uses PRU_READ to establish a
[`db_connection()`](https://eurocontrol.github.io/eurocontrol/reference/db_connection.md).

## Examples

``` r
if (FALSE) { # \dontrun{
ps <- airspace_profiles_tidy(wef = "2023-01-01", til = "2023-04-01")
# IMPORTANT: always close the DB connection when done
DBI::dbDisconnect(ps$src$con)

# if you re-use DB connections
conn <- eurocontrol::db_connection("PRU_READ")
ps <- airspace_profiles_tidy(conn = conn)

# ... do something else with conn
# ...
# then manually close the connection to the DB
DBI::dbDisconnect(conn)
} # }
```
