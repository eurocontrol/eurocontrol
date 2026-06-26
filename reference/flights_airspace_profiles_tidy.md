# Extract the segments intersecting a set of airspace IDs in an interval of time and complement them with flight information

The returned
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
includes scheduled and non-scheduled flights whose airspace segments
temporally intersecting the right-opened interval `[wef, til)`. General
aviation, State, military and sensitive flight are excluded.

## Usage

``` r
flights_airspace_profiles_tidy(
  conn = NULL,
  wef,
  til,
  airspaces,
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

- airspaces:

  list of airspace ids

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
with the same columns as
[`flights_tidy()`](https://eurocontrol.github.io/eurocontrol/reference/flights_tidy.md)
plus the airspace id, `AIRSPACE_ID`, being crossed, one row per crossed
airspace.

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
withr::local_envvar(c(TZ = "UTC", ORA_SDTZ = "UTC", NLS_LANG = ".AL32UTF8"))
conn <- withr::local_db_connection(db_connection("PRU_READ"))
flights_airspace_profiles_tidy(
  conn = conn,
  wef = "2023-01-01",
  til = "2023-04-01",
  airspace = "ES",
  profile = "CTFM",
  airspaces = c("LYBALW2", "LYBATW2")
)
} # }
```
