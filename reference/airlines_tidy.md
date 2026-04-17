# Airline info including group affiliation

Airline info including group affiliation

## Usage

``` r
airlines_tidy(conn = NULL)
```

## Arguments

- conn:

  Optional connection to the PRU_READ schema.

## Value

A
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
with the following columns:

- `AO_CODE`: the the [ICAO Airline
  Designator](https://en.wikipedia.org/wiki/List_of_airline_codes), i.e.
  'OAL'

- `AO_NAME`: the airline's name, i.e. 'Olympic'

- `AO_GRP_CODE`: the airline's affiliation group code, i.e. 'AEE_GRP'

- `AO_GRP_NAME`: the airline's affiliation group, i.e. 'AEGEAN Group'

- `AO_ISO_CTRY_CODE`: the ISO2C code of the airline's country, i.e. 'GR'

- `EU`: (a character) whether the airlines is in a EUROCONTROL's Member
  State (full, comprehensive or transition plus Kosovo), i.e. 'TRUE'

## Note

You need to either provide a connection `conn` that has access to
`PRUDEV.V_COVID_DIM_AO` or go with the default which uses PRU_READ to
establish a
[`db_connection()`](https://eurocontrol.github.io/eurocontrol/reference/db_connection.md).

## Examples

``` r
if (FALSE) { # \dontrun{
arls <- airlines_tidy()
# other operations on arls, i.e. filtering,
# followed by a collect() to retrieve the concrete data frame
arls_filtered <- arls |>
   filter(stringr::str_starts("A")) |>
   collect()

# NOTE: you can reuse the connection for other API calls
conn <- arls$src$con

# other ops requiring conn
# ...

# IMPORTANT: close the DB connection
DBI::dbDisconnect(conn)
} # }
```
