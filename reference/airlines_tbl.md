# Return a reference to the Airlines table

The returned
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
is referencing the airlines table in PRISME. You can use
`dplyr`/`dbplyr` verbs to filter, join, ... with other datasets.

## Usage

``` r
airlines_tbl(conn = NULL)
```

## Arguments

- conn:

  Database connection or instantiate the default one.

## Value

a
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
referencing the Oracle table for airlines.

## Note

You need to either provide a connection `conn` that has access to
`PRUDEV.V_COVID_DIM_AO` or go with the default which uses PRU_READ to
establish a
[`db_connection()`](https://eurocontrol.github.io/eurocontrol/reference/db_connection.md).

## Examples

``` r
if (FALSE) { # \dontrun{
arl <- airlines_tbl()
# other operations on arl, i.e. filtering,
# followed by a collect() to retrieve the concrete data frame
arl_filtered <- arl |>
   dplyr::filter(AO_ISO_CTRY_CODE == "IT") |>
   collect()

# NOTE: you can reuse the connection for other API calls
conn <- arl$src$con

# other ops requiring conn
# ...

# IMPORTANT: close the DB connection
DBI::dbDisconnect(conn)
} # }
```
