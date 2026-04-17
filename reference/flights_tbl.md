# Return a reference to the Flights table

The returned
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
is referencing the flights table in PRISME. You can use `dplyr`/`dbplyr`
verbs to filter, join, ... with other datasets.

## Usage

``` r
flights_tbl(conn = NULL)
```

## Arguments

- conn:

  Database connection or instantiate the default one.

## Value

a
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
object referencing the Oracle table for flights.

## Note

You need to either provide a connection `conn` that has access to
`SWH_FCT.V_FAC_FLIGHT_MS` or go with the default which uses PRU_READ to
establish a
[`db_connection()`](https://eurocontrol.github.io/eurocontrol/reference/db_connection.md).
Market Segment is not available before 2004.

## Examples

``` r
if (FALSE) { # \dontrun{
flt <- flights_tbl()
# other operations on flt, i.e. filtering,
# followed by a collect() to retrieve the concrete data frame
flt_filtered <- flt |>
   filter(TO_DATE("2023-06-01 10:00", "YYYY-MM-DD HH24:MI") <= IOBT,
                  IOBT < TO_DATE("2023-06-02 10:30", "YYYY-MM-DD HH24:MI")) |>
   collect()


# NOTE: you can reuse the connection for other API calls
conn <- flt$src$con

# other ops requiring conn
# ...

# IMPORTANT: close the DB connection
DBI::dbDisconnect(conn)
} # }
```
