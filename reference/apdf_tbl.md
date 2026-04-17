# Return a reference to the Airport Operator Data Flow table

The returned
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
is referencing the airport operator data flow table in PRISME. You can
use `dplyr`/`dbplyr` verbs to filter, join, ... with other datasets.

## Usage

``` r
apdf_tbl(conn = NULL)
```

## Arguments

- conn:

  Database connection or instantiate the default one.

## Value

a
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
referencing the Oracle table for airport operator data flow.

## Note

You need to either provide a connection `conn` that has access to
`SWH_FCT.FAC_APDS_FLIGHT_IR691` or go with the default which uses
PRU_READ to establish a
[`db_connection()`](https://eurocontrol.github.io/eurocontrol/reference/db_connection.md).

## Examples

``` r
if (FALSE) { # \dontrun{
apdf <- apdf_tbl()
# ...
# IMPORTANT: close the DB connection when done with `aodf`
DBI::dbDisconnect(apdf$src$con)
} # }
```
