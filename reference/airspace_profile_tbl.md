# Return a reference to the Airspace Profile table

The returned
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
is referencing the airspace profiles table in PRISME. You can use
`dplyr`/`dbplyr` verbs to filter, join, ... with other datasets.

## Usage

``` r
airspace_profile_tbl(conn = NULL)
```

## Arguments

- conn:

  Database connection or instantiate the default one.

## Value

a
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
referencing the Oracle table for airspace profiles.

## Note

You need to either provide a connection `conn` that has access to
`FSD.ALL_FT_ASP_PROFILE` or go with the default which uses PRU_READ to
establish a
[`db_connection()`](https://eurocontrol.github.io/eurocontrol/reference/db_connection.md).

## Examples

``` r
if (FALSE) { # \dontrun{
pp <- airspace_profile_tbl()
# other operations on pp, i.e. filtering,
# followed by a collect() to retrieve the concrete data frame
# IMPORTANT: close the DB connection when done
DBI::dbDisconnect(pp$src$con)

# if you use a DB connection for many different APIs
conn <- eurocontrol::db_connection("PRU_READ")
pp <- airspace_profile_tbl(conn = conn)

# ... do something else with conn
# ...
# then manually close the connection to the DB
DBI::dbDisconnect(conn)
} # }
```
