# Return a reference to the Point Profile table

The returned
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
is referencing the point profiles table in PRISME. You can use
`dplyr`/`dbplyr` verbs to filter, join, ... with other datasets.

## Usage

``` r
point_profile_tbl(conn = NULL)
```

## Arguments

- conn:

  Database connection or instantiate the default one.

## Value

a
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
referencing the Oracle table for point profiles.

## Note

You need to either provide a connection `conn` that has access to
`FSD.ALL_FT_POINT_PROFILE` or go with the default which uses PRU_READ to
establish a
[`db_connection()`](https://eurocontrol.github.io/eurocontrol/reference/db_connection.md).

## Examples

``` r
if (FALSE) { # \dontrun{
pt <- point_profile_tbl()

# if you re-use DB connections
conn <- eurocontrol::db_connection("PRU_READ")
pt <- point_profile_tbl(conn = conn)

# ... do something else with conn
# ...
# then manually close the connection to the DB
DBI::dbDisconnect(conn)
} # }
```
