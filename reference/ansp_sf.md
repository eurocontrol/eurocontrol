# Retrieve ACE ANSP airspace geometries for a given AIRAC cycle

Returns ANSP (Air Navigation Service Provider) airspace geometries from
the ACE (ATM Cost-Effectiveness) benchmarking dataset as an `sf` object
for the specified CFMU AIRAC cycle.

Airspaces with code `AIRPORT`, `UNKNOWN` or `MILITARY` are excluded.

## Usage

``` r
ansp_sf(conn = NULL, cfmu_airac)
```

## Arguments

- conn:

  Database connection or instantiate the default one.

- cfmu_airac:

  the AIRAC cycle number (CFMU format), e.g. `"517"` or `517`. Both
  character and numeric inputs are accepted. See `airac::cfmu_airac()`
  to look up the cycle number for a given date.

## Value

An `sf` object with the following columns:

- `airac_cfmu`: the AIRAC cycle id

- `id`: the airspace identifier

- `code`: the ANSP code

- `name`: the ANSP name

- `ace_code`: the ACE benchmarking code

- `min_fl`: the minimum flight level

- `max_fl`: the maximum flight level

- `airspace_type`: the airspace type

- `geometry`: the airspace polygon geometry

## Note

You need to either provide a connection `conn` that has access to
`ENV_SP.AIRSPACE`, `PRU_CFMU_ANSP` and `SWH_MAP` or go with the default
which uses PRU_DEV to establish a
[`db_connection()`](https://eurocontrol.github.io/eurocontrol/reference/db_connection.md).

The `sf` package is required but only suggested; install it if not
available.

## See also

`airac::cfmu_airac()` from the
[airac](https://github.com/eurocontrol/airac) package.

## Examples

``` r
if (FALSE) { # \dontrun{
ansps <- ansp_sf(cfmu_airac = "481")

# write to GeoJSON
sf::st_write(ansps, "ansp_ace_481.geojson", driver = "GeoJSON")

# if you re-use DB connections
conn <- eurocontrol::db_connection("PRU_DEV")
ansps <- ansp_sf(conn = conn, cfmu_airac = "481")

# ... do something else with conn
# ...
# then manually close the connection to the DB
DBI::dbDisconnect(conn)
} # }
```
