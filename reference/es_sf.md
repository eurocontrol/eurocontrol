# Retrieve Elementary Sector airspace geometries for a given AIRAC cycle

Returns ES (Elementary Sector) airspace geometries as an `sf` object for
the specified CFMU AIRAC cycle.

## Usage

``` r
es_sf(conn = NULL, cfmu_airac)
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

- `AC_ID`: the AIRAC cycle id

- `AV_AIRSPACE_ID`: the airspace identifier

- `MIN_FLIGHT_LEVEL`: the minimum flight level

- `MAX_FLIGHT_LEVEL`: the maximum flight level

- `NAME`: the airspace name

- `CODE`: the airspace code

- `AIRSPACE_TYPE`: the airspace type (`ES`)

- `geometry`: the airspace polygon geometry

## Note

You need to either provide a connection `conn` that has access to
`ENV_SP.AIRSPACE`, `PRU_CFMU_ES` and `SWH_MAP` or go with the default
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
es <- es_sf(cfmu_airac = "517")

# write to GeoJSON
sf::st_write(es, "es-517.geojson", driver = "GeoJSON")

# if you re-use DB connections
conn <- eurocontrol::db_connection("PRU_DEV")
es <- es_sf(conn = conn, cfmu_airac = "517")

# ... do something else with conn
# ...
# then manually close the connection to the DB
DBI::dbDisconnect(conn)
} # }
```
