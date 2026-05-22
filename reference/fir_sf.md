# Retrieve FIR (Flight Information Region) airspace geometries for a given AIRAC cycle

Returns FIR (Flight Information Region) airspace geometries as an `sf`
object for the specified CFMU AIRAC cycle.

An `icao` column is derived from the first two characters of the
airspace `code`.

## Usage

``` r
fir_sf(conn = NULL, cfmu_airac)
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

- `icao`: the 2-letter ICAO prefix (derived from `code`)

- `id`: the airspace volume id

- `code`: the FIR code

- `name`: the FIR name

- `min_fl`: the minimum flight level

- `max_fl`: the maximum flight level

- `airspace_type`: the airspace type (`FIR`)

- `geometry`: the airspace polygon geometry

## Note

You need to either provide a connection `conn` that has access to
`ENV_SP.AIRSPACE_VOLUME`, `PRU_CFMU_FIR` and `SWH_MAP` or go with the
default which uses PRU_DEV to establish a
[`db_connection()`](https://eurocontrol.github.io/eurocontrol/reference/db_connection.md).

The `sf` package is required but only suggested; install it if not
available.

## See also

`airac::cfmu_airac()` from the
[airac](https://github.com/eurocontrol/airac) package.

## Examples

``` r
if (FALSE) { # \dontrun{
firs <- fir_sf(cfmu_airac = "517")

# write to GeoJSON
sf::st_write(firs, "ir-517.geojson", driver = "GeoJSON")

# if you re-use DB connections
conn <- eurocontrol::db_connection("PRU_DEV")
firs <- fir_sf(conn = conn, cfmu_airac = "517")

# ... do something else with conn
# ...
# then manually close the connection to the DB
DBI::dbDisconnect(conn)
} # }
```
