# Export trajectory profiles to SO6 format

The data frame for point trajectories needs to have the following
columns:

|                     |                                 |          |
|---------------------|---------------------------------|----------|
| **Name**            | **Description**                 | **Type** |
| `FLIGHT_ID`         | Flight ID                       | int      |
| `TIME_OVER`         | Time over point                 | datetime |
| `LONGITUDE`         | Longitude (decimal degrees)     | double   |
| `LATITUDE`          | Latitude (decimal degrees)      | double   |
| `FLIGHT_LEVEL`      | Flight level                    | int      |
| `POINT_ID`          | Point ID or NO_POINT            | char     |
| `AIR_ROUTE`         | Air route or NO_ROUTE           | char     |
| `LOBT`              | Last Off-block Time             | datetime |
| `SEQ_ID`            | Positions' sequence number      | int      |
| `CALLSIGN`          | Flight call sign                | char     |
| `REGISTRATION`      | Aircraft registration           | char     |
| `MODEL_TYPE`        | Aircraft model                  | char     |
| `AIRCRAFT_TYPE`     | Aircraft ICAO type              | char     |
| `AIRCRAFT_OPERATOR` | Aircraft operator               | char     |
| `ADEP`              | Departing aerodrome (ICAO) ID   | char     |
| `ADES`              | Destination aerodrome (ICAO) ID | char     |

## Usage

``` r
generate_so6(trajectory)
```

## Arguments

- trajectory:

  A data frame for point profile trajectories.

## Value

A data frame for trajectories in SO6 format.

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- eurocontrol::db_connection("PRU_READ")
pf <- point_profiles_tidy(conn = conn,
                          wef = "2020-01-01",
                          til = "2020-01-10") |>
  generate_so6()

# ... do something else with conn
# ...
# then manually close the connection to the DB
DBI::dbDisconnect(conn)
generate_so6(trj)
} # }
```
