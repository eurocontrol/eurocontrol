# Export point profile from NM trajectories

Extract NM point profile trajectories from PRISME database. When a bbox
is defined, we return only the (full) point profiles for the flights
flying thru the region.

## Usage

``` r
point_profiles_tidy(
  conn = NULL,
  wef,
  til = lubridate::today(tzone = "UTC"),
  profile = "CTFM",
  bbox = NULL
)
```

## Arguments

- conn:

  Database connection or instantiate the default one.

- wef:

  **W**ith **EF**fect date (included) at Zulu time in a format
  recognized by
  [`lubridate::as_datetime()`](https://lubridate.tidyverse.org/reference/as_date.html)

- til:

  un**TIL**l date (excluded) at Zulu time in a format recognized by
  [`lubridate::as_datetime()`](https://lubridate.tidyverse.org/reference/as_date.html)

- profile:

  the [model of the
  trajectory](https://ansperformance.eu/definition/flight-models/)
  profile (default: 'CTFM'), one of:

  - 'FTFM', Filed Tactical Flight Model

  - 'RTFM', Regulated Tactical Flight Model

  - 'CTFM', Current Tactical Flight Model

  - 'CPF', Correlated Position reports for a Flight

  - 'DCT', Direct route

  - 'SCR', Shortest Constrained Route

  - 'SRR', Shortest RAD restrictions applied Route

  - 'SUR', Shortest Unconstrained Route

- bbox:

  (Optional) axis aligned bounding box (xmin, ymin, xmax, ymax)

## Value

a dataframe representing a flight trajectory with the following columns:

- FLIGHT_ID: a unique identifier for the flight

- TIME_OVER: the time over llon/lat

- LONGITUDE: the longitude

- LATITUDE: the latitude

- FLIGHT_LEVEL: the [flight
  level](https://ansperformance.eu/acronym/fl/)

- POINT_ID: the published point ID ('NO_POINT' otherwise)

- AIR_ROUTE: the air rout name ('DCT' otherwise)

- LOBT: the [last off-block
  time](https://ansperformance.eu/acronym/lobt/)

- SEQ_ID: the progressive sequence number in the trajectory points

- CALLSIGN: the [callsign](https://w.wiki/9XN6) of the flight

- REGISTRATION: the aircraft [registration](https://w.wiki/9XN7)

- MODEL_TYPE: the trajectory model as per `profile` input parameter

- AIRCRAFT_TYPE: the ICAO aircraft type

- AIRCRAFT_OPERATOR: the flight operator

- ICAO24: the ICAO 24-bit address of the aircraft

- ADEP: the Aerodrom of Departure

- ADES: the aerodrome of Destination

## Note

You need to either provide a connection `conn` that has access to as
noted in
[`airspace_profile_tbl()`](https://eurocontrol.github.io/eurocontrol/reference/airspace_profile_tbl.md)
and
[`flights_tidy()`](https://eurocontrol.github.io/eurocontrol/reference/flights_tidy.md)
or go with the default which uses PRU_READ to establish a
[`db_connection()`](https://eurocontrol.github.io/eurocontrol/reference/db_connection.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# export 1 day of NM (planned) trajectories
pf1 <- point_profiles_tidy(wef = "2019-07-14",
                           til = "2019-07-15",
                           profile = "FTFM")

# export 2 hours of NM (flown) trajectories
pf2 <- point_profiles_tidy(wef = "2019-07-14 22:00",
                           til = "2019-07-15")

# export 1 day of NM (flown) trajectories
pf3 <- point_profiles_tidy(wef = "2019-07-14",
                           til = "2019-07-15",
                           profile = "CTFM")

# export all CTFM trajectories within a bounding box 40 NM around EDDF
bb <- c(xmin = 7.536746, xmax = 9.604390, ymin = 49.36732, ymax = 50.69920)
pf4 <- point_profiles_tidy(wef = "2019-01-01 00:00",
                           til = "2019-01-02 00:00",
                           bbox = bb)


# if you re-use DB connections
conn <- eurocontrol::db_connection("PRU_READ")
pf <- point_profiles_tidy(conn = conn,
                          wef = "2020-01-01",
                          til = "2020-01-10")

# ... do something else with conn
# ...
# then manually close the connection to the DB
DBI::dbDisconnect(conn)
} # }
```
