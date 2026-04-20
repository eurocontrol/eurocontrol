# Package index

## Access to Data(bases)

DB conection.

- [`db_connection()`](https://eurocontrol.github.io/eurocontrol/reference/db_connection.md)
  : Provide a connection to the relevant Oracle database

### Data

Useful data.

- [`member_state`](https://eurocontrol.github.io/eurocontrol/reference/member_state.md)
  : EUROCONTROL's Member States
- [`aircraft_type`](https://eurocontrol.github.io/eurocontrol/reference/aircraft_type.md)
  : ICAO's Aircraft types
- [`aircraft_model`](https://eurocontrol.github.io/eurocontrol/reference/aircraft_model.md)
  : ICAO's Manufacturer codes

### Datasets

Helpers for typical queries.

- [`airlines_tidy()`](https://eurocontrol.github.io/eurocontrol/reference/airlines_tidy.md)
  : Airline info including group affiliation
- [`airspace_profiles_tidy()`](https://eurocontrol.github.io/eurocontrol/reference/airspace_profiles_tidy.md)
  : Provide all airspace profile segments intersecting an interval of
  interest
- [`apdf_tidy()`](https://eurocontrol.github.io/eurocontrol/reference/apdf_tidy.md)
  : Extract a clean airport operator data flow list in an interval
- [`flights_airspace_profiles_tidy()`](https://eurocontrol.github.io/eurocontrol/reference/flights_airspace_profiles_tidy.md)
  : Extract the flights list for the airspace profile segments
  intersecting an interval of interest
- [`flights_tidy()`](https://eurocontrol.github.io/eurocontrol/reference/flights_tidy.md)
  : Extract a clean flights list in an interval
- [`point_profiles_tidy()`](https://eurocontrol.github.io/eurocontrol/reference/point_profiles_tidy.md)
  : Export point profile from NM trajectories
- [`airports_oa()`](https://eurocontrol.github.io/eurocontrol/reference/airports_oa.md)
  : retrieve latest airport list from OurAirports
- [`adrr_flights_tidy()`](https://eurocontrol.github.io/eurocontrol/reference/adrr_flights_tidy.md)
  : Extract ADRR flight list in an interval of dates

### Formats

Helpers for format conversions.

- [`generate_so6()`](https://eurocontrol.github.io/eurocontrol/reference/generate_so6.md)
  : Export trajectory profiles to SO6 format

### IATA season

Helpers for IATA season.

- [`season_iata()`](https://eurocontrol.github.io/eurocontrol/reference/season_iata.md)
  : return the interval for an IATA season
- [`iata_season_for_date()`](https://eurocontrol.github.io/eurocontrol/reference/iata_season_for_date.md)
  : Return the corresponding IATA season for a date

### Tables

Access to typical tables.

- [`airlines_tbl()`](https://eurocontrol.github.io/eurocontrol/reference/airlines_tbl.md)
  : Return a reference to the Airlines table
- [`airspace_profile_tbl()`](https://eurocontrol.github.io/eurocontrol/reference/airspace_profile_tbl.md)
  : Return a reference to the Airspace Profile table
- [`apdf_tbl()`](https://eurocontrol.github.io/eurocontrol/reference/apdf_tbl.md)
  : Return a reference to the Airport Operator Data Flow table
- [`flights_tbl()`](https://eurocontrol.github.io/eurocontrol/reference/flights_tbl.md)
  : Return a reference to the Flights table
- [`point_profile_tbl()`](https://eurocontrol.github.io/eurocontrol/reference/point_profile_tbl.md)
  : Return a reference to the Point Profile table
