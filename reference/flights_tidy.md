# Extract a clean flights list in an interval

The returned
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
includes scheduled and non-scheduled flight departing in the
right-opened interval `[wef, til)`.

Defaults values will assure that General aviation, State, military and
sensitive flight will excluded. They can be retrieved via the other
function call arguments in case of need.

## Usage

``` r
flights_tidy(
  conn = NULL,
  wef,
  til,
  icao_flt_types = c("S", "N"),
  ids = NULL,
  include_sensitive = FALSE,
  include_military = FALSE,
  include_head = FALSE
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

- icao_flt_types:

  the types of flights as described below in `ICAO_FLT_TYPE`, default
  `c('S', 'N')`, NULL includes all notwithstanding other argument
  options. When including military via `include_military` you should
  either pass NULL or make sure 'M' is included

- ids:

  list of `ID`s (aka `SAM ID`) to return, default NULL for all flights

- include_sensitive:

  include sensitive flights, default FALSE

- include_military:

  include military flights, default FALSE

- include_head:

  include Head of State flights, default FALSE

## Value

A
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
with the following columns (grouped here by flight details, aerodrome
details, aircraft info, aircraft operator info and operational details):

### Flight details

- FLT_UID: flight unique id.

- ID: the so called `SAM ID`, used internally by PRISME

- AIRCRAFT_ID: the
  [callsign](https://www.skybrary.aero/articles/aircraft-call-sign) of
  the relevant flight, e.g. BAW6VB.

- LOBT: **L**ast received **O**ff-**B**lock **T**ime.

- IOBT: **I**nitial **O**ff-**B**lock **T**ime.

- FLT_RULES (see [FPL Item
  8](https://www.skybrary.aero/articles/flight-plan-completion)): which
  sets of regulations the flight is operated under. Possible values are:

  - `I` for IFR

  - `V` for VFR

  - `Y` first IFR thereafter VFR

  - `Z` first VFR thereafter IFR

- ICAO_FLT_TYPE (see [FPL Item
  8](https://www.skybrary.aero/articles/flight-plan-completion)): flight
  type. Possible values:

  - `S` for scheduled air service

  - `N` for non-scheduled air service

  - `G` for general aviation

  - `M` for military (note: filtered out)

  - `X` for other than the preceding categories

- RULE_NAME: market segment type as defined on the [Market Segment
  Rules](https://www.eurocontrol.int/publication/market-segment-rules),
  it can be:

  - “Mainline”

  - “Regional”

  - “Low-Cost”

  - “Business Aviation”

  - “All-Cargo”

  - “Charter” (Non-Scheduled)

  - “Military”

  - “Other”

  - "Not classified"

- SENSITIVE: 'Y' if sensitive

- SPECIAL_EXEMPT: reasons for special handling by ATS. One of:

  - "AEAP" ATFM exemption approved

  - "EMER" emergency

  - "FIRE" fire fighting

  - "HEAD" flights with Head of State status

  - "MEDE" medical evacuation

  - "NEXE" not exempted

  - "SERE" search & rescue

### Aerodrome details

- ADEP: [ICAO code](https://observablehq.com/@openaviation/airports) of
  the **A**erodrome of **DEP**arture

- NAME_ADEP: the (AIU) name of the `ADEP` airport

- COUNTRY_CODE_ADEP: the ISO 2-alpha country code for `ADEP`

- COUNTRY_NAME_ADEP: the country name for `ADEP`

- ADES: [ICAO code](https://observablehq.com/@openaviation/airports) of
  the **A**erodrome of **DES**tination (different from `ADES_FILED` in
  case of diversion)

- NAME_ADES: the (AIU) name of the `ADES` airport

- COUNTRY_CODE_ADES: the ISO 2-alpha country code for `ADES`

- COUNTRY_NAME_ADES: the country name for `ADES`

- ADES_FILED: [ICAO
  code](https://observablehq.com/@openaviation/airports) of the
  **A**erodrome of **DES**tination filed in the Flight Plan. **Note**:
  it can be different from `ADES` in case of diversion

- NAME_ADES_FILED: the (AIU) name of the `ADES_FILED` airport

- COUNTRY_CODE_ADES_FILED: the ISO 2-alpha country code for `ADES_FILED`

- COUNTRY_NAME_ADES_FILED: the country name for `ADES_FILED`

### Aircraft info

- REGISTRATION: the [aircraft
  registration](https://en.wikipedia.org/wiki/Aircraft_registration)
  (with spaces, dashes, ... stripped), e.g. GEUUU.

- AIRCRAFT_ADDRESS: the [ICAO 24-bit
  address](https://en.wikipedia.org/wiki/Aviation_transponder_interrogation_modes#ICAO_24-bit_address)
  of the airframe for ADS-B/Mode S broadcasting.

- AIRCRAFT_TYPE_ICAO_ID: the [ICAO code for the aircraft
  type](https://www.icao.int/publications/doc8643/pages/search.aspx),
  for example A30B for an Airbus A-300B2-200.

- WK_TBL_CAT (see [FPL Item
  9](https://www.skybrary.aero/articles/flight-plan-completion)): wake
  turbulence category, can be

  - `L` LIGHT, i.e. maximum certificated takeoff mass of 7000 kg (15_500
    lbs) or less.

  - `M` MEDIUM, i.e maximum certificated takeoff mass less than 136_000
    kg (300_000 lbs), but more than 7_000 kg (15_500 lbs)

  - `H` HEAVY, i.e. maximum certificated takeoff mass of 136_000 kg
    (300_000 lbs) or more (except those specified as `J`)

  - `J` SUPER, presently the only the AIRBUS A-380-800

### Aircraft operator details

- AIRCRAFT_OPERATOR: the [ICAO Airline
  Designator](https://en.wikipedia.org/wiki/List_of_airline_codes), i.e.
  `OAL` for `Olympic`

- AO_GRP_CODE: Aircraft Operator group (code), i.e. AEE_GRP

- AO_GRP_NAME: : Aircraft Operator group (name), i.e. AEGEAN Group

- AO_ISO_CTRY_CODE: ISO country code for AO

### Operational details

- EOBT_1: **E**stimated **O**ff-**B**lock **T**ime for FPL-based (M1)
  trajectory

- ARVT_1: **AR**ri**V**al **T**ime for FPL-based (M1) trajectory

- TAXI_TIME_1: Taxi time for FPL-based (M1) trajectory

- AOBT_3: **A**ctual **O**ff-**B**lock **T**ime for flown (M3)
  trajectory

- ARVT_3: **AR**i**V**al **T**ime for flown (M3) trajectory

- TAXI_TIME_3: Taxi time for flown (M3) trajectory

- RTE_LEN_1: route length (in Nautical Miles) for FPL-based (M1)
  trajectory

- RTE_LEN_3: route length (in Nautical Miles) for for flown (M3)
  trajectory

- FLT_DUR_1: route duration (in minutes) for FPL-based (M1) trajectory

- FLT_DUR_3: route length (in minutes) for flown (M3) trajectory

- FLT_TOW: takeoff weight

## Note

You need to either provide a connection `conn` that has access to
`SWH_FCT.DIM_FLIGHT_TYPE_RULE` (for `FLT_RULES`),
`PRUDEV.V_COVID_DIM_AO` (for aircraft and aircraft group info) and
`SWH_FCT.V_FAC_FLIGHT_MS` (for market segment info) or go with the
default which uses PRU_READ to establish a
[`db_connection()`](https://eurocontrol.github.io/eurocontrol/reference/db_connection.md).

## Examples

``` r
if (FALSE) { # \dontrun{
flts <- flights_tidy(wef = "2023-01-01", til = "2023-01-05")
# other operations on flts, i.e. filtering,
# followed by a collect() to retrieve the concrete data frame
flts_filtered <- flts |>
   filter(TO_DATE("2023-06-01 10:00", "YYYY-MM-DD HH24:MI") <= IOBT,
                  IOBT < TO_DATE("2023-01-02 10:30", "YYYY-MM-DD HH24:MI")) |>
   collect()

# NOTE: you can reuse the connection for other API calls
conn <- flts$src$con

# other ops requiring conn
# ...

# IMPORTANT: close the DB connection
DBI::dbDisconnect(conn)
} # }
```
