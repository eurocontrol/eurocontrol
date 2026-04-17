# Extract a clean airport operator data flow list in an interval

The returned
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
includes movements information in the interval `[wef, til)`. **NOTE**:
it can only cover ONE month at a time

## Usage

``` r
apdf_tidy(conn = NULL, wef, til)
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

## Value

A
[`dbplyr::tbl_dbi()`](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html)
with the following columns:

- APDS_ID: the airport operator dataflow unique record id.

- ID: the so called `SAM ID`, used internally by PRISME

- AP_C_FLTID: flight identifier (aource Airport)

- AP_C_FLTRUL: which sets of regulations the flight is operated under.
  Possible values are:

  - `IFR` for IFR

  - `VFR` for VFR

  - `NA` if unknown

- AP_C_REG: the [aircraft
  registration](https://en.wikipedia.org/wiki/Aircraft_registration)
  (with spaces, dashes, ... stripped), e.g. GEUUU.

- ADEP_ICAO: ([ICAO
  code](https://observablehq.com/@openaviation/airports) of the)
  **A**erodrome of **DEP**arture (source airport).

- ADES_ICAO: ([ICAO
  code](https://observablehq.com/@openaviation/airports) of the)
  **A**erodrome of **DES**tination (source airport).

- SRC_PHASE: flight phase. `DEP`=departure, `ARR`=arrival.

- MVT_TIME_UTC: (best available) movement time (takeoff if `SRC_PHASE` =
  `DEP`, landing if `SRC_PHASE` = `ARR`).

- BLOCK_TIME_UTC: Block time (off-block if `SRC_PHASE` = `DEP`, in-block
  if `SRC_PHASE` = `ARR`).

- SCHED_TIME_UTC: scheduled time (of departure if `SRC_PHASE` = `DEP`,
  of arrival if `SRC_PHASE` = `ARR`; source airport).

- ARCTYP: (best available) the [ICAO code for the aircraft
  type](https://www.icao.int/publications/doc8643/pages/search.aspx),
  for example A21N for Airbus A321neo.

- AP_C_RWY: Runway ID (of departure if `SRC_PHASE` = `DEP`, of arrival
  if `SRC_PHASE` = `ARR`; source airport).

- AP_C_STND: Stand ID (of departure if `SRC_PHASE` = `DEP`, of arrival
  if `SRC_PHASE` = `ARR`; source airport).

- C40_CROSS_TIME: time of first (last) crossing at 40 NM from ARP for
  departure (arrival).

- C40_CROSS_LAT: latitude of first (last) crossing at 40 NM from ARP for
  departure (arrival).

- C40_CROSS_LON: longitude of first (last) crossing at 40 NM from ARP
  for departure (arrival).

- C40_CROSS_FL: flight level of first (last) crossing at 40 NM from ARP
  for departure (arrival).

- C40_BEARING: bearing of first (last) crossing at 40 NM from ARP for
  departure (arrival).

- C100_CROSS_TIME: time of first (last) crossing at 100 NM from ARP for
  departure (arrival).

- C100_CROSS_LAT: latitude of first (last) crossing at 100 NM from ARP
  for departure (arrival).

- C100_CROSS_LON: longitude of first (last) crossing at 100 NM from ARP
  for departure (arrival).

- C100_CROSS_FL: flight level of first (last) crossing at 100 NM from
  ARP for departure (arrival).

- C100_BEARING: bearing of first (last) crossing at 100 NM from ARP for
  departure (arrival).

## Note

You need to either provide a connection `conn` that has access to
`SWH_FCT.FAC_APDS_FLIGHT_IR691`, or go with the default which uses
PRU_READ to establish a
[`db_connection()`](https://eurocontrol.github.io/eurocontrol/reference/db_connection.md).

## Examples

``` r
if (FALSE) { # \dontrun{
my_apdf <- apdf_tidy(wef = "2023-01-01", til = "2023-01-02")
# ...
DBI::dbDisconnect(my_apdf$src$con)
} # }
```
