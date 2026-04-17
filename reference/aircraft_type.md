# ICAO's Aircraft types

A data frame with the following fields

- designator:

  the aircraft type designator, e.g. "A310".

- aircraft_description:

  the aircraft description, e.g. "LandPlane".

- description:

  the aircraft , e.g. "IT".

- wtc:

  the aircraft [wake turbulence
  category](https://www.skybrary.aero/articles/icao-wake-turbulence-category),
  e.g. "M".

- engine_count:

  the number pf engines, e.g. "2". **Note**: this is not a number
  unfortunately, there is one model encoded `C`

- engine_type:

  the engine type, e.g. "Jet".

- last_updated:

  the date when the data have been last updated, e.g. "2023-05-19".

## Usage

``` r
aircraft_type
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with
2719 rows and 7 columns.
