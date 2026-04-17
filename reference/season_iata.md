# return the interval for an IATA season

IATA summer season begins on the last Sunday of March and ends on the
last Saturday of October. IATA winter season begins on the last Sunday
of October and ends Saturday of before next year summer season.

## Usage

``` r
season_iata(year, season = "summer")
```

## Arguments

- year:

  the year for the season definition

- season:

  the (northern hemisphere) season, either "summer" (default) or
  "winter"

## Value

an interval for the season definition, end/start dates are inclusive

## Examples

``` r
if (FALSE) { # \dontrun{
season_iata(2019)
} # }
```
