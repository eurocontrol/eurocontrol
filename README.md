
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The `eurocontrol` package

<!-- badges: start -->
<!-- badges: end -->

The goal of the `eurocontrol` package is to provide helper functions to
the Agency’s employees for setup, data retrieval from internal
databases, … .

## Installation

You can install the development version of the `eurocontrol` package
from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("eurocontrol/eurocontrol")
```

**NOTE**: This package (hiddenly) depends on the `ROracle` package,
please have it installed according to the guidelines as published
[here](https://github.com/euctrl-pru/howto/wiki/Tools-Installation-and-Setup-%28For-R%29#roracle).

## Setup

Some of the functions in this package access data from internal Oracle
schema. The code relies on having the relevant credentials stored in
specifically named environment variables. The pattern for the
environment variable names is

    PRU_XXXX_DBNAME 
    PRU_XXXX_PWD
    PRU_XXXX_USR

where `XXXX` is something like `DEV`, `PROD`, …

## Example

A typical use case is to retrieve a list of flights operated in the area
managed by EUROCONTROL’s Network Manager.

For example for the flight list of March 2023 you can run (you need
access and setup for `PRU_DEV` schema):

``` r
library(eurocontrol)

flights_tidy(wef = "2023-01-01", til = "2023-04-01")
```