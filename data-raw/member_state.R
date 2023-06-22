# EUROCONTROL Member States (+ Kosovo)
# Name, ICAO code, ISO 2 letter code, ISO #, ISO region

library(readr)
library(dplyr)

member_state <- read_csv(here::here("data-raw", "member_state.csv")) %>%
  select(name, iso3c, iso2c, icao, iso3n, date, status) %>%
  arrange("name")
usethis::use_data(
  member_state,
  compress = "bzip2",
  overwrite = TRUE)
