# ICAO aircraft type and manufacturer designators

library(httr)
library(magrittr)
library(lubridate)
library(jsonlite)
library(dplyr)
library(janitor)
library(usethis)

# get "Last Updates:" date in
#   https://www.icao.int/publications/DOC8643/Pages/Search.aspx
# now moved to https://doc8643.icao.int/External/Stats
# You get something like
# {
#   "LastUpdated":"11 June 2026",
#   "NextUpdate":"09 July 2026",
#   "AircraftTypeCount":10351,
#   "ManufacturerCount":2081
# }
update_date <- httr::POST("https://doc8643.icao.int/External/Stats") |>
  httr::content() |>
  magrittr::extract2("LastUpdated") |>
  lubridate::parse_date_time("%d %B %Y")

# SCRAPE the DATA from THE WEB
# https://www.icao.int/operational-safety/doc-8643-aircraft-type-designators/search
# if you look at the network traffic you see where
#    https://www.icao.int/publications/doc8643/pages/search.aspx
# now at https://doc8643.icao.int/External/AircraftTypes
# is fishing for the data ;-)
p <- httr::POST("https://doc8643.icao.int/External/AircraftTypes")
r <- httr::content(p, as = "text")
atype <- jsonlite::fromJSON(r) |>
  dplyr::as_tibble() |>
  janitor::clean_names() |>
  dplyr::mutate(last_updated = lubridate::ymd(update_date))


aircraft_type <- atype |>
  dplyr::select(
    designator,
    aircraft_description,
    description,
    wtc,
    engine_count,
    engine_type,
    last_updated
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(designator)

usethis::use_data(aircraft_type, overwrite = TRUE)

aircraft_model <- atype |>
  dplyr::select(model_full_name, manufacturer_code, designator, last_updated)

usethis::use_data(aircraft_model, overwrite = TRUE)

# TODO:
#  https://www4.icao.int/doc8643/External/Manufacturers

# p <- httr::POST("https://www4.icao.int/doc8643/External/Manufacturers")
# r <- httr::content(p, as = "text")
# amanu <- jsonlite::fromJSON(r)  |>
#   dplyr::as_tibble()
