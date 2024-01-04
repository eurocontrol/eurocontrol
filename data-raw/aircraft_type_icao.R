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
#  (in fact in some JS you find the URL for the relevant POST request ;-)
update_date <- httr::POST("https://www4.icao.int/doc8643/External/Stats") |>
  httr::content() |>
  magrittr::extract2("LastUpdated") |>
  lubridate::parse_date_time("%d %B %Y")

# SCRAPE the DATA from THE WEB
# if you look at the network traffic you see wher
#    https://www.icao.int/publications/doc8643/pages/search.aspx
# is fishing for the data ;-)
p <- httr::POST("https://www4.icao.int/doc8643/External/AircraftTypes")
r <- httr::content(p, as = "text")
atype <- jsonlite::fromJSON(r)  |>
  dplyr::as_tibble() |>
  janitor::clean_names() |>
  dplyr::mutate(last_updated = lubridate::ymd(update_date))


aircraft_type <- atype %>%
  dplyr::select(designator, aircraft_description,
         description,
         wtc,
         engine_count, engine_type,
         last_updated) |>
  dplyr::distinct() |>
  dplyr::arrange(designator)

usethis::use_data(aircraft_type, overwrite = TRUE)

aircraft_model <- atype %>%
  dplyr::select(model_full_name, manufacturer_code, designator, last_updated)

usethis::use_data(aircraft_model, overwrite = TRUE)


# TODO:
#  https://www4.icao.int/doc8643/External/Manufacturers

# p <- httr::POST("https://www4.icao.int/doc8643/External/Manufacturers")
# r <- httr::content(p, as = "text")
# amanu <- jsonlite::fromJSON(r)  |>
#   dplyr::as_tibble()

