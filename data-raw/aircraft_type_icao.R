# ICAO aircraft type and manufacturer dedignators

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(lubridate)
# library(readr)
# library(here)
library(janitor)
library(usethis)
library(magrittr)

# get "Last Updates:" date in
#   https://www.icao.int/publications/DOC8643/Pages/Search.aspx
#  (in fact in some JS you find the URL for the relevant POST request ;-)
update_date <- POST("https://www4.icao.int/doc8643/External/Stats") |>
  httr::content() |>
  extract2("LastUpdated") |>
  lubridate::parse_date_time("%d %B %Y")

# SCRAPE the DATA from THE WEB
# if you look at the network traffic you see wher
#    https://www.icao.int/publications/doc8643/pages/search.aspx
# is fishing for the data ;-)
p <- POST("https://www4.icao.int/doc8643/External/AircraftTypes")
r <- httr::content(p, as = "text")
atype <- fromJSON(r)  |>
  as_tibble() |>
  clean_names() |>
  mutate(last_updated = ymd(update_date))


aircraft_type <- atype %>%
  select(designator, aircraft_description,
         description,
         wtc,
         engine_count, engine_type,
         last_updated) %>%
  distinct() %>%
  arrange(designator)

usethis::use_data(aircraft_type, overwrite = TRUE)

aircraft_model <- atype %>%
  select(model_full_name, manufacturer_code, designator, last_updated)

usethis::use_data(aircraft_model, overwrite = TRUE)


# TODO:
#  https://www4.icao.int/doc8643/External/Manufacturers
