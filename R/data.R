#' EUROCONTROL's Member States
#'
#' A data frame with the following fields
#' \describe{
#'   \item{name}{the country name, e.g. "Italy"}
#'   \item{iso3c}{the 3-letter ISO code, e.g. "ITA"}
#'   \item{iso2c}{the 2-letter ISO code, e.g. "IT"}
#'   \item{icao}{the 2-letter ICAO code, e.g. "LI"}
#'   \item{iso3n}{the 3-digits ISO code, e.g. "380"}
#'   \item{date}{the date of `status` code, e.g. 1996-04-01}
#'   \item{status}{the status code, e.g. "M"
#'   (`M` Member State, `C` Comprehensive Agreement State, `T` Transitional State)}
#' }
#' These are useful to grab the right spatial polygons in case of need.
#' Note: Kosovo is also included in the list.
"member_state"



#' ICAO's Aircraft types
#'
#' A data frame with the following fields
#' \describe{
#'   \item{designator}{the aircraft type designator, e.g. "A310".}
#'   \item{aircraft_description}{the aircraft description, e.g. "LandPlane".}
#'   \item{description}{the aircraft , e.g. "IT".}
#'   \item{wtc}{the aircraft [wake turbulence category](https://www.skybrary.aero/articles/icao-wake-turbulence-category), e.g. "M".}
#'   \item{engine_count}{the number pf engines, e.g. "2".}
#'   \item{engine_type}{the engine type, e.g. "Jet".}
#'   \item{last_updated}{the last updated date when the data have been updated, e.g. "2023-05-19".}
#' }
"aircraft_type"


#' ICAO's Manufacturer codes
#'
#' A data frame with the following fields
#' \describe{
#'   \item{model_full_name}{the model full name, e.g. "A-320neo".}
#'   \item{manufacturer_code}{the manufacturer's code, e.g. "AIRBUS".}
#'   \item{designator}{the model's designator, e.g. "A20N"}
#'   \item{last_updated}{the last updated date when the data have been updated, e.g. "2023-05-19".}
#' }
"aircraft_model"



