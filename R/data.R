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
"member_states"

