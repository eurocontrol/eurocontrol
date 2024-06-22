#' return the interval for an IATA season
#'
#' IATA summer season begins on the last Sunday of March and ends on the last Saturday of October.
#' IATA winter season begins on the last Sunday of October and ends Saturday of before next year
#' summer season.
#'
#'
#' @param year the year for the season definition
#' @param season the (northern hemisphere) season, either "summer" (default) or "winter"
#'
#' @return an interval for the season definition, end/start dates are inclusive
#'
#' @examples
#' \dontrun{
#' season_iata(2019)
#' }
season_iata <- function(year, season = "summer") {
  # TODO: vectorize over `year`
  # if (!(rlang::is_integer(year, n = 1) && length(year) == 1)) {
  #   cli::cli_abort(
  #     "{.var year} must be an integer vector of lenght 1."
  #   )
  # }
  if (!rlang::is_character(season)) {
    cli::cli_abort(
      "{.var season} must be a character vector of lenght 1, not {.obj_type_friendly {season}}."
    )
  }
  if (!(season == "summer" || season == "winter")) {
    cli::cli_abort(
      "{.var season} must either be 'summer' or 'winter', not '{season}'."
    )
  }


  #--- winter
  # get all days of October
  summer_end <- seq(from = lubridate::ymd(stringr::str_glue("{year}-10-01")),
                    to   = lubridate::ymd(stringr::str_glue("{year}-10-31")),
                    by = "days") |>
    tibble::as_tibble_col(column_name = "date") |>
    # get all Sundays
    dplyr::mutate(is_saturday = lubridate::wday(date, label = TRUE, abbr = FALSE) == "Saturday") |>
    dplyr::filter(.data$is_saturday == TRUE) |>
    # pick last
    dplyr::arrange(date) |>
    dplyr::filter(dplyr::row_number() == dplyr::n()) |>
    dplyr::pull(date)

  #--- summer
  # get all days of March
  summer_beg <- seq(from = lubridate::ymd(stringr::str_glue("{year}-03-01")),
                    to   = lubridate::ymd(stringr::str_glue("{year}-03-31")),
                    by = "days") |>
    tibble::as_tibble_col(column_name = "date") |>
    # get all Sundays
    dplyr::mutate(is_sunday = lubridate::wday(date, label = TRUE, abbr = FALSE) == "Sunday") |>
    dplyr::filter(.data$is_sunday == TRUE) |>
    # pick last
    dplyr::arrange(date) |>
    dplyr::filter(dplyr::row_number() == dplyr::n()) |>
    dplyr::pull(date)

  iata_season <- lubridate::interval(start = summer_beg, end = summer_end)
  if (season == "winter") {
    aa <- summer_end |> magrittr::add(lubridate::ddays(1))
    bb <- season_iata(year + 1, "summer") |>
      lubridate::int_start() |> magrittr::subtract(lubridate::ddays(1))

    iata_season <- lubridate::interval(start = aa, end = bb)
  }

  iata_season
}

iata_season_year <- function(date) {
  iata_season_for_date(date) |>
    stringr::str_sub(start = -4L) |>
    as.integer()
}

iata_season_start <- function(year, season = "summer") {
  season <- tolower(season)
  stopifnot(season %in% c("summer", "winter"))
  m <- ifelse(season == "summer", "03", "10")
  d <- "{year}-{m}-31" |>
    stringr::str_glue() |>
    lubridate::as_date()
  season_start <- d - lubridate::days(lubridate::wday(d) - 1)
  season_start
}

#' Return the corresponding IATA season for a date
#'
#' @param date a date
#'
#' @return the name of the IATA season in the form `summer-yyyy`
#' @export
#'
#' @examples
#' \dontrun{
#' season_iata("2024-04-01")
#' }
iata_season_for_date <- function(date) {
  date <- lubridate::as_date(date)
  year_of_date <- lubridate::year(date)
  summer_start <- iata_season_start(year_of_date, "summer")
  winter_start <- iata_season_start(year_of_date, "winter")

  if (summer_start <= date && date < winter_start) {
    return(stringr::str_glue("summer-{yyyy}", yyyy = year_of_date))
  } else if (date < summer_start) {
    return(stringr::str_glue("winter-{yyyy}", yyyy = year_of_date - 1))
  } else {
    return(stringr::str_glue("winter-{yyyy}", yyyy = year_of_date))
  }
}

# TODO: export it
days_in_season <- function(season_int) {
  1 + as.numeric((lubridate::int_end(season_int)) - (lubridate::int_start(season_int)))
}
