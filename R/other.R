#' retrieve latest airport list from OurAirports
#'
#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' apts <- airports_oa()
#' }
airports_oa <- function() {
  oa_url <- "https://raw.githubusercontent.com/davidmegginson/ourairports-data/main/airports.csv"
  readr::read_csv(oa_url) |>
    dplyr::mutate(
      gps_code = stringr::str_trim(.data$gps_code),
      ident    = stringr::str_trim(.data$ident)) |>
    dplyr::mutate(icao = dplyr::if_else(.data$ident != .data$gps_code,
                                        .data$gps_code,
                                        .data$ident)) |>
    dplyr::filter(
      # keep the closed ones...EDDT is closed after 2019!
      !.data$type %in% c("closed"),
      stringr::str_detect(.data$gps_code, "^[A-Z]{4}"),
      .data$icao != "EDDT") |>
    dplyr::select(
      "icao",
      "iata"      = "iata_code",
      "latitude"  = "latitude_deg",
      "longitude" = "longitude_deg",
      "elevation" = "elevation_ft",
      "type",
      "name",
      "iso_country",
      "iso_region",
      "continent",
      NULL
    ) |>
    dplyr::union(
      # from https://metar-taf.com/airport/ENFB-statfjord-b
      dplyr::tribble(
        # ,
        ~icao, ~iata, ~longitude, ~latitude, ~elevation,      ~type,              ~name, ~iso_country, ~iso_region, ~continent,
        "EHFO",   "",    4.82722,  54.21583,      400.0, "heliport",            "F15-A",         "NL",           "",       "EU",
        "EHGO",   "",    5.43472,  54.16944,      400.0, "heliport",            "G14-B",         "NL",           "",       "EU",
        "EHGQ",   "",    5.43194,  54.04917,      400.0, "heliport",           "G17D-A",         "NL",           "",       "EU",
        "EHGN",   "",    5.49861,  54.22389,      400.0, "heliport",            "G14-A",         "NL",           "",       "EU",
        "EHFT",   "",    4.51278,  53.81806,      400.0, "heliport",             "L5-D",         "NL",           "",       "EU",
        "EHFR",   "",    4.35111,  53.81083,      400.0, "heliport",          "L5-FA-1",         "NL",           "",       "EU",
        "EHFD",   "",    4.69472,  54.85306,      400.0, "heliport",          "F3-FB-1",         "NL",           "",       "EU",
        "EHFB",   "",     4.5725,  54.94444,      400.0, "heliport",             "F2-A",         "NL",           "",       "EU",
        "EHAX",   "",    3.83167,    55.105,      400.0, "heliport",            "A18-A",         "NL",           "",       "EU",
        "EHFQ",   "",    4.49611,  53.96056,      400.0, "heliport",          "L2-FA-1",         "NL",           "",       "EU",
        "EKAF",   "",       3.99,      55.8,      400.0, "heliport",              "A6A",         "NL",           "",       "EU",
        "ENUG",   "",      22.25,      71.3,      400.0, "heliport",           "Goliat",         "NO",           "",       "EU",
        "ENDR",   "",    7.79167,  64.35556,      400.0, "heliport",          "Draugen",         "NO",           "",       "EU",
        "ENFB",   "",    1.82889,  61.20639,      400.0, "heliport",      "Statfjord B",         "NO",           "",       "EU",
        "ENGA",   "",     2.1875,  61.17556,      400.0, "heliport",       "Gullfaks A",         "NO",           "",       "EU",
        "ENGC",   "",  2.2709835,61.2140628,      400.0, "heliport",       "Gullfaks C",         "NO",           "",       "EU",
        "ENHE",   "",    7.31583,  65.32556,      400.0, "heliport",        "Heidrun A",         "NO",           "",       "EU",
        "ENHM",   "",    2.23333,  59.56667,      400.0, "heliport",          "Heimdal",         "NO",           "",       "EU",
        "ENJS",   "",    2.54556,  58.83528,      400.0, "heliport",   "Johan Sverdrup",         "NO",           "",       "EU",
        "ENLE",   "",    3.21667,   56.5333,      400.0, "heliport", "Ekofisk Oil Pltf",         "NO",           "",       "EU",
        "ENNE",   "",    8.08333,  66.03333,      400.0, "heliport",          "Norne A",         "NO",           "",       "EU",
        "ENQG",   "",  2.1988320,61.2021082,      400.0, "heliport",       "Gullfaks B",         "NO",           "",       "EU",
        "ENQJ",   "",      3.895,  61.33167,      400.0, "heliport",             "Gjoa",         "NO",           "",       "EU",
        "ENQK",   "",        2.5,  61.08333,      400.0, "heliport",       "Kvitebjorn",         "NO",           "",       "EU",
        "ENQL",   "",    2.07333,  60.49833,      400.0, "heliport",   "Martin Linge B",         "NO",           "",       "EU",
        "ENQM",   "",    2.01472,   60.5075,      400.0, "heliport",   "Martin Linge A",         "NO",           "",       "EU",
        "ENQW",   "",    2.36694,     61.04,      400.0, "heliport",          "Valemon",         "NO",           "",       "EU",
        "ENQR",   "",    2.21667,  61.51667,      400.0, "heliport",         "Snorre B",         "NO",           "",       "EU",
        "ENQS",   "",    1.90083,   61.2961,      400.0, "heliport",      "Statfjord C",         "NO",           "",       "EU",
        # little evidence of geo coordinates for ENUC
        "ENUC",   "",     6.9259,   65.0863,      400.0, "heliport",         "Asgard C",         "NO",           "",       "EU",
        "ENQV",   "",    2.45722,  61.36972,      400.0, "heliport",         "Visund A",         "NO",           "",       "EU",
        "ENQW",   "",    2.36694,     61.04,      400.0, "heliport",          "Valemon",         "NO",           "",       "EU",
        "ENSE",   "",       2.15,     61.45,      400.0, "heliport",         "Snorre A",         "NO",           "",       "EU",
        "ENSF",   "",  1.8521147,61.2551784,      400.0, "heliport",      "Statfjord A",         "NO",           "",       "EU",
        "ENUA",   "",    6.72556,  65.06417,      400.0, "heliport",          "Kristin",         "NO",           "",       "EU",
        "ENUB",   "",    6.78944,     65.11,      400.0, "heliport",        "Aasgard B",         "NO",           "",       "EU",
        "ENUS",   "",    7.65083,  65.69972,      400.0, "heliport",            "Skarv",         "NO",           "",       "EU",
        "ENUK",   "",    6.55056,  64.99417,      400.0, "heliport",        "Aasgard A",         "NO",           "",       "EU",
        "ENUR",   "",    7.36722,  65.34333,      400.0, "heliport",        "Heidrun B",         "NO",           "",       "EU",
        "ENXW",   "",    2.48667,    59.165,      400.0, "heliport",            "Grane",         "NO",           "",       "EU",
        "ENWE",   "",    2.24722,   58.8425,      400.0, "heliport",     "Edvard Grieg",         "NO",           "",       "EU",
        "ENWG",   "",    1.74333,    58.845,      400.0, "heliport",           "Gudrun",         "NO",           "",       "EU",
        "ENWI",   "",    2.19778,  58.92222,      400.0, "heliport",       "Ivar Aasen",         "NO",           "",       "EU",
        "ENWK",   "",    1.69528,  58.57139,      400.0, "heliport",        "Gina Krog",         "NO",           "",       "EU",
        "ENWR",   "",    1.73278,  58.58333,      400.0, "heliport",         "Randgrid",         "NO",           "",       "EU",
        "ENWS",   "",    3.26278,  56.37361,      400.0, "heliport",        "Eldfisk S",         "NO",           "",       "EU",
        "ENWV",   "",        3.4,  56.28333,      400.0, "heliport",       "Valhall PH",         "NO",           "",       "EU"
      )
    ) |>
    dplyr::mutate(last_updated = lubridate::today())
}
