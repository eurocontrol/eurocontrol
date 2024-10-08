% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trajectories.R
\name{generate_so6}
\alias{generate_so6}
\title{Export trajectory profiles to SO6 format}
\usage{
generate_so6(trajectory)
}
\arguments{
\item{trajectory}{A data frame for point profile trajectories.}
}
\value{
A data frame for trajectories in SO6 format.
}
\description{
The data frame for point trajectories needs to have the following columns:

\tabular{lll}{
\strong{Name}            \tab \strong{Description}            \tab \strong{Type} \cr
\code{FLIGHT_ID}         \tab Flight ID                       \tab int           \cr
\code{TIME_OVER}         \tab Time over point                 \tab datetime      \cr
\code{LONGITUDE}         \tab Longitude (decimal degrees)     \tab double        \cr
\code{LATITUDE}          \tab Latitude (decimal degrees)      \tab double        \cr
\code{FLIGHT_LEVEL}      \tab Flight level                    \tab int           \cr
\code{POINT_ID}          \tab Point ID or NO_POINT            \tab char          \cr
\code{AIR_ROUTE}         \tab Air route or NO_ROUTE           \tab char          \cr
\code{LOBT}              \tab Last Off-block Time             \tab datetime      \cr
\code{SEQ_ID}            \tab Positions' sequence number      \tab int           \cr
\code{CALLSIGN}          \tab Flight call sign                \tab char          \cr
\code{REGISTRATION}      \tab Aircraft registration           \tab char          \cr
\code{MODEL_TYPE}        \tab Aircraft model                  \tab char          \cr
\code{AIRCRAFT_TYPE}     \tab Aircraft ICAO type              \tab char          \cr
\code{AIRCRAFT_OPERATOR} \tab Aircraft operator               \tab char          \cr
\code{ADEP}              \tab Departing aerodrome (ICAO) ID   \tab char          \cr
\code{ADES}              \tab Destination aerodrome (ICAO) ID \tab char
}
}
\examples{
\dontrun{
conn <- eurocontrol::db_connection("PRU_DEV")
pf <- point_profiles_tidy(conn = conn,
                          wef = "2020-01-01",
                          til = "2020-01-10") |>
  generate_so6()

# ... do something else with conn
# ...
# then manually close the connection to the DB
DBI::dbDisconnect(conn)
generate_so6(trj)
}
}
