% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trajectories.R
\name{airspace_profiles_tidy}
\alias{airspace_profiles_tidy}
\title{Provide all airspace profile segments intersecting an interval of interest}
\usage{
airspace_profiles_tidy(
  conn = NULL,
  wef,
  til,
  airspace = "FIR",
  profile = "CTFM"
)
}
\arguments{
\item{conn}{Database connection or instantiate the default one.}

\item{wef}{\strong{W}ith \strong{EF}fect date (included) at Zulu time
in a format recognized by \code{\link[lubridate:as_date]{lubridate::as_datetime()}}}

\item{til}{un\strong{TIL}l date (excluded) at Zulu time
in a format recognized by \code{\link[lubridate:as_date]{lubridate::as_datetime()}}}

\item{airspace}{the type of airspace (default: 'FIR'), one of:
\itemize{
\item 'FIR' (\href{https://observablehq.com/@openaviation/flight-information-regions}{Flight Information Region})
\item 'NAS' (National Airspace)
\item 'AUA' (ATC Unit Airspace)
\item 'ES' (Elementary Sector)
}}

\item{profile}{the \href{https://ansperformance.eu/definition/flight-models/}{model of the trajectory}
profile (default: 'CTFM'), one of:
\itemize{
\item 'FTFM', Filed Tactical Flight Model
\item 'RTFM', Regulated Tactical Flight Model
\item 'CTFM', Current Tactical Flight Model
\item 'CPF', Correlated Position reports for a Flight
\item 'DCT', Direct route
\item 'SCR', Shortest Constrained Route
\item 'SRR', Shortest RAD restrictions applied Route
\item 'SUR', Shortest Unconstrained Route
}}
}
\value{
a \code{\link[dbplyr:tbl.src_dbi]{dbplyr::tbl_dbi()}} with the following columns
\itemize{
\item ID: the so called \verb{SAM ID}, used internally by PRISME
\item SEQ_ID: the sequence number of the segment for the relevant airspace profile
\item ENTRY_TIME: the time of entry into the relevant airspace
\item ENTRY_LON:  the longitude of entry into the relevant airspace
\item ENTRY_LAT: the latitude of entry into the relevant airspace
\item ENTRY_FL: the flight level of entry into the relevant airspace
\item EXIT_TIME: the time of exit out of the relevant airspace
\item EXIT_LON: the longitude of exit out of the relevant airspace
\item EXIT_LAT: the latitude of exit out of the relevant airspace
\item EXIT_FL: the flight level of exit out of the relevant airspace
\item AIRSPACE_ID: the airspace ID
\item AIRSPACE_TYPE: the airspace type as per \code{airspace} input parameter
\item MODEL_TYPE: the trajectory model as per \code{profile} input parameter
}
}
\description{
The returned \code{\link[dbplyr:tbl.src_dbi]{dbplyr::tbl_dbi()}} includes segments for scheduled and non-scheduled flights
temporally intersecting the right-opened interval \verb{[wef, til)}.

General aviation, State, military and sensitive flight are excluded.
}
\section{Note}{
You need to either provide a connection \code{conn} that has access to as noted in
\code{\link[=airspace_profile_tbl]{airspace_profile_tbl()}} and \code{\link[=flights_tidy]{flights_tidy()}} or go with the default which uses
PRU_DEV to establish a \code{\link[=db_connection]{db_connection()}}.
}

\examples{
\dontrun{
ps <- airspace_profiles_tidy(wef = "2023-01-01", til = "2023-04-01")
# IMPORTANT: always close the DB connection when done
DBI::dbDisconnect(ps$src$con)

# if you re-use DB connections
conn <- eurocontrol::db_connection("PRU_DEV")
ps <- airspace_profiles_tidy(conn = conn)

# ... do something else with conn
# ...
# then manually close the connection to the DB
DBI::dbDisconnect(conn)
}
}
