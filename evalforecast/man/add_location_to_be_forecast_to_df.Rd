% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecast_locations.R
\name{add_location_to_be_forecast_to_df}
\alias{add_location_to_be_forecast_to_df}
\title{Add location_to_be_forecast information to df}
\usage{
add_location_to_be_forecast_to_df(
  df,
  forecast_locations_info,
  response,
  incidence_period,
  ahead
)
}
\arguments{
\item{df}{data frame with columns "location", "reference_date", "issue_date",
"variable_name" and "value".}

\item{forecast_locations_info}{output of function get_forecast_locations_info}

\item{response}{name of the variable that your_quantile_forecaster is
designed to predict (it should appear in the "variable_name" column of df).
If using incidence_period "epiweek", it should be something for which
summing daily values over an epiweek makes sense (e.g., counts or
proportions but not log(counts) or log(proportions)).}

\item{incidence_period}{one of "epiweek" or "day"}

\item{ahead}{how many epiweeks/days ahead are you forecasting?  If
incidence_period is "epiweek", then ahead = 1 means the epiweek that
includes the forecast date.  If incidence_period is "day" then ahead = 1
means the day after forecast date.}
}
\description{
Adds a new variable to df called "location_to_be_forecast" that is 
binary indicating whether this is a location to be forecast.
}
