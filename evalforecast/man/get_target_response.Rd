% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/target.R
\name{get_target_response}
\alias{get_target_response}
\title{Get target response data frame}
\usage{
get_target_response(
  df,
  forecast_dates,
  forecast_locations,
  response,
  incidence_period,
  ahead
)
}
\description{
Computes target periods and actual value of response, summed over
incidence_period, at each location within each target period. In particular,
returns a data frame with columns "location", "location_name", "actual", 
"forecast_date", "target_start", and "target_end".
}
