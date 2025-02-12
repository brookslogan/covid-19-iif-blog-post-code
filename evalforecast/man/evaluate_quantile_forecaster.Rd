% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate.R
\name{evaluate_quantile_forecaster}
\alias{evaluate_quantile_forecaster}
\title{Evaluate a quantile forecaster}
\usage{
evaluate_quantile_forecaster(
  df,
  your_quantile_forecaster,
  response,
  incidence_period,
  ahead,
  geo_type,
  forecast_dates = NULL,
  err_measure = weighted_interval_score,
  backfill_buffer = 10,
  n_locations = 200
)
}
\arguments{
\item{df}{data frame with columns "location", "reference_date", "issue_date",
"variable_name" and "value".}

\item{your_quantile_forecaster}{a function that follows the template of
my_quantile_forecaster}

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

\item{geo_type}{one of the geographical types, matching the terms used on the
api ("county", "dma", "hrr", "msa", "state")}

\item{forecast_dates}{a vector of Dates, on which forecasts will be made
about some period (e.g., epiweek).  For example, if forecast_date is
ymd("2020-05-11"), incidence_period is "day",  and ahead = 3, then, we'd be
making forecasts for "2020-05-14".  If NULL, then a sequence of 6 Mondays
will be chosen that are as late as possible given what's available in df.
See documentation for the function latest_possible_forecast_date, for more
on how these dates are chosen. To specify your own range, you can use
something of the form seq(ymd("2020-04-06"), ymd("2020-05-11"), by = 7).}

\item{err_measure}{a function that takes a data frame with two columns
"probs" and "quantiles" and an actual (i.e. observed) scalar value and
returns some measure of error.}

\item{backfill_buffer}{How many days until response is deemed trustworthy
enough to be taken as correct?}

\item{n_locations}{How many locations to evaluate predictions at? See
function top_n_locations for more details about how these locations are
chosen.}
}
\value{
a score card
}
\description{
This function evaluates \code{your_quantile_forecaster} by applying it on a
series of forecast_dates and then calculating the error according to a
provided error measure (default is weighted_interval_score).
}
