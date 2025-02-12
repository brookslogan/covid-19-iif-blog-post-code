% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_corrections.R
\name{apply_corrections}
\alias{apply_corrections}
\title{Apply corrections, if available, to upstream data frame}
\usage{
apply_corrections(
  df,
  geo_type = c("county", "state"),
  forecast_date,
  log_info = TRUE
)
}
\arguments{
\item{df}{the upstream data frame corresponding to the geo type}

\item{geo_type}{the geo_type corresponding to the upstream data
frame}

\item{forecast_date}{the forecast date as a date object to account
for the response variable name change that happened on
`2020-07-13`}

\item{log_info}{a boolean flag indicating whether to log
information on changes, default `TRUE`}
}
\value{
a df with corrections applied if the corrections are
    available, or same dataframe
}
\description{
Corrections data are collection of replacement records for the
original data. This means that the variables appear exactly in the
same order as in the original record and only the `value` of a
variable is potentially different. The replacement process returns
a new tibble by removing the matching original data, matched by the
variables `location`, `reference_date` and `variable_name` and
appending the entire corrections data at the end. Ideally, this
function should only make corrections that a properly versioned
data frame cannot account for, i.e. persistent bad data rows that
are likely to mess up forecasting algorithms (this has the salutory
effect of keeping the number of corrections small). Note that
`issue_date` is not accounted for; this function will have to
modified to account for non-`NA` `issue_date`.
}
\examples{

\dontrun{
  e  <- new.env()
  load("upstream_df_state_2020-08-30.Rdata", envir = e)
  new_df <- apply_corrections(df = e$df, geo_type = "state", forecast_date = lubridate::ymd("2020-08-09"))
  nrow(e$df) == nrow(new_df)  # Same number of rows?
}
}
