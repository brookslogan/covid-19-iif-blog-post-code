% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate.R
\name{evaluate_quantile_predictions_cards}
\alias{evaluate_quantile_predictions_cards}
\title{Evaluate a list of prediction cards}
\usage{
evaluate_quantile_predictions_cards(
  df,
  predictions_cards,
  err_measure = weighted_interval_score,
  backfill_buffer = 10
)
}
\arguments{
\item{df}{data frame with columns "location", "reference_date", "issue_date",
"variable_name" and "value".}

\item{predictions_cards}{a list of prediction cards from the same forecaster
that are all for the same prediction task, meaning they are for the same
response, incidence_period, ahead, geo_type, and n_locations. Each should 
be from a different forecast date.  A predictions card is created by the 
function get_predictions_card.}

\item{err_measure}{a function that takes a data frame with two columns
"probs" and "quantiles" and an actual (i.e. observed) scalar value and
returns some measure of error.}

\item{backfill_buffer}{How many days until response is deemed trustworthy
enough to be taken as correct?}
}
\value{
a score card
}
\description{
Evaluate a list of prediction cards
}
