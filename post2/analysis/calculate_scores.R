# load packages
library(covidData)
library(covidHubUtils)
library(covidEnsembles)
library(tidyverse)
library(dplyr)

# it is expected that this script is run with the working directory set to the
# root of the covid-19-iif-blog-post-code repository, and that the
# covid-19-iif-blog-post-code and covid-19-iif-blog-post-data repositories are
# set to the same location

knitr::opts_chunk$set(echo = FALSE, cache.lazy = FALSE)
options(width = 200)

# Dates of forecast submission for forecasts included in this analysis
first_forecast_date <- lubridate::ymd("2020-06-22")
last_forecast_date <- lubridate::ymd("2021-01-18")
num_forecast_weeks <- as.integer(last_forecast_date -
                         first_forecast_date) / 7 + 1

forecast_dates <- first_forecast_date +
  seq(from = 0, length = num_forecast_weeks) * 7

# calculate forecast scores
all_scores <- calc_retrospective_ensemble_scores(
  submissions_root =
    "../covid-19-iif-blog-post-data/post2/retrospective-forecasts/",
  forecast_dates = forecast_dates,
  spatial_scales = "state",
  response_vars = c("inc_case", "inc_death"),
  truth_as_of = "2021-02-14"
)

# filter out location 60 (American Samoa) for early forecast dates when such
# forecasts were not available from trained ensemble variations
all_scores <- all_scores %>%
  dplyr::filter(
    !(location == "60" & forecast_date < "2020-07-13")
  )

# filter out the first trained ensemble forecast date for each target --
# the prospective selection method isn't available for those dates.
# NOTE: we decided not to include the prospective selection method in this blog
# post, so comment out this filter for now

# all_scores <- all_scores %>%
#   dplyr::filter(
#     (target_variable == "inc death" & forecast_date > "2020-06-22") |
#     (target_variable == "inc case" & forecast_date > "2020-09-14")
#   )

# extract more useful variables describing ensemble formulation
all_model_cases <- purrr::map_dfr(
  unique(all_scores$model),
  function(x) {
    parse_model_case(x) %>% dplyr::mutate(model = x)
  }
) %>%
  dplyr::mutate(
    combine_method = ifelse(is.na(combine_method), "prospective_selection", combine_method)
  )

all_scores <- all_scores %>%
  dplyr::left_join(all_model_cases, by = "model")

# save
saveRDS(
  all_scores,
  "../covid-19-iif-blog-post-data/post2/retrospective-scores/retrospective_scores.rds")
