#' Get a predictions card
#' 
#' Returns a "predictions card" which is a tibble with a row per location and
#' three columns:
#' 1. location
#' 2. forecast_date
#' 3. forecast_distribution: this is a list column... each element itself
#'      contains a tibble with the cdc quantiles
#' A predictions card also has attributes that specify the exact forecasting
#' task that was being carried out.
#'
#' @param df data frame with columns "location", "reference_date", "issue_date",
#'    "variable_name" and "value".
#' @param your_quantile_forecaster a function that follows the template of
#'    my_quantile_forecaster
#' @param forecast_date an object of class Date on which forecasts will be made.
#'    e.g. \code{lubridate::ymd("2020-05-04")}
#' @param forecast_locations The set of locations where predictions are to be made.
#'    If df contains location_to_be_forecast as a variable_name, then must
#'    leave this argument NULL.  Otherwise, NULL means that predictions at all
#'    locations will be used.
#' @param response name of the variable that your_quantile_forecaster is
#'   designed to predict (it should appear in the "variable_name" column of df).
#'   If using incidence_period "epiweek", it should be something for which
#'   summing daily values over an epiweek makes sense (e.g., counts or
#'   proportions but not log(counts) or log(proportions)).
#' @param incidence_period one of "epiweek" or "day"
#' @param ahead how many epiweeks/days ahead are you forecasting?  If
#'   incidence_period is "epiweek", then ahead = 1 means the epiweek that
#'   includes the forecast date.  If incidence_period is "day" then ahead = 1
#'   means the day after forecast date.
#' @param geo_type one of the geographical types, matching the terms used on the
#'   api ("county", "dma", "hrr", "msa", "state")
#' @param n_locations How many locations to evaluate predictions at? See
#'   function \code{\link{top_n_locations}} for more details about how these 
#'   locations are chosen.
#' @return a predictions card
#' @export
get_predictions_card <- function(df,
                                 your_quantile_forecaster,
                                 forecast_date,
                                 forecast_locations = NULL,
                                 response,
                                 incidence_period,
                                 ahead,
                                 geo_type,
                                 n_locations) {
  if (!is.null(forecast_locations) &
      "location_to_be_forecast" %in% df$variable_name)
    stop("df already has location_to_be_forecast information, ",
         "so must either leave forecast_locations = NULL or else ",
         "remove these rows from df before calling get_predictions_card.")
  if (!("location_to_be_forecast" %in% df$variable_name)) {
    # modify df to include location_to_be_forecast variable:
    if (is.null(forecast_locations)) forecast_locations <- unique(df$location)
    df_location_info <- distinct(df %>% select(location, location_name)) %>%
      mutate(reference_date = forecast_date,
             issue_date = NA,
             variable_name = "location_to_be_forecast",
             value = if_else(location %in% forecast_locations, 1, 0))
    df <- bind_rows(df, df_location_info)
  }
  preds_card <- your_quantile_forecaster(df = df, 
                                         forecast_date = forecast_date)
  if (!is.na(preds_card) && nrow(preds_card) > 0) {
    preds_card = preds_card %>%
      group_by(location) %>%
      group_modify(~ tibble(forecast_date = forecast_date,
                            forecast_distribution = list(.))) %>%
      ungroup()
  }
  attr(preds_card, "call_args") <- list(
    response = response,
    incidence_period = incidence_period,
    ahead = ahead,
    geo_type = geo_type,
    n_locations = n_locations,
    forecast_date = forecast_date)
  return(preds_card)
}