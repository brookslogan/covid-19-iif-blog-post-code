

#' Evaluate a list of prediction cards
#' 
#' @param df data frame with columns "location", "reference_date", "issue_date",
#'   "variable_name" and "value".
#' @param predictions_cards a list of prediction cards from the same forecaster
#' that are all for the same prediction task, meaning they are for the same
#' response, incidence_period, ahead, geo_type, and n_locations. Each should 
#' be from a different forecast date.  A predictions card is created by the 
#' function get_predictions_card.
#' @param err_measure a function that takes a data frame with two columns
#'   "probs" and "quantiles" and an actual (i.e. observed) scalar value and
#'   returns some measure of error.
#' @param backfill_buffer How many days until response is deemed trustworthy
#'   enough to be taken as correct?
#' @return a score card
#' @export
evaluate_quantile_predictions_cards <- function(df,
                                         predictions_cards,
                                         err_measure = weighted_interval_score,
                                         backfill_buffer = 10) {
  # if a forecaster does not make predictions, it will return an empty or NA predictions_card
  # we remove those empty predictions_cards because we are not evaluating them
  predictions_cards =
    predictions_cards[!is.na(predictions_cards)]
  predictions_cards =
    predictions_cards[!(predictions_cards %>% map_int(nrow) == 0)]
  if (length(predictions_cards) == 0) {
    stop("All prediction cards are either NA or empty.")
  }
  # check that the predictions cards are from the same task,
  # differing only by forecast_date
  attribs <- predictions_cards %>% map(attributes)
  if (attribs %>% map_lgl(~ length(.x) == 0) %>% any)
    stop("At least one predictions_card is missing attributes.")
  if (attribs %>% map_lgl(~ !("call_args" %in% names(.x))) %>% any)
    stop("At least one predictions_card is missing the call_args attribute.")
  call_args <- attribs %>% map("call_args")
  task_params <- c("response", "incidence_period", "ahead", "geo_type",
                   "n_locations", "forecast_date") 
  if(call_args %>% map_lgl(~ !all(task_params %in% names(.x))) %>% any)
    stop("At least one predictions_card's call_args attribute has missing info.")
  # using do.call in the next line to keep these of class Date:
  # https://stackoverflow.com/questions/15659783/why-does-unlist-kill-dates-in-r
  forecast_dates <- do.call("c", call_args %>% map("forecast_date"))
  params <- call_args %>% map(~ list_modify(.x, "forecast_date" = NULL))
  if (length(unique(params)) > 1) 
    stop("Task parameters of predictions_cards do not match.")
  if (length(unique(forecast_dates)) != length(forecast_dates))
    stop("Each prediction card must be for a different forecast_date.")
  params <- params[[1]] # these define the forecast task
  
  stopifnot(c("location",
              "reference_date",
              "issue_date",
              "variable_name",
              "value") %in% names(df))
  # check that df is compatible with prediction cards
  stopifnot(params$response %in% df$variable_name)
  forecast_dates <- get_or_check_forecast_dates(df = df,
                                                response = params$response,
                                                incidence_period = params$incidence_period,
                                                ahead = params$ahead,
                                                backfill_buffer = backfill_buffer,
                                                forecast_dates = forecast_dates)
  
  # calculate the locations at which we'd like to predict:
  locations_list <- predictions_cards %>% map("location")
  if (length(unique(locations_list)) != 1) {
    message("Predictions cards for the different forecast dates do not have ",
            "identical locations.  This is permitted, but care should be ",
            "taken when comparing results across different forecast dates.")
  }
  locations <- unique(unlist(locations_list))
  # compute the actual value of what we are trying to predict at each location
  # within each target period:
  target_response <- get_target_response(df = df, 
                                         forecast_dates = forecast_dates, 
                                         forecast_locations = locations, 
                                         response = params$response,
                                         incidence_period = params$incidence_period, 
                                         ahead = params$ahead)
  # combine all prediction cards into a single data frame with an additional
  # column called forecast_date:
  predicted <- map2_dfr(predictions_cards, forecast_dates,
                        ~ mutate(.x, forecast_date = .y))
  # check_for_unsupported_predicted_locations(predicted$location,
  #                                           forecast_locations_info$locations)
  
  # join together the data frames target_response and predicted:
  score_card <- target_response %>%
    inner_join(predicted, by = c("location", "forecast_date")) %>%
    select(location,
           location_name,
           forecast_date,
           target_start,
           target_end,
           actual,
           forecast_distribution)
  invalid <- check_valid_forecaster_output(score_card)
  if (nrow(invalid) > 0)
    stop("The following forecast_date, location pairs have invalid forecasts:",
         invalid %>%
           str_glue_data("({forecast_date}, {location})") %>%
           paste0(collapse = ", "))
  
  # compute the error
  score_card <- score_card %>%
    rowwise() %>%
    mutate(err = err_measure(forecast_distribution, actual)) %>%
    ungroup()
  
  # add warning flag
  first_date <- backfill_variables %>%
    filter(variable_name == params$response) %>%
    pull(first_date)
  if (length(first_date) == 0) {
    # response is not a backfilling variable
    first_date <- min(forecast_dates) # ensures flag = FALSE below
  }
  score_card <- score_card %>%
    mutate(unfair_old_data_flag = forecast_date < first_date,
           na_prediction = forecast_distribution %>%
             map_lgl(~ any(is.na(.x$quantiles))))
  na_predictions <- score_card %>%
    filter(na_prediction) %>%
    count(location) %>%
    pull(location)
  if (length(na_predictions) > 0)
    message("-> Forecaster gives NAs at the following locations: ",
            paste0(na_predictions, collapse = ", "))
  
  score_card_attributes <- list(response = params$response,
                                incidence_period = params$incidence_period,
                                ahead = params$ahead,
                                geo_type = params$geo_type,
                                err_measure = deparse(substitute(err_measure)),
                                backfill_buffer = backfill_buffer,
                                n_locations = params$n_locations)
  
  attr(score_card, "metadata") <- score_card_attributes
  
  evaluation_attributes <- list(evaluation_date = Sys.Date())
  
  attr(score_card, "hidden_metadata") <- evaluation_attributes  ## Hello there!  You found me.  -RS
  
  return(score_card)
}

#' Evaluate a quantile forecaster
#'
#' This function evaluates \code{your_quantile_forecaster} by applying it on a
#' series of forecast_dates and then calculating the error according to a
#' provided error measure (default is weighted_interval_score).
#'
#' @param df data frame with columns "location", "reference_date", "issue_date",
#'   "variable_name" and "value".
#' @param your_quantile_forecaster a function that follows the template of
#'   my_quantile_forecaster
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
#' @param forecast_dates a vector of Dates, on which forecasts will be made
#'   about some period (e.g., epiweek).  For example, if forecast_date is
#'   ymd("2020-05-11"), incidence_period is "day",  and ahead = 3, then, we'd be
#'   making forecasts for "2020-05-14".  If NULL, then a sequence of 6 Mondays
#'   will be chosen that are as late as possible given what's available in df.
#'   See documentation for the function latest_possible_forecast_date, for more
#'   on how these dates are chosen. To specify your own range, you can use
#'   something of the form seq(ymd("2020-04-06"), ymd("2020-05-11"), by = 7).
#' @param err_measure a function that takes a data frame with two columns
#'   "probs" and "quantiles" and an actual (i.e. observed) scalar value and
#'   returns some measure of error.
#' @param backfill_buffer How many days until response is deemed trustworthy
#'   enough to be taken as correct?
#' @param n_locations How many locations to evaluate predictions at? See
#'   function top_n_locations for more details about how these locations are
#'   chosen.
#' @return a score card
#' @export
#' @importFrom lubridate is.Date
evaluate_quantile_forecaster <- function(df,
                                your_quantile_forecaster,
                                response,
                                incidence_period,
                                ahead,
                                geo_type,
                                forecast_dates = NULL,
                                err_measure = weighted_interval_score,
                                backfill_buffer = 10,
                                n_locations = 200) {

  stopifnot(length(response) == 1,
            length(ahead) == 1,
            length(your_quantile_forecaster) == 1,
            length(incidence_period) == 1)
  stopifnot(is.data.frame(df),
            is.function(your_quantile_forecaster),
            is.character(response),
            is.numeric(ahead),
            is.numeric(n_locations))
  if (!is.null(forecast_dates)) stopifnot(is.Date(forecast_dates))
  stopifnot(c("location",
              "reference_date",
              "issue_date",
              "variable_name",
              "value") %in% names(df))
  if (!identical(names(formals(your_quantile_forecaster)),
                 c("df", "forecast_date")))
     stop("your_quantile_forecaster must have arguments df and forecast_date.")
  stopifnot(response %in% df$variable_name)
  stopifnot(ahead >= 1, round(ahead) == ahead)
  stopifnot(n_locations >= 1,round(n_locations) == n_locations)

  forecast_dates <- get_or_check_forecast_dates(df = df,
                                                response = response,
                                                incidence_period = incidence_period,
                                                ahead = ahead,
                                                backfill_buffer = backfill_buffer,
                                                forecast_dates = forecast_dates)
  
  # calculate the locations at which we'd like to predict:
  forecast_locations_info <- get_forecast_locations_info(df,
                                                         forecast_dates,
                                                         response,
                                                         incidence_period,
                                                         ahead,
                                                         n_locations)

  # add location_to_be_forecast binary variable to df:
  df <- add_location_to_be_forecast_to_df(df,
                                          forecast_locations_info,
                                          response,
                                          incidence_period,
                                          ahead)
  
  pred_cards <- as.list(forecast_dates) %>%
    map(~ {
      predictions_card <- get_predictions_card(df = df,
                                               your_quantile_forecaster = your_quantile_forecaster,
                                               forecast_date = .x,
                                               forecast_locations = NULL,
                                               response = response,
                                               incidence_period = incidence_period,
                                               ahead = ahead,
                                               geo_type = geo_type,
                                               n_locations = n_locations)
      return(predictions_card)
      })
  pred_locations <- pred_cards %>% map("location") %>% unlist()
  check_for_unsupported_predicted_locations(pred_locations,
                                            forecast_locations_info$locations)
  score_card <- evaluate_quantile_predictions_cards(df = df,
                                                   predictions_cards = pred_cards,
                                                   err_measure = err_measure,
                                                   backfill_buffer = backfill_buffer)
  return(score_card)
}

#' @export
check_valid_forecaster_output <- function(score_card) {
  # score_card: tibble of the form created within evaluate_quantile_forecaster
  # but before the error measure is added
  null_forecasts <- score_card$forecast_distribution %>%
    map_lgl(is.null)
  cdc_probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  wrong_probs <- score_card$forecast_distribution %>%
    map_lgl(~ all(abs(.x$probs - cdc_probs) > 1e-8))
  bad_quantiles <- score_card$forecast_distribution %>%
    map_lgl(~ all(diff(.x$quantiles) < -1e-8))
  score_card %>%
    mutate(null_forecasts = null_forecasts,
           wrong_probs = wrong_probs,
           bad_quantiles = bad_quantiles) %>%
    filter(null_forecasts | wrong_probs | bad_quantiles)
}
