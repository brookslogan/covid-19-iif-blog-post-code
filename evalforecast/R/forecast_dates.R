
#' @export
latest_possible_forecast_date <- function(df,
                                          response,
                                          incidence_period,
                                          ahead,
                                          backfill_buffer) {
  # Inputs:
  #  df, incidence_period, ahead: see evaluate_quantile_forecaster
  #  backfill_buffer: how many days until response is deemed
  #    trustworthy enough to be taken as correct?
  #
  # This function returns the latest possible forecast date for which
  # the target period has data on the response that has been sufficiently
  # backfilled to be considered reliable.
  # If incidence_period is "epiweek", then this will be a Monday.
  first_date <- backfill_variables %>%
    filter(variable_name == response) %>%
    pull(first_date)
  if (length(first_date) == 1) {
    possible_target_days <- df %>%
      mutate(issue_date = replace_na(issue_date, first_date)) %>%
      filter(variable_name == response,
             issue_date >= reference_date + backfill_buffer) %>%
      pull(reference_date)
  } else {
    # response did not appear in backfill_variables,
    # which means we are not worrying about backfill for this
    # variable
    possible_target_days <- df %>%
      filter(variable_name == response) %>%
      pull(reference_date)
  }
  if (length(possible_target_days) == 0)
    stop("No response data is beyond the backfill_buffer.")
  last_possible_target_day <- max(possible_target_days)
  if (incidence_period == "day")
    return(last_possible_target_day - ahead)
  if (incidence_period != "epiweek") stop("Unsupported incidence_period.")
  epiweek <- MMWRweek(last_possible_target_day)
  monday_of_ew <- MMWRweek2Date(MMWRyear = epiweek$MMWRyear,
                                MMWRweek = epiweek$MMWRweek,
                                MMWRday = 2) # 2 is Monday
  if (epiweek$MMWRday == 7) {
    # it's a Saturday, so we have reliably seen this full epiweek
    # and can use it as a target
    return(monday_of_ew - 7 * (ahead - 1))
  }
  # we did not reliably see the response for the full epiweek, so we must
  # back up to the previous epiweek:
  return(monday_of_ew - 7 * ahead)
}



#' If forecast_dates is non-NULL, it makes sure these dates are ok in terms of
#' backfill buffer.  If NULL, then produces latest possible forecast dates.
#' @export
get_or_check_forecast_dates <- function(df,
                                        response,
                                        incidence_period,
                                        ahead,
                                        backfill_buffer,
                                        num_dates = 6,
                                        forecast_dates = NULL) {
  last_forecast_date <- latest_possible_forecast_date(df = df,
                                                      response = response,
                                                      incidence_period = incidence_period,
                                                      ahead = ahead,
                                                      backfill_buffer = backfill_buffer)
  if (!is.null(forecast_dates)) {
    if (max(forecast_dates) > last_forecast_date) {
      stop("One or more of forecast_dates is past ",
           last_forecast_date, " and therefore too recent to be reliable ",
           "given the choice of the backfill_buffer argument.")
    }
  } else {
    # consecutive Mondays ending on last_forecast_date:
    forecast_dates <- last_forecast_date - 7 * ((num_dates - 1):0)
  }
  first_date <- backfill_variables %>%
    filter(variable_name == response) %>%
    pull(first_date)
  if (length(first_date) == 1) {
    if (any(forecast_dates < first_date)) {
      message("-> One or more forecast dates is before ",
              first_date, ", which is the first date that the variable ",
              response, " was issued to us (and therefore forecaster",
              " is receiving better quality ", response, " data than would",
              " in practice be available.")
    }
  }
  return(forecast_dates)
}