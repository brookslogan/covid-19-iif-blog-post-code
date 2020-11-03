#' @export
#' @importFrom MMWRweek MMWRweek  MMWRweek2Date
#' @importFrom lubridate wday
get_target_period <- function(forecast_date, incidence_period, ahead) {
  # This function gives the start and end dates of the target period,
  # based on the system described in the CDC competition rules here:
  # https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md
  #
  # Inputs:
  #  forecast_date: can be a vector of dates
  #  incidence_period: one of "epiweek" or "day"
  #  ahead: how many epiweeks/days ahead are you forecasting?
  if (incidence_period == "day")
    return(tibble(start = forecast_date + ahead, end = forecast_date + ahead))
  if (incidence_period != "epiweek") stop("Unsupported incidence_period")
  # incidence_period: epiweek
  ew_frcst_date <- MMWRweek(forecast_date) # get epiweek of forecast_dates
  sunday_of_ew_frcst_date <- MMWRweek2Date(MMWRyear = ew_frcst_date$MMWRyear,
                                           MMWRweek = ew_frcst_date$MMWRweek,
                                           MMWRday = 1) # 1 is Sunday
  # From https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md:
  # "For week-ahead forecasts with forecast_date of Sunday or Monday of EW12, a
  # 1 week ahead forecast corresponds to EW12 and should have target_end_date of
  # the Saturday of EW12. For week-ahead forecasts with forecast_date of Tuesday
  # through Saturday of EW12, a 1 week ahead forecast corresponds to EW13 and
  # should have target_end_date of the Saturday of EW13."
  week_ahead <- ifelse(wday(forecast_date) <= 2, # forecasting on a Sun/Monday
                       ahead - 1,
                       ahead)
  tibble(start = sunday_of_ew_frcst_date + week_ahead * 7,
         end = sunday_of_ew_frcst_date + (week_ahead + 1) * 7 - 1)
}


#' Get target response data frame
#' 
#' Computes target periods and actual value of response, summed over
#' incidence_period, at each location within each target period. In particular,
#' returns a data frame with columns "location", "location_name", "actual", 
#' "forecast_date", "target_start", and "target_end".
#' @export
get_target_response <- function(df, 
                                forecast_dates, 
                                forecast_locations, 
                                response,
                                incidence_period, 
                                ahead) {
  # calculate the thing we're predicting across all target periods & locations
  target_period <- get_target_period(forecast_dates, incidence_period, ahead)
  target_period <- target_period %>%
    mutate(forecast_date = forecast_dates)
  target_response <- target_period %>%
    pmap_dfr(function(forecast_date, start, end) {
      # get latest issued data for each location-reference_date:
      df_latest <- df %>%
        filter(location %in% forecast_locations,
               reference_date >= start,
               reference_date <= end,
               variable_name == response) %>%
        mutate(issue_date = if_else(is.na(issue_date),
                                    reference_date - 1,
                                    issue_date)) %>%
        group_by(location, reference_date) %>%
        top_n(n = 1, wt = issue_date) %>% # NA only chosen if it's all there is
        ungroup()
      df_latest %>%
        group_by(location, location_name) %>%
        summarize(actual = sum(value)) %>%
        mutate(forecast_date = forecast_date,
               target_start = start, target_end = end) %>%
        ungroup()
    })
  return(target_response)
}
