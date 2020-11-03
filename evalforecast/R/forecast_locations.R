#' Get the locations at which to forecast
get_forecast_locations_info <- function(df,
                                        forecast_dates,
                                        response,
                                        incidence_period,
                                        ahead,
                                        n_locations) {
  latest_date_for_location_selection <- min(                           # choose locations based
    get_target_period(min(forecast_dates), incidence_period,ahead) %>% # only on information
      pull(start) - 1,                                                 # before any target period
    min(forecast_dates) - 1                                            # or forecast date
  )
  forecast_locations <- get_top_n_locations(df = df,
                                            response = response,
                                            latest_date = latest_date_for_location_selection,
                                            n = n_locations)
  list(locations = forecast_locations, 
       latest_date = latest_date_for_location_selection)
}

#' Add location_to_be_forecast information to df
#' 
#' Adds a new variable to df called "location_to_be_forecast" that is 
#' binary indicating whether this is a location to be forecast.
#' 
#' @param df data frame with columns "location", "reference_date", "issue_date",
#'   "variable_name" and "value".
#' @param forecast_locations_info output of function get_forecast_locations_info
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
#' @export
add_location_to_be_forecast_to_df <- function(df,
                                              forecast_locations_info,
                                              response,
                                              incidence_period,
                                              ahead) {
  df_location_info <- distinct(df %>% select(location, location_name)) %>%
    mutate(reference_date = forecast_locations_info$latest_date,
           issue_date = NA,
           variable_name = "location_to_be_forecast",
           value = if_else(location %in% forecast_locations_info$locations, 1, 0))
  df <- bind_rows(df, df_location_info)
  return(df)
}

check_for_unsupported_predicted_locations <- function(predicted_locations, 
                                                      forecast_locations) {
  unsupported_predicted_locations <- setdiff(predicted_locations,
                                             forecast_locations)
  if(length(unsupported_predicted_locations) > 0){
    if(length(unsupported_predicted_locations) > 50){
      # keep the warning from being too long
      unsupported_predicted_locations <- c(unsupported_predicted_locations[1:50],"...")
    }
    warning("The following locations: ",
            paste0(unsupported_predicted_locations, collapse = " "),
            " have predictions, but are not in the top ", n_locations,
            " locations as computed by the function get_forecast_locations", 
            ", and these predictions will not be evaluated.")
  }
}


#' Get the top n (or more) locations
#' 
#' This function returns the n** locations which have observed the largest
#' values of the response, cumulatively over all dates up to and including the
#' latest date
#'
#' **: (if there are multiple locations with value equal to the nth location,
#' they are all included)
#'
#' @param df data frame with columns location, reference_date, issue_date,
#'   variable_name, and value (and possibly others, which we will ignore)
#' @param response string indicating the name of the response
#' @param latest_date the latest date which we will use when making our location
#'   choice
#' @param n how many locations should we pick? If it is greater than the total
#'   number of locations, than we pick all of them.
#'
#' @export
get_top_n_locations <- function(df,response,latest_date,n){
  stopifnot(is.data.frame(df),
            is.character(response),
            is.Date(latest_date),
            is.numeric(n))
  stopifnot(c("location",
              "reference_date",
              "issue_date",
              "variable_name",
              "value") %in% names(df))
  stopifnot(response %in% df$variable_name)
  stopifnot(n > 0, n == round(n))
  
  df_use <- df %>% select(location,reference_date,issue_date,variable_name,value)
  df_latest <- df_use %>%
    filter(reference_date <= latest_date,
           variable_name == response) %>%
    mutate(issue_date = if_else(is.na(issue_date),
                                reference_date - 1,
                                issue_date)) %>%
    group_by(location, reference_date) %>%
    top_n(n = 1, wt = issue_date) %>% # NA only chosen if it's all there is
    ungroup()
  top_n_locations <- df_latest %>%
    group_by(location) %>%
    summarise(value = sum(value,na.rm = T)) %>% # treat NA values as 0
    ungroup() %>%
    top_n(n = !!n,wt = value) %>% # may not result in exactly n locations, due to ties.
    pull(location)
  
  return(top_n_locations)
}
