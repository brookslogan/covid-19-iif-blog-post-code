#library(zoo)
#library(MMWRweek)

# Utility functions for local forecasting.

## NOTE THE SPELLING MISTAKE, should be multinomial_preprocessor!
## SO make an alias below!
#' @export
multinomial_preprocesser <- function(dat, response, max_lag = Inf){
  # Preprocesses the response values in a data frame.
  # Replaces zeroes (which we suspect are due to lagged reporting) by samples from a multinomial.
  # Input:
  #  dat: a data frame with columns reference_date, location, variable_name, and value
  #       (and possibly some other columns we will ignore.)
  #  response: the name of the response variable.
  # max_lag: maximum amount counts can be moved.

  # Put data in the right order.
  dat <- arrange(dat,reference_date)

  # Impute at each location.
  locs <- unique(dat$location)
  for(ii in 1:length(locs))
  {
    indices <- which(dat$location == locs[ii] & dat$variable_name == response)
    df_use <- dat[indices,]

    # Guard against misuse of this function.
     if( any( duplicated(df_use %>% select(reference_date)) ) )
       stop("Multiple response values for the same reference_date. Did you select an issue_date for this reference_date yet?")
    x <- pull(df_use,value)
    dat[indices,"value"] <- multinomial_roll_sum(x, max_lag = max_lag)
  }
  return(dat)
}

## Alias
#' @export
multinomial_preprocessor  <- multinomial_preprocesser


#' @export
multinomial_roll_sum <-  function(x, max_lag = Inf){
  # Preprocesses x by replacing NAs by zeros, then eliminating zeros/negatives and corresponding summed counts.
  # E.g. takes x = (0,NA,-10,0,100) and replaces it by x* ~ Multinomial(90, [1/5,1/5,1/5,1/5,1/5])
  # Input: x, a numeric vector, consisting of integers (possibly negative) and NAs.

  # Replace NAs by zeros.
  x <- replace_na(x,0)

  # x should be an integer
  if(!all(x == floor(x)))
  {
    warning("Variable is non-integer; to do multinomial trick, replacing it by its floor.")
    x <- floor(x)
  }


  # Find break-points
  breaks <- which(x > 0) + 1
  x_by_break <- unname(split(x, cumsum(seq_along(x) %in% breaks)))

  # Sample from multinomial
  return( unlist(x_by_break %>% map(generate_multinomial, max_lag = max_lag)) )
}

#' @export
#' @importFrom stats rmultinom
generate_multinomial <- function(y, max_lag) {
  if (length(y) <= max_lag)
    rmultinom(1,size = max(sum(y),0),prob = rep(1,length(y)))
  else {
    ii_lag <- seq(length(y) - max_lag + 1, length(y))
    c(y[-ii_lag], rmultinom(1,
                            size = max(sum(y[ii_lag]), 0),
                            prob = rep(1, length(ii_lag))))
  }
}

#' @export
#' @importFrom stats rnorm
make_gaussian_bootstrap <- function(ave){
  # Closure to make a gaussian boostrap
  # Input:
  # -- ave: a function which takes as inputs x and w, and computes a weighted average.

  gaussian_bootstrap <- function(B, resids, w){
    warning("You are using a deprecated version of the Gaussian bootstrap.")
    sigma_hat <- sqrt(ave(resids^2,w = w))
    bootstrap_residuals <- data.frame(value =
                                         sigma_hat * rnorm(B))
    return(bootstrap_residuals)
  }
}

#' @export
empirical_bootstrap <- function(B, resids, w){
  # Draw samples from (a weighted form of) the empirical distribution.
  bootstrap_residuals <- data.frame(value = sample(resids, B*length(target_period_dates), replace = T, prob = w))
  return(bootstrap_residuals)
}

#' @export
weighted_median <- function(x,w){
  # Take a weighted median of x
  #
  # x: numeric vector.
  # wts: numeric vector.
  stopifnot(all(w >= 0))
  stopifnot(sum(w) > 0)
  w <- w / sum(w)
  order <- order(x)
  x <- x[order]
  w <- w[order]
  med.index <- min(which(cumsum(w) >= .5))
  return(x[med.index])
}

#' @export
#' @importFrom MMWRweek MMWRweek
day_to_epiweek <- function(df,funs = sum){
  # Aggregate a day df to an epiweek df.
  # Input:
  # -- df: a data frame with columns location, location_name, reference_date, variable_name, and value
  #        (and possibly others, which will be ignored).
  # -- funs: one of,
  #            -- an aggregation function, to aggregate from day to epiweek for all variables.
  #          or
  #            -- a named list of aggregation functions, to aggregate from day to epiweek for each variable.
  #               The names of the list must be the variables in df.
  #               There must be one such function for each variable_name in df.

  # Checks
  stopifnot(c("location","location_name","reference_date","variable_name","value") %in% names(df))
  if( any( duplicated(df %>% select(reference_date,location,variable_name)) ) )
    stop("Multiple values for the same (location,reference_date,variable_name). Did you select an issue_date for this reference_date yet?")
  stopifnot(all(!is.na(df$value)))
  stopifnot(length(funs) == 1 | names(funs) == unique(df$variable_name) )

  # Add epiweek column
  df_epiweek <- df %>%
    mutate(epiweek = MMWRweek(reference_date)$MMWRweek) %>%
    group_by(location,location_name,epiweek,variable_name)
  if(length(funs) == 1)
  {
     df_epiweek <- df_epiweek %>%
      summarise(value = funs(value)) %>%
      ungroup
  } else{
    df_by_variable <- list()
    for(ii in 1:length(funs)){
      df_variable <- df %>% filter(variable_name == names(funs)[[ii]])
      df_by_variable[[ii]] <- df_variable %>%
        summarise(value = funs[[ii]](value)) %>%
        ungroup
    }
    df_epiweek <- bind_rows(df_by_variable)
  }

  return(df_epiweek)
}

#' @export
tricube <- function(u){
  # Formula for the tricube kernel from Wikipedia.
  pmax(70/81 * (1 - u^3)^3,0)
}

#' @export
#' @importFrom magrittr %$%
make_data_with_lags <- function(df_use,
                                forecast_date,incidence_period,ahead,
                                response,features)
{
  ## This function assembles all the data we will need for training and predicting.
  ## This means **I guarantee** the output of this function should have
  ## an entry for each (variable_name, location,date) triple
  ## in either my training or test period.

  ## (a) All possible location, date, variable_name triples.
  locations <- distinct(df_use %>% filter(variable_name == response) %>% select(location,location_name))
  target_dates <- get_target_period(forecast_date,incidence_period,ahead) %$%
    seq(start,end,by = "days")
  reference_dates <- unique(c(df_use %>% pull(reference_date),target_dates))
  variable_names <- unique(c(response,features$variable_name))
  df_empty <- expand_grid(locations,
                          reference_date = reference_dates,
                          variable_name = variable_names)

  ## (b) Join with available data.
  df_all <- left_join(df_empty,
                      df_use,
                      by = c("location", "location_name","reference_date","variable_name"))

  ## (c) Add lags.
  lags <- unique(features$lag)
  if(!is.null(lags))
  {
    stopifnot(is.numeric(lags))

    # Lags for all variables.
    lag_functions <- lags %>%
      map(function(x) ~ lag(., n = x, order_by = reference_date,
                            default = first(.,order_by = reference_date)))
    names(lag_functions) <- paste0("lag_", lags)
    df_all_with_lags <- df_all %>%
      group_by(location, variable_name) %>%
      mutate_at(vars(value),.funs = lag_functions) %>%
      ungroup() %>%
      rename(lag_0 = value) %>%
      pivot_longer(contains("lag_"),
                   names_to = "lag",values_to = "value")

    # Tidy up columns
    df_all_with_lags <- df_all_with_lags %>%
      mutate(variable_name = paste0(variable_name,"_",lag)) %>%
      select(-lag)

    # Just select the ones we want.
    feature_names <- paste0(features$variable_name,"_lag_",features$lag)
    response_name <- paste0(response,"_lag_",0)
    df_with_lags <- df_all_with_lags %>%
      filter(variable_name %in% c(feature_names,response_name))

  } else{
    df_with_lags <- df_all %>%
      mutate(variable_name = paste0(variable_name,"_lag_0"))
  }
  return(df_with_lags)
}

## DEPRECATED ##

#' @export
get_target_align_dates <- function(df_use, forecast_date, incidence_period, ahead)
{
  # Function which returns all of the align_dates which will fall in the target period
  # for at least one location.
  target_dates <- get_target_period(forecast_date,incidence_period,ahead) %$%
    seq(start,end,by = "days")

  df_align <- filter(df_use %>% select(variable_name,location,reference_date,original_value),
                     variable_name == "days_since_cumulative_cases_count_thresh_100") %>%
    filter(!is.na(original_value)) %>%
    group_by(location) %>%
    slice(which.min(reference_date)) %>%
    mutate(date0 = reference_date - original_value) %>%
    select(location,date0)

  target_align_dates <- expand_grid(df_align,reference_date = target_dates) %>%
    mutate(align_date = ifelse(reference_date - date0 >= 0,reference_date - date0,NA)) %>%
    pull(align_date) %>% unique

  return(target_align_dates)
}

#' Return the correct name of the response variables because of the data change on July 13, 2020
#' @param forecast_date the date on which the forecast is being done, such as `lubridate::ymd("2020-07-13")`
#' @return a list of two elements with names `county` and `state` for the respective response variables
#' @export
get_response_variables <-  function(forecast_date) {
    if (forecast_date >= lubridate::ymd("2020-07-13")) {
        list(county = "usa-facts_deaths_incidence_num",
             state = "jhu-csse_deaths_incidence_num")
    } else {
        list(county = "usafacts_deaths_incidence_num",
             state = "jhu-csse_deaths_incidence_num")
    }
}
