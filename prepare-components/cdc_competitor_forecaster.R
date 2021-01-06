#----------------------------------------#
# CDC competitor forecaster.
# All this should do is take in a CDC competitor's forecasts,
# as found here: https://github.com/reichlab/covid19-forecast-hub/tree/master/
# and reformat to make compatible with our evaluator.
#----------------------------------------#

current_or_next_monday <- function(date) {
  date + (8L - as.POSIXlt(date)$wday) %% 7L
}

make_cdc_competitor_forecaster <- function(path_to_competitor_dir, response = "jhu-csse_deaths_incidence_num",
                                           ahead = 1, target_period = "epiweek",
                                           fall_back_to_inferred_incidence=TRUE)
{
  # Closure, which makes a forecaster which just reads off a competitor's forecasts
  # and puts them in the right format for our evaluator.
  #
  # Input:
  # -- path_to_competitor_dir: string.
  # -- response: string, name of response variable
  # -- ahead: numeric, number of target periods ahead to predict
  # -- target_period: string, one of "epiweek" or "day"
  # 
  # In addition to the arguments to my closure, which I want to be able to change
  # I also want certain variables which should not be subject to change.
  stopifnot(target_period %in% c("epiweek","day"))
  cdc_probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  cdc_competitor_forecaster <- function(df, forecast_date){
    
    # Find the right csv in a directory full of of them.
    competitor_raw_forecast_files <- list.files(path_to_competitor_dir) %>% 
      subset(.,grepl("csv",.))
    competitor_forecast_dates_all <- as.Date(gsub("^([^-]*-[^-]*-[^-]*)-.*$","\\1",competitor_raw_forecast_files))
    competitor_forecast_dates_visible <- competitor_forecast_dates_all[competitor_forecast_dates_all <= forecast_date]
    if (length(competitor_forecast_dates_visible) == 0L) {
      ## no submissions visible from this forecaster visible on the forecast date
      ## return (tibble::tibble(location=character(0L), probs=numeric(0L), quantiles=numeric(0L)))
      ## return (tibble::tibble(location=character(0L), forecast_date=as.Date(character(0L)), forecast_distribution=list()))
      return (NA)
    }
    competitor_forecast_date_candidate = max(competitor_forecast_dates_visible)
    if (abs(current_or_next_monday(competitor_forecast_date_candidate) - current_or_next_monday(forecast_date)) > 0.1) {
      if (current_or_next_monday(competitor_forecast_date_candidate) > current_or_next_monday(forecast_date)) {
        ## bug or other issue
        stop ('Unexpectedly had competitor forecast date from later submission cycle than forecast date.')
      } else {
        ## competitor did not submit this cycle; candidate is from a prior cycle
        ## return (tibble::tibble(location=character(0L), probs=numeric(0L), quantiles=numeric(0L)))
        ## return (tibble::tibble(location=character(0L), forecast_date=as.Date(character(0L)), forecast_distribution=list()))
        return (NA)
      }
    } else {
      competitor_forecast_date = competitor_forecast_date_candidate
    }
    if(forecast_date != competitor_forecast_date)
      warning(paste0("Forecast date is ", forecast_date, ", competitor uploaded on ", competitor_forecast_date))
    path_to_competitor_csv <- file.path(path_to_competitor_dir,
      competitor_raw_forecast_files[grepl(competitor_forecast_date,competitor_raw_forecast_files)]
      )
    # Load competitor's raw forecasts
    competitor_raw_forecast_df <- read_csv(path_to_competitor_csv, 
                                           col_types = cols(
                                             forecast_date = col_date(),
                                             target = col_character(),
                                             target_end_date = col_date(),
                                             location = col_character(),
                                             location_name = col_character(),
                                             type = col_character(),
                                             quantile = col_double(),
                                             value = col_double()
                                           )
      )
    
    # Grab the right variable (epiweek incident deaths)
    target_variable_name <- get_target_variable_name(ahead,target_period,response)
    
    competitor_forecast_df <- competitor_raw_forecast_df %>%
      filter(target == target_variable_name & as.character(quantile) %in% as.character(cdc_probs)) %>%
      select(c(location,quantile,value))
    
    # If the competitor did not enter this variable, progress to plan B.
    if(nrow(competitor_forecast_df) == 0)
    {
      # Right now, we have only designed repairs for the special case of incident deaths predictions.
      stopifnot(fall_back_to_inferred_incidence)
      stopifnot(response == "jhu-csse_deaths_incidence_num")
      
      # Impossible to get incidence distribution from cumulative distribution,
      # unless we are doing one week ahead prediction.
      stopifnot(ahead == 1)
      stopifnot(any(competitor_raw_forecast_df$target == "1 wk ahead cum death"))
      
      
      warning("Getting competitor predictions using a surrogate variable.")
      
      # Grab the wrong variable (epiweek cumulative deaths)
      target_variable_name <- get_target_variable_name(ahead,target_period,response,repair = 1) 
      competitor_cum_forecast_df <- competitor_raw_forecast_df %>%
        filter(grepl(target_variable_name,target) & 
               as.character(quantile) %in% as.character(cdc_probs)) %>%
        mutate(epiweek = MMWRweek(target_end_date)$MMWRweek) %>%
        select(location,quantile,value,epiweek)
      
      # Use **our** JHU CSSE data as a representation for cumulative deaths as of the forecast date
      last_observed_date <- MMWRweek2Date(2020, min(competitor_cum_forecast_df$epiweek) - 1, 7)
      ## fixme
      cum_death_df <- df %>% filter(variable_name == "jhu-csse_deaths_cumulative_num" &
                                    reference_date == last_observed_date) %>%
       mutate(epiweek = min(competitor_cum_forecast_df$epiweek) - 1) %>%
       expand_grid(quantile = cdc_probs) %>%
       select(location,quantile,value,epiweek)
      
      competitor_cum_forecast_df <- bind_rows(competitor_cum_forecast_df,cum_death_df)
      
      # Go from cumulative to incidence
      competitor_forecast_df <- competitor_cum_forecast_df %>% 
        arrange(epiweek) %>%
        mutate(quantile_chr = as.character(quantile)) %>%
        group_by(location,quantile_chr) %>%
        mutate(value = diff_na_pad(value)) %>%
        ungroup() %>%
        select(-quantile_chr)
      
      # Grab the right epiweek
      target_epiweek <- if_else(MMWRweek(forecast_date)$MMWRday <= 2, 
                                  MMWRweek(forecast_date)$MMWRweek + ahead - 1,
                                  MMWRweek(forecast_date)$MMWRweek + ahead)
      competitor_forecast_df <- competitor_forecast_df %>% 
        filter(epiweek == target_epiweek) %>%
        select(-epiweek)
    }
    all_predictions <- expand_grid(location = unique(df$location),
                                   probs = cdc_probs) %>%
      mutate(probs_chr = as.character(probs))
    predictions <-  left_join(all_predictions,
                              competitor_forecast_df %>% 
                                rename(probs = quantile,quantiles = value) %>%
                                mutate(probs_chr = as.character(probs)),
                              by = c("location","probs_chr")) %>%
      rename(probs = probs.x) %>%
      mutate(quantiles = pmax(quantiles,0))  %>%
      select(-c(probs_chr,probs.y))
    return(predictions)
  }
}

diff_na_pad <- function(x)
{
  # Useful wrapper for mutating in place.
  return(c(NA,diff(x)))
}

get_target_variable_name <- function(ahead,target_period,response,repair = 0){
  # Get the right target variable name
  # Inputs
  #   repair: numeric, indicating whether we need to progress to a backup variable
  
  stopifnot(target_period %in% c("day","epiweek"))
  stopifnot(response %in% c("jhu-csse_deaths_incidence_num", "reich_lab_jhu-csse_deaths_incidence_num"))
  
  if(repair == 0)
  {
    tgt_pd <- if(target_period == "day")  "day" else "wk"
    return(paste(ahead,tgt_pd,"ahead","inc","death",sep = " "))
  } else if(repair == 1){
    tgt_pd <- if(target_period == "day")  "day" else "wk"
    return(paste(tgt_pd,"ahead","cum","death",sep = " "))
  }
  
}
