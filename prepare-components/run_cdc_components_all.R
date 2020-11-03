library(logger)
library(sessioninfo)
library(fs)
library(readr)
library(stringr)
source("cdc_competitor_forecaster.R")
devtools::load_all("../evalforecast")

aheads = 1:4
responses = list(state = "jhu-csse_deaths_incidence_num")

data_list = list(
  "state" = readRDS("../../covid-19-iif-blog-post-data/signals/upstream_df_state_2020-10-11_final_corrected_trimmed.RDS")
)

incidence_period <- "epiweek"
n_locations  <- 200
## forecast_dates = as.character(lubridate::ymd("2020-04-06") + 7*seq(0,19))
forecast_dates = as.character(lubridate::ymd("2020-04-06") + 7*seq(0,27))
forecaster_dirs = Sys.glob("../../../covid19-forecast-hub/data-processed/*/")
forecaster_dirs = str_sub(forecaster_dirs,end=-2)
output_path_1 = "../../covid-19-iif-blog-post-data/smallcards/historical_cdc_components_all"
geo_type = "state"

for (ahead in aheads) {
  response <- responses[[geo_type]]
  for (forecast_date in forecast_dates) {
    forecast_date = lubridate::ymd(forecast_date)
    forecasters = purrr::map(forecaster_dirs, function(dir) { make_cdc_competitor_forecaster(dir, response=response, ahead=ahead, fall_back_to_inferred_incidence=TRUE)})
    names(forecasters) = purrr::map_chr(forecaster_dirs, function(dir) { tail(str_split(dir,"/")[[1]],n=1)})
    result  <- evalforecast::run_forecasters(forecasters = forecasters,
                                             response = response,
                                             incidence_period = incidence_period,
                                             ahead = ahead,
                                             geo_type = geo_type,
                                             n_locations = n_locations,
                                             forecast_date = forecast_date,
                                             data_list = data_list)
    logger::log_info("Saving prediction outputs")
    for (fn in names(result)) {
      output_path  <- fs::path(output_path_1, forecast_date, ahead, response, geo_type, incidence_period, n_locations, fn)
      if (!fs::dir_exists(output_path)) fs::dir_create(output_path, recurse = TRUE)
      output_file_path  <- fs::path(output_path, "out.RDS")
      current_result  <- result[[fn]]
      saveRDS(current_result, file = output_file_path)
    }
  }
}

logger::log_info("Producing scorecards")
for (forecast_date in forecast_dates) {
  for (ahead in 1:4) {
    response <- responses[[geo_type]]
    
    logger::log_info("Making scorecards")
    folders = Sys.glob(fs::path(output_path_1,forecast_date, ahead, response, geo_type, incidence_period, n_locations,"*"))
    for (f in folders) {
      print(f)
      try({
        prediction_card = readRDS(fs::path(f,"out.RDS"))
        scorecard = evalforecast::evaluate_quantile_predictions_cards(data_list[[geo_type]],list(prediction_card))
        saveRDS(scorecard, file = fs::path(f, "scorecard.RDS"))
      })
    }
  }
}
