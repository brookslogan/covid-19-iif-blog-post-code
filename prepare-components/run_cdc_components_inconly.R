library(logger)
library(sessioninfo)
library(fs)
library(readr)
library(stringr)
source("cdc_competitor_forecaster.R")
devtools::load_all("../evalforecast")
devtools::load_all("../../../covidData")

aheads = 1:4
config_tbl = tibble::tribble(~geo_type, ~response, "state", "jhu-csse_deaths_incidence_num")

analysis_date = as.Date("2020-12-07")

data_list = list(
  "state" =
    dplyr::bind_rows(
             covidData::load_jhu_data(analysis_date, measure="deaths", spatial_resolution="state", temporal_resolution="weekly") %>%
             dplyr::left_join(covidData::fips_codes[c("location","abbreviation")], by="location") %>%
             dplyr::transmute(location,
                              location_name=abbreviation,
                              reference_date=date,
                              issue_date=as.Date(NA),
                              variable_name="jhu-csse_deaths_incidence_num",
                              value=inc),
             covidData::load_jhu_data(analysis_date, measure="cases", spatial_resolution="state", temporal_resolution="weekly") %>%
             dplyr::left_join(covidData::fips_codes[c("location","abbreviation")], by="location") %>%
             dplyr::transmute(location,
                              location_name=abbreviation,
                              reference_date=date,
                              issue_date=as.Date(NA),
                              variable_name="jhu-csse_confirmed_incidence_num",
                              value=inc)
           )
)

incidence_period <- "epiweek"
n_locations  <- 200
## forecast_dates = as.character(lubridate::ymd("2020-04-06") + 7*seq(0,19))
forecast_dates = as.character(lubridate::ymd("2020-04-06") + 7*seq(0,27))
forecaster_dirs = Sys.glob("../../../covid19-forecast-hub/data-processed/*/")
forecaster_dirs = str_sub(forecaster_dirs,end=-2)
output_path_1 = "../../covid-19-iif-blog-post-data/smallcards/historical_cdc_components_inconly"
geo_type = "state"

for (config_row_i in seq_len(nrow(config_tbl))) {
  geo_type = config_tbl[[config_row_i, "geo_type"]]
  response = config_tbl[[config_row_i, "response"]]
  for (ahead in aheads) {
    for (forecast_date in forecast_dates) {
      forecast_date = lubridate::ymd(forecast_date)
      forecasters = purrr::map(forecaster_dirs, function(dir) { make_cdc_competitor_forecaster(dir, response=response, ahead=ahead, fall_back_to_inferred_incidence=FALSE)})
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
}

logger::log_info("Producing scorecards")
for (config_row_i in seq_len(nrow(config_tbl))) {
  geo_type = config_tbl[[config_row_i, "geo_type"]]
  response = config_tbl[[config_row_i, "response"]]
  for (ahead in aheads) {
    for (forecast_date in forecast_dates) {
      logger::log_info("Making scorecards")
      folders = Sys.glob(fs::path(output_path_1, forecast_date, ahead, response, geo_type, incidence_period, n_locations,"*"))
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
}
