library(glmnet)
library(quantgen)
library(dplyr)

devtools::load_all("../evalforecast")
devtools::load_all("../quantileEnsemble")
## devtools::load_all("../../covidcast/R-packages/covidcast")

data_list = list(
  state = readRDS("../../covid-19-iif-blog-post-data/signals/upstream_df_state_2020-10-11_final_corrected_trimmed.RDS")
)

## these variables specify the forecasting task:
## responses <-  list(county = "usafacts_deaths_incidence_num", state = "jhu-csse_deaths_incidence_num")
## responses <-  list(county = "usa-facts_deaths_incidence_num", state = "jhu-csse_deaths_incidence_num")
responses <-  list(state = "jhu-csse_deaths_incidence_num")

output_path_1  <- "../../covid-19-iif-blog-post-data/largecards"

incidence_period <- "epiweek"
n_locations <- 200
for (ahead in 1:4) {
  for (geo_type in names(responses)) {
    response <- responses[[geo_type]]
    ensemble_forecasters = quantileEnsemble::get_dev_forecasters(response=response, incidence_period=incidence_period, ahead=ahead,
                                                                 forecast_date=as.Date(NA),
                                                                 geo_type=geo_type,
                                                                 n_locations=n_locations,
                                                                 repo_root_dirpath = "../../covid-19-iif-blog-post-data")
    ensemble_forecasters <- ensemble_forecasters[grepl("(ensemble3.*inconly)|(qspace.*inconly)|(ensemble3.*all)|(qspace.*all)", names(ensemble_forecasters))]
    ## ensemble_forecasters <- ensemble_forecasters[grepl("qspace_ew_md_ens_v1_all", names(ensemble_forecasters))]

    for (ensemble_forecaster_i in seq_along(ensemble_forecasters)) {
      ensemble_forecaster = ensemble_forecasters[[ensemble_forecaster_i]]
      ensemble_forecaster_name = names(ensemble_forecasters)[[ensemble_forecaster_i]]
      cat(sprintf("GENERATING FORECASTS FOR FORECASTER %s...\n", ensemble_forecaster_name))

      ## try({
        forecast_dates = lubridate::ymd("2020-10-05") - (7L * seq.int(ahead + 1L, 25L))
        cat(paste0("Forecasting dates ",toString(forecast_dates), " for ahead ",ahead,"\n"))
        output_path  <- fs::path(output_path_1, ahead, response, geo_type, incidence_period, n_locations, ensemble_forecaster_name)
        if (!file.exists(fs::path(output_path, "scorecard.RDS"))) {
          score_card = evalforecast::evaluate_quantile_forecaster(data_list[[geo_type]],
                                                                  ensemble_forecaster[["forecaster"]],
                                                                  response, incidence_period, ahead, geo_type,
                                                                  n_locations = n_locations, forecast_dates = forecast_dates,
                                                                  backfill_buffer = 7)
          if (!fs::dir_exists(output_path)) fs::dir_create(output_path, recurse = TRUE)
          saveRDS(score_card, file = fs::path(output_path, "scorecard.RDS"))
        }
      ## })
    }
  }
}
