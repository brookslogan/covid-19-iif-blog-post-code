library(glmnet)
library(quantgen)
library(dplyr)

devtools::load_all("../evalforecast")
devtools::load_all("../lemur")
devtools::load_all("../../../covidData")

analysis_date = as.Date("2020-12-07")

data_list = list(
  state = covidData::load_jhu_data(analysis_date) %>%
    dplyr::left_join(covidData::fips_codes[c("location","abbreviation")], by="location") %>%
    dplyr::transmute(location,
                     location_name=abbreviation,
                     reference_date=date,
                     issue_date=as.Date(NA),
                     variable_name="jhu-csse_deaths_incidence_num",
                     value=inc)
)

## these variables specify the forecasting task:
## responses <-  list(county = "usafacts_deaths_incidence_num", state = "jhu-csse_deaths_incidence_num")
## responses <-  list(county = "usa-facts_deaths_incidence_num", state = "jhu-csse_deaths_incidence_num")
responses <-  list(state = "jhu-csse_deaths_incidence_num")

output_path_1  <- "../../covid-19-iif-blog-post-data/smallcards/subset_ensembles"

incidence_period <- "epiweek"
n_locations <- 200

for (subset_size in c(4L,8L,16L)) {
  n_subsets = 30L
  inconly_components_dirpath = file.path("..", "..", "covid-19-iif-blog-post-data", "smallcards", "historical_cdc_components_inconly")
  inconly_forecasters = get_hub_components_names_helper(inconly_components_dirpath)
  inconly_forecasters <- setdiff(inconly_forecasters, "COVIDhub-ensemble")
  set.seed(3295848L)

  for (subset_i in seq_len(n_subsets)) {
    cat(sprintf("SUBSET %d FOR SIZE %d\n", subset_i, subset_size))
    subset_forecasters = sample(inconly_forecasters, subset_size)
    for (ahead in 1:4) {
      for (geo_type in names(responses)) {
        response <- responses[[geo_type]]
        ensemble_forecasters = lemur::get_dev_forecasters(response=response, incidence_period=incidence_period, ahead=ahead,
                                                          forecast_date=as.Date(NA),
                                                          geo_type=geo_type,
                                                          n_locations=n_locations,
                                                          repo_root_dirpath = "../../covid-19-iif-blog-post-data",
                                                          inconly_forecasters_override=subset_forecasters,
                                                          debug_weights_folder_suffix=NULL)
        ensemble_forecasters <- ensemble_forecasters[c("qspace_ew_md_ens_v1_inconly","qspace_ew_mean_ens_v1_inconly","ensemble3_cdc_inconly")]
        forecast_dates = lubridate::ymd("2020-11-23") - (7L * seq.int(ahead + 1L, 32L))
        cat(paste0("Forecasting dates ",toString(forecast_dates), " for ahead ",ahead,"\n"))
        for (forecast_date_i in seq_along(forecast_dates)) {
          forecast_date = forecast_dates[[forecast_date_i]]
          cat(paste0("Forecasting date ",forecast_date, " for ahead ",ahead,"\n"))
          forecast_issue = forecast_date - 1L # consider only "finalized" issues
          if (forecast_issue >= max(covidData::jhu_deaths_data[["issue_date"]])) {
            stop ('`forecast_issue` lies beyond issues in loaded version of covidData or is the last issue and might be subject to revision in unseen commits to the data repository')
          }
          stopifnot(response=="jhu-csse_deaths_incidence_num")
          observations_tbl = covidData::load_jhu_data(forecast_issue) %>% transmute(location,date,value=inc)
          for (ensemble_forecaster_i in seq_along(ensemble_forecasters)) {
            ensemble_forecaster = ensemble_forecasters[[ensemble_forecaster_i]]
            ensemble_forecaster_name = names(ensemble_forecasters)[[ensemble_forecaster_i]]
            cat(sprintf("GENERATING FORECASTS FOR FORECASTER %s...\n", ensemble_forecaster_name))

            output_path  <- fs::path(output_path_1, subset_size, subset_i, forecast_date, ahead, response, geo_type, incidence_period, n_locations, ensemble_forecaster_name)
            try({
              if (!file.exists(fs::path(output_path, "scorecard.RDS"))) {
                score_card = evalforecast::evaluate_quantile_forecaster(data_list[[geo_type]],
                                                                        function(df, forecast_date) ensemble_forecaster[["forecaster"]](df, forecast_date, observations_tbl=observations_tbl),
                                                                        response, incidence_period, ahead, geo_type,
                                                                        n_locations = n_locations, forecast_dates = forecast_date,
                                                                        backfill_buffer = 7)
                if (!fs::dir_exists(output_path)) fs::dir_create(output_path, recurse = TRUE)
                saveRDS(score_card, file = fs::path(output_path, "scorecard.RDS"))
              }
            })
          }
        }
      }
    }
  }
}
