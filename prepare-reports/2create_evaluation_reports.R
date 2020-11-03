# location scorecards and get a useful data frame with their info:
library(tidyverse)
library(lubridate)
devtools::load_all("../../forecaster_pkgs/pipeline_pkgs/evalforecast")

scorecard_meta <- locate_cards("ensemble_evaluation",
                               dir_order = c("ahead", 
                                             "response", 
                                             "geo_type", 
                                             "incidence_period",
                                             "n_locations",
                                             "forecaster"))
ensemble_scorecard_meta <- locate_cards("../ensemble_scorecards",
                                        dir_order = c("ahead", 
                                                      "response", 
                                                      "geo_type", 
                                                      "incidence_period",
                                                      "n_locations",
                                                      "forecaster"))
scorecard_meta <- bind_rows(ensemble_scorecard_meta, scorecard_meta)
## data_paths <- paste0("../../../",
##                      c("state_level_upstream_df/upstream_df_state_2020-06-29_final.RData",
##                        "county_level_upstream_df/upstream_df_county_2020-06-29_final.Rdata"))
data_paths <- paste0(
  "../../../covidcast-forecast-scratch/trimmed-07-27/",
  c(
    "upstream_df_state_2020-07-27_final_trimmed.Rdata",
    "upstream_df_county_2020-07-27_final_trimmed.Rdata"
  )
)
eval_report_params <- scorecard_meta %>% 
  group_by(ahead, response, geo_type, incidence_period, n_locations) %>% 
  summarize(scorecard_files = paste(filename, collapse = ";")) %>% 
  ungroup() %>% 
  mutate(path_to_data = if_else(geo_type == "state", data_paths[1], data_paths[2]))


eval_report_params %>%
  pwalk(function(path_to_data, scorecard_files, ahead, geo_type, ...) {
    rmarkdown::render("evaluation_report_compare.Rmd", 
                      output_file = paste0("evaluation_reports/", geo_type, ahead, ".pdf"),
                      params = list(
                        path_to_github = "../../../covid-19",
                        path_to_data = path_to_data,
                        scorecard_files = scorecard_files))
    })
