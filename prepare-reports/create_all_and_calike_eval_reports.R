# location scorecards and get a useful data frame with their info:
library(tidyverse)
library(lubridate)

devtools::load_all("../../../forecaster_pkgs/pipeline_pkgs/evalforecast")

path_to_github <- "../../../../covid-19"
## data_dir <- "../../../../"
## data_paths <- paste0(data_dir,
##                      c("state_level_upstream_df/upstream_df_state_2020-07-13_final.Rdata",
##                        "county_level_upstream_df/upstream_df_county_2020-07-13_final.Rdata"))
data_paths <- paste0(
  "../../../../covidcast-forecast-scratch/trimmed-07-27/",
  c(
    "upstream_df_state_2020-07-27_final_trimmed.Rdata",
    "upstream_df_county_2020-07-27_final_trimmed.Rdata"
  )
)
# get data frame with all the scorecard files and their meta info:
scorecard_meta <-
  dplyr::bind_rows(
      evalforecast::locate_cards("../../ensemble_scorecards",
          dir_order = c(
              "ahead",
              "response",
              "geo_type",
              "incidence_period",
              "n_locations",
              "forecaster"
          ),
          cards_type = "scorecard.RDS"
      ),
      evalforecast::locate_cards("../../evaluation/ensemble_evaluation",
          dir_order = c(
              "ahead",
              "response",
              "geo_type",
              "incidence_period",
              "n_locations",
              "forecaster"
          ),
          cards_type = "scorecard.RDS"
      )
  )
# let's say we only want reports for state level with ahead = 2 and ahead = 3:
scorecard_meta <- scorecard_meta %>% 
  filter(ahead %in% 1:4, geo_type == "state")

# maybe we want these displayed in a particular order:
scorecard_meta <- scorecard_meta %>% 
  mutate(forecaster = ordered(forecaster, 
                              levels = c("ensemble3_cdc_impute_calike",
                                         "COVIDhub-ensemble",
                                         "hayman_ensemble_v1_selectHub7",
                                         "YYG-ParamSearch",
                                         "COVIDhub-baseline",
                                         "ensemble3_cdc_impute"
                                         ))) %>% 
  filter(!is.na(forecaster)) %>%
  arrange(forecaster)

# let's shorten the name of the forecasters so it will look better on the report: 
levels(scorecard_meta$forecaster) <- dplyr::recode(levels(scorecard_meta$forecaster),
                                                   "hayman_ensemble_v1_selectHub2"="haymanV1S2",
                                                   "COVIDhub-ensemble"="CHens",
                                                   "COVIDhub-base" = "CHbase",
                                                   "hayman_ensemble_v1_selectHub4"="haymanV1S4",
                                                   "hayman_ensemble_v0_selectHub2"="haymanV0S2",
                                                   "hayman_ensemble_v0_selectHub4"="haymanV0S4",
                                                   "hayman_ensemble_v1_selectHub7"="haymanV1S7",
                                                   "ensemble3_cdc_impute"="qg3_cdc_impu",
                                                   "ensemble3_cdc_impute_calike" = "qg3_cdc_impu_calike"
                                                   )

# the evaluation report expects parameters that we will set up next:
eval_report_params <- scorecard_meta %>% 
  group_by(ahead, response, geo_type, incidence_period, n_locations) %>% 
  summarize(scorecard_files = paste(filename, collapse = ";"),
            forecaster_names = paste(forecaster, collapse = ";")) %>% 
  ungroup() %>% 
  mutate(path_to_data = if_else(geo_type == "state", data_paths[1], data_paths[2]))



# system("mkdir evaluation_reports")
eval_report_params %>%
  pwalk(function(path_to_data, scorecard_files, forecaster_names, ahead, geo_type, ...) {
    print(path_to_data)
    rmarkdown::render("eval_report_simplified.Rmd", 
                      output_file = paste0("evaluation_reports/", geo_type, ahead, ".pdf"),
                      params = list(
                        path_to_github = path_to_github,
                        path_to_data = path_to_data,
                        scorecard_files = scorecard_files,
                        forecaster_names = forecaster_names,
                        locations_considered = "ALL"))
    })

dir.create("evaluation_reports_top20")
eval_report_params %>%
    pwalk(function(path_to_data, scorecard_files, forecaster_names, ahead, geo_type, ...) {
        print(path_to_data)
        rmarkdown::render("eval_report_simplified.Rmd",
            output_file = paste0("evaluation_reports_top20/", geo_type, ahead, ".pdf"),
            params = list(
              path_to_github = path_to_github,
              path_to_data = path_to_data,
              scorecard_files = scorecard_files,
              forecaster_names = forecaster_names,
              locations_considered = "06;48;12;36;42;17;39;13;37;26;34;51;53;04;25;47;18;29;24;55"
            )
        )
    })
