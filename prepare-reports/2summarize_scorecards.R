
library(tidyverse)
library(lubridate)

devtools::load_all("../../forecaster_pkgs/pipeline_pkgs/evalforecast")

scorecard_meta <- locate_cards("ensemble_evaluation",
                               dir_order = c("ahead",
                                             "response",
                                             "geo_type",
                                             "incidence_period",
                                             "n_locations",
                                             "forecaster")) %>%
    dplyr::filter(
               ## grepl("cookies_and_cream|poiszero|zyzzyva", forecaster) |
               ## grepl("cookies_and_cream|poiszero|customized", forecaster) |
               ## grepl("cookies_and_cream|YYG", forecaster) |
               ## forecaster %in% c("ensemble3train_cdc_impute", "ensemble3_cdc_impute","YYG-ParamSearch","COVIDhub-baseline","COVIDhub-ensemble") |
               ## forecaster %in% c("ensemble3train_cdc_impute", "ensemble3_cdc_impute", "COVIDhub-baseline", "COVIDhub-ensemble") |
               ## forecaster %in% c("ensemble3_cdc_impute", "COVIDhub-baseline", "COVIDhub-ensemble") |
               ## forecaster %in% c("ensemble3_cdc_impute") |
               ## forecaster %in% c("ensemble3_cdc_impute","ensemble3train_cdc_impute","ensemble3_cdc_impute_calike") |
               ## forecaster %in% c("COVIDhub-baseline", "IHME-CurveFit", "LANL-GrowthRate", "MOBS-GLEAM_COVID", "OliverWyman-Navigator", "UMass-MechBayes", "UT-Mobility", "YYG-ParamSearch") |
               ## forecaster %in% c("ensemble3_cdc_impute","YYG-ParamSearch","COVIDhub-baseline","COVIDhub-ensemble") |
               ## forecaster %in% c("ensemble3_cdc_impute","YYG-ParamSearch","COVIDhub-ensemble") |
               ## forecaster %in% c("ensemble3_cdc_impute","YYG-ParamSearch","COVIDhub-baseline","COVIDhub-ensemble","LANL-GrowthRate","UMass-MechBayes","UCLA-SuEIR","UT-Mobility") |
               ## forecaster %in% c("YYG-ParamSearch","COVIDhub-baseline","COVIDhub-ensemble","LANL-GrowthRate","UMass-MechBayes","UCLA-SuEIR","UT-Mobility") |
               ## forecaster %in% c("ensemble1_cdc_impute","ensemble3_cdc_impute","YYG-ParamSearch","COVIDhub-baseline","COVIDhub-ensemble") |
               ## forecaster %in% c("ensemble1_cdc_impute","ensemble3_cdc_impute","ensemble1_cdc","ensemble3_cdc","YYG-ParamSearch","COVIDhub-baseline","COVIDhub-ensemble") |
               ## forecaster %in% c("ensemble1_cdc_all") |
               ## forecaster %in% c("COVIDhub-baseline","YYG-ParamSearch","MOBS-GLEAM_COVID","UT-Mobility","UMass-MechBayes","IHME-CurveFit") |
               ## forecaster %in% c("COVIDhub-baseline","YYG-ParamSearch","MOBS-GLEAM_COVID","UT-Mobility","UMass-MechBayes","IHME-CurveFit","JHU_IDD-CovidSP") |
               ## forecaster %in% c("COVIDhub-ensemble","COVIDhub-baseline","YYG-ParamSearch","MOBS-GLEAM_COVID","UT-Mobility","UMass-MechBayes","IHME-CurveFit","JHU_IDD-CovidSP") |
               ## forecaster %in% c("COVIDhub-baseline","YYG-ParamSearch","MOBS-GLEAM_COVID","UT-Mobility","UMass-MechBayes","LANL-GrowthRate","CovidActNow-SEIR_CAN","CU-select","CU-scenario_high","Imperial-ensemble2","Imperial-ensemble1","UCLA-SuEIR","CU-scenario_mid","CU-scenario_low") |
               forecaster %in% c("COVIDhub-ensemble","COVIDhub-baseline") |
               ## forecaster %in% get_hub_components_names() |
               ## forecaster %in% c("YYG-ParamSearch", "LANL-GrowthRate", "COVIDhub-ensemble", "UMass-MechBayes", "UCLA-SuEIR",
               ##                   "COVIDhub-baseline")
               ## forecaster %in% c("YYG-ParamSearch", "COVIDhub-ensemble", "COVIDhub-baseline")
               ## forecaster %in% c("YYG-ParamSearch", "COVIDhub-ensemble")
               ## forecaster %in% c("YYG-ParamSearch", "COVIDhub-baseline")
               ## forecaster %in% c("YYG-ParamSearch")
               FALSE
           ) %>%
    {.}
ensemble_scorecard_meta <-
  locate_cards("../ensemble_scorecards",
                                        dir_order = c("ahead",
                                                      "response",
                                                      "geo_type",
                                                      "incidence_period",
                                                      "n_locations",
                                                      "forecaster")) %>%
    dplyr::filter(
               ## TRUE
               ## grepl("cookies_and_cream|YYG", forecaster)
               ## grepl("selectHub2", forecaster)
               ## grepl("selectHub3", forecaster)
               ## grepl("selectHub4", forecaster)
               ## grepl("selectHub(2|4)", forecaster)
               ## grepl("selectHub(2|4|5|6)", forecaster) &
               ## grepl("selectHub6", forecaster) & grepl("hayman_ensemble_v0", forecaster) |
               ## grepl("selectHub7", forecaster) & grepl("hayman_ensemble_v0", forecaster) |
               ## grepl("calike", forecaster) |
             ## grepl("cdf_ens", forecaster) |
             ## grepl("selhub8", forecaster) |
             ## forecaster %in% c("cdf_ens_v0_selectHub6", "cdf_rwt2_ens_v0_selhub8", "cdf_rwt2imp_ens_v0_selhub8", "cdf_rwt2_ens_v0_selhub8_dup", "cdf_rwt2_ens_v0_selhub8_v1pre") |
             ## forecaster %in% c("hayman_ens_v1_selhub8","ensemble1_cdc_dup","ensemble3_cdc_dup","ensemble3train_cdc_dup","ensemble1_cdc_all_dup","ensemble3_cdc_all_dup","ensemble3train_cdc_all_dup","cdf_ens_v0_selectHub6", "cdf_rwt2_ens_v0_selhub8", "cdf_rwt2imp_ens_v0_selhub8", "cdf_rwt2_ens_v0_selhub8_dup", "cdf_rwt2_ens_v0_selhub8_v1pre","cdf_rwt2_ens_v0_selhub8_v1","ensemble3_comb4","hayman_ens_v1_comb4","cdf_rwt2_ens_v1_comb4","ensemble3_comb4_impute","cdf_rwt2_ens_v2_comb4","ensemble1_cdc_impute","ensemble3_cdc_impute","ensemble3train_cdc_impute") |
             ## forecaster %in% c("hayman_ens_v1_selhub8","ensemble1_cdc_all_dup","ensemble3_cdc_all_dup","ensemble3train_cdc_all_dup","cdf_ens_v0_selectHub6", "cdf_rwt2_ens_v0_selhub8", "cdf_rwt2imp_ens_v0_selhub8", "cdf_rwt2_ens_v0_selhub8_dup", "cdf_rwt2_ens_v0_selhub8_v1pre","cdf_rwt2_ens_v0_selhub8_v1","ensemble1_cdc_impute","ensemble3_cdc_impute","ensemble3train_cdc_impute") |
             ## forecaster %in% c("hayman_ens_v1_selhub8","ensemble1_cdc_all_dup","ensemble3_cdc_all_dup","ensemble3train_cdc_all_dup","cdf_rwt2imp_ens_v0_selhub8","ensemble1_cdc_impute","ensemble3_cdc_impute","ensemble3train_cdc_impute") |
             ## forecaster %in% c("hayman_ens_v1_selhub8","ensemble1_cdc_all_dup","ensemble3_cdc_all_dup","ensemble3train_cdc_all_dup","cdf_rwt2imp_ens_v0_selhub8","ensemble1_cdc_impute","ensemble3_cdc_impute","ensemble3train_cdc_impute","cdf_rwt2_ens_v0_selhub8","cdf_ens_v0_selectHub6","cdf_raw_ens_v0_selhub8") |
             forecaster %in% c("hayman_ens_v1_selhub8","ensemble1_cdc_all_dup","ensemble3_cdc_all_dup","ensemble3train_cdc_all_dup","cdf_rwt2imp_ens_v0_selhub8","ensemble1_cdc_impute","ensemble3_cdc_impute","ensemble3train_cdc_impute","cdf_rwt2_ens_v0_selhub8","cdf_raw_ens_v0_selhub8","cdf_rwt2_ens_v0_selhub8_v1pre","cdf_rwt2_ens_v0_selhub8_v1","ensemble3_comb4_impute") |
             FALSE
           )

forecaster.colors = c(
  "COVIDhub-baseline"             = "#003300",
  "COVIDhub-ensemble"             = "#336633",
  "hayman_ens_v1_selhub8"         = "#888888",
  "ensemble1_cdc_dup"             = "#444444",
  "ensemble3_cdc_dup"             = "#999944",
  "ensemble23_cdc_dup"            = "#EEEE44",
  "ensemble1_cdc_impute"          = "#004444",
  "ensemble3_cdc_impute"          = "#009944",
  "ensemble23_cdc_impute"         = "#00EE44",
  "ensemble3train_cdc_impute"     = "#009999",
  "ensemble3_comb4_impute"        = "#449944",
  "ensemble1_cdc_all_dup"         = "#AA00AA",
  "ensemble3_cdc_all_dup"         = "#CC00AA",
  "ensemble23_cdc_all_dup"        = "#FF00AA",
  "ensemble3train_cdc_all_dup"    = "#FF00FF",
  "cdf_raw_ens_v0_selhub8"        = "#440000",
  "cdf_rwt2_ens_v0_selhub8"       = "#770000",
  "cdf_rwt2_ens_v0_selhub8_v1pre" = "#990000",
  "cdf_rwt2_ens_v0_selhub8_v1"    = "#CC0000",
  "cdf_rwt2imp_ens_v0_selhub8"    = "#FF0000"
)
forecaster.labels = c(
  "COVIDhub-baseline"             = "CHbase",
  "COVIDhub-ensemble"             = "CHens",
  "hayman_ens_v1_selhub8"         = "hay_v1",
  "ensemble1_cdc_dup"             = "QG1",
  "ensemble3_cdc_dup"             = "QG3",
  "ensemble23_cdc_dup"            = "QG23",
  "ensemble1_cdc_impute"          = "QG1_imp",
  "ensemble3_cdc_impute"          = "QG3_imp",
  "ensemble23_cdc_impute"         = "QG23_imp",
  "ensemble3train_cdc_impute"     = "QG3only_imp",
  "ensemble3_comb4_impute"        = "QG3_comb4_imp",
  "ensemble1_cdc_all_dup"         = "QG1_all_imp",
  "ensemble3_cdc_all_dup"         = "QG3_all_imp",
  "ensemble23_cdc_all_dup"        = "QG23_all_imp",
  "ensemble3train_cdc_all_dup"    = "QG3only_all_imp",
  "cdf_raw_ens_v0_selhub8"        = "CDF_v0.0",
  "cdf_rwt2_ens_v0_selhub8"       = "CDF_v0.2",
  "cdf_rwt2_ens_v0_selhub8_v1pre" = "CDF_v1.2.0",
  "cdf_rwt2_ens_v0_selhub8_v1"    = "CDF_v1.2.1",
  "cdf_rwt2imp_ens_v0_selhub8"    = "CDF_v1.2.1_imp"
)

corrections =
    readr::read_csv("../../forecaster_code/pipeline_code/common_funs/data/corrections_state.csv")

median_k = 12L

validation_window_length = 4L

all_scorecards =
    dplyr::bind_rows(scorecard_meta, ensemble_scorecard_meta) %>%
    tidyr::pivot_wider(id_cols=c(ahead,response,geo_type,incidence_period,n_locations), names_from="forecaster", values_from="filename") %>%
    na.omit() %>%
    tidyr::pivot_longer(-c(ahead,response,geo_type,incidence_period,n_locations), names_to="forecaster", values_to="filename") %>%
    dplyr::mutate(scorecard=lapply(filename, readRDS)) %>%
    tidyr::unnest(scorecard) %>%
    dplyr::mutate(target_sunday = target_start - as.POSIXlt(target_start)$wday) %>%
    dplyr::left_join(corrections %>%
                     dplyr::mutate(is_corrected=TRUE) %>%
                     dplyr::mutate(location=as.character(location)) %>%
                     dplyr::mutate(value = as.numeric(value)) %>%
                     dplyr::mutate(target_sunday = reference_date - as.POSIXlt(reference_date)$wday),
                     by=c("location", "location_name", "target_sunday", "response"="variable_name")) %>%
    mutate(actual = dplyr::if_else(!is.na(is_corrected), value, actual),
           err = {
               if (any(!is.na(is_corrected) & is_corrected & !is.na(value))) stop ('cannot handle corrections to non-NA values')
               else dplyr::if_else(!is.na(is_corrected) & is_corrected, NA_real_, err)
           }) %>%
    dplyr::select(-reference_date, -issue_date, -value) %>%
    dplyr::filter(!is.na(actual)) %>%
    dplyr::group_by(ahead, response, geo_type, incidence_period, n_locations, forecaster, filename) %>%
    tidyr::nest() %>%
    dplyr::rename(scorecard=data) %>%
    ##
    dplyr::mutate(scorecard = lapply(scorecard, . %>% mutate(forecast_date=as.Date(forecast_date),
                                                      target_start=as.Date(target_start),
                                                      target_end=as.Date(target_end)
                                                      ))) %>%
    tidyr::unnest(scorecard) %>%
    dplyr::mutate(point_est = vapply(forecast_distribution, function(df) df[["quantiles"]][[median_k]], numeric(1L))) %>%
    dplyr::filter(!is.na(err)) %>%
    dplyr::mutate(ahead=as.integer(ahead)) %>%
    ## dplyr::filter(forecast_date >= max(forecast_date) - 7L*validation_window_length) %>%
    ## dplyr::filter(forecast_date <= max(forecast_date) - 7L*(validation_window_length+1L+ahead)) %>%
    dplyr::group_by(ahead,response,geo_type,incidence_period,n_locations,location,location_name,forecast_date) %>%
    dplyr::filter(., dplyr::n() == length(unique(.[["forecaster"]]))) %>%
    dplyr::ungroup() %>%
    {.}

scorecards = all_scorecards %>%
  ## dplyr::filter(location %in% strsplit("06;48;12;36;42;17;39;13;37;26;34;51;53;04;25;47;18;29;24;55",";")[[1L]]) %>%
  ## dplyr::filter(forecast_date <= as.Date("2020-06-22")) %>%
  ## dplyr::filter(forecast_date <= as.Date("2020-06-15")) %>%
  {.}

## scorecards %>%
##     dplyr::rowwise() %>%
##     dplyr::do(
##                forecaster = .[["forecaster"]],
##                mean_err = mean(.[["scorecard"]][["err"]]),
##                sd_err   = sd(.[["scorecard"]][["err"]])
##            ) %>%
##     tidyr::unnest(dplyr::everything()) %>%
##     ## dplyr::group_by(forecaster) %>%
##     ## dplyr::summarize(mean_err=mean(err)) %>%
##     {.}

## scorecards %>%
##     mutate(scorecard = lapply(scorecard, . %>% mutate(forecast_date=as.Date(forecast_date),
##                                                       target_start=as.Date(target_start),
##                                                       target_end=as.Date(target_end)
##                                                       ))) %>%
##     unnest(scorecard) %>%
##     dplyr::count(forecaster, forecast_date) %>%
##     dplyr::arrange(n) %>%
##     {print(head(.));print(tail(.));invisible(.)}

source("summarize_scorecards_helper.R")
