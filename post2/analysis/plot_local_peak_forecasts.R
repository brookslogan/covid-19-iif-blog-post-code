library(dplyr)
library(ggplot2)
library(scales)
library(covidData)
library(covidHubUtils)

library(covidData)
library(covidEnsembles)
library(tidyverse)
library(gridExtra)
library(knitr)
library(DT)

get_weights <- function(root_dir, estimation_level, quantile_groups, window_size, target_variable, check_missingness_by_target) {
  require(tidyverse)
  path_name <- paste0("intercept_FALSE-combine_method_convex-missingness_impute-",
                      "quantile_groups_", quantile_groups,
                      "-window_size_", window_size,
                      "-check_missingness_by_target_", check_missingness_by_target,
                      "-do_standard_checks_FALSE-do_baseline_check_FALSE/")
  path_to_files <- file.path(root_dir, estimation_level, path_name)
  
  ## get files and datenames
  target_files <- list.files(path_to_files, pattern=paste(target_variable, "*"))
  nchar_target_var <- nchar(target_variable)+1
  dates_of_files <- substr(target_files, start=nchar_target_var+1, stop=nchar_target_var+10)
  
  ## get full filenames
  target_files_full_dir <- list.files(path_to_files, pattern=paste(target_variable, "*"), full.names=TRUE)
  
  ## read in first file
  all_weights <- purrr::map_dfr(target_files,
                                function(x) {
                                  target_file_full_dir <- file.path(path_to_files, x)
                                  nchar_target_var <- nchar(target_variable)+1
                                  date_of_file <- substr(x, start=nchar_target_var+1, stop=nchar_target_var+10)
                                  dat <- read_csv(target_file_full_dir, col_types = cols()) %>%
                                    mutate(forecast_date = date_of_file)
                                  return(dat)
                                })
  return(all_weights)
}


forecast_date <- "2021-01-11"

state_fips <- covidData::fips_codes %>%
    dplyr::filter(abbreviation == "OK") %>%
    dplyr::pull(location)

weights_cases <- get_weights(
  root_dir = "../covid-19-iif-blog-post-data/post2/retrospective-weights/",
  estimation_level = "state",
  quantile_groups = "3_groups",
  window_size = "full_history",
  target_variable = "inc_case",
  check_missingness_by_target = TRUE) %>%
  mutate(model = reorder(model, -weight, FUN = sum)) %>%
  left_join(covidHubUtils::hub_locations, by = c("location" = "fips"))

component_models <- weights_cases %>%
  dplyr::filter(quantile == 0.5, location == state_fips, forecast_date == UQ(forecast_date)) %>%
  dplyr::arrange(dplyr::desc(weight)) %>%
  head(2) %>%
  dplyr::pull(model) %>%
  as.character()

# how much weight were they given?
weights_cases %>%
  dplyr::filter(quantile == 0.5, location == state_fips, forecast_date == UQ(forecast_date)) %>%
  dplyr::arrange(dplyr::desc(weight)) %>%
  head(2) %>%
  dplyr::pull(weight) %>%
  sum()

component_forecasts <- covidHubUtils::load_latest_forecasts(
  models = component_models,
  last_forecast_date = forecast_date,
  forecast_date_window_size = 6,
  locations = state_fips,
  targets = paste0(1:4, " wk ahead inc case"),
  source = "local_hub_repo",
  hub_repo_path = "../../covid19-forecast-hub"
) %>%
  dplyr::mutate(
    model = dplyr::case_when(
      model == component_models[1] ~ "Component Forecaster 1",
      model == component_models[2] ~ "Component Forecaster 2",
      model == component_models[3] ~ "Component Forecaster 3"
    )
  )

median_ensemble_forecast <- covidHubUtils::load_forecast_files_repo(
  file_paths = "../covid-19-iif-blog-post-data/post2/retrospective-forecasts/state/intercept_FALSE-combine_method_median-missingness_by_location_group-quantile_groups_per_model-window_size_0-check_missingness_by_target_FALSE-do_standard_checks_FALSE-do_baseline_check_FALSE/inc_case-2021-01-11-intercept_FALSE-combine_method_median-missingness_by_location_group-quantile_groups_per_model-window_size_0-check_missingness_by_target_FALSE-do_standard_checks_FALSE-do_baseline_check_FALSE.csv"
) %>%
  dplyr::filter(location == state_fips) %>%
  dplyr::mutate(model = "Median")

trained_ensemble_forecast <- covidHubUtils::load_forecast_files_repo(
  file_paths = "../covid-19-iif-blog-post-data/post2/retrospective-forecasts/state/intercept_FALSE-combine_method_convex-missingness_impute-quantile_groups_3_groups-window_size_full_history-check_missingness_by_target_TRUE-do_standard_checks_FALSE-do_baseline_check_FALSE/inc_case-2021-01-11-intercept_FALSE-combine_method_convex-missingness_impute-quantile_groups_3_groups-window_size_full_history-check_missingness_by_target_TRUE-do_standard_checks_FALSE-do_baseline_check_FALSE.csv"
) %>%
  dplyr::filter(location == state_fips) %>%
  dplyr::mutate(model = "Trained")

combined_forecasts <- dplyr::bind_rows(
  component_forecasts,
  median_ensemble_forecast,
  trained_ensemble_forecast
)

plot_data_forecast <- pivot_forecasts_wider(
  forecast_data = combined_forecasts,
  quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975))

get_palette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(4, "Blues"))
blues = get_palette(9)
ribbon_colors <- blues[c(4, 6)]

truth <- #dplyr::bind_rows(
  covidData::load_data(as_of = "2021-02-15", measure = "cases") %>%
    dplyr::filter(location == state_fips) %>%
    dplyr::mutate(as_of = "2021-02-15")#,
  # covidData::load_data(as_of = "2021-02-22") %>%
  #   dplyr::filter(location == "39") %>%
  #   dplyr::mutate(as_of = "2021-02-22"))

p <-
  ggplot2::ggplot(data = plot_data_forecast, ggplot2::aes(x= target_end_date)) +
  ggplot2::geom_ribbon(data = plot_data_forecast %>%
              dplyr::filter(type == "quantile"),
            mapping = ggplot2::aes(ymin=lower, 
                                    ymax=upper,
                                    group = interaction(`Prediction Interval`, model, 
                                                        location, forecast_date),
                                    fill = interaction(`Prediction Interval`))) +
  ggplot2::scale_fill_manual(name = "Prediction Interval", values = ribbon_colors) +
  # plot point forecasts
  ggplot2::geom_line(data = plot_data_forecast %>%
              dplyr::filter(!is.na(point), point_type == "median quantile"),
            mapping = ggplot2::aes(x = target_end_date, 
                                    y = point, 
                                    group = interaction(model, location, forecast_date)),
            color = blues[9]) +
  ggplot2::geom_point(data = plot_data_forecast %>%
                dplyr::filter(!is.na(point), point_type == "median quantile"),
              mapping = ggplot2::aes(x = target_end_date, 
                                    y = point),
            color = blues[9]) +
  #truth
#  ggnewscale::new_scale_color() +
  ggplot2::geom_line(data = truth,
                      mapping = ggplot2::aes(x = date, 
                                            y = inc)) +
  ggplot2::scale_linetype("Data Version") +
  ggplot2::facet_wrap(~ model) +
  ggplot2::xlab("Date") +
  ggplot2::ylab("Weekly Cases") +
  scale_y_continuous(labels = comma) +
#  ggplot2::ggtitle("Forecasts of Incident Deaths in Ohio Generated Feb. 15, 2021") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

png("../covid-19-iif-blog-post-data/post2/figures/inc_case_forecasts_OK_2021_01_11.png", width = 8, height = 5, units = "in", res = 600)
p
dev.off()

