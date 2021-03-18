library(covidData)
library(covidEnsembles)
library(tidyverse)
library(gridExtra)

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


weights_deaths <- purrr::map_dfr(
  c(4, "full_history"),
  function(window_size) {
    get_weights(root_dir = "../covid-19-iif-blog-post-data/post2/retrospective-weights/",
      estimation_level = "state",
      quantile_groups = "3_groups",
      window_size = window_size,
      target_variable = "inc_death",
      check_missingness_by_target = TRUE) %>%
      mutate(model = reorder(model, -weight, FUN = sum), window_size = window_size) %>%
      left_join(covidHubUtils::hub_locations, by = c("location" = "fips"))
  }
) %>%
  dplyr::mutate(
    window_size = dplyr::case_when(
      window_size == 4 ~ "4 Weeks",
      window_size == "full_history" ~ "Full History"
    ),
    measure = "Deaths"
  )

weights_cases <- purrr::map_dfr(
  c(4, "full_history"),
  function(window_size) {
    get_weights(root_dir = "../covid-19-iif-blog-post-data/post2/retrospective-weights/",
      estimation_level = "state",
      quantile_groups = "3_groups",
      window_size = window_size,
      target_variable = "inc_case",
      check_missingness_by_target = TRUE) %>%
      mutate(model = reorder(model, -weight, FUN = sum), window_size = window_size) %>%
      left_join(covidHubUtils::hub_locations, by = c("location" = "fips"))
  }
) %>%
  dplyr::mutate(
    window_size = dplyr::case_when(
      window_size == 4 ~ "4 Weeks",
      window_size == "full_history" ~ "Full History"
    ),
    measure = "Cases"
  )

weights <- dplyr::bind_rows(weights_cases, weights_deaths)

weights_to_plot <- weights %>%
  dplyr::filter(
    location == "01",
    quantile == .5) %>%#,
#    weight >= 0.01,
#    measure == "Deaths") %>%
  dplyr::mutate(
    window_size = factor(window_size, levels = c("4 Weeks", "Full History")))

weights_to_plot <- weights_to_plot %>%
  dplyr::mutate(
    model = factor(model,
      levels = weights_to_plot %>%
        group_by(model) %>%
        summarize(weight = max(weight)) %>%
        pull(model))
  )

p <- ggplot(data = weights_to_plot %>% dplyr::mutate(measure = paste("Incident", measure))) +
    geom_bar(
      mapping = aes(x = as.Date(forecast_date), y = weight, fill = model),
      color = "black",
      stat = "identity"
    ) +
  scale_color_discrete("Model") +
  facet_grid(window_size ~ measure) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  xlab("Forecast Date") +
  ylab("Model Weight") +
  theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1))

# png("../covid-19-iif-blog-post-data/post2/figures/weights_by_window_and_time.png", width = 8, height = 5, units = "in", res = 600)
# p
# dev.off()

png("../covid-19-iif-blog-post-data/post2/figures/weights_by_window_and_time_no_legend.png", width = 8, height = 5, units = "in", res = 600)
p
dev.off()

# top-weighted models for figure caption
max_weekly_weights <- weights_deaths %>%
  dplyr::filter(window_size == "Full History", location == "01", quantile == 0.5) %>%
  dplyr::group_by(model) %>%
  dplyr::slice_max(weight) %>%
  dplyr::arrange(desc(weight)) %>%
  head(5)

paste(max_weekly_weights$model, round(max_weekly_weights$weight, 3), sep = " (", collapse = "); ")

max_weekly_weights <- weights_cases %>%
  dplyr::filter(window_size == "Full History", location == "01", quantile == 0.5) %>%
  dplyr::group_by(model) %>%
  dplyr::slice_max(weight) %>%
  dplyr::arrange(desc(weight)) %>%
  head(5)

paste(max_weekly_weights$model, round(max_weekly_weights$weight, 3), sep = " (", collapse = "); ")
