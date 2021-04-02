library(dplyr)
library(ggplot2)
library(scales)
library(covidData)
library(covidHubUtils)

component_forecasts <- covidHubUtils::load_latest_forecasts(
  last_forecast_date = "2021-02-15",
  forecast_date_window_size = 6,
  locations = covidData::fips_codes %>%
    dplyr::filter(abbreviation == "OH") %>%
    dplyr::pull(location),
  targets = paste0(1:4, " wk ahead inc death"),
  source = "local_hub_repo",
  hub_repo_path = "../../covid19-forecast-hub"
)

trained_ensemble_forecast <- covidHubUtils::load_forecast_files_repo(
  file_paths = "../covid-19-iif-blog-post-data/post2/retrospective-forecasts/state/prospective_selection/inc_death-2021-02-15-prospective_selection-include_full_history_FALSE.csv"
) %>%
  dplyr::filter(location == "39")

combined_forecasts <- dplyr::bind_rows(
  component_forecasts,
  trained_ensemble_forecast
)

plot_data_forecast <- pivot_forecasts_wider(
  forecast_data = combined_forecasts,
  quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975))

plot_data_forecast <- plot_data_forecast %>%
  dplyr::filter(model %in% c("epiforecasts-ensemble1", "Columbia_UNC-SurvCon",
    "prospective_selection-include_full_history_FALSE", "COVIDhub-ensemble")) %>%
  dplyr::mutate(
    model = dplyr::case_when(
      model == "epiforecasts-ensemble1" ~ "Component Forecaster 1",
      model == "Columbia_UNC-SurvCon" ~ "Component Forecaster 2",
      model == "prospective_selection-include_full_history_FALSE" ~ "Trained",
      model == "COVIDhub-ensemble" ~ "Median"
    )
  )

ribbon_colors <- RColorBrewer::brewer.pal(4, "Greys")[1:3]
ribbon_colors <- RColorBrewer::brewer.pal(4, "Greys")[3:4]
get_palette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(4, "Blues"))
blues = get_palette(9)
ribbon_colors <- blues[c(4, 6)]

truth <- dplyr::bind_rows(
  covidData::load_data(as_of = "2021-02-15") %>%
    dplyr::filter(location == "39") %>%
    dplyr::mutate(as_of = "2021-02-15"),
  covidData::load_data(as_of = "2021-02-22") %>%
    dplyr::filter(location == "39") %>%
    dplyr::mutate(as_of = "2021-02-22"))

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
                                            y = inc, 
                                            linetype = as_of)) +
  ggplot2::scale_linetype("Data Version") +
  ggplot2::geom_label(
    data = data.frame(
      text = LETTERS[1:4],
      model = c("Component Forecaster 1", "Component Forecaster 2", "Median", "Trained")),
    mapping = aes(label = text),
    x = -Inf, y = Inf,
    hjust = -0.5, vjust = 1.5,
    inherit.aes = FALSE
  ) +
  ggplot2::xlab("Date") +
  ggplot2::ylab("Weekly Deaths") +
  scale_y_continuous(labels = comma) +
#  ggplot2::ggtitle("Forecasts of Incident Deaths in Ohio Generated Feb. 15, 2021") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

png("../covid-19-iif-blog-post-data/post2/figures/inc_death_forecasts_Ohio_2021_02_15.png", width = 8, height = 5, units = "in", res = 600)
p +
  ggplot2::facet_wrap(~ model, scales = "free_y")
dev.off()

# png("../covid-19-iif-blog-post-data/post2/figures/inc_death_forecasts_Ohio_2021_02_15_log_scale_y.png", width = 8, height = 5, units = "in", res = 600)
# p +
#   ggplot2::facet_wrap(~ model) +
#   scale_y_log10()
# dev.off()


