library(dplyr)
library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(ggnewscale)
library(scales)

all_scores <- readRDS(
  "../covid-19-iif-blog-post-data/post2/retrospective-scores/retrospective_scores.rds") %>%
  dplyr::filter(
    grepl("3_groups", model) |
    grepl("combine_method_ew", model) |
    grepl("combine_method_median", model) |
    grepl("prospective_selection", model)
  )

all_scores %>%
  dplyr::mutate(
    model_brief = dplyr::case_when(
      combine_method == "prospective_selection" ~ "prospective_selection",
      combine_method == "convex" ~ paste0("trained_window_size_", window_size),
      combine_method == "ew" ~ "mean",
      combine_method == "median" ~ "median"),
    model_display = dplyr::case_when(
      combine_method == "prospective_selection" ~ "prospective_selection",
      combine_method == "convex" ~ "trained",
      combine_method == "ew" ~ "mean",
      combine_method == "median" ~ "median"),
  ) %>%
  dplyr::group_by(target_variable, target_end_date, model_display, model_brief, window_size) %>%
  dplyr::summarize(wis = mean(wis)) %>%
  ggplot() +
    geom_line(mapping = aes(x = target_end_date, y = wis, color = model_display, alpha = model_display, size = model_display, group = model_brief)) +
    facet_wrap( ~ target_variable, scales = "free_y") +
    scale_size_manual(values = c("prospective_selection" = 1, "trained" = 0.5, "mean" = 1, "median" = 1)) +
    scale_alpha_manual(values = c("prospective_selection" = 1, "trained" = 0.25, "mean" = 1, "median" = 1)) +
    scale_color_manual(values = c("prospective_selection" = "orange", "trained" = "black", "mean" = "cornflowerblue", "median" = "purple")) +
    theme_bw()

get_palette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(4, "Blues"))
blues = get_palette(9)

all_scores %>%
  dplyr::mutate(
    model_brief = dplyr::case_when(
      combine_method == "prospective_selection" ~ "prospective_selection",
      combine_method == "convex" ~ paste0("weighted_window_size_", window_size),
      combine_method == "ew" ~ "mean",
      combine_method == "median" ~ "median"),
    model_display = dplyr::case_when(
      combine_method == "prospective_selection" ~ "prospective_selection",
      combine_method == "convex" ~ "weighted",
      combine_method == "ew" ~ "mean",
      combine_method == "median" ~ "median"),
  ) %>%
  dplyr::group_by(target_variable, target_end_date, model_display, model_brief, window_size) %>%
  dplyr::summarize(wis = mean(wis)) %>%
  dplyr::filter(model_display %in% c("weighted", "prospective_selection")) %>%
  ggplot() +
    geom_line(mapping = aes(x = target_end_date, y = wis, color = model_brief, size = model_display, group = model_brief)) +
    facet_wrap( ~ target_variable, scales = "free_y") +
    scale_size_manual(values = c("prospective_selection" = 1, "weighted" = 0.5, "mean" = 1, "median" = 1)) +
    scale_color_manual(
      values = c(
        "prospective_selection" = "orange",
        "weighted_window_size_3" = blues[1],
        "weighted_window_size_4" = blues[2],
        "weighted_window_size_5" = blues[3],
        "weighted_window_size_6" = blues[4],
        "weighted_window_size_7" = blues[5],
        "weighted_window_size_8" = blues[6],
        "weighted_window_size_9" = blues[7],
        "weighted_window_size_10" = blues[8],
        "weighted_window_size_full_history" = blues[9])) +
    theme_bw()



plot_data <- all_scores %>%
  dplyr::mutate(
    model_brief = dplyr::case_when(
      combine_method == "prospective_selection" ~ "prospective_selection",
      combine_method == "convex" ~ paste0("Trained, Window Size ", window_size),
      combine_method == "ew" ~ "Mean",
      combine_method == "median" ~ "Median"),
    model_brief = ifelse(
      model_brief == "Trained, Window Size full_history",
      "Trained, Full History",
      model_brief),
    model_display = dplyr::case_when(
      combine_method == "prospective_selection" ~ "prospective_selection",
      combine_method == "convex" ~ "weighted",
      combine_method == "ew" ~ "mean",
      combine_method == "median" ~ "median"),
    horizon = paste0("Forecast Horizon: ", horizon, ifelse(horizon == 1, " week", " weeks"))
  ) %>%
  dplyr::group_by(target_variable, forecast_date, target_end_date, horizon, model_display, model_brief, window_size) %>%
  dplyr::summarize(wis = mean(wis))

p_cases <- ggplot(
    data = plot_data %>%
      dplyr::filter(
        target_variable == "inc case",
        model_brief %in% c("Mean", "Median", "Trained, Window Size 4", "Trained, Full History")
      ) %>%
      dplyr::mutate(
        model_brief = factor(model_brief, levels = c("Mean", "Median", "Trained, Window Size 4", "Trained, Full History"))
      )) +
  geom_line(mapping = aes(x = target_end_date, y = wis, color = model_brief, group = model_brief)) +
  facet_wrap( ~ horizon) +
  scale_color_manual(
    "Model",
    values = c(
      "Median" = "black",
      "Mean" = "orange",
      "Trained, Window Size 4" = blues[6],
      "Trained, Full History" = blues[8])
  ) +
  ylab("Mean WIS") +
  xlab("Target End Date") +
  ggtitle("   (a) Incident Cases") +
  theme_bw()
p_cases

p_deaths <- ggplot(
    data = plot_data %>%
      dplyr::filter(
        target_variable == "inc death",
        model_brief %in% c("Mean", "Median", "Trained, Window Size 4", "Trained, Full History")
      ) %>%
      dplyr::mutate(
        model_brief = factor(model_brief, levels = c("Mean", "Median", "Trained, Window Size 4", "Trained, Full History"))
      )) +
  geom_line(mapping = aes(x = target_end_date, y = wis, color = model_brief, group = model_brief)) +
  facet_wrap( ~ horizon) +
  scale_color_manual(
    "Model",
    values = c(
      "Median" = "black",
      "Mean" = "orange",
      "Trained, Window Size 4" = blues[6],
      "Trained, Full History" = blues[8])
  ) +
  ylab("Mean WIS") +
  xlab("Target End Date") +
  ggtitle("   (b) Incident Deaths") +
  theme_bw()

p_deaths

legend <- ggpubr::get_legend(p_cases)
p_cases <- p_cases + theme(legend.position = "none")
p_deaths <- p_deaths + theme(legend.position = "none")

png("../covid-19-iif-blog-post-data/post2/figures/scores_by_week.png", width = 8, height = 8, units = "in", res = 600)

plot_layout <- grid.layout(
  nrow = 2, ncol = 2,
  widths = unit(c(1, 0.3), rep("null", 2)),
  heights = unit(c(1, 1), rep("null", 2)))

grid.newpage()
pushViewport(viewport(layout = plot_layout))

print(p_cases, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p_deaths, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(as_ggplot(legend), vp = viewport(layout.pos.row = 1:2, layout.pos.col = 2))

dev.off()


plot_data <- dplyr::bind_rows(
  covidData::load_data(
    as_of = "2021-02-14",
    spatial_resolution = "national",
    temporal_resolution = "weekly",
    measure = "cases") %>%
    dplyr::transmute(
      forecast_date = date,
      target_end_date = date,
      value = inc,
      quantity = "National Data",
      model_brief = "Reported Incidence",
      target_variable = "inc case"
    ),
  covidData::load_data(
    as_of = "2021-02-14",
    spatial_resolution = "national",
    temporal_resolution = "weekly",
    measure = "deaths") %>%
    dplyr::transmute(
      forecast_date = date,
      target_end_date = date,
      value = inc,
      quantity = "National Data",
      model_brief = "Reported Incidence",
      target_variable = "inc death"
    ),
  all_scores %>%
    dplyr::mutate(
      model_brief = dplyr::case_when(
        combine_method == "prospective_selection" ~ "prospective_selection",
        combine_method == "convex" ~ paste0("Trained, Window Size ", window_size),
        combine_method == "ew" ~ "Mean",
        combine_method == "median" ~ "Median"),
      model_brief = ifelse(
        model_brief == "Trained, Window Size full_history",
        "Trained, Full History",
        model_brief),
      model_display = dplyr::case_when(
        combine_method == "prospective_selection" ~ "prospective_selection",
        combine_method == "convex" ~ "weighted",
        combine_method == "ew" ~ "mean",
        combine_method == "median" ~ "median"),
      quantity = paste0("Forecast Horizon: ", horizon, ifelse(horizon == 1, " week", " weeks"))
    ) %>%
    dplyr::filter(
      model_brief %in% c("Mean", "Median", "Trained, Window Size 4", "Trained, Full History", "Observed Data")
    ) %>%
    dplyr::group_by(target_variable, forecast_date, target_end_date, quantity, model_brief) %>%
    dplyr::summarize(wis = mean(wis)) %>%
    dplyr::transmute(
      forecast_date, target_end_date, value = wis, model_brief, quantity
    )
)

plot_data <- plot_data %>%
  dplyr::mutate(
    quantity = factor(quantity,
      levels = c("National Data", paste0("Forecast Horizon: ", 1:4, ifelse(1:4 == 1, " week", " weeks")))
    )
  )

p_cases <- ggplot() +
  geom_line(
    data = plot_data %>%
      dplyr::filter(
        target_variable == "inc case", model_brief == "Reported Incidence", forecast_date >= "2020-06-22"
      ), # %>%
      # dplyr::mutate(
      #   model_brief = factor(model_brief, levels = c("Mean", "Median", "Weighted, Window Size 4", "Weighted, Full History"))
      # ),
    mapping = aes(x = forecast_date, y = value, color = model_brief, group = model_brief)) +
  scale_color_manual("National Data", values = "black") +
  ggnewscale::new_scale_color() +
  geom_line(
    data = plot_data %>%
      dplyr::filter(
        target_variable == "inc case", forecast_date >= "2020-06-22"
      ) %>%
      dplyr::mutate(
        model_brief = factor(model_brief, levels = c("Mean", "Median", "Trained, Window Size 4", "Trained, Full History"))
      ),
    mapping = aes(x = forecast_date, y = value, color = model_brief, group = model_brief)) +
  facet_wrap( ~ quantity, scales = "free_y", ncol = 1) +
  scale_color_manual(
    "Model",
    values = c(
      "Median" = "black",
      "Mean" = "orange",
      "Trained, Window Size 4" = blues[6],
      "Trained, Full History" = blues[8])
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) +
  ylab("") +
  xlab("") +
  ggtitle("Cases") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(0, 0, -0.5, -0.25, "cm"),
    legend.position = "none")

p_deaths <- ggplot() +
  geom_line(
    data = plot_data %>%
      dplyr::filter(
        target_variable == "inc death", model_brief == "Reported Incidence", forecast_date >= "2020-06-22"
      ), # %>%
      # dplyr::mutate(
      #   model_brief = factor(model_brief, levels = c("Mean", "Median", "Weighted, Window Size 4", "Weighted, Full History"))
      # ),
    mapping = aes(x = forecast_date, y = value, color = model_brief, group = model_brief)) +
  scale_color_manual("National Data", values = "black") +
  ggnewscale::new_scale_color() +
  geom_line(
    data = plot_data %>%
      dplyr::filter(
        target_variable == "inc death", forecast_date >= "2020-06-22"
      ) %>%
      dplyr::mutate(
        model_brief = factor(model_brief, levels = c("Mean", "Median", "Trained, Window Size 4", "Trained, Full History"))
      ),
    mapping = aes(x = forecast_date, y = value, color = model_brief, group = model_brief)) +
  facet_wrap( ~ quantity, scales = "free_y", ncol = 1) +
  scale_color_manual(
    "Model",
    values = c(
      "Median" = "black",
      "Mean" = "orange",
      "Trained, Window Size 4" = blues[6],
      "Trained, Full History" = blues[8])
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) +
  ylab("") +
  xlab("") +
  ggtitle("Deaths") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(0, 0, -0.5, -0.25, "cm"),
    legend.position = "none")

p_data_temp <- ggplot() +
  geom_line(
    data = plot_data %>%
      dplyr::filter(
        target_variable == "inc death", model_brief == "Reported Incidence", forecast_date >= "2020-06-22"
      ), # %>%
      # dplyr::mutate(
      #   model_brief = factor(model_brief, levels = c("Mean", "Median", "Weighted, Window Size 4", "Weighted, Full History"))
      # ),
    mapping = aes(x = forecast_date, y = value, color = model_brief, group = model_brief)) +
  scale_color_manual("National Data", values = "black") +
  theme_bw()
legend_data <- ggpubr::get_legend(p_data_temp)

p_wis_temp <- ggplot() +
  geom_line(
    data = plot_data %>%
      dplyr::filter(
        target_variable == "inc death", forecast_date >= "2020-06-22", model_brief %in% c("Mean", "Median", "Trained, Window Size 4", "Trained, Full History")
      ) %>%
      dplyr::mutate(
        model_brief = factor(model_brief, levels = c("Mean", "Median", "Trained, Window Size 4", "Trained, Full History"))
      ),
    mapping = aes(x = forecast_date, y = value, color = model_brief, group = model_brief)) +
  scale_color_manual(
    "Ensemble Model:",
    values = c(
      "Median" = "black",
      "Mean" = "orange",
      "Trained, Window Size 4" = blues[6],
      "Trained, Full History" = blues[8])
  ) +
  theme_bw()
# legend_wis <- ggpubr::get_legend(p_wis_temp)
legend_wis_horiz <- ggpubr::get_legend(p_wis_temp, position = "bottom")

# png("../covid-19-iif-blog-post-data/post2/figures/scores_by_week_with_data.png", width = 8, height = 8, units = "in", res = 600)

# plot_layout <- grid.layout(
#   nrow = 7, ncol = 4,
#   widths = unit(c(1, 0.925, 0.9, 0.6), c("lines", rep("null", 3))),
#   heights = unit(c(1.5, rep(1, 5), 1.5), c("lines", rep("null", 5), "lines")))

# grid.newpage()
# pushViewport(viewport(layout = plot_layout))

# print(as_ggplot(legend_data), vp = viewport(layout.pos.row = 2, layout.pos.col = 4))
# print(as_ggplot(legend_wis), vp = viewport(layout.pos.row = 3:6, layout.pos.col = 4))
# print(p_cases, vp = viewport(layout.pos.row = 1:6, layout.pos.col = 2))
# print(p_deaths, vp = viewport(layout.pos.row = 1:6, layout.pos.col = 3))
# grid.text("Forecast Creation Date",
#   just = "center",
#   gp = gpar(fontsize = 11),
#   vp = viewport(layout.pos.row = 7, layout.pos.col = 2:3))
# grid.text("Incidence",
#   just = "center",
#   rot = 90,
#   gp = gpar(fontsize = 11),
#   vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
# grid.text("Mean WIS",
#   just = "center",
#   rot = 90,
#   gp = gpar(fontsize = 11),
#   vp = viewport(layout.pos.row = 3:6, layout.pos.col = 1))

# dev.off()



png("../covid-19-iif-blog-post-data/post2/figures/scores_by_week_with_data_legend_bottom.png", width = 8, height = 8, units = "in", res = 600)
plot_layout <- grid.layout(
  nrow = 8, ncol = 3,
  widths = unit(c(2, 0.925, 0.9), c("lines", rep("null", 3))),
  heights = unit(c(1.5, rep(1, 5), 1.5, 3), c("lines", rep("null", 5), "lines", "lines")))

grid.newpage()
pushViewport(viewport(layout = plot_layout))

print(as_ggplot(legend_wis_horiz), vp = viewport(layout.pos.row = 8, layout.pos.col = 2:3))
print(p_cases, vp = viewport(layout.pos.row = 1:6, layout.pos.col = 2))
print(p_deaths, vp = viewport(layout.pos.row = 1:6, layout.pos.col = 3))
grid.text("Forecast Creation Date",
  just = "center",
  gp = gpar(fontsize = 11),
  vp = viewport(layout.pos.row = 7, layout.pos.col = 2:3))
grid.text("    Weekly\n    Cases or Deaths",
  just = "center",
  rot = 90,
  gp = gpar(fontsize = 11),
  vp = viewport(layout.pos.row = 2, layout.pos.col = 1))

print(
  ggplot() +
    geom_line(
      data = data.frame(x = c(1, 1), y = c(0.095, 0.97)),
      mapping = aes(x = x, y = y)) +
    xlim(0, 1) +
    scale_y_continuous(limits = c(0, 1), expand = expansion(0, 0)) +
    theme_void(),
  vp = viewport(layout.pos.row = 3:6, layout.pos.col = 1)
)
grid.text("                 Mean WIS",
  just = "center",
  rot = 90,
  gp = gpar(fontsize = 11),
  vp = viewport(layout.pos.row = 3:6, layout.pos.col = 1))

dev.off()
