library(dplyr)
library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(ggnewscale)

all_scores <- readRDS(
  "../covid-19-iif-blog-post-data/post2/retrospective-scores/retrospective_scores.rds") %>%
  dplyr::filter(
    grepl("3_groups", model) |
    grepl("combine_method_ew", model) |
    grepl("combine_method_median", model) |
    grepl("prospective_selection", model)
  )

get_palette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(4, "Blues"))
blues = get_palette(9)

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
      quantity = paste0("Forecast Horizon: ", horizon, ifelse(horizon == 1, " week", " weeks")),
      target_variable = dplyr::case_when(
        target_variable == "inc case" ~ "Incident Cases",
        target_variable == "inc death" ~ "Incident Deaths",
      )
    ) %>%
    dplyr::filter(
      model_brief %in% c("Mean", "Median", "Trained, Window Size 4", "Trained, Full History", "Observed Data")
    )

png("../covid-19-iif-blog-post-data/post2/figures/scores_boxplot.png", width = 8, height = 4, units = "in", res = 600)
p <- ggplot() +
  geom_boxplot(
    data = plot_data %>%
      dplyr::group_by(target_variable, model_brief, forecast_date) %>%
      dplyr::summarize(wis = mean(wis)),
    mapping = aes(x = model_brief, y = wis)) +
  geom_point(
    data = plot_data %>%
      dplyr::group_by(target_variable, model_brief) %>%
      dplyr::summarize(wis = mean(wis)),
    mapping = aes(x = model_brief, y = wis),
    shape = "+",
    size = 7) +
  facet_wrap( ~ target_variable, scales = "free_y") +
  scale_y_log10() +
  xlab("Ensemble Model") +
  ylab("WIS (log scale)") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p)
dev.off()
