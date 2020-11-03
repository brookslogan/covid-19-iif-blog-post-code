
devtools::load_all("../../forecaster_pkgs/pipeline_pkgs/evalforecast")

for (eval_ahead in 1:4) {
  rmarkdown::render("mixed_models.Rmd",
                    params=list(
                      ## forecaster_names = "COVIDhub-ensemble;hayman_ens_v1_selhub8;qspace_ew_md_ens_v1_selhub8;ensemble3_cdc_all_dup_re;ensemble3_cdc_impute;cdf_rwt2imp_ens_v0_selhub8;cdf_rwt2_ens_v0_selhub8_v1",
                      ## forecaster_names = "qspace_ew_md_ens_v1_selhub8;COVIDhub-ensemble;hayman_ens_v1_selhub8;ensemble3_cdc_all_dup_re;ensemble3_cdc_impute;cdf_rwt2imp_ens_v0_selhub8;cdf_rwt2_ens_v0_selhub8_v1",
                      ## forecaster_names = "qspace_ew_md_ens_v1_all;qspace_ew_md_ens_v1_selhub8;hayman_ens_v1_selhub8;ensemble3_cdc_all_dup_re;ensemble3_cdc_impute;cdf_rwt2imp_ens_v0_selhub8;cdf_rwt2_ens_v0_selhub8_v1",
                      ## forecaster_names = "qspace_ew_md_ens_v1_inconly;qspace_ew_md_ens_v1_all;qspace_ew_md_ens_v1_selhub8;hayman_ens_v1_selhub8;ensemble3_cdc_all_dup_re;ensemble3_cdc_impute;cdf_rwt2imp_ens_v0_selhub8;cdf_rwt2_ens_v0_selhub8_v1",
                      forecaster_names = "qspace_ew_md_ens_v1_inconly;qspace_ew_md_ens_v1_all;qspace_ew_md_ens_v1_selhub8;hayman_ens_v1_selhub8;ensemble3_cdc_inconly;ensemble3_cdc_all_dup_re;ensemble3_cdc_impute;cdf_rwt2imp_ens_v0_selhub8;cdf_rwt2_ens_v0_selhub8_v1",
                      eval_ahead=as.character(eval_ahead),
                      ## min_available_aheads_per_timeloc = "4",
                      plot_prefix=sprintf("mixed_model_vsQEWMD_ahead_%d_", eval_ahead)
                    ),
                    output_file=sprintf("mixed_model_vsQEWMD_ahead_%d_report.html", eval_ahead)
                    )
}

for (eval_ahead in 1:4) {
  rmarkdown::render("mixed_models.Rmd",
                    params=list(
                      forecaster_names = "COVIDhub-ensemble;hayman_ens_v1_selhub8;qspace_ew_md_ens_v1_all;qspace_ew_md_ens_v1_selhub8;ensemble3_cdc_all_dup_re;ensemble3_cdc_impute;cdf_rwt2imp_ens_v0_selhub8;cdf_rwt2_ens_v0_selhub8_v1",
                      eval_ahead=as.character(eval_ahead),
                      ## min_available_aheads_per_timeloc = "4",
                      plot_prefix=sprintf("mixed_model_ahead_%d_", eval_ahead)
                    ),
                    output_file=sprintf("mixed_model_ahead_%d_report.html", eval_ahead)
  )
}

for (eval_ahead in 1:4) {
  rmarkdown::render("mixed_models.Rmd",
                    params=list(
                      forecaster_names = "hayman_ens_v1_selhub8;ensemble3_cdc_all_dup_re;ensemble3_cdc_impute;cdf_rwt2imp_ens_v0_selhub8;cdf_rwt2_ens_v0_selhub8_v1",
                      eval_ahead=as.character(eval_ahead),
                      ## min_available_aheads_per_timeloc = "4",
                      plot_prefix=sprintf("mixed_model_noCHens_ahead_%d_", eval_ahead)
                    ),
                    output_file=sprintf("mixed_model_noCHens_ahead_%d_report.html", eval_ahead)
                    )
}

## for (eval_ahead in 1:4) {
##   rmarkdown::render("mixed_models.Rmd",
##                     params=list(
##                       forecaster_names = "COVIDhub-ensemble;hayman_ens_v1_selhub8;ensemble3_cdc_all_dup_re;ensemble3train_cdc_all_dup_re;ensemble1_cdc_all_dup_re;ensemble23_cdc_all_dup_re;ensemble3_cdc_impute;ensemble3train_cdc_impute;ensemble1_cdc_impute;ensemble23_cdc_impute;ensemble3_cdc_dup;cdf_rwt2imp_ens_v0_selhub8;cdf_rwt2_ens_v0_selhub8;cdf_raw_ens_v0_selhub8;cdf_rwt2_ens_v0_selhub8_v1pre;cdf_rwt2_ens_v0_selhub8_v1",
##                       eval_ahead=as.character(eval_ahead),
##                       ## min_available_aheads_per_timeloc = "4",
##                       plot_prefix=sprintf("mixed_model_morefcs_ahead_%d_", eval_ahead)
##                     ),
##                     output_file=sprintf("mixed_model_morefcs_ahead_%d_report.html", eval_ahead)
##                     )
## }

for (eval_ahead in 1:4) {
  rmarkdown::render("mixed_models.Rmd",
                    params=list(
                      forecaster_names = "ensemble3_cdc_all_dup_re;ensemble3train_cdc_all_dup_re;ensemble1_cdc_all_dup_re;ensemble23_cdc_all_dup_re;ensemble3_cdc_impute;ensemble3train_cdc_impute;ensemble1_cdc_impute;ensemble23_cdc_impute;ensemble3_cdc_dup;ensemble1_cdc_dup;ensemble23_cdc_dup",
                      eval_ahead=as.character(eval_ahead),
                      ## min_available_aheads_per_timeloc = "4",
                      plot_prefix=sprintf("mixed_model_quantgen_ahead_%d_", eval_ahead)
                    ),
                    output_file=sprintf("mixed_model_quantgen_ahead_%d_report.html", eval_ahead)
                    )
}


for (eval_ahead in 1:4) {
  rmarkdown::render("mixed_models.Rmd",
                    params=list(
                      forecaster_names = "ensemble3_cdc_all_dup_re;ensemble3train_cdc_all_dup_re;ensemble1_cdc_all_dup_re;ensemble23_cdc_all_dup_re;ensemble3_cdc_impute;ensemble3train_cdc_impute;ensemble1_cdc_impute;ensemble23_cdc_impute",
                      eval_ahead=as.character(eval_ahead),
                      ## min_available_aheads_per_timeloc = "4",
                      plot_prefix=sprintf("mixed_model_quantgenimp_ahead_%d_", eval_ahead)
                    ),
                    output_file=sprintf("mixed_model_quantgenimp_ahead_%d_report.html", eval_ahead)
                    )
  sc_df %>%
    dplyr::mutate(err_log = log(err)) %>%
    tidyr::pivot_wider(id_cols = c(ahead, response, geo_type, incidence_period, n_locations, forecast_date, location, location_name),
                       names_from = c(forecaster),
                       values_from = c(err, err_log, err_abs, err_abs_log)) %>%
    ggplot2::ggplot(ggplot2::aes_string(sprintf("`err_log_%s`",forecaster_names[[1L]]),
                                        sprintf("`err_log_%s`",forecaster_names[[2L]]))) +
    ggplot2::geom_point()
  ggplot2::ggsave(sprintf("mixed_model_qgvssim_ahead_%d_wis_scatter2fcs.pdf", eval_ahead), width=7, height=5, units="in")
}


for (eval_ahead in 1:4) {
  rmarkdown::render("mixed_models.Rmd",
                    params=list(
                      forecaster_names = "Covid19Sim-Simulator;ensemble1_cdc_all_dup_re;YYG-ParamSearch",
                      eval_ahead=as.character(eval_ahead),
                      ## min_available_aheads_per_timeloc = "4",
                      plot_prefix=sprintf("mixed_model_qgvssim_ahead_%d_", eval_ahead)
                    ),
                    output_file=sprintf("mixed_model_qgvssim_ahead_%d_report.html", eval_ahead)
                    )
  sc_df %>%
    dplyr::mutate(err_log = log(err)) %>%
    tidyr::pivot_wider(id_cols = c(ahead, response, geo_type, incidence_period, n_locations, forecast_date, location, location_name),
                       names_from = c(forecaster),
                       values_from = c(err, err_log, err_abs, err_abs_log)) %>%
    ggplot2::ggplot(ggplot2::aes_string(sprintf("`err_log_%s`",forecaster_names[[1L]]),
                                        sprintf("`err_log_%s`",forecaster_names[[2L]]))) +
    ggplot2::geom_point()
  ggplot2::ggsave(sprintf("mixed_model_qgvssim_ahead_%d_wis_scatter2fcs.pdf", eval_ahead), width=7, height=5, units="in")
}


for (eval_ahead in 1:4) {
  rmarkdown::render("mixed_models.Rmd",
                    params=list(
                      forecaster_names = "cdf_rwt2imp_ens_v0_selhub8;cdf_rwt2_ens_v0_selhub8;cdf_raw_ens_v0_selhub8;cdf_rwt2_ens_v0_selhub8_v1pre;cdf_rwt2_ens_v0_selhub8_v1",
                      eval_ahead=as.character(eval_ahead),
                      ## min_available_aheads_per_timeloc = "4",
                      plot_prefix=sprintf("mixed_model_cdfs_ahead_%d_", eval_ahead)
                    ),
                    output_file=sprintf("mixed_model_cdfs_ahead_%d_report.html", eval_ahead)
                    )
}





for (eval_ahead in 1:4) {
  rmarkdown::render("mixed_models.Rmd",
                    params=list(
                      ## forecaster_names = "qspace_ew_md_ens_v1_inconly;qspace_ew_md_ens_v1_all;qspace_ew_md_ens_v1_selhub8;qspace_ew_mean_ens_v1_inconly;qspace_ew_mean_ens_v1_all;qspace_ew_mean_ens_v1_selhub8;ensemble3_cdc_inconly;ensemble3_cdc_all_dup_re;ensemble3_cdc_impute",
                      forecaster_names = "qspace_ew_md_ens_v1_inconly;qspace_ew_md_ens_v1_all;qspace_ew_mean_ens_v1_inconly;qspace_ew_mean_ens_v1_all;ensemble3_cdc_inconly;ensemble3_cdc_all_dup_re;ensemble3_cdc_inconly_testdate",
                      eval_ahead=as.character(eval_ahead),
                      ## min_available_aheads_per_timeloc = "4",
                      plot_prefix=sprintf("mixed_model_mainqspace_ahead_%d_", eval_ahead)
                    ),
                    output_file=sprintf("mixed_model_mainqspace_ahead_%d_report.html", eval_ahead)
                    )
}


for (eval_ahead in 1:4) {
  rmarkdown::render("mixed_models.Rmd",
                    params=list(
                      forecaster_names = "qspace_ew_md_ens_v1_inconly;qspace_ew_mean_ens_v1_inconly;ensemble3_cdc_inconly;ensemble3_cdc_inconly_testdate",
                      eval_ahead=as.character(eval_ahead),
                      ## min_available_aheads_per_timeloc = "4",
                      plot_prefix=sprintf("mixed_model_vscheating_ahead_%d_", eval_ahead)
                    ),
                    output_file=sprintf("mixed_model_vscheating_ahead_%d_report.html", eval_ahead)
                    )
}

for (eval_ahead in 1:4) {
  rmarkdown::render("mixed_models.Rmd",
                    params=list(
                      forecaster_names = "qspace_ew_md_ens_v1_inconly;qspace_ew_mean_ens_v1_inconly;ensemble3_cdc_inconly",
                      eval_ahead=as.character(eval_ahead),
                      ## min_available_aheads_per_timeloc = "4",
                      plot_prefix=sprintf("mixed_model_mainqspace_ahead_%d_", eval_ahead)
                    ),
                    output_file=sprintf("mixed_model_mainqspace_ahead_%d_report.html", eval_ahead)
                    )
}

for (eval_ahead in 1:4) {
  rmarkdown::render("mixed_models.Rmd",
                    params=list(
                      forecaster_names = "qspace_ew_md_ens_v1_inconly;qspace_ew_md_ens_v1_all;qspace_ew_mean_ens_v1_inconly;qspace_ew_mean_ens_v1_all;ensemble3_cdc_inconly;ensemble3_cdc_all_dup_re",
                      eval_ahead=as.character(eval_ahead),
                      ## min_available_aheads_per_timeloc = "4",
                      plot_prefix=sprintf("mixed_model_incvswide_ahead_%d_", eval_ahead)
                    ),
                    output_file=sprintf("mixed_model_incvswide_ahead_%d_report.html", eval_ahead)
                    )
}


for (eval_ahead in 1:4) {
  rmarkdown::render("mixed_models.Rmd",
                    params=list(
                      forecaster_names = "qspace_ew_mean_ens_v1_inconly;ensemble3_cdc_inconly",
                      eval_ahead=as.character(eval_ahead),
                      ## min_available_aheads_per_timeloc = "4",
                      plot_prefix=sprintf("mixed_model_qgvsuntrained_ahead_%d_", eval_ahead)
                    ),
                    output_file=sprintf("mixed_model_qgvsuntrained_ahead_%d_report.html", eval_ahead)
                    )
}


for (eval_ahead in 1:4) {
  rmarkdown::render("mixed_models.Rmd",
                    params=list(
                      forecaster_names = "qspace_ew_mean_ens_v1_inconly;qspace_ew_mean_ens_v1_all;ensemble3_cdc_inconly;ensemble3_cdc_all_dup_re",
                      eval_ahead=as.character(eval_ahead),
                      ## min_available_aheads_per_timeloc = "4",
                      plot_prefix=sprintf("mixed_model_meanincvswide_ahead_%d_", eval_ahead)
                    ),
                    output_file=sprintf("mixed_model_meanincvswide_ahead_%d_report.html", eval_ahead)
                    )
}
