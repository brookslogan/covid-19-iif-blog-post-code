
devtools::load_all("../evalforecast")

## TODO adjust ensembles to require at least 3 weeks of component data reporting before producing output, eliminate use of dropfirst here to simulate

rmarkdown::render("ensemble_evaluation_overview.Rmd",
                  params = list(
                    forecaster_names = "qspace_ew_md_ens_v1_inconly;qspace_ew_mean_ens_v1_inconly;ensemble3_cdc_inconly",
                    eval_aheads = "1;2;3;4",
                    dropfirst = "0",
                    plot_prefix = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_nodrop2_")
                  ),
                  output_file = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_nodrop2_ahead.html")
                  )

rmarkdown::render("ensemble_evaluation_overview.Rmd",
                  params = list(
                    forecaster_names = "COVIDhub-baseline;qspace_ew_mean_ens_v1_inconly;qspace_ew_md_ens_v1_inconly;ensemble3_cdc_inconly",
                    eval_aheads = "1;2;3;4",
                    dropfirst = "2",
                    plot_prefix = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_mainqspace_")
                  ),
                  output_file = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_mainqspace_ahead.html")
                  )

sc_df %>% group_by(ahead,response,geo_type,incidence_period,n_locations,forecast_date,location) %>% summarize(abs_diff_err=abs(err[[which(forecaster=="ensemble3_cdc_inconly")]] - err[[which(forecaster=="qspace_ew_md_ens_v1_inconly")]]), .groups="drop_last") %>% group_by(ahead,response,geo_type,incidence_period,n_locations,forecast_date) %>% summarize(max_var_prop = max(abs_diff_err)/sum(abs_diff_err), .groups="drop_last") %>% group_by(ahead,response,geo_type,incidence_period,n_locations) %>% summarize(mean_max_var_prop = mean(max_var_prop), .groups="drop_last")

sc_df %>% group_by(ahead,response,geo_type,incidence_period,n_locations,forecast_date,location) %>% summarize(abs_diff_err=abs(err[[which(forecaster=="ensemble3_cdc_inconly")]] - err[[which(forecaster=="qspace_ew_md_ens_v1_inconly")]]), .groups="drop_last") %>% group_by(ahead,response,geo_type,incidence_period,n_locations,forecast_date) %>% summarize(max_var_prop = max(abs_diff_err)/sum(abs_diff_err), .groups="drop_last") %>% group_by(ahead,response,geo_type,incidence_period,n_locations) %>% summarize(mean_max_var_prop = mean(max_var_prop), .groups="drop_last")

rmarkdown::render("ensemble_evaluation_overview.Rmd",
                  params = list(
                    forecaster_names = "qspace_ew_md_ens_v1_inconly;qspace_ew_mean_ens_v1_inconly;ensemble3_cdc_inconly;COVIDhub-ensemble;hayman_ens_v1_selhub8;cdf_rwt2imp_ens_v0_selhub8;ensemble3_cdc_impute",
                    eval_aheads = "1;2;3;4",
                    dropfirst = "3",
                    plot_prefix = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_misc1_")
                  ),
                  output_file = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_misc1_ahead.html")
                  )

rmarkdown::render("ensemble_evaluation_overview.Rmd",
                  params = list(
                    forecaster_names = "qspace_ew_md_ens_v1_inconly;qspace_ew_mean_ens_v1_inconly;ensemble3_cdc_inconly;COVIDhub-ensemble;hayman_ens_v1_selhub8",
                    eval_aheads = "1;2;3;4",
                    dropfirst = "3",
                    plot_prefix = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_misc2_")
                  ),
                  output_file = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_misc2_ahead.html")
                  )

rmarkdown::render("ensemble_evaluation_overview.Rmd",
                  params = list(
                    ## forecaster_names = "qspace_ew_md_ens_v1_inconly;qspace_ew_mean_ens_v1_inconly;ensemble3_cdc_inconly;YYG-ParamSearch;UMass-MechBayes;COVIDhub-baseline",
                    ## forecaster_names = "COVIDhub-baseline;YYG-ParamSearch;UMass-MechBayes;qspace_ew_mean_ens_v1_inconly;qspace_ew_md_ens_v1_inconly;ensemble3_cdc_inconly",
                    ## forecaster_names = "COVIDhub-baseline;YYG-ParamSearch;UMass-MechBayes;qspace_ew_md_ens_v1_inconly;ensemble3_cdc_inconly",
                    forecaster_names = "YYG-ParamSearch;UMass-MechBayes;qspace_ew_md_ens_v1_inconly;ensemble3_cdc_inconly",
                    eval_aheads = "1;2;3;4",
                    ## dropfirst = "2",
                    dropfirst = "1",
                    plot_prefix = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_mainensncomps_")
                  ),
                  output_file = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_mainensncomps_ahead.html")
                  )

rmarkdown::render("ensemble_evaluation_overview.Rmd",
                  params = list(
                    forecaster_names = "ensemble3_cdc_inconly;ensemble3_cdc_inconly_screen12;ensemble3_cdc_inconly_screen8;ensemble3_cdc_inconly_screen4",
                    eval_aheads = "1;2;3;4",
                    dropfirst = "2",
                    plot_prefix = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_screening3group_")
                  ),
                  output_file = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_screening3group_ahead.html")
                  )



## need a better trained ensemble filter check than the dropfirst hack

rmarkdown::render("ensemble_evaluation_overview.Rmd",
                  params = list(
                    forecaster_names = "COVIDhub-baseline;subset/4/1/qspace_ew_mean_ens_v1_inconly;subset/4/1/qspace_ew_md_ens_v1_inconly;subset/4/1/ensemble3_cdc_inconly;qspace_ew_mean_ens_v1_inconly;qspace_ew_md_ens_v1_inconly;ensemble3_cdc_inconly",
                    eval_aheads = "1;2;3;4",
                    dropfirst = "0",
                    plot_prefix = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_vsrandsubset4draw1_")
                  ),
                  output_file = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_vsrandsubset4draw1_ahead.html")
                  )

rmarkdown::render("ensemble_evaluation_overview.Rmd",
                  params = list(
                    forecaster_names = "COVIDhub-baseline;cheating_subset/4/qspace_ew_mean_ens_v1_inconly;cheating_subset/4/qspace_ew_md_ens_v1_inconly;cheating_subset/4/ensemble3_cdc_inconly;qspace_ew_mean_ens_v1_inconly;qspace_ew_md_ens_v1_inconly;ensemble3_cdc_inconly",
                    eval_aheads = "1;2;3;4",
                    dropfirst = "0",
                    plot_prefix = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_vsretrosubset4_")
                  ),
                  output_file = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_vsretrosubset4_ahead.html")
                  )

rmarkdown::render("ensemble_evaluation_overview.Rmd",
                  params = list(
                    forecaster_names = "COVIDhub-baseline;cheating_subset/8/qspace_ew_mean_ens_v1_inconly;cheating_subset/8/qspace_ew_md_ens_v1_inconly;cheating_subset/8/ensemble3_cdc_inconly;qspace_ew_mean_ens_v1_inconly;qspace_ew_md_ens_v1_inconly;ensemble3_cdc_inconly",
                    eval_aheads = "1;2;3;4",
                    dropfirst = "0",
                    plot_prefix = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_vsretrosubset8_")
                  ),
                  output_file = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_vsretrosubset8_ahead.html")
                  )

rmarkdown::render("ensemble_evaluation_overview.Rmd",
                  params = list(
                    forecaster_names = "COVIDhub-baseline;ensemble3_cdc_inconly;cheating_subset/16/ensemble3_cdc_inconly;cheating_subset/8/ensemble3_cdc_inconly;cheating_subset/4/ensemble3_cdc_inconly",
                    forecaster_labels = "Baseline;QuantTrained;QuantTrained on 16 highly-weighted in-sample;QuantTrained on 8 highly-weighted in-sample;QuantTrained on 4 highly-weighted in-sample",
                    forecaster_colors = paste(collapse=";",RColorBrewer::brewer.pal(5L, "Paired")),
                    eval_aheads = "1;2;3;4",
                    dropfirst = "0",
                    plot_prefix = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_qgretrosubset_")
                  ),
                  output_file = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_qgretrosubset_ahead.html")
                  )

rmarkdown::render("ensemble_evaluation_overview.Rmd",
                  params = list(
                    forecaster_names = "COVIDhub-baseline;qspace_ew_md_ens_v1_inconly;cheating_subset/16/qspace_ew_md_ens_v1_inconly;cheating_subset/8/qspace_ew_md_ens_v1_inconly;cheating_subset/4/qspace_ew_md_ens_v1_inconly",
                    forecaster_labels = "Baseline;QuantMedian;QuantMedian on 16 highly-weighted in-sample;QuantMedian on 8 highly-weighted in-sample;QuantMedian on 4 highly-weighted in-sample",
                    forecaster_colors = paste(collapse=";",RColorBrewer::brewer.pal(5L, "Paired")),
                    eval_aheads = "1;2;3;4",
                    dropfirst = "0",
                    plot_prefix = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_mdretrosubset_")
                  ),
                  output_file = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_mdretrosubset_ahead.html")
                  )

rmarkdown::render("ensemble_evaluation_overview.Rmd",
                  params = list(
                    forecaster_names = "COVIDhub-baseline;qspace_ew_md_ens_v1_inconly;cheating_subset/8/qspace_ew_md_ens_v1_inconly;oos_subset/8/qspace_ew_md_ens_v1_inconly",
                    forecaster_labels = "Baseline;QuantMedian;QuantMedian on 8 highly-weighted in-sample;QuantMedian on 8 highly-weighted out-of-sample",
                    forecaster_colors = paste(collapse=";",RColorBrewer::brewer.pal(4L, "Paired")),
                    eval_aheads = "1;2;3;4",
                    dropfirst = "0",
                    plot_prefix = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_mdsubset8_")
                  ),
                  output_file = sprintf("../../covid-19-iif-blog-post-data/reports/evaluation-overviews/eval_overview_mdsubset8_ahead.html")
                  )

