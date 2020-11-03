

devtools::load_all("../quantileEnsemble")
prob10i <- which(cdc_probs == 0.1)
prob90i <- which(round(cdc_probs, 3L) == 0.900)
prob20i = which(cdc_probs == 0.2)
prob80i = which(cdc_probs == 0.8)
problowi <- 1L
probhighi <- length(cdc_probs)

scorecard_group_summary = function(scorecards_df) {
    scorecards_df %>%
        dplyr::summarize(
                   mean_err = mean(err),
                   mean_abs_err = mean(abs(point_est - actual)),
                   pbelow20 = mean(actual < sapply(forecast_distribution, function(df) df[["quantiles"]][[prob20i]])),
                   pabove80 = mean(actual > sapply(forecast_distribution, function(df) df[["quantiles"]][[prob80i]])),
                   cov2080 = 1 - pbelow20 - pabove80,
                   pbelow10 = mean(actual < sapply(forecast_distribution, function(df) df[["quantiles"]][[prob10i]])),
                   pabove90 = mean(actual > sapply(forecast_distribution, function(df) df[["quantiles"]][[prob90i]])),
                   cov1090 = 1 - pbelow10 - pabove90,
                   pbelowlow = mean(actual < sapply(forecast_distribution, function(df) df[["quantiles"]][[problowi]])),
                   pabovehigh = mean(actual > sapply(forecast_distribution, function(df) df[["quantiles"]][[probhighi]])),
                   covlowhigh = 1 - pbelowlow - pabovehigh,
                   median_err = median(err),
                   eighty_err = quantile(err, 0.8),
                   wi_eighty_merr = mean(err[err <= quantile(err, 0.8)]),
                   respweight_mean_err = weighted.mean(err, actual),
                   wi_eighty_mae = {ae = abs(point_est - actual); mean(ae[ae <= quantile(ae, 0.8)])},
                   n = dplyr::n(),
                   mean_actual = mean(actual),
                   .groups="drop_last") %>%
        {.}
}

scorecard_summary =
    scorecards %>%
    dplyr::group_by(ahead,geo_type,forecaster) %>%
    scorecard_group_summary() %>%
    ## dplyr::arrange(ahead, geo_type, respweight_mean_err) %>%
    ## tidyr::pivot_wider(names_from="forecaster", values_from=c("mean_err","respweight_mean_err","n")) %>%
    ## tidyr::spread(forecaster, respweight_mean_err) %>%
    {.}

scorecard_summary_with_time =
    scorecards %>%
    dplyr::group_by(ahead,geo_type,forecaster,forecast_date) %>%
    scorecard_group_summary() %>%
    {.}

scorecard_summary_with_location =
    scorecards %>%
    dplyr::group_by(ahead,geo_type,forecaster,location,location_name) %>%
    scorecard_group_summary() %>%
    {.}

scorecard_summary %>%
    dplyr::filter(grepl("ens", forecaster)) %>%
    ## dplyr::filter(grepl("ensemble|chocolate|poiszero|customized",forecaster)) %>%
    ## dplyr::filter(grepl("comb1$|cream$|customized$|poiszero|strawman", forecaster)) %>%
    ## ggplot2::ggplot(ggplot2::aes(forecast_date, mean_err, colour=forecaster, group=forecaster)) +
    ggplot2::ggplot(ggplot2::aes(ahead, mean_abs_err, colour = forecaster, group = forecaster)) +
    ggplot2::facet_wrap(~ geo_type) +
    ggplot2::expand_limits(y=0) +
    ## ggplot2::scale_colour_brewer(palette="Paired") +
    ## ggplot2::scale_colour_brewer(palette="Spectral") +
    ggplot2::geom_point() +
    ggplot2::geom_line()

scorecard_summary_with_time %>%
    ## dplyr::filter(grepl("ensemble|chocolate|poiszero|customized",forecaster)) %>%
    ## dplyr::filter(grepl("comb1$|cream$|customized$|poiszero|strawman", forecaster)) %>%
    ## ggplot2::ggplot(ggplot2::aes(forecast_date, mean_err, colour=forecaster, group=forecaster)) +
    ggplot2::ggplot(ggplot2::aes(forecast_date, mean_abs_err, colour=forecaster, group=forecaster)) +
    ## ggplot2::facet_grid(geo_type ~ ahead) +
    ggplot2::facet_wrap(~ ahead) +
    ## ggplot2::scale_colour_brewer(palette="Paired") +
    ## ggplot2::scale_colour_brewer(palette="Spectral") +
    ggplot2::scale_colour_manual(values = forecaster.colors, labels=forecaster.labels ) +
    ggplot2::geom_point(position=ggplot2::position_dodge(width=3), size=0.5) +
    ggplot2::geom_line(position=ggplot2::position_dodge(width=3), size=0.5) +
    ggplot2::theme()
ggplot2::ggsave("mae_with_time.pdf", width=7, height=5, units="in")

scorecard_summary_with_location %>%
    dplyr::mutate(., location = ordered(location,
                                        dplyr::group_by(., location) %>% dplyr::summarize(to_rank=mean(mean_actual), .groups="drop_last") %>% dplyr::arrange(to_rank) %>% dplyr::pull(location)
                                        )) %>%
    ## dplyr::filter(grepl("ensemble|chocolate|poiszero|customized",forecaster)) %>%
    ## dplyr::filter(grepl("comb1$|cream$|customized$|poiszero|strawman", forecaster)) %>%
    ## dplyr::filter(grepl("comb1$|cream$|customized$|poiszero|strawman|Search", forecaster)) %>%
    ## dplyr::filter(grepl("flexible|simple|hayman", forecaster)) %>%
    ## dplyr::filter(grepl("cream$|customized$|Search", forecaster)) %>%
    ## ggplot2::ggplot(ggplot2::aes(location, mean_err, colour=forecaster, group=forecaster)) +
    ggplot2::ggplot(ggplot2::aes(location, mean_abs_err, colour=forecaster, group=forecaster)) +
    ## ggplot2::facet_grid(geo_type ~ ahead) +
    ggplot2::facet_wrap(~ ahead) +
    ## ggplot2::scale_colour_brewer(palette="Paired") +
    ## ggplot2::scale_colour_brewer(palette="Spectral") +
    ggplot2::scale_colour_manual(values = forecaster.colors, labels=forecaster.labels ) +
    ggplot2::geom_line()
ggplot2::ggsave("mae_with_location.pdf", width=7, height=5, units="in")


scorecard_summary %>%
    dplyr::filter(grepl("ensemble|chocolate|poiszero|customized", forecaster) |
                  forecaster %in% c("YYG-ParamSearch", "LANL-GrowthRate", "COVIDhub-ensemble", "UMass-MechBayes", "UCLA-SuEIR")) %>%
    ggplot2::ggplot(ggplot2::aes(ahead, mean_err, colour=forecaster, group=forecaster)) +
    ## ggplot2::scale_colour_brewer(palette="Paired") +
    ## ggplot2::scale_colour_brewer(palette="Spectral") +
    ggplot2::geom_line()

scorecard_summary %>%
    ## dplyr::select(ahead, geo_type, forecaster, respweight_mean_err) %>%
    ## dplyr::arrange(ahead, geo_type, respweight_mean_err) %>%
    dplyr::select(ahead, geo_type, forecaster, mean_err) %>%
    dplyr::arrange(ahead, geo_type, mean_err) %>%
    ## dplyr::filter(ahead=="1", geo_type=="county") %>%
    ## dplyr::filter(ahead=="2", geo_type=="county") %>%
    ## dplyr::filter(ahead=="3", geo_type=="county") %>%
    ## dplyr::filter(ahead=="4", geo_type=="county") %>%
    ## dplyr::filter(ahead=="1", geo_type=="state") %>%
    ## dplyr::filter(ahead=="2", geo_type=="state") %>%
    ## dplyr::filter(ahead=="3", geo_type=="state") %>%
    ## dplyr::filter(ahead=="4", geo_type=="state") %>%
    {.}

scorecard_summary %>%
    dplyr::filter(geo_type == "state") %>%
    dplyr::mutate(., forecaster = ordered(forecaster,
                                          dplyr::group_by(., forecaster) %>%
                                          ## dplyr::summarize(mean_mean_err=mean(mean_err), .groups="drop_last") %>%
                                          dplyr::summarize(mean_mean_err=weighted.mean(mean_err, n), .groups="drop_last") %>%
                                          dplyr::arrange(-dplyr::coalesce(mean_mean_err, Inf)) %>%
                                          dplyr::pull(forecaster))) %>%
    ## dplyr::mutate(., forecaster = ordered(forecaster,
    ##                                       ## dplyr::filter(., ahead==1L & geo_type=="state") %>%
    ##                                       dplyr::filter(., ahead==2L & geo_type=="state") %>%
    ##                                       dplyr::arrange(-dplyr::coalesce(mean_err, Inf)) %>%
    ##                                       dplyr::pull(forecaster))) %>%
    ggplot2::ggplot(ggplot2::aes(ahead, forecaster)) +
    ggplot2::geom_tile(ggplot2::aes(fill=mean_err)) +
    ggplot2::geom_text(ggplot2::aes(label=round(mean_err,1L))) +
    ggplot2::scale_fill_distiller(palette="RdYlBu") +
    ggplot2::scale_y_discrete(labels=forecaster.labels) +
    ggplot2::ggtitle("mean_err")
ggplot2::ggsave("wis_heatmaptable.pdf", width=7, height=5, units="in")

scorecard_summary %>%
    dplyr::filter(geo_type == "state") %>%
    dplyr::mutate(., forecaster = ordered(forecaster,
                                          dplyr::group_by(., forecaster) %>%
                                          ## dplyr::summarize(mean_mean_abs_err=mean(mean_abs_err), .groups="drop_last") %>%
                                          dplyr::summarize(mean_mean_abs_err=weighted.mean(mean_abs_err, n), .groups="drop_last") %>%
                                          dplyr::arrange(-dplyr::coalesce(mean_mean_abs_err, Inf)) %>%
                                          dplyr::pull(forecaster))) %>%
    ## dplyr::mutate(., forecaster = ordered(forecaster,
    ##                                       ## dplyr::filter(., ahead==1L & geo_type=="state") %>%
    ##                                       dplyr::filter(., ahead==2L & geo_type=="state") %>%
    ##                                       dplyr::arrange(-dplyr::coalesce(mean_abs_err, Inf)) %>%
    ##                                       dplyr::pull(forecaster))) %>%
    ggplot2::ggplot(ggplot2::aes(ahead, forecaster)) +
    ggplot2::geom_tile(ggplot2::aes(fill=mean_abs_err)) +
    ggplot2::geom_text(ggplot2::aes(label=round(mean_abs_err,1L))) +
    ggplot2::scale_fill_distiller(palette="RdYlBu") +
    ggplot2::scale_y_discrete(labels=forecaster.labels) +
    ggplot2::ggtitle("mean_abs_err")
ggplot2::ggsave("mae_heatmaptable.pdf", width=7, height=5, units="in")

scorecard_summary %>%
    dplyr::filter(geo_type == "state") %>%
    dplyr::mutate(., forecaster = ordered(forecaster,
                                          dplyr::group_by(., forecaster) %>%
                                          dplyr::summarize(mean_cov2080=mean(cov2080), .groups="drop_last") %>%
                                          dplyr::arrange(-dplyr::coalesce(mean_cov2080, Inf)) %>%
                                          dplyr::pull(forecaster))) %>%
    ## dplyr::mutate(., forecaster = ordered(forecaster,
    ##                                       ## dplyr::filter(., ahead==1L & geo_type=="state") %>%
    ##                                       dplyr::filter(., ahead==2L & geo_type=="state") %>%
    ##                                       dplyr::arrange(-dplyr::coalesce(cov2080, Inf)) %>%
    ##                                       dplyr::pull(forecaster))) %>%
    ggplot2::ggplot(ggplot2::aes(ahead, forecaster)) +
    ggplot2::geom_tile(ggplot2::aes(fill=cov2080)) +
    ggplot2::geom_text(ggplot2::aes(label=round(cov2080,2L))) +
    ggplot2::scale_fill_distiller(palette="PRGn", direction=1) +
    ggplot2::scale_y_discrete(labels=forecaster.labels) +
    ggplot2::ggtitle("cov2080")
ggplot2::ggsave("cov2080_heatmaptable.pdf", width=7, height=5, units="in")

scorecard_summary %>%
    dplyr::filter(geo_type == "state") %>%
    dplyr::mutate(., forecaster = ordered(forecaster,
                                          dplyr::group_by(., forecaster) %>%
                                          dplyr::summarize(mean_cov1090=mean(cov1090), .groups="drop_last") %>%
                                          dplyr::arrange(-dplyr::coalesce(mean_cov1090, Inf)) %>%
                                          dplyr::pull(forecaster))) %>%
    ## dplyr::mutate(., forecaster = ordered(forecaster,
    ##                                       ## dplyr::filter(., ahead==1L & geo_type=="state") %>%
    ##                                       dplyr::filter(., ahead==2L & geo_type=="state") %>%
    ##                                       dplyr::arrange(-dplyr::coalesce(cov1090, Inf)) %>%
    ##                                       dplyr::pull(forecaster))) %>%
    ggplot2::ggplot(ggplot2::aes(ahead, forecaster)) +
    ggplot2::geom_tile(ggplot2::aes(fill=cov1090)) +
    ggplot2::geom_text(ggplot2::aes(label=round(cov1090,2L))) +
    ggplot2::scale_fill_distiller(palette="PRGn", direction=1) +
    ggplot2::scale_y_discrete(labels=forecaster.labels) +
    ggplot2::ggtitle("cov1090")
ggplot2::ggsave("cov1090_heatmaptable.pdf", width=7, height=5, units="in")

scorecard_summary %>%
    dplyr::filter(geo_type == "state") %>%
    dplyr::mutate(., forecaster = ordered(forecaster,
                                          dplyr::group_by(., forecaster) %>%
                                          dplyr::summarize(mean_covlowhigh=mean(covlowhigh), .groups="drop_last") %>%
                                          dplyr::arrange(-dplyr::coalesce(mean_covlowhigh, Inf)) %>%
                                          dplyr::pull(forecaster))) %>%
    ## dplyr::mutate(., forecaster = ordered(forecaster,
    ##                                       ## dplyr::filter(., ahead==1L & geo_type=="state") %>%
    ##                                       dplyr::filter(., ahead==2L & geo_type=="state") %>%
    ##                                       dplyr::arrange(-dplyr::coalesce(covlowhigh, Inf)) %>%
    ##                                       dplyr::pull(forecaster))) %>%
    ggplot2::ggplot(ggplot2::aes(ahead, forecaster)) +
    ggplot2::geom_tile(ggplot2::aes(fill=covlowhigh)) +
    ggplot2::geom_text(ggplot2::aes(label=round(covlowhigh,2L))) +
    ggplot2::scale_fill_distiller(palette="PRGn", direction=1) +
    ggplot2::ggtitle("covlowhigh")



## TODO coverage

## scorecard_summary %>%
##     dplyr::filter(geo_type == "state") %>%
##     dplyr::mutate(., forecaster = ordered(forecaster,
##                                           dplyr::group_by(., forecaster) %>%
##                                           dplyr::summarize(mean_wi_eighty_merr=mean(wi_eighty_merr), .groups="drop_last") %>%
##                                           dplyr::arrange(-dplyr::coalesce(mean_wi_eighty_merr, Inf)) %>%
##                                           dplyr::pull(forecaster))) %>%
##     ## dplyr::mutate(., forecaster = ordered(forecaster,
##     ##                                       ## dplyr::filter(., ahead==1L & geo_type=="state") %>%
##     ##                                       dplyr::filter(., ahead==2L & geo_type=="state") %>%
##     ##                                       dplyr::arrange(-dplyr::coalesce(wi_eighty_merr, Inf)) %>%
##     ##                                       dplyr::pull(forecaster))) %>%
##     ggplot2::ggplot(ggplot2::aes(ahead, forecaster)) +
##     ggplot2::geom_tile(ggplot2::aes(fill=wi_eighty_merr)) +
##     ggplot2::geom_text(ggplot2::aes(label=round(wi_eighty_merr,1L))) +
##     ggplot2::scale_fill_distiller(palette="RdYlBu") +
##     ggplot2::ggtitle("wi_eighty_merr")

## scorecard_summary %>%
##     dplyr::filter(geo_type == "state") %>%
##     dplyr::mutate(., forecaster = ordered(forecaster,
##                                           dplyr::group_by(., forecaster) %>%
##                                           dplyr::summarize(mean_wi_eighty_mae=mean(wi_eighty_mae), .groups="drop_last") %>%
##                                           dplyr::arrange(-dplyr::coalesce(mean_wi_eighty_mae, Inf)) %>%
##                                           dplyr::pull(forecaster))) %>%
##     ## dplyr::mutate(., forecaster = ordered(forecaster,
##     ##                                       ## dplyr::filter(., ahead==1L & geo_type=="state") %>%
##     ##                                       dplyr::filter(., ahead==2L & geo_type=="state") %>%
##     ##                                       dplyr::arrange(-dplyr::coalesce(wi_eighty_mae, Inf)) %>%
##     ##                                       dplyr::pull(forecaster))) %>%
##     ggplot2::ggplot(ggplot2::aes(ahead, forecaster)) +
##     ggplot2::geom_tile(ggplot2::aes(fill=wi_eighty_mae)) +
##     ggplot2::geom_text(ggplot2::aes(label=round(wi_eighty_mae,1L))) +
##     ggplot2::scale_fill_distiller(palette="RdYlBu") +
##     ggplot2::ggtitle("wi_eighty_mae")




## TODO exclude NJ & ...? use Alden's known errors. or something robust? duplicating scorecard...

## selected from hub:
## - YYG = YYG-ParamSearch
## - LANL = LANL-GrowthRate
## - covidhub ensemble = COVIDhub-ensemble
## - UMass MB = UMass-MechBayes
## - UCLA = UCLA-SuEIR

## COVIDhub-baseline

## scorecards %>%
##     dplyr::group_by(ahead,response,geo_type,incidence_period,n_locations) %>%

## source("../../../../covid-19/hospitalization/pipeline/evaluation_pipeline/prob_forecast_evaluation_coverage.R")

coverage_summary = function(grouped.scorecards) {
  grouped.scorecards %>%
    dplyr::do(
             dplyr::mutate(., forecast_distribution = lapply(forecast_distribution, function(forecast.distribution) {
               forecast.distribution %>%
                 dplyr::mutate(cquantiles = rev(quantiles)) %>%
                 dplyr::filter(probs <= 0.5)
             })) %>%
             tidyr::unnest(., forecast_distribution) %>%
             dplyr::group_by(probs) %>%
             dplyr::summarize(
                      pbelow = mean(actual < quantiles),
                      pabove = mean(actual > cquantiles),
                      err_contrib = mean(probs*(cquantiles-quantiles) + pmax(actual-cquantiles,quantiles-actual,0)),
                      .groups = "drop_last"
                    )
           )
}

coverage.summary =
  scorecards %>%
  ## dplyr::filter(forecaster == "ensemble3_cdc_impute") %>%
  dplyr::group_by(forecaster) %>%
  ## compute_quantile_coverage() %>%
  ## dplyr::group_by(nominal_quantile) %>%
  ## dplyr::summarize(
  ##   prop_below=mean(prop_below),
  ##   prop_above=mean(prop_above),
  ##   .groups="drop_last"
  ## ) %>%
  coverage_summary() %>%
  ## dplyr::group_by(forecaster) %>%
  ## dplyr::summarize(merr=sum(err_contrib)) %>%
  ## dplyr::mutate(rerr=merr/max(merr)) %>>%
  {.}

  ## ## dplyr::group_by(nominal_quantile) %>%
  ## ## dplyr::summarize(
  ## ##   prop_below=mean(prop_below),
  ## ##   prop_above=mean(prop_above),
  ## ##   .groups="drop_last"
  ## ## ) %>%
  ## ## dplyr::select(-pbelow, -pabove) %>%
  ## tidyr::spread(forecaster, err_contrib) %>%
  ## print(n=30L)

coverage.summary %>%
  dplyr::group_by(probs, forecaster) %>%
  dplyr::summarize(merr=sum(err_contrib)) %>%
  dplyr::mutate(rerr=merr/max(merr)) %>>%
  ## print(n=50L)
  ggplot2::ggplot(ggplot2::aes(probs, merr, colour=forecaster)) +
  ggplot2::geom_line()

## TODO ensemble just with hub competitors
## TODO our ensemble w/ and w/o YYG
## TODO train on components where have at least 4wk of training
## TODO also look at point estimation

## TODO is hayman well-calibrated if all components are well-calibrated? why is
## its selectHub perf near top? Aaron: for density ensembles, not the case
##
## P(M1 <= Y) = 0.5, P(M2 <= Y) = 0.5
##
## i'(t) = (beta(t) s(t) - gamma) i(t)
## y(t) = rho N i(t)
## y'(t) = rho N i'(t) = rho N (beta(t)s(t) - gamma) i(t) = (beta(t)s(t) - gamma) y(t)

## DONE YYG+aardvark EW vs. separate

## TODO explain differences YYG+cookies vs.
## TODO doc and improve hayman interface
## TODO density/cdf stacking
## TODO investigate why quantgen ensembles following trend of one of the forecasters in particular? customized/YYG. no. look at comb1 2ahead err

## Aaron used: baseline, ensemble, IHME, MOBS, OliverWyman Navigator, UMass, UT, YYG
## only with missingness: Mobs and OliverWyman only missing non-territory states

## TODO mixed model notebook for MAE, log MAE, cov2080, cov1090
## - multiple aheads... fixed effect for interaction of forecaster and ahead? VS. run separately for each ahead
## - initially: run separately
## - priority 1: mixed model; are differences significant
## - priority 2: Ryan's focusing on particular instances
