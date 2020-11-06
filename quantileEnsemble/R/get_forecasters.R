## This file incorporates content from the template package which was under ../LICENSE-template.

#' @source hayman-ensemble.R
#' @source simple-ensemble.R
NULL

#' Get the list of forecasters provided by this package.
#'
#' @description The evaluator will first call this function with the
#'     parameters shown below to determine all the forecasters
#'     available. It expects to get back a named list of forecasting
#'     functions. If a forecasting function is not available for a
#'     given set of parameters, an `NA` should returned _instead of_ a
#'     function. This tells the evaluator to ignore that forecaster in
#'     a run: it ignores anything that is not a function. See examples below.
#'
#' @param response the response (e.g. "usafacts_deaths_incidence_num")
#' @param incidence_period the incidence period (e.g. "epiweek" for
#'     now, for all forecasters)
#' @param ahead the ahead parameter (e.g. 1, 2, 3, 4)
#' @param forecast_date the date of the forecast
#' @param geo_type the geographic type (e.g "county" or "state" or
#'     "hrr" or "msa"... but for now only the first two),
#' @param n_locations the number of locations (for now we will use 200
#'     for this)
#' @return a named list of forecaster functions with unavailable
#'     forecasters returning `NA` instead of a function
#'
#' @export get_forecasters
get_forecasters  <- function(response, incidence_period = c("epiweek"), ahead, forecast_date,
                             geo_type = c("county", "state", "hrr", "msa"),
                             n_locations = 200) {
    get_dev_forecasters(response, incidence_period, ahead, forecast_date, geo_type, n_locations)[
        c("hayman_ensemble_v0_comb1","simple_ensemble_v0_comb1","flexible_ensemble_v0_comb1")
    ]
}

#' Get list of dev forecasters provided by this package.
#'
#' @description The evaluator will first call this function with the
#'     parameters shown below to determine all the forecasters
#'     available. It expects to get back a named list of forecasting
#'     functions. If a forecasting function is not available for a
#'     given set of parameters, an `NA` should returned _instead of_ a
#'     function. This tells the evaluator to ignore that forecaster in
#'     a run: it ignores anything that is not a function. See examples below.
#'
#' @param response the response (e.g. "usafacts_deaths_incidence_num")
#' @param incidence_period the incidence period (e.g. "epiweek" for
#'     now, for all forecasters)
#' @param ahead the ahead parameter (e.g. 1, 2, 3, 4)
#' @param forecast_date the date of the forecast
#' @param geo_type the geographic type (e.g "county" or "state" or
#'     "hrr" or "msa"... but for now only the first two),
#' @param n_locations the number of locations (for now we will use 200
#'     for this)
#' @return a named list of forecaster functions with unavailable
#'     forecasters returning `NA` instead of a function
#'
#' @export
get_dev_forecasters  <- function(response, incidence_period = c("epiweek"), ahead, forecast_date,
                                 geo_type = c("county", "state", "hrr", "msa"),
                                 n_locations = 200,
                                 repo_root_dirpath=get_repo_root_dirpath()) {
    if (response == "usa-facts_deaths_incidence_num") {
        ## provide old name as alternative name:
        names(response) <- "usafacts_deaths_incidence_num"
    }
    geo_type <- match.arg(geo_type)
    ## TODO add more checks to ensure that the given args can be handled.
    ##
    ## Common (re)naming scheme; some entries are redundant, and is not
    ## necessarily comprehensive:
    common_naming_scheme = c(
      ## "strawman", "strawman1",
      ## "poiszero",
      ## "aardvark"="aardvark_vanilla", "aardvark_vanilla", "aardvark_chocolate", "aardvark_cookies_and_cream",
      ## "stackedLasso"="stackedLasso_customized", "stackedLasso_customized", "stackedLasso_cvquantile",
      ## "YYG-ParamSearch", "LANL-GrowthRate", "COVIDhub-ensemble", "UMass-MechBayes", "UCLA-SuEIR",
      ## "COVIDhub-baseline", "MOBS-GLEAM_COVID", "UT-Mobility", "IHME-CurveFit",
      ## ## The code lines before currently leads to errors, indicating issues
      ## ## with using certain hub components and with using
      ## ## include_all_other_forecasters=TRUE; maybe some hub component names
      ## ## are different in different contexts? (At least some have undergone
      ## ## some sort of renaming.)
      ## ##
      ## ## XXX check what errors this triggers before adding a component to the naming scheme to make sure it's not subject to any of the potential multiple-name issues noted.
      ## get_hub_components_names(repo_root_dirpath),
      character(0L)
    )
    quantgen_cdc_forecasters = c("COVIDhub-baseline", "IHME-CurveFit", "LANL-GrowthRate", "MOBS-GLEAM_COVID", "OliverWyman-Navigator", "UMass-MechBayes", "UT-Mobility", "YYG-ParamSearch")
    all_forecasters = get_hub_components_names(repo_root_dirpath)
    all_forecasters2 = setdiff(all_forecasters,
                               ## hack to remove things that have been renamed causing issues with hayman:
                               c("MOBS_NEU-GLEAM_COVID","MIT_CovidAnalytics-DELPHI","UChicago-CovidIL_10_increase","UChicago-CovidIL_30_increase","MIT_CovAlliance-SIR")
                               )
    inconly_components_dirpath = file.path(repo_root_dirpath, "smallcards", "historical_cdc_components_inconly")
    inconly_forecasters = get_hub_components_names_helper(inconly_components_dirpath)
    comb4 = c("aardvark_cookies_and_cream","poiszero","zyzzyva_covidcast")
    fit_taus = cdc_probs
    fit_taus3 = c(0.2,0.5,0.8)
    tau_groups3 = c(rep(1, 4), rep(2, 15), rep(3, 4))
    calike_locations = strsplit("06;48;12;36;42;17;39;13;37;26;34;51;53;04;25;47;18;29;24;55", ";")[[1L]]
    lapply(
        FUN=function(forecaster_fn) list(forecaster=forecaster_fn, type="ensemble"),
        list(
            hayman_ensemble_v0_comb0fix = hayman_ensemble_forecaster_v0(
                ## NOTE hayman_ensemble_forecaster_v0 behavior changed due to bug; current behavior should be fixed to be consistent with original version, but not intermediate buggy version
                response, incidence_period, ahead, forecast_date, geo_type, n_locations,
                required_forecaster_names=c("strawman"),
                include_all_other_forecasters=FALSE,
                optional_forecaster_names=c("strawman1", "poiszero", "aardvark_vanilla", "aardvark_chocolate"),
                shared_naming_scheme=common_naming_scheme,
                repo_root_dirpath=repo_root_dirpath
            ),
            simple_ensemble_v0_comb0 = quantgen_ensemble_forecaster_v0(
                response, incidence_period, ahead, geo_type, n_locations,
                forecasters=c("strawman", "strawman1", "poiszero", "aardvark", "aardvark_chocolate"),
                tau_groups=rep(1L, length(cdc_probs)),
                repo_root_dirpath=repo_root_dirpath
            ),
            flexible_ensemble_v0_comb0 = quantgen_ensemble_forecaster_v0(
                response, incidence_period, ahead, geo_type, n_locations,
                forecasters=c("strawman", "strawman1", "poiszero", "aardvark", "aardvark_chocolate"),
                tau_groups=seq_along(cdc_probs),
                repo_root_dirpath=repo_root_dirpath
            ),
            hayman_ensemble_v0_comb1 = hayman_ensemble_forecaster_v0(
                response, incidence_period, ahead, forecast_date, geo_type, n_locations,
                required_forecaster_names=c("strawman"),
                include_all_other_forecasters=FALSE,
                optional_forecaster_names=c("strawman1", "poiszero", "aardvark_vanilla", "aardvark_chocolate", "stackedLasso_customized", "stackedLasso_cvquantile"),
                shared_naming_scheme=common_naming_scheme,
                repo_root_dirpath=repo_root_dirpath
            ),
            ## ## Address the naming scheme issues noted above and ensure that this reads components correctly before uncommenting:
            ## hayman_ensemble_v0_comb1pHub = hayman_ensemble_forecaster_v0(
            ##     response, incidence_period, ahead, forecast_date, geo_type, n_locations,
            ##     required_forecaster_names=c("strawman"),
            ##     include_all_other_forecasters=FALSE,
            ##     optional_forecaster_names=c("strawman1", "poiszero", "aardvark_vanilla", "aardvark_chocolate", "stackedLasso_customized","stackedLasso_cvquantile", get_hub_components_names(repo_root_dirpath)),
            ##     shared_naming_scheme=common_naming_scheme,
            ##     repo_root_dirpath=repo_root_dirpath
            ## ),
            simple_ensemble_v0_comb1 = quantgen_ensemble_forecaster_v0(
                response, incidence_period, ahead, geo_type, n_locations,
                forecasters=c("strawman", "strawman1", "poiszero", "aardvark", "aardvark_chocolate", "aardvark_cookies_and_cream", "stackedLasso"="stackedLasso_customized"),
                tau_groups=rep(1L, length(cdc_probs)),
                repo_root_dirpath=repo_root_dirpath
            ),
            flexible_ensemble_v0_comb1 = quantgen_ensemble_forecaster_v0(
                response, incidence_period, ahead, geo_type, n_locations,
                forecasters=c("strawman", "strawman1", "poiszero", "aardvark", "aardvark_chocolate", "aardvark_cookies_and_cream", "stackedLasso"="stackedLasso_customized"),
                tau_groups=seq_along(cdc_probs),
                repo_root_dirpath=repo_root_dirpath
            ),
            hayman_ensemble_v0_comb1pSelectHub = hayman_ensemble_forecaster_v0(
                response, incidence_period, ahead, forecast_date, geo_type, n_locations,
                required_forecaster_names=c("strawman"),
                include_all_other_forecasters=FALSE,
                optional_forecaster_names=c("strawman1", "poiszero", "aardvark_vanilla", "aardvark_chocolate", "stackedLasso_customized","stackedLasso_cvquantile", "YYG-ParamSearch", "LANL-GrowthRate", "COVIDhub-ensemble", "UMass-MechBayes", "UCLA-SuEIR"),
                shared_naming_scheme=common_naming_scheme,
                repo_root_dirpath=repo_root_dirpath
            ),
            ## simple_ensemble_v0_comb1pSelectHub = quantgen_ensemble_forecaster_v0(
            ##     response, incidence_period, ahead, geo_type, n_locations,
            ##     forecasters=c("strawman", "strawman1", "poiszero", "aardvark", "aardvark_chocolate", "aardvark_cookies_and_cream", "stackedLasso"="stackedLasso_customized", "YYG-ParamSearch", "LANL-GrowthRate", "COVIDhub-ensemble", "UMass-MechBayes", "UCLA-SuEIR"),
            ##     tau_groups=rep(1L, length(cdc_probs)),
            ##     repo_root_dirpath=repo_root_dirpath
            ## ),
            ## flexible_ensemble_v0_comb1pSelectHub = quantgen_ensemble_forecaster_v0(
            ##     response, incidence_period, ahead, geo_type, n_locations,
            ##     forecasters=c("strawman", "strawman1", "poiszero", "aardvark", "aardvark_chocolate", "aardvark_cookies_and_cream", "stackedLasso"="stackedLasso_customized", "YYG-ParamSearch", "LANL-GrowthRate", "COVIDhub-ensemble", "UMass-MechBayes", "UCLA-SuEIR"),
            ##     tau_groups=seq_along(cdc_probs),
            ##     repo_root_dirpath=repo_root_dirpath
            ## ),
            hayman_ensemble_v0_selectHub = hayman_ensemble_forecaster_v0(
                response, incidence_period, ahead, forecast_date, geo_type, n_locations,
                required_forecaster_names=c("YYG-ParamSearch"),
                include_all_other_forecasters=FALSE,
                optional_forecaster_names=c("LANL-GrowthRate", "COVIDhub-ensemble", "UMass-MechBayes", "UCLA-SuEIR"),
                shared_naming_scheme=common_naming_scheme,
                repo_root_dirpath=repo_root_dirpath
            ),
            hayman_ensemble_v0_comb2pSelectHub = hayman_ensemble_forecaster_v0(
                response, incidence_period, ahead, forecast_date, geo_type, n_locations,
                required_forecaster_names=c("aardvark_cookies_and_cream"),
                include_all_other_forecasters=FALSE,
                optional_forecaster_names=c("poiszero", "stackedLasso_customized", "YYG-ParamSearch", "LANL-GrowthRate", "COVIDhub-ensemble", "UMass-MechBayes", "UCLA-SuEIR"),
                shared_naming_scheme=common_naming_scheme,
                repo_root_dirpath=repo_root_dirpath
            ),
            hayman_ensemble_v0_comb3pSelectHub = hayman_ensemble_forecaster_v0(
                response, incidence_period, ahead, forecast_date, geo_type, n_locations,
                required_forecaster_names=c("aardvark_cookies_and_cream"),
                include_all_other_forecasters=FALSE,
                optional_forecaster_names=c("stackedLasso_customized", "YYG-ParamSearch", "LANL-GrowthRate", "COVIDhub-ensemble", "UMass-MechBayes", "UCLA-SuEIR"),
                shared_naming_scheme=common_naming_scheme,
                repo_root_dirpath=repo_root_dirpath
            ),
            hayman_ensemble_v0_cookiesAndCreamPSelectHub = hayman_ensemble_forecaster_v0(
                response, incidence_period, ahead, forecast_date, geo_type, n_locations,
                required_forecaster_names=c("aardvark_cookies_and_cream"),
                include_all_other_forecasters=FALSE,
                optional_forecaster_names=c("YYG-ParamSearch", "LANL-GrowthRate", "COVIDhub-ensemble", "UMass-MechBayes", "UCLA-SuEIR"),
                shared_naming_scheme=common_naming_scheme,
                repo_root_dirpath=repo_root_dirpath
            ),
            hayman_ensemble_v0_customizedPSelectHub = hayman_ensemble_forecaster_v0(
                response, incidence_period, ahead, forecast_date, geo_type, n_locations,
                required_forecaster_names=c("stackedLasso_customized"),
                include_all_other_forecasters=FALSE,
                optional_forecaster_names=c("YYG-ParamSearch", "LANL-GrowthRate", "COVIDhub-ensemble", "UMass-MechBayes", "UCLA-SuEIR"),
                shared_naming_scheme=common_naming_scheme,
                repo_root_dirpath=repo_root_dirpath
            ),
            hayman_ensemble_v0_cookiesAndCreamPYYG = hayman_ensemble_forecaster_v0(
                response, incidence_period, ahead, forecast_date, geo_type, n_locations,
                required_forecaster_names=c("aardvark_cookies_and_cream","YYG-ParamSearch"),
                include_all_other_forecasters=FALSE,
                optional_forecaster_names=character(0L),
                shared_naming_scheme=common_naming_scheme,
                repo_root_dirpath=repo_root_dirpath
            ),
            simple_ensemble_v0_cookiesAndCreamPYYG = quantgen_ensemble_forecaster_v0(
                response, incidence_period, ahead, geo_type, n_locations,
                forecasters=c("aardvark_cookies_and_cream", "YYG-ParamSearch"),
                tau_groups=rep(1L, length(cdc_probs)),
                repo_root_dirpath=repo_root_dirpath
            ),
            flexible_ensemble_v0_cookiesAndCreamPYYG = quantgen_ensemble_forecaster_v0(
                response, incidence_period, ahead, geo_type, n_locations,
                forecasters=c( "aardvark_cookies_and_cream", "YYG-ParamSearch"),
                tau_groups=seq_along(cdc_probs),
                repo_root_dirpath=repo_root_dirpath
            ),
            hayman_ensemble_v0_selectHub2 = hayman_ensemble_forecaster_v0(
              response, incidence_period, ahead, forecast_date, geo_type, n_locations,
              required_forecaster_names=c("COVIDhub-baseline"),
              include_all_other_forecasters=FALSE,
              optional_forecaster_names=c("YYG-ParamSearch","MOBS-GLEAM_COVID","UT-Mobility","UMass-MechBayes","IHME-CurveFit","JHU_IDD-CovidSP"),
              shared_naming_scheme=common_naming_scheme,
              repo_root_dirpath=repo_root_dirpath
            ),
            simple_ensemble_v0_selectHub2 = quantgen_ensemble_forecaster_v0(
              response, incidence_period, ahead, geo_type, n_locations,
              forecasters=c("COVIDhub-baseline","YYG-ParamSearch","MOBS-GLEAM_COVID","UT-Mobility","UMass-MechBayes","IHME-CurveFit","JHU_IDD-CovidSP"),
              tau_groups=rep(1L, length(cdc_probs)),
              repo_root_dirpath=repo_root_dirpath
            ),
            flexible_ensemble_v0_selectHub2 = quantgen_ensemble_forecaster_v0(
              response, incidence_period, ahead, geo_type, n_locations,
              forecasters=c("COVIDhub-baseline","YYG-ParamSearch","MOBS-GLEAM_COVID","UT-Mobility","UMass-MechBayes","IHME-CurveFit","JHU_IDD-CovidSP"),
              tau_groups=seq_along(cdc_probs),
              repo_root_dirpath=repo_root_dirpath
            ),
            hayman_ensemble_v0_selectHub3 = hayman_ensemble_forecaster_v0(
              response, incidence_period, ahead, forecast_date, geo_type, n_locations,
              required_forecaster_names=c("COVIDhub-baseline"),
              include_all_other_forecasters=FALSE,
              optional_forecaster_names=c("YYG-ParamSearch","MOBS-GLEAM_COVID"),
              shared_naming_scheme=common_naming_scheme,
              repo_root_dirpath=repo_root_dirpath
            ),
            simple_ensemble_v0_selectHub3 = quantgen_ensemble_forecaster_v0(
              response, incidence_period, ahead, geo_type, n_locations,
              forecasters=c("COVIDhub-baseline","YYG-ParamSearch","MOBS-GLEAM_COVID"),
              tau_groups=rep(1L, length(cdc_probs)),
              repo_root_dirpath=repo_root_dirpath
            ),
            flexible_ensemble_v0_selectHub3 = quantgen_ensemble_forecaster_v0(
              response, incidence_period, ahead, geo_type, n_locations,
              forecasters=c("COVIDhub-baseline","YYG-ParamSearch","MOBS-GLEAM_COVID"),
              tau_groups=seq_along(cdc_probs),
              repo_root_dirpath=repo_root_dirpath
            ),
            hayman_ensemble_v0_selectHub4 = hayman_ensemble_forecaster_v0(
              response, incidence_period, ahead, forecast_date, geo_type, n_locations,
              required_forecaster_names=c("COVIDhub-baseline"),
              include_all_other_forecasters=FALSE,
              optional_forecaster_names=c("YYG-ParamSearch","MOBS-GLEAM_COVID","UT-Mobility","UMass-MechBayes","IHME-CurveFit"),
              shared_naming_scheme=common_naming_scheme,
              repo_root_dirpath=repo_root_dirpath
            ),
            simple_ensemble_v0_selectHub4 = quantgen_ensemble_forecaster_v0(
              response, incidence_period, ahead, geo_type, n_locations,
              forecasters=c("COVIDhub-baseline","YYG-ParamSearch","MOBS-GLEAM_COVID","UT-Mobility","UMass-MechBayes","IHME-CurveFit"),
              tau_groups=rep(1L, length(cdc_probs)),
              repo_root_dirpath=repo_root_dirpath
            ),
            flexible_ensemble_v0_selectHub4 = quantgen_ensemble_forecaster_v0(
              response, incidence_period, ahead, geo_type, n_locations,
              forecasters=c("COVIDhub-baseline","YYG-ParamSearch","MOBS-GLEAM_COVID","UT-Mobility","UMass-MechBayes","IHME-CurveFit"),
              tau_groups=seq_along(cdc_probs),
              repo_root_dirpath=repo_root_dirpath
            ),
            hayman_ensemble_v1_selectHub2 = hayman_ensemble_forecaster_v1(
              response, incidence_period, ahead, forecast_date, geo_type, n_locations,
              required_forecaster_names=c("COVIDhub-baseline"),
              include_all_other_forecasters=FALSE,
              optional_forecaster_names=c("YYG-ParamSearch","MOBS-GLEAM_COVID","UT-Mobility","UMass-MechBayes","IHME-CurveFit","JHU_IDD-CovidSP"),
              shared_naming_scheme=common_naming_scheme,
              repo_root_dirpath=repo_root_dirpath
            ),
            hayman_ensemble_v1_selectHub4 = hayman_ensemble_forecaster_v1(
              response, incidence_period, ahead, forecast_date, geo_type, n_locations,
              required_forecaster_names=c("COVIDhub-baseline"),
              include_all_other_forecasters=FALSE,
              optional_forecaster_names=c("YYG-ParamSearch","MOBS-GLEAM_COVID","UT-Mobility","UMass-MechBayes","IHME-CurveFit"),
              shared_naming_scheme=common_naming_scheme,
              repo_root_dirpath=repo_root_dirpath
            ),
          hayman_ensemble_v1_selectHub5 = hayman_ensemble_forecaster_v1(
            response, incidence_period, ahead, forecast_date, geo_type, n_locations,
            required_forecaster_names=c("COVIDhub-baseline"),
            include_all_other_forecasters=FALSE,
            optional_forecaster_names=c("UMass-MechBayes","IHME-CurveFit","YYG-ParamSearch"),
            shared_naming_scheme=common_naming_scheme,
            repo_root_dirpath=repo_root_dirpath
          ),
          hayman_ensemble_v1_selectHub6 = hayman_ensemble_forecaster_v1(
            response, incidence_period, ahead, forecast_date, geo_type, n_locations,
            required_forecaster_names=c("COVIDhub-baseline"),
            include_all_other_forecasters=FALSE,
            optional_forecaster_names=c("UMass-MechBayes","YYG-ParamSearch","UT-Mobility","LANL-GrowthRate","UCLA-SuEIR"),
            shared_naming_scheme=common_naming_scheme,
            repo_root_dirpath=repo_root_dirpath
          ),
          hayman_ensemble_v1_selectHub7 = hayman_ensemble_forecaster_v1(
            response, incidence_period, ahead, forecast_date, geo_type, n_locations,
            required_forecaster_names=character(0L),
            include_all_other_forecasters=FALSE,
            optional_forecaster_names=c("UMass-MechBayes","YYG-ParamSearch","UT-Mobility","LANL-GrowthRate","UCLA-SuEIR"),
            shared_naming_scheme=common_naming_scheme,
            repo_root_dirpath=repo_root_dirpath
         )
       , ensemble3_cdc_impute_dup = quantgen_ensemble_forecaster_v0(
           response, incidence_period, ahead, geo_type, n_locations,
           repo_root_dirpath = repo_root_dirpath,
           forecasters=quantgen_cdc_forecasters, tau_groups=tau_groups3, impute_missing=TRUE
         )
       , ensemble3_cdc_impute_calike = quantgen_ensemble_forecaster_v0(
           response, incidence_period, ahead, geo_type, n_locations,
           repo_root_dirpath = repo_root_dirpath,
           forecasters=quantgen_cdc_forecasters, tau_groups=tau_groups3, impute_missing=TRUE,
           training_locations_considered=calike_locations
         )
       , ensemble1_cdc_dup = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                             repo_root_dirpath = repo_root_dirpath,
                                                             forecasters=quantgen_cdc_forecasters, tau_groups=rep(1,23), impute_missing = FALSE,
                                                             fit_taus = fit_taus)
       , ensemble3_cdc_dup = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                             repo_root_dirpath = repo_root_dirpath,
                                                             forecasters=quantgen_cdc_forecasters, tau_groups=tau_groups3, impute_missing = FALSE,
                                                             fit_taus = fit_taus)
       , ensemble23_cdc_dup = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                              repo_root_dirpath = repo_root_dirpath,
                                                              forecasters=quantgen_cdc_forecasters, tau_groups=seq_len(23L), impute_missing = FALSE,
                                                              fit_taus = fit_taus)
       , ensemble3train_cdc_dup = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                                  repo_root_dirpath = repo_root_dirpath,
                                                                  forecasters=quantgen_cdc_forecasters, tau_groups=rep(1,3), impute_missing = FALSE,
                                                                  fit_taus = fit_taus3)
       , ensemble1_cdc_impute = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                                repo_root_dirpath = repo_root_dirpath,
                                                                forecasters=quantgen_cdc_forecasters, tau_groups=rep(1,23), impute_missing = TRUE,
                                                                fit_taus = fit_taus)
       , ensemble3_cdc_impute = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                                repo_root_dirpath = repo_root_dirpath,
                                                                forecasters=quantgen_cdc_forecasters, tau_groups=tau_groups3, impute_missing = TRUE,
                                                                fit_taus = fit_taus)
       , ensemble23_cdc_impute = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                                 repo_root_dirpath = repo_root_dirpath,
                                                                 forecasters=quantgen_cdc_forecasters, tau_groups=seq_len(23L), impute_missing = TRUE,
                                                                 fit_taus = fit_taus)
       , ensemble3train_cdc_impute = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                                     repo_root_dirpath = repo_root_dirpath,
                                                                     forecasters=quantgen_cdc_forecasters, tau_groups=rep(1,3), impute_missing = TRUE,
                                                                     fit_taus = fit_taus3)
       , ensemble1_cdc_all_dup = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                                 repo_root_dirpath = repo_root_dirpath,
                                                                 forecasters=all_forecasters, tau_groups=rep(1,23), impute_missing = TRUE,
                                                                 fit_taus = fit_taus)
       , ensemble3_cdc_all_dup = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                                 repo_root_dirpath = repo_root_dirpath,
                                                                 forecasters=all_forecasters, tau_groups=tau_groups3, impute_missing = TRUE,
                                                                 fit_taus = fit_taus)
       , ensemble23_cdc_all_dup = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                                  repo_root_dirpath = repo_root_dirpath,
                                                                  forecasters=all_forecasters, tau_groups=seq_len(23L), impute_missing = TRUE,
                                                                  fit_taus = fit_taus)
       , ensemble3train_cdc_all_dup = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                                      repo_root_dirpath = repo_root_dirpath,
                                                                      forecasters=all_forecasters, tau_groups=rep(1,3), impute_missing = TRUE,
                                                                      fit_taus = fit_taus3)
       , cdf_ens_v0_selectHub6 = cdf_stacking_forecaster_v0(
           response, incidence_period, ahead, geo_type, n_locations,
           panterpolate_cdf = panterpolate_raw_jump_cdf,
           component_forecaster_names = c("COVIDhub-baseline", "UMass-MechBayes", "YYG-ParamSearch", "UT-Mobility", "LANL-GrowthRate", "UCLA-SuEIR"),
           repo_root_dirpath=repo_root_dirpath
         )
       , rwt_cdf_ens_v0_selhub6 = cdf_stacking_forecaster_v0(
           response, incidence_period, ahead, geo_type, n_locations,
           panterpolate_cdf = panterpolate_reweight_bad_jump_cdf,
           component_forecaster_names = c("COVIDhub-baseline", "UMass-MechBayes", "YYG-ParamSearch", "UT-Mobility", "LANL-GrowthRate", "UCLA-SuEIR"),
           repo_root_dirpath=repo_root_dirpath
         )
       , rwt_noexpand_cdf_ens_v0_selhub6 = cdf_stacking_forecaster_v0(
           response, incidence_period, ahead, geo_type, n_locations,
           panterpolate_cdf = panterpolate_reweight_bad_jump_cdf,
           prerange.proportional.extension=0,
           component_forecaster_names = c("COVIDhub-baseline", "UMass-MechBayes", "YYG-ParamSearch", "UT-Mobility", "LANL-GrowthRate", "UCLA-SuEIR"),
           repo_root_dirpath=repo_root_dirpath
         )
       , rwt_cdf_ens_v0_selhub6_tr1 = cdf_stacking_forecaster_v0(
           response, incidence_period, ahead, geo_type, n_locations,
           panterpolate_cdf = panterpolate_reweight_bad_jump_cdf,
           n_training_dates = 1L, n_required_instances_per_forecaster=5,
           component_forecaster_names = c("COVIDhub-baseline", "UMass-MechBayes", "YYG-ParamSearch", "UT-Mobility", "LANL-GrowthRate", "UCLA-SuEIR"),
           repo_root_dirpath=repo_root_dirpath
         )
       , hayman_ens_v1_selhub8 = hayman_ensemble_forecaster_v1(
           response, incidence_period, ahead, forecast_date, geo_type, n_locations,
           required_forecaster_names = character(0L),
           include_all_other_forecasters = FALSE,
           optional_forecaster_names = quantgen_cdc_forecasters,
           shared_naming_scheme = common_naming_scheme,
           repo_root_dirpath = repo_root_dirpath
         )
       , cdf_raw_ens_v0_selhub8 = cdf_stacking_forecaster_v0(
           response, incidence_period, ahead, geo_type, n_locations,
           panterpolate_cdf = panterpolate_raw_jump_cdf,
           component_forecaster_names = quantgen_cdc_forecasters,
           repo_root_dirpath=repo_root_dirpath
         )
       , cdf_rwt_ens_v0_selhub8 = cdf_stacking_forecaster_v0(
           response, incidence_period, ahead, geo_type, n_locations,
           panterpolate_cdf = panterpolate_reweight_bad_jump_cdf,
           component_forecaster_names = quantgen_cdc_forecasters,
           repo_root_dirpath=repo_root_dirpath
         )
       , cdf_rwt_ens_v0_selhub8_t2 = cdf_stacking_forecaster_v0(
           response, incidence_period, ahead, geo_type, n_locations,
           panterpolate_cdf = panterpolate_reweight_bad_jump_cdf,
           n_training_dates = 2L,
           component_forecaster_names = quantgen_cdc_forecasters,
           repo_root_dirpath=repo_root_dirpath
         )
       , cdf_rwt_ens_v0_selhub8_t1 = cdf_stacking_forecaster_v0(
           response, incidence_period, ahead, geo_type, n_locations,
           panterpolate_cdf = panterpolate_reweight_bad_jump_cdf,
           n_training_dates = 1L,
           component_forecaster_names = quantgen_cdc_forecasters,
           repo_root_dirpath=repo_root_dirpath
         )
       , cdf_rwt2_ens_v0_selhub8 = cdf_stacking_forecaster_v0(
           response, incidence_period, ahead, geo_type, n_locations,
           panterpolate_cdf = panterpolate_reweight2_jump_cdf,
           component_forecaster_names = quantgen_cdc_forecasters,
           repo_root_dirpath=repo_root_dirpath
         )
       , cdf_rwt2imp_ens_v0_selhub8 = cdf_stacking_forecaster_v2(
           response, incidence_period, ahead, geo_type, n_locations,
           panterpolate_cdf = panterpolate_reweight2_jump_cdf,
           component_forecaster_names = quantgen_cdc_forecasters,
           repo_root_dirpath=repo_root_dirpath
         )
       , cdf_rwt2_ens_v0_selhub8_v1pre = cdf_stacking_forecaster_v1pre(
           response, incidence_period, ahead, geo_type, n_locations,
           panterpolate_cdf = panterpolate_reweight2_jump_cdf,
           component_forecaster_names = quantgen_cdc_forecasters,
           repo_root_dirpath=repo_root_dirpath
         )
       , cdf_rwt2_ens_v0_selhub8_v1 = cdf_stacking_forecaster_v1(
           response, incidence_period, ahead, geo_type, n_locations,
           panterpolate_cdf = panterpolate_reweight2_jump_cdf,
           component_forecaster_names = quantgen_cdc_forecasters,
           repo_root_dirpath=repo_root_dirpath
         )
       , ensemble3_comb4 = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                           forecasters=comb4, tau_groups=tau_groups3, impute_missing = FALSE,
                                                           fit_taus = fit_taus)
       , hayman_ens_v1_comb4 = hayman_ensemble_forecaster_v1(
           response, incidence_period, ahead, forecast_date, geo_type, n_locations,
           required_forecaster_names = character(0L),
           include_all_other_forecasters = FALSE,
           optional_forecaster_names = comb4,
           shared_naming_scheme = common_naming_scheme,
           repo_root_dirpath = repo_root_dirpath
         )
       , cdf_rwt2_ens_v1_comb4 = cdf_stacking_forecaster_v1(
           response, incidence_period, ahead, geo_type, n_locations,
           panterpolate_cdf = panterpolate_reweight2_jump_cdf,
           component_forecaster_names = comb4,
           repo_root_dirpath=repo_root_dirpath
         )
       , ensemble3_comb4_impute = quantgen_ensemble_forecaster_v0(
           response, incidence_period, ahead, geo_type, n_locations,
           forecasters=comb4, tau_groups=tau_groups3, impute_missing=TRUE
         )
       , cdf_rwt2_ens_v2_comb4 = cdf_stacking_forecaster_v2(
           response, incidence_period, ahead, geo_type, n_locations,
           panterpolate_cdf = panterpolate_reweight2_jump_cdf,
           component_forecaster_names = comb4,
           repo_root_dirpath=repo_root_dirpath
         )
       , qspace_ew_md_ens_v1_selhub8 = untrained_qspace_ensemble_forecaster_v1(
           response, incidence_period, ahead, forecast_date, geo_type, n_locations,
           ensemble_function = median,
           required_forecaster_names = character(0L),
           include_all_other_forecasters = FALSE,
           optional_forecaster_names = quantgen_cdc_forecasters,
           shared_naming_scheme = common_naming_scheme,
           repo_root_dirpath = repo_root_dirpath
         )
       , qspace_ew_md_ens_v1_all = untrained_qspace_ensemble_forecaster_v1(
           response, incidence_period, ahead, forecast_date, geo_type, n_locations,
           ensemble_function = median,
           required_forecaster_names = character(0L),
           include_all_other_forecasters = FALSE,
           optional_forecaster_names = all_forecasters2,
           shared_naming_scheme = common_naming_scheme,
           repo_root_dirpath = repo_root_dirpath
         )
       , qspace_ew_md_ens_v1_inconly = untrained_qspace_ensemble_forecaster_v1(
           response, incidence_period, ahead, forecast_date, geo_type, n_locations,
           ensemble_function = median,
           required_forecaster_names = character(0L),
           include_all_other_forecasters = FALSE,
           optional_forecaster_names = inconly_forecasters,
           shared_naming_scheme = common_naming_scheme,
           repo_root_dirpath = repo_root_dirpath,
           hub_components_dirpath = inconly_components_dirpath
         )
       , ensemble1_cdc_inconly = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                                 repo_root_dirpath = repo_root_dirpath,
                                                                 forecasters=inconly_forecasters, tau_groups=rep(1,23), impute_missing = TRUE,
                                                                 fit_taus = fit_taus,
                                                                 debug_weights_folder = file.path(repo_root_dirpath, "debug_quantgen_weights", "ensemble1_cdc_inconly"))
       , ensemble3_cdc_inconly = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                                 repo_root_dirpath = repo_root_dirpath,
                                                                 forecasters=inconly_forecasters, tau_groups=tau_groups3, impute_missing = TRUE,
                                                                 fit_taus = fit_taus,
                                                                 debug_weights_folder = file.path(repo_root_dirpath, "debug_quantgen_weights", "ensemble3_cdc_inconly"))
       , ensemble23_cdc_inconly = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                                  repo_root_dirpath = repo_root_dirpath,
                                                                  forecasters=inconly_forecasters, tau_groups=seq_len(23L), impute_missing = TRUE,
                                                                  fit_taus = fit_taus)
       , ensemble3train_cdc_inconly = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                                      repo_root_dirpath = repo_root_dirpath,
                                                                      forecasters=inconly_forecasters, tau_groups=rep(1,3), impute_missing = TRUE,
                                                                      fit_taus = fit_taus3)
       , ensemble1_cdc_all_dup_re = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                                    repo_root_dirpath = repo_root_dirpath,
                                                                    forecasters=all_forecasters2, tau_groups=rep(1,23), impute_missing = TRUE,
                                                                    fit_taus = fit_taus)
       , ensemble3_cdc_all_dup_re = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                                    repo_root_dirpath = repo_root_dirpath,
                                                                    forecasters=all_forecasters2, tau_groups=tau_groups3, impute_missing = TRUE,
                                                                    fit_taus = fit_taus)
       , ensemble23_cdc_all_dup_re = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                                     repo_root_dirpath = repo_root_dirpath,
                                                                     forecasters=all_forecasters2, tau_groups=seq_len(23L), impute_missing = TRUE,
                                                                     fit_taus = fit_taus)
       , ensemble3train_cdc_all_dup_re = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                                         repo_root_dirpath = repo_root_dirpath,
                                                                         forecasters=all_forecasters2, tau_groups=rep(1,3), impute_missing = TRUE,
                                                                         fit_taus = fit_taus3)
       , ensemble3_cdc_inconly_testdate = quantgen_ensemble_forecaster_v0(response, incidence_period, ahead, geo_type, n_locations,
                                                                          repo_root_dirpath = repo_root_dirpath,
                                                                          forecasters=inconly_forecasters, tau_groups=tau_groups3, impute_missing = TRUE,
                                                                          fit_taus = fit_taus,
                                                                          cheating_fit_on_test_date_instead=TRUE)
       , qspace_ew_mean_ens_v1_selhub8 = untrained_qspace_ensemble_forecaster_v1(
           response, incidence_period, ahead, forecast_date, geo_type, n_locations,
           ensemble_function = mean,
           required_forecaster_names = character(0L),
           include_all_other_forecasters = FALSE,
           optional_forecaster_names = quantgen_cdc_forecasters,
           shared_naming_scheme = common_naming_scheme,
           repo_root_dirpath = repo_root_dirpath
         )
       , qspace_ew_mean_ens_v1_all = untrained_qspace_ensemble_forecaster_v1(
           response, incidence_period, ahead, forecast_date, geo_type, n_locations,
           ensemble_function = mean,
           required_forecaster_names = character(0L),
           include_all_other_forecasters = FALSE,
           optional_forecaster_names = all_forecasters2,
           shared_naming_scheme = common_naming_scheme,
           repo_root_dirpath = repo_root_dirpath
         )
       , qspace_ew_mean_ens_v1_inconly = untrained_qspace_ensemble_forecaster_v1(
           response, incidence_period, ahead, forecast_date, geo_type, n_locations,
           ensemble_function = mean,
           required_forecaster_names = character(0L),
           include_all_other_forecasters = FALSE,
           optional_forecaster_names = inconly_forecasters,
           shared_naming_scheme = common_naming_scheme,
           repo_root_dirpath = repo_root_dirpath,
           hub_components_dirpath = inconly_components_dirpath
         )
        )
    )
}
