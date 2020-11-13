
#' @source path-utils.R
#' @source dt-utils.R
#' @source validate.R
#' @source common-ensemble-settings.R
NULL

#' @importFrom data.table data.table set fcoalesce
find_candidate_card_paths_dt <- function(forecasts_dirpath, naming_scheme, file_basename="out.RDS") {
    ## Turn naming_scheme from terse form into a recoding key, with no missing names:
    if (is.null(names(naming_scheme))) {
        names(naming_scheme) <- naming_scheme
    } else {
        names(naming_scheme)[names(naming_scheme)==""] <- naming_scheme[names(naming_scheme)==""]
    }
    candidate_predcard_partial_path_pattern = sprintf("^(\\d+-\\d{2}-\\d{2})/(\\d+)/([-\\w]+)/([-\\w]+)/([-\\w]+)/(\\d+)/([-\\w]+)/%s$", file_basename)
    ## \\1 forecast_date, \\2 ahead, \\3 response, \\4 geo_type, \\5 incidence_period, \\6 n_locations, \\7 forecaster
    all_predcard_partial_paths = list.files(forecasts_dirpath, recursive=TRUE)
    candidate_predcard_partial_paths = grep(value=TRUE, candidate_predcard_partial_path_pattern, all_predcard_partial_paths, perl=TRUE)
    candidate_predcard_full_paths = file.path(forecasts_dirpath, candidate_predcard_partial_paths)
    candidate_paths_dt = data.table(
        full_path = candidate_predcard_full_paths,
        ## xxx vs. using regmatches and regexec:
        response         =            sub(candidate_predcard_partial_path_pattern, "\\3", candidate_predcard_partial_paths, perl=TRUE),
        incidence_period =            sub(candidate_predcard_partial_path_pattern, "\\5", candidate_predcard_partial_paths, perl=TRUE),
        ahead            = as.integer(sub(candidate_predcard_partial_path_pattern, "\\2", candidate_predcard_partial_paths, perl=TRUE)),
        forecast_date    = as.Date   (sub(candidate_predcard_partial_path_pattern, "\\1", candidate_predcard_partial_paths, perl=TRUE)),
        geo_type         =            sub(candidate_predcard_partial_path_pattern, "\\4", candidate_predcard_partial_paths, perl=TRUE),
        n_locations      = as.integer(sub(candidate_predcard_partial_path_pattern, "\\6", candidate_predcard_partial_paths, perl=TRUE)),
        forecaster_name  =            sub(candidate_predcard_partial_path_pattern, "\\7", candidate_predcard_partial_paths, perl=TRUE)
    )
    set(candidate_paths_dt,,"forecaster_name", fcoalesce(unname(naming_scheme[candidate_paths_dt[["forecaster_name"]]]), candidate_paths_dt[["forecaster_name"]]))
    candidate_paths_dt
}

#' Read prediction or score cards for all times and components
#'
#' All character arguments are meant to be actual values rather than bash globs.
#' \code{forecast_dates} can hold multiple values,  while others are expected to
#' be single values.
#'
#' @examples
#' read_forecasts_dt("jhu-csse_deaths_incidence_num","epiweek",2L,"2020-04-06","state",200L,
#'                   get_historical_components_dirpath(), c("strawman"))
#'
#' read_forecasts_dt("jhu-csse_deaths_incidence_num","epiweek",2L,"2020-06-22","state",200L,
#'                   get_historical_components_dirpath(), c("aardvark"="aardvark_vanilla","aardvark_vanilla","poiszero"))
#'
#' read_forecasts_dt("jhu-csse_deaths_incidence_num","epiweek",2L,"2020-04-06","state",200L,
#'                   get_historical_components_dirpath(), c("bogus1"="bogus1_renamed","bogus2","bogus2_altname"="bogus2"))
#'
#' read_forecasts_dt("jhu-csse_deaths_incidence_num","epiweek",2L,"2020-06-22","state",200L,
#'                   get_prospective_components_dirpath(), c("aardvark"="aardvark_vanilla","aardvark_vanilla","poiszero"))
#'
#' read_forecasts_dt("jhu-csse_deaths_incidence_num","epiweek",2L,"2020-06-22","state",200L,
#'                   get_hub_components_dirpath(), character(0L),
#'                   file_basename="scorecard.RDS")
#'
#' @importFrom data.table data.table as.data.table set .SD
#' @export
read_cards_dt <- function(candidate_paths_dt,
                          response, incidence_period, ahead, forecast_dates, geo_type, n_locations,
                          expect_found_forecaster_names=character(0L),
                          only_read_forecaster_names=NULL) {
    forecast_dates <- as.Date(forecast_dates)
    expect_found_forecaster_names <- unique(expect_found_forecaster_names)
    expect_found_names_are_present = expect_found_forecaster_names %in% unique(candidate_paths_dt[["forecaster_name"]])
    names(expect_found_names_are_present) <- expect_found_forecaster_names
    if (!all(expect_found_names_are_present)) {
        print(expect_found_names_are_present)
        print(unique(candidate_paths_dt[["forecaster_name"]]))
        stop (sprintf('Not all names expected to be found were encountered.  Name check results:\n%s\nUnique names encountered:\n%s',
                      paste(collapse="\n", capture.output(print(expect_found_names_are_present))),
                      paste(collapse="\n", capture.output(print(unique(candidate_paths_dt[["forecaster_name"]]))))))
    }
    response_aliases = c(response, names(response))
    selected_paths_dt = candidate_paths_dt[,.SD[
        .SD[["response"]]       %in% ..response_aliases &
        .SD[["incidence_period"]] == ..incidence_period &
        .SD[["ahead"]]            == ..ahead &
        .SD[["forecast_date"]]  %in% ..forecast_dates &
        .SD[["geo_type"]]         == ..geo_type &
        .SD[["n_locations"]]      == ..n_locations &
        (is.null(only_read_forecaster_names) | .SD[["forecaster_name"]] %in% only_read_forecaster_names)
    ]]
    selected_predcard_full_paths = selected_paths_dt[["full_path"]]
    predcard_dts = lapply(selected_predcard_full_paths, function(filepath) {
        object = readRDS(filepath)
        if (!is.data.frame(object) && length(object) == 1L && is.na(object)) {
            NULL
        } else {
            predcard_dt = as.data.table(object)
            ## some cards have location as class "character", others as
            ## "factor"; convert latter to former:
            set(predcard_dt,, "location", as.character(predcard_dt[["location"]]))
            set(predcard_dt,, "forecast_distribution_dt", lapply(predcard_dt[["forecast_distribution"]], as.data.table))
            set(predcard_dt,, "forecast_distribution", NULL)
            predcard_dt
        }
    })
    predcards_dt = data.table(
        forecast_date   = selected_paths_dt[["forecast_date"]],
        forecaster_name = selected_paths_dt[["forecaster_name"]],
        predcard_dt     = predcard_dts,
        key = c("forecast_date", "forecaster_name")
    )
    validate_predcards_dt(predcards_dt)
    visible_dt(predcards_dt)
}

## TODO allow wildcards (maybe wildcard class) in `ahead`, etc., args, and add columns to show such/all args in the output

#' Read backoff component predcards/scorecards into a dt
#'
#' Do not use with untrusted argument values, as this may allow reading of files
#' from  arbitrary locations.  All character  arguments are  meant to  be actual
#' values  rather  than  bash  globs. \code{forecast_dates}  can  hold  multiple
#' values, while most others are expected to be single values.
#'
#' Here "backoff" refers  to using historical component data if  available for a
#' given forecast_date and forecaster_name,  otherwise the prospective component
#' data if available.
#'
#' @examples
#' read_backoff_ensemble_components_dt("jhu-csse_deaths_incidence_num","epiweek",2L,"2020-06-22","state",200L,
#'                                                c("aardvark"="aardvark_vanilla","aardvark_vanilla","poiszero"))
#'
#' read_backoff_ensemble_components_dt("usafacts_deaths_incidence_num","epiweek",2L,"2020-06-22","county",200L,
#'                                                c("aardvark"="aardvark_vanilla","aardvark_vanilla","poiszero"))
#'
#' @importFrom data.table rbindlist set
read_backoff_ensemble_components_dt <- function(response, incidence_period, ahead, forecast_dates, geo_type, n_locations,
                                                shared_naming_scheme = character(0L),
                                                only_read_forecaster_names = NULL,
                                                file_basename = "out.RDS",
                                                repo_root_dirpath = get_repo_root_dirpath(),
                                                historical_components_dirpath=get_historical_components_dirpath(repo_root_dirpath),
                                                prospective_components_dirpath=get_prospective_components_dirpath(repo_root_dirpath),
                                                hub_components_dirpath=get_hub_components_dirpath(repo_root_dirpath),
                                                historical_naming_scheme=shared_naming_scheme,
                                                prospective_naming_scheme=shared_naming_scheme,
                                                hub_naming_scheme=shared_naming_scheme) {
    forecast_dates <- as.Date(forecast_dates)
    ## xxx tag these based on being historical or prospective or hub?
    historical_candidate_paths_dt = find_candidate_card_paths_dt(historical_components_dirpath, historical_naming_scheme, file_basename)
    prospective_candidate_paths_dt = find_candidate_card_paths_dt(prospective_components_dirpath, prospective_naming_scheme, file_basename)
    hub_candidate_paths_dt = find_candidate_card_paths_dt(hub_components_dirpath, hub_naming_scheme, file_basename)
    backoff_candidate_paths_dt = Reduce(x=list(historical_candidate_paths_dt, prospective_candidate_paths_dt, hub_candidate_paths_dt),
                                        right=FALSE,
                                        f=function(agg_dt, elt_dt) {
                                            rbind(agg_dt, elt_dt[!agg_dt, on=c("forecast_date","forecaster_name")])
                                        })
    backoff_selected_cards_dt = read_cards_dt(backoff_candidate_paths_dt,
                                              response, incidence_period, ahead, forecast_dates, geo_type, n_locations,
                                              expect_found_forecaster_names = c(only_read_forecaster_names,
                                                                                historical_naming_scheme, prospective_naming_scheme, hub_naming_scheme),
                                              only_read_forecaster_names = only_read_forecaster_names)
    return (backoff_selected_cards_dt)
}

## FIXME no need to read all the data if not going to use it

#' Filter predcards_dt to rows for the given test_forecast_date
#'
#' @examples
#' library("pipeR")
#' read_backoff_ensemble_components_dt("jhu-csse_deaths_incidence_num","epiweek",2L,"2020-04-06","state",200L,c("aardvark"="aardvark_vanilla","stackedLasso"="stackedLasso_customized"),c("strawman","poiszero","aardvark_vanilla","stackedLasso_customized")) %>>%
#'   testtime_ensemble_components_dt("2020-04-06") %>>%
#'   {.}
#'
#' @importFrom data.table data.table
testtime_ensemble_components_dt <- function(predcards_dt, test_forecast_date) {
    validate_predcards_dt(predcards_dt)
    if (length(test_forecast_date) > 1L) {
        stop ('test_forecast_date must be length 1')
    }
    test_forecast_date <- as.Date(test_forecast_date)
    ## result = predcards_dt[list(test_forecast_date), !"forecast_date", on="forecast_date", nomatch=NA]
    result = predcards_dt[list(test_forecast_date), on="forecast_date", nomatch=NA]
    ## TODO check for nonempty result?
    return (visible_dt(result))
}

#' @examples
#' read_backoff_ensemble_components_dt("jhu-csse_deaths_incidence_num","epiweek",2L,"2020-04-06","state",200L,
#'                                     c("aardvark"="aardvark_vanilla","stackedLasso"="stackedLasso_customized"),
#'                                     c("strawman","poiszero","aardvark_vanilla","stackedLasso_customized")) %>>%
#'   testtime_ensemble_components_dt("2020-04-06") %>>%
#'   desired_components_predcards_dt_or_feedback(FALSE, required_forecaster_names=c("strawman","aarkvark_vanilla"), FALSE, c("stackedLasso_customized")) %>>%
#'   cat()
desired_components_predcards_dt_or_feedback <- function(predcards_dt,
                                                        complete_output,
                                                        required_forecaster_names, include_all_other_forecasters, optional_forecaster_names=character(0L)) {
  validate_predcards_dt(predcards_dt)
  desired_dt_rows_or_feedback(predcards_dt,
                              c("forecast_date"),
                              function(DT) !vapply(DT[["predcard_dt"]], is.null, logical(1L)),
                              "balk", complete_output, required_forecaster_names, include_all_other_forecasters, optional_forecaster_names)
}

#' @examples
#'
#' library("pipeR")
#'
#' read_backoff_ensemble_components_dt("jhu-csse_deaths_incidence_num", "epiweek", 1, as.Date("2020-04-06"), "state", 200L, character(0L), only_read_forecaster_names=c("poiszero","IHME-CurveFit")) %>>% desired_components_predcards_dt_or_feedback(FALSE,c("poiszero","IHME-CurveFit"),FALSE) %>>% unnest_dt_column_in_dt("predcard_dt") %>>% {.[,list(forecaster_name,location,sapply(.$forecast_distribution,function(df)sum(!is.na(df$quantiles))))]} %>>% (V3)
#'
#' read_backoff_ensemble_components_dt("jhu-csse_deaths_incidence_num", "epiweek", 1, as.Date("2020-04-06"), "state", 200L, character(0L), only_read_forecaster_names=c("poiszero","IHME-CurveFit")) %>>% desired_components_predcards_dt_or_feedback(FALSE,c("poiszero","IHME-CurveFit"),FALSE) %>>% unnest_dt_column_in_dt("predcard_dt") %>>% desired_components_forecast_distribution_dt_or_feedback(FALSE,c("poiszero"),TRUE)
desired_components_forecast_distribution_dt_or_feedback <- function(unnested_dt,
                                                                    complete_output,
                                                                    required_forecaster_names, include_all_other_forecasters, optional_forecaster_names=character(0L)) {
  ## XXX assumes that desired locations and probs each appear at least once in
  ## required forecasters / in all forecasters, and that extraneous values never
  ## appear; should improve interface.
  desired_dt_rows_or_feedback(unnested_dt,
                              c("forecast_date", "location"),
                              function(DT) {
                                vapply(DT[["forecast_distribution_dt"]], FUN.VALUE=logical(1L), function(forecast.distribution.dt) {
                                  if (is.null(forecast.distribution.dt)) {
                                    FALSE
                                  } else if (!identical(round(forecast.distribution.dt[["probs"]], 6L),
                                                        round(cdc_probs,                           6L))) {
                                    stop ('expected to see probs identical to cdc_probs after rounding to 6 decimal places, but did not')
                                  } else {
                                    !any(is.na(forecast.distribution.dt[["quantiles"]]))
                                  }
                                })
                              },
                              "drop", complete_output, required_forecaster_names, include_all_other_forecasters, optional_forecaster_names)
}

#' @details not currently used
#' @examples
#' read_backoff_ensemble_components_dt("jhu-csse_deaths_incidence_num", "epiweek", 1, as.Date("2020-04-06"), "state", 200L, character(0L), only_read_forecaster_names=c("poiszero","IHME-CurveFit")) %>>% desired_components_predcards_dt_or_feedback(FALSE,c("poiszero","IHME-CurveFit"),FALSE) %>>% unnest_dt_column_in_dt("predcard_dt") %>>% unnest_dt_column_in_dt("forecast_distribution_dt") %>>% desired_components_quantiles_dt_or_feedback(FALSE,c("IHME-CurveFit"),TRUE)
desired_components_quantiles_dt_or_feedback <- function(unnested_dt,
                                                        complete_output,
                                                        required_forecaster_names, include_all_other_forecasters, optional_forecaster_names=character(0L)) {
  ## XXX assumes that desired locations and probs each appear at least once in
  ## required forecasters / in all forecasters, and that extraneous values never
  ## appear; should improve interface.
  desired_dt_rows_or_feedback(unnested_dt,
                              c("forecast_date", "location", "probs"),
                              function(DT) !is.na(DT[["quantiles"]]),
                              "drop", complete_output, required_forecaster_names, include_all_other_forecasters, optional_forecaster_names)
}

#' @importFrom stats setNames
#' @importFrom data.table CJ
desired_dt_rows_or_feedback <- function(instances_dt,
                                        instance_index_colnames, rows_are_valid,
                                        missing_action=c("balk","drop"), complete_output,
                                        required_forecaster_names, include_all_other_forecasters, optional_forecaster_names=character(0L)) {
    missing_action <- match.arg(missing_action)
    required_indices_dt = do.call(CJ, c(lapply(setNames(instance_index_colnames, instance_index_colnames),
                                               function(colname) unique(instances_dt[[colname]])),
                                        list(forecaster_name = required_forecaster_names)))
    available_instances_dt = instances_dt[rows_are_valid(instances_dt)]
    missing_indices_dt = required_indices_dt[!available_instances_dt, on=names(required_indices_dt)]
    trimmed_instances_dt =
      switch(missing_action,
             balk=
               if (nrow(missing_indices_dt) != 0L) {
                 return (sprintf('Some required instances were missing or invalid:\n%s\n',
                                 paste(capture.output(print(missing_indices_dt)), collapse="\n")))
               } else {
                 available_instances_dt
               },
             drop=
               available_instances_dt[!missing_indices_dt, on=instance_index_colnames]
             )
    selected_instances_dt =
        if (include_all_other_forecasters) {
            trimmed_instances_dt
        } else {
            trimmed_instances_dt[list(c(required_forecaster_names, optional_forecaster_names)), on="forecaster_name", nomatch=NULL]
        }
    if (!complete_output) {
        return (selected_instances_dt)
    } else {
        completed_indices_dt = do.call(CJ, c(lapply(setNames(instance_index_colnames, instance_index_colnames),
                                                    function(colname) unique(selected_instances_dt[[colname]])),
                                             list(forecaster_name = selected_instances_dt[["forecaster_name"]])))
        completed_instances_dt = selected_instances_dt[completed_indices_dt, on=names(completed_indices_dt), nomatch=NA]
        return (completed_instances_dt)
    }
    ## TODO require at least one instance per date (in case none in particular
    ## are specifically required and all are unavailable some date)
}

## TODO checking availability before reading files

## TODO desired components factoring in location availability

## todo turn some of examples into tests

#' Convert quantile levels to importance weights to assign to quantiles (in a particular way)
#'
#' This is  a utility  function used  when approximating  the distribution  of a
#' random variable by a pmf based on a fixed selection of quantiles. It converts
#' the  quantile  levels  into  importance  sampling  weights  (more  precisely,
#' sampling probabilities / pmf y-values, as the output is normalized) to attach
#' to the quantiles (pdf x-values).
#'
#' There  are multiple  ways to  go about  this task.  This approach appears to satisfy two desirable properties:
#' (a) \code{all.equal(result, rev(result))} if \code{all.equal(taus,rev(1-taus))}, and (b)
#'
#' ```{r,   eval=FALSE}
#' all.equal(q, weighted_quantile_type1(q, quantile_level_sample_weights_alt(taus), taus))
#' ```
#'
#' for all tested \code{q} and \code{taus}.
#'
#' @examples
#'
#' quantile_level_sample_weights(c(0.05,0.1,0.3,0.5,0.7,0.9,0.95))
#'
#' library("pipeR")
#'
#' set.seed(42L)
#' taus = runif(100L) %>>% c(1-.) %>>% sort()
#' all.equal(taus, rev(1-taus))
#' all.equal(quantile_level_sample_weights(taus), rev(quantile_level_sample_weights(taus)))
#'
#' taus = sort(runif(100L))
#' q = qexp(taus)
#' all.equal(q, weighted_quantile_type1(q, quantile_level_sample_weights(taus), taus))
#'
#' @seealso \code{\link{weighted_quantile_type1}}
#'
#' @details  Other approaches  can be  formulated if  we think  of this  task as
#'     interpolating  and extrapolating  an  ECDF from  its  defining values  by
#'     optionally extending or contracting horizontal lines without crossing any
#'     of its defining x values. The  approach for this function interpolates so
#'     that internal  jumps occur halfway between  consecutive defining quantile
#'     levels (\code{taus}).
quantile_level_sample_weights <- function(taus) {
    if (length(taus) == 0L) {
        stop ('expected at least one quantile level')
    } else if (length(taus) == 1L) {
        ## single level, gets all the weight
        return (1)
    } else {
        ## multiple levels, assign roughly according to the space between the levels, but with some adjustments to account for edges
        if (is.unsorted(taus)) {
            stop ('expected taus to be sorted')
        }
        weights = taus %>>%
            ## average together consecutive levels --- vertical locations of internal jumps for the implicit modified ECDF
            {(.[-1L] + .[-length(.)])/2} %>>%
            ## tack on 0 and 1 (also vertical locations in an ECDF)
            {c(0, ., 1)} %>>%
            ## calculate the size of the vertical jumps and assign that amount of mass/weight
            diff()
        return (weights)
    }
}

#' Calculates a weighted type-1 quantiles
#'
#' @param x instance values
#' @param w instance importance weights
#' @param probs quantile levels to calculate
#'
#' @return quantile values corresponding to levels \code{probs}
#'
#' @seealso quantile
#'
#' @importFrom data.table setDT set
weighted_quantile_type1 <- function(x, w, probs, na.rm=FALSE) {
    if (!na.rm && any(is.na(x))) {
        stop ('na.rm=FALSE but there are NAs in x')
    }
    ## xxx also check w?
    should_include = !is.na(x)
    x <- x[should_include]
    w <- w[should_include]
    DT = setDT(data.frame(x=x, w=w), key="x")
    set(DT,, "p", cumsum(DT[["w"]]))
    DT[list(probs*sum(w)), x, on="p", roll=-Inf, rollends=c(TRUE,TRUE), nomatch=NA]
}


#' Form a hayman forecast
#'
#' @param response length-1 character vector, optionally named; the response for
#'   which to generate a forecast; this ensemble is formed from components with
#'   a response matching either the content or name of this argument
#' @param incidence_period length-1 character vector; the
#'   \code{incidence_period} for which to generate a forecast; this ensemble is
#'   formed from components with the same \code{incidence_period}
#' @param ahead length-1 integer-valued \code{is.numeric}; the \code{ahead} for
#'   which to generate a forecast; this ensemble is formed from components with
#'   the same \code{ahead}
#' @param forecast_date ignored (to completely discover whether an ensemble can
#'   be formed requires reading card files to find whether they are NA, but this
#'   makes \code{\link{quantileEnsemble::get_forecasters}} run slowly;
#'   currently, no checks are done on the "outer" \code{forecast_date} fed to
#'   this function, and only the \code{forecast_date} fed to a forecaster
#'   function output from this function is considered)
#' @param geo_type length-1 character vector; the \code{geo_type} for which to
#'   generate a forecast; this ensemble is formed from components with the same
#'   \code{geo_type}
#'   @param n_locations length-1 integer-valued \code{is.numeric}; the
#'   \code{n_locations} for which to generate a forecast; this ensemble is
#'   formed from components with the same \code{n_locations}
#' @param required_forecaster_names names of forecasters that are required for
#'   the ensemble to be formed; no forecast will be generated if absent
#' @param include_all_other_forecasters \code{TRUE} or \code{FALSE}; if
#'   \code{TRUE}, all forecasters for the designated response, etc., within the
#'   3 directory trees will be used; if \code{FALSE}, only the explicitly
#'   designated required and optional forecasters will be considered
#' @param optional_forecaster_names names of forecasters that should be included
#'   if present, but not prevent ensemble formation if absent
#' @param shared_naming_scheme (re)naming scheme used as default for historical,
#'   prospective, and hub forecasters; see details
#' @param repo_root_dirpath length-1 character vector; path to root of the
#'   covidcast-forecast repository working directory from which to read
#'   component forecasts; defaults to the lowest ancestor of the \code{getwd}
#'   with a .git file/directory
#' @param historical_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for historical_components_dirpath
#' @param prospective_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for prospective_components_dirpath
#' @param hub_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for hub_components_dirpath
#' @param historical_components_dirpath override for the path to the directory
#'   (tree) containing historical component prediction/score cards
#' @param prospective_components_dirpath override for the path to the directory
#'   (tree) containing prospective component prediction/score cards
#' @param hub_components_dirpath override for the path to the directory (tree)
#'   containing COVID-19 Forecasting Hub component prediction/score cards
#'
#' @details Each (re)naming scheme is a character vector: each named vector
#'   entry denotes renaming forecasters with the entry's name to the entry
#'   itself; each unnamed vector entry denotes a name that is expected to be
#'   found somewhere in the searched paths; each entry (named or unnamed) is
#'   checked for appearance somewhere in any of the searched paths. However, the
#'   converse is not checked, i.e., it is fine for there to be paths found that
#'   do not correspond to an entry in the naming schemes.
#'
#' @importFrom data.table is.data.table
#' @importFrom pipeR %>>%
hayman_ensemble_forecaster_v1 <- function(response, incidence_period, ahead, forecast_date, geo_type, n_locations,
                                          required_forecaster_names, include_all_other_forecasters, optional_forecaster_names,
                                          shared_naming_scheme,
                                          repo_root_dirpath=get_repo_root_dirpath(),
                                          historical_naming_scheme=shared_naming_scheme,
                                          prospective_naming_scheme=shared_naming_scheme,
                                          hub_naming_scheme=shared_naming_scheme,
                                          historical_components_dirpath=get_historical_components_dirpath(repo_root_dirpath),
                                          prospective_components_dirpath=get_prospective_components_dirpath(repo_root_dirpath),
                                          hub_components_dirpath=get_hub_components_dirpath(repo_root_dirpath)) {
    if (incidence_period != "epiweek") {
        NA
    } else {
            function(df, forecast_date) {
                target_locations = df[["location"]][df[["variable_name"]]=="location_to_be_forecast" & df[["value"]]==1]
                rm(df)
                ## Assume we only use matching args for now (although later, we may want
                ## to pool across geo_type's, or even combine different aheads from
                ## different forecast_date's):
                desired_components_dt_or_feedback =
                    read_backoff_ensemble_components_dt(response, incidence_period, ahead, forecast_date, geo_type, n_locations,
                                                        shared_naming_scheme,
                                                        only_read_forecaster_names =
                                                            if (include_all_other_forecasters) NULL
                                                            else c(required_forecaster_names, optional_forecaster_names),
                                                        historical_components_dirpath,
                                                        prospective_components_dirpath,
                                                        hub_components_dirpath,
                                                        historical_naming_scheme,
                                                        prospective_naming_scheme,
                                                        hub_naming_scheme) %>>%
                    testtime_ensemble_components_dt(forecast_date) %>>%
                    desired_components_predcards_dt_or_feedback(
                        complete_output=FALSE,
                        required_forecaster_names, include_all_other_forecasters, optional_forecaster_names
                    ) %>>%
                    {.}
                if (!is.data.table(desired_components_dt_or_feedback)) {
                    print(desired_components_dt_or_feedback)
                    tibble(location=character(0L), probs=numeric(0L), quantiles=numeric(0L))
                } else {
                    predcard_dt =
                        desired_components_dt_or_feedback %>>%
                        hayman_ensemble_v1_predcard_dt(forecast_date) %>>%
                        {.}
                    result = as_tibble(unnest_dt_column_in_dt(predcard_dt[,list(location,forecast_distribution_dt)],"forecast_distribution_dt"))
                    result <- result[result[["location"]] %in% target_locations,]
                    rm(desired_components_dt_or_feedback)
                    rm(predcard_dt)
                    gc()
                    result
                }
            }
    }
}

#' Prepare a hayman v0 predcard using predcards for the same forecast_date
#'
#' @param predcards_dt a \code{predcards_dt}  containing only the predcards that
#'     should be  incorporated in  the ensemble
#'
#' @param test_forecast_date a length-1 \code{\link{Date}}-compatible object
#'
#' @examples
#' library("pipeR")
#' read_historical_ensemble_components_dt("jhu-csse_deaths_incidence_num","epiweek",2L,"2020-04-06","state",200L) %>>%
#'   testtime_ensemble_components_dt("2020-04-06") %>>%
#'   desired_components_predcards_dt_or_feedback(FALSE, c("strawman"), FALSE, c("aardvark")) %>>%
#'   hayman_ensemble_v0_predcard_dt("2020-04-06") %>>%
#'   predcard_dt_to_predcard_tbl() %>>%
#'   {.}
#' @importFrom data.table set :=
#' @importFrom pipeR %>>%
hayman_ensemble_v0_predcard_dt <- function(predcards_dt, test_forecast_date,
                                           allow_other_generation_dates=FALSE, allow_other_prediction_dates=FALSE) {
    validate_predcards_dt(predcards_dt)
    set(predcards_dt,, "predcard_dt", lapply(predcards_dt[["predcard_dt"]], function(predcard_dt) {
        ## select the minimal columns for a predcard_dt:
        predcard_dt[,c("location", "forecast_date", "forecast_distribution_dt")]
    }))
    if (nrow(predcards_dt) == 0L) {
        print(predcards_dt)
        stop ('no predcards to base on')
    }
    ## if ("forecast_date" %in% names(predcards_dt)) {
    ##     stop ('expected forecast_date to be removed from predcards_dt during a filtering operation (but for forecast_date to still appear within the predcards in the predcard column)')
    ## }
    if (!allow_other_generation_dates && any(predcards_dt[["forecast_date"]] != test_forecast_date)) {
        stop ('expected to have only predcards generated "on" test_forecast_date')
    }
    unnested_dt = predcards_dt[!vapply(predcard_dt, is.null, logical(1L))] %>>%
        unnest_dt_column_in_dt("predcard_dt") %>>%
        unnest_dt_column_in_dt("forecast_distribution_dt")
    unnested_dt[, weights := quantile_level_sample_weights(probs),
                by=c(setdiff(names(predcards_dt),"predcard_dt"),"location","forecast_date")]
    if (!allow_other_prediction_dates && !all(unnested_dt[["forecast_date"]] == test_forecast_date)) {
        print(unnested_dt[["forecast_date"]])
        stop ('expected to have only predcards reporting forecasts for test_forecast_date')
    }
    output_probs = cdc_probs
    hayman_predcard_dt = unnested_dt[, list(
        forecast_distribution_dt=list(data.table(
            probs=output_probs,
            quantiles=weighted_quantile_type1(quantiles, weights, output_probs, na.rm=TRUE)
        ))
    ), by=c("location", "forecast_date")]
    validate_predcard_dt(hayman_predcard_dt)
    hayman_predcard_dt
}

#' Form a hayman v0 forecast
#'
#' @param response length-1 character vector, optionally named; the response for
#'   which to generate a forecast; this ensemble is formed from components with
#'   a response matching either the content or name of this argument
#' @param incidence_period length-1 character vector; the
#'   \code{incidence_period} for which to generate a forecast; this ensemble is
#'   formed from components with the same \code{incidence_period}
#' @param ahead length-1 integer-valued \code{is.numeric}; the \code{ahead} for
#'   which to generate a forecast; this ensemble is formed from components with
#'   the same \code{ahead}
#' @param forecast_date ignored (to completely discover whether an ensemble can
#'   be formed requires reading card files to find whether they are NA, but this
#'   makes \code{\link{quantileEnsemble::get_forecasters}} run slowly;
#'   currently, no checks are done on the "outer" \code{forecast_date} fed to
#'   this function, and only the \code{forecast_date} fed to a forecaster
#'   function output from this function is considered)
#' @param geo_type length-1 character vector; the \code{geo_type} for which to
#'   generate a forecast; this ensemble is formed from components with the same
#'   \code{geo_type}
#' @param n_locations length-1 integer-valued \code{is.numeric}; the
#'   \code{n_locations} for which to generate a forecast; this ensemble is
#'   formed from components with the same \code{n_locations}
#' @param required_forecaster_names names of forecasters that are required for
#'   the ensemble to be formed; no forecast will be generated if absent
#' @param include_all_other_forecasters \code{TRUE} or \code{FALSE}; if
#'   \code{TRUE}, all forecasters for the designated response, etc., within the
#'   3 directory trees will be used; if \code{FALSE}, only the explicitly
#'   designated required and optional forecasters will be considered
#' @param optional_forecaster_names names of forecasters that should be included
#'   if present, but not prevent ensemble formation if absent
#' @param shared_naming_scheme (re)naming scheme used as default for historical,
#'   prospective, and hub forecasters; see details
#' @param repo_root_dirpath length-1 character vector; path to root of the
#'   covidcast-forecast repository working directory from which to read
#'   component forecasts; defaults to the lowest ancestor of the \code{getwd}
#'   with a .git file/directory
#' @param historical_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for historical_components_dirpath
#' @param prospective_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for prospective_components_dirpath
#' @param hub_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for hub_components_dirpath
#' @param historical_components_dirpath override for the path to the directory
#'   (tree) containing historical component prediction/score cards
#' @param prospective_components_dirpath override for the path to the directory
#'   (tree) containing prospective component prediction/score cards
#' @param hub_components_dirpath override for the path to the directory (tree)
#'   containing COVID-19 Forecasting Hub component prediction/score cards
#'
#' @details Each (re)naming scheme is a character vector: each named vector
#'   entry denotes renaming forecasters with the entry's name to the entry
#'   itself; each unnamed vector entry denotes a name that is expected to be
#'   found somewhere in the searched paths; each entry (named or unnamed) is
#'   checked for appearance somewhere in any of the searched paths. However, the
#'   converse is not checked, i.e., it is fine for there to be paths found that
#'   do not correspond to an entry in the naming schemes.
#'
#' @importFrom data.table is.data.table
#' @importFrom pipeR %>>%
hayman_ensemble_forecaster_v0 <- function(response, incidence_period, ahead, forecast_date, geo_type, n_locations,
                                          required_forecaster_names, include_all_other_forecasters, optional_forecaster_names,
                                          shared_naming_scheme,
                                          repo_root_dirpath=get_repo_root_dirpath(),
                                          historical_naming_scheme=shared_naming_scheme,
                                          prospective_naming_scheme=shared_naming_scheme,
                                          hub_naming_scheme=shared_naming_scheme,
                                          historical_components_dirpath=get_historical_components_dirpath(repo_root_dirpath),
                                          prospective_components_dirpath=get_prospective_components_dirpath(repo_root_dirpath),
                                          hub_components_dirpath=get_hub_components_dirpath(repo_root_dirpath)) {
    if (incidence_period != "epiweek") {
        NA
    } else {
            function(df, forecast_date) {
                target_locations = df[["location"]][df[["variable_name"]]=="location_to_be_forecast" & df[["value"]]==1]
                rm(df)
                ## Assume we only use matching args for now (although later, we may want
                ## to pool across geo_type's, or even combine different aheads from
                ## different forecast_date's):
                desired_components_dt_or_feedback =
                    read_backoff_ensemble_components_dt(response, incidence_period, ahead, forecast_date, geo_type, n_locations,
                                                        shared_naming_scheme,
                                                        only_read_forecaster_names =
                                                            if (include_all_other_forecasters) NULL
                                                            else c(required_forecaster_names, optional_forecaster_names),
                                                        historical_components_dirpath=historical_components_dirpath,
                                                        prospective_components_dirpath=prospective_components_dirpath,
                                                        hub_components_dirpath=hub_components_dirpath,
                                                        historical_naming_scheme=historical_naming_scheme,
                                                        prospective_naming_scheme=prospective_naming_scheme,
                                                        hub_naming_scheme=hub_naming_scheme) %>>%
                    testtime_ensemble_components_dt(forecast_date) %>>%
                    desired_components_predcards_dt_or_feedback(
                        complete_output=FALSE,
                        required_forecaster_names, include_all_other_forecasters, optional_forecaster_names
                    ) %>>%
                    {.}
                if (!is.data.table(desired_components_dt_or_feedback)) {
                    print(desired_components_dt_or_feedback)
                    tibble(location=character(0L), probs=numeric(0L), quantiles=numeric(0L))
                } else {
                    predcard_dt =
                        desired_components_dt_or_feedback %>>%
                        hayman_ensemble_v0_predcard_dt(forecast_date) %>>%
                        {.}
                    result = as_tibble(unnest_dt_column_in_dt(predcard_dt[,list(location,forecast_distribution_dt)],"forecast_distribution_dt"))
                    result <- result[result[["location"]] %in% target_locations,]
                    rm(desired_components_dt_or_feedback)
                    rm(predcard_dt)
                    gc()
                    result
                }
            }
    }
}

#' Prepare a hayman v1 predcard using predcards for the same forecast_date
#'
#' Differs from v0 in that it performs the required forecaster checks on a
#' per-location-forecast_time-task and complete-distribution basis vs. just a
#' per-forecast_time-task and card-presence basis.
#'
#' @param predcards_dt a \code{predcards_dt}  containing only the predcards that
#'     should be  incorporated in  the ensemble
#'
#' @param test_forecast_date a length-1 \code{\link{Date}}-compatible object
#'
#' @importFrom data.table set :=
#' @importFrom pipeR %>>%
hayman_ensemble_v1_predcard_dt <- function(predcards_dt, test_forecast_date,
                                           required_forecaster_names=character(0L), include_all_other_forecasters=TRUE, optional_forecaster_names=character(0L),
                                           allow_other_generation_dates=FALSE, allow_other_prediction_dates=FALSE) {
    validate_predcards_dt(predcards_dt)
    set(predcards_dt,, "predcard_dt", lapply(predcards_dt[["predcard_dt"]], function(predcard_dt) {
        ## select the minimal columns for a predcard_dt:
        predcard_dt[,c("location", "forecast_date", "forecast_distribution_dt")]
    }))
    if (nrow(predcards_dt) == 0L) {
        print(predcards_dt)
        stop ('no predcards to base on')
    }
    ## if ("forecast_date" %in% names(predcards_dt)) {
    ##     stop ('expected forecast_date to be removed from predcards_dt during a filtering operation (but for forecast_date to still appear within the predcards in the predcard column)')
    ## }
    if (!allow_other_generation_dates && any(predcards_dt[["forecast_date"]] != test_forecast_date)) {
        stop ('expected to have only predcards generated "on" test_forecast_date')
    }
    unnested_dt = predcards_dt[!vapply(predcard_dt, is.null, logical(1L))] %>>%
        unnest_dt_column_in_dt("predcard_dt") %>>%
        desired_components_forecast_distribution_dt_or_feedback(FALSE,required_forecaster_names,include_all_other_forecasters,optional_forecaster_names) %>>%
        unnest_dt_column_in_dt("forecast_distribution_dt")
    if (nrow(unnested_dt) == 0L || !all(c("probs","quantiles") %in% names(unnested_dt))) {
      stop ('required/all forecasts not found, missing, or improperly formatted')
    }
    unnested_dt[, weights := quantile_level_sample_weights(probs),
                by=c(setdiff(names(predcards_dt),"predcard_dt"),"location","forecast_date")]
    if (!allow_other_prediction_dates && !all(unnested_dt[["forecast_date"]] == test_forecast_date)) {
        print(unnested_dt[["forecast_date"]])
        stop ('expected to have only predcards reporting forecasts for test_forecast_date')
    }
    output_probs = cdc_probs
    hayman_predcard_dt = unnested_dt[, list(
        forecast_distribution_dt=list(data.table(
            probs=output_probs,
            quantiles=weighted_quantile_type1(quantiles, weights, output_probs, na.rm=TRUE)
        ))
    ), by=c("location", "forecast_date")]
    validate_predcard_dt(hayman_predcard_dt)
    hayman_predcard_dt
}

#' Form a hayman v1 forecast
#'
#' See \code{\link{hayman_ensemble_v1_predcard_dt}} for description of v1 vs. v0.
#'
#' @param response length-1 character vector, optionally named; the response for
#'   which to generate a forecast; this ensemble is formed from components with
#'   a response matching either the content or name of this argument
#' @param incidence_period length-1 character vector; the
#'   \code{incidence_period} for which to generate a forecast; this ensemble is
#'   formed from components with the same \code{incidence_period}
#' @param ahead length-1 integer-valued \code{is.numeric}; the \code{ahead} for
#'   which to generate a forecast; this ensemble is formed from components with
#'   the same \code{ahead}
#' @param forecast_date ignored (to completely discover whether an ensemble can
#'   be formed requires reading card files to find whether they are NA, but this
#'   makes \code{\link{quantileEnsemble::get_forecasters}} run slowly;
#'   currently, no checks are done on the "outer" \code{forecast_date} fed to
#'   this function, and only the \code{forecast_date} fed to a forecaster
#'   function output from this function is considered)
#' @param geo_type length-1 character vector; the \code{geo_type} for which to
#'   generate a forecast; this ensemble is formed from components with the same
#'   \code{geo_type}
#' @param n_locations length-1 integer-valued \code{is.numeric}; the
#'   \code{n_locations} for which to generate a forecast; this ensemble is
#'   formed from components with the same \code{n_locations}
#' @param required_forecaster_names names of forecasters that are required for
#'   the ensemble to be formed; no forecast will be generated if absent
#' @param include_all_other_forecasters \code{TRUE} or \code{FALSE}; if
#'   \code{TRUE}, all forecasters for the designated response, etc., within the
#'   3 directory trees will be used; if \code{FALSE}, only the explicitly
#'   designated required and optional forecasters will be considered
#' @param optional_forecaster_names names of forecasters that should be included
#'   if present, but not prevent ensemble formation if absent
#' @param shared_naming_scheme (re)naming scheme used as default for historical,
#'   prospective, and hub forecasters; see details
#' @param repo_root_dirpath length-1 character vector; path to root of the
#'   covidcast-forecast repository working directory from which to read
#'   component forecasts; defaults to the lowest ancestor of the \code{getwd}
#'   with a .git file/directory
#' @param historical_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for historical_components_dirpath
#' @param prospective_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for prospective_components_dirpath
#' @param hub_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for hub_components_dirpath
#' @param historical_components_dirpath override for the path to the directory
#'   (tree) containing historical component prediction/score cards
#' @param prospective_components_dirpath override for the path to the directory
#'   (tree) containing prospective component prediction/score cards
#' @param hub_components_dirpath override for the path to the directory (tree)
#'   containing COVID-19 Forecasting Hub component prediction/score cards
#'
#' @details Each (re)naming scheme is a character vector: each named vector
#'   entry denotes renaming forecasters with the entry's name to the entry
#'   itself; each unnamed vector entry denotes a name that is expected to be
#'   found somewhere in the searched paths; each entry (named or unnamed) is
#'   checked for appearance somewhere in any of the searched paths. However, the
#'   converse is not checked, i.e., it is fine for there to be paths found that
#'   do not correspond to an entry in the naming schemes.
#'
#' @importFrom data.table is.data.table
#' @importFrom pipeR %>>%
hayman_ensemble_forecaster_v1 <- function(response, incidence_period, ahead, forecast_date, geo_type, n_locations,
                                          required_forecaster_names, include_all_other_forecasters, optional_forecaster_names,
                                          shared_naming_scheme,
                                          repo_root_dirpath=get_repo_root_dirpath(),
                                          historical_naming_scheme=shared_naming_scheme,
                                          prospective_naming_scheme=shared_naming_scheme,
                                          hub_naming_scheme=shared_naming_scheme,
                                          historical_components_dirpath=get_historical_components_dirpath(repo_root_dirpath),
                                          prospective_components_dirpath=get_prospective_components_dirpath(repo_root_dirpath),
                                          hub_components_dirpath=get_hub_components_dirpath(repo_root_dirpath)) {
    if (incidence_period != "epiweek") {
        NA
    } else {
            function(df, forecast_date) {
                target_locations = df[["location"]][df[["variable_name"]]=="location_to_be_forecast" & df[["value"]]==1]
                rm(df)
                ## Assume we only use matching args for now (although later, we may want
                ## to pool across geo_type's, or even combine different aheads from
                ## different forecast_date's):
                desired_components_dt_or_feedback =
                    read_backoff_ensemble_components_dt(response, incidence_period, ahead, forecast_date, geo_type, n_locations,
                                                        shared_naming_scheme,
                                                        only_read_forecaster_names =
                                                            if (include_all_other_forecasters) NULL
                                                            else c(required_forecaster_names, optional_forecaster_names),
                                                        repo_root_dirpath=repo_root_dirpath,
                                                        historical_components_dirpath=historical_components_dirpath,
                                                        prospective_components_dirpath=prospective_components_dirpath,
                                                        hub_components_dirpath=hub_components_dirpath,
                                                        historical_naming_scheme=historical_naming_scheme,
                                                        prospective_naming_scheme=prospective_naming_scheme,
                                                        hub_naming_scheme=hub_naming_scheme) %>>%
                    testtime_ensemble_components_dt(forecast_date) %>>%
                    desired_components_predcards_dt_or_feedback(
                        complete_output=FALSE,
                        required_forecaster_names, include_all_other_forecasters, optional_forecaster_names
                    ) %>>%
                    {.}
                if (!is.data.table(desired_components_dt_or_feedback)) {
                    print(desired_components_dt_or_feedback)
                    tibble(location=character(0L), probs=numeric(0L), quantiles=numeric(0L))
                } else {
                    predcard_dt =
                        desired_components_dt_or_feedback %>>%
                        hayman_ensemble_v1_predcard_dt(forecast_date, required_forecaster_names, include_all_other_forecasters, optional_forecaster_names) %>>%
                        {.}
                    result = as_tibble(unnest_dt_column_in_dt(predcard_dt[,list(location,forecast_distribution_dt)],"forecast_distribution_dt"))
                    result <- result[result[["location"]] %in% target_locations,]
                    rm(desired_components_dt_or_feedback)
                    rm(predcard_dt)
                    gc()
                    result
                }
            }
    }
}

#' Prepare an untrained quantile-space ensemble v1 predcard_dt
#'
#' @param predcards_dt a \code{predcards_dt}  containing only the predcards that
#'     should be  incorporated in  the ensemble
#'
#' @param test_forecast_date a length-1 \code{\link{Date}}-compatible object
#'
#' @param ensemble_function a function, such as \code{\link{median}}, that
#'   combines quantile predictions from multiple forecasters for a single
#'   level&location&date&ahead into a single ensemble quantile prediction
#'
#' @importFrom data.table set := .SD
#' @importFrom pipeR %>>%
untrained_qspace_ensemble_v1_predcard_dt <- function(predcards_dt, test_forecast_date,
                                                     ensemble_function,
                                                     required_forecaster_names=character(0L), include_all_other_forecasters=TRUE, optional_forecaster_names=character(0L),
                                                     allow_other_generation_dates=FALSE, allow_other_prediction_dates=FALSE) {
  .GlobalEnv[["debug.env"]] <- environment()
    validate_predcards_dt(predcards_dt)
    ensemble_function <- match.fun(ensemble_function)
    set(predcards_dt,, "predcard_dt", lapply(predcards_dt[["predcard_dt"]], function(predcard_dt) {
        ## select the minimal columns for a predcard_dt:
        predcard_dt[,c("location", "forecast_date", "forecast_distribution_dt")]
    }))
    if (nrow(predcards_dt) == 0L) {
        print(predcards_dt)
        stop ('no predcards to base on')
    }
    ## if ("forecast_date" %in% names(predcards_dt)) {
    ##     stop ('expected forecast_date to be removed from predcards_dt during a filtering operation (but for forecast_date to still appear within the predcards in the predcard column)')
    ## }
    if (!allow_other_generation_dates && any(predcards_dt[["forecast_date"]] != test_forecast_date)) {
        stop ('expected to have only predcards generated "on" test_forecast_date')
    }
    unnested_dt = predcards_dt[!vapply(predcard_dt, is.null, logical(1L))] %>>%
        unnest_dt_column_in_dt("predcard_dt") %>>%
        desired_components_forecast_distribution_dt_or_feedback(FALSE,required_forecaster_names,include_all_other_forecasters,optional_forecaster_names) %>>%
        unnest_dt_column_in_dt("forecast_distribution_dt")
    if (nrow(unnested_dt) == 0L || !all(c("probs","quantiles") %in% names(unnested_dt))) {
      stop ('required/all forecasts not found, missing, or improperly formatted')
    }
    unnested_dt[, weights := quantile_level_sample_weights(probs),
                by=c(setdiff(names(predcards_dt),"predcard_dt"),"location","forecast_date")]
    if (!allow_other_prediction_dates && !all(unnested_dt[["forecast_date"]] == test_forecast_date)) {
        print(unnested_dt[["forecast_date"]])
        stop ('expected to have only predcards reporting forecasts for test_forecast_date')
    }
    output_probs = sort(unique(unnested_dt[["probs"]]))
    if (any(diff(output_probs)*2/(head(output_probs,-1L)+tail(output_probs,-1L)) < 1e-8)) {
      stop (sprintf('at least two unique output_probs were very close to each other, likely due to floating-point numeric issues in calculating what should be the same exact numbers; refusing to continue; output_probs: %s', toString(output_probs)))
    }
    qspace_ew_md_predcard_dt = (
      unnested_dt
      ## order-filter-complete each component forecast on probs:
      [, .SD[list("probs" = output_probs), on="probs", nomatch=NA],
       by=c("location","forecast_date","forecaster_name")]
      ## throw out incomplete or invalid component forecasts:
      [, .SD[!anyNA(quantiles) && !is.unsorted(quantiles)],
       by=c("location","forecast_date","forecaster_name")]
      ## calculate ensemble predictions:
      [, list(quantiles=ensemble_function(quantiles)),
       by=c("location", "forecast_date", "probs")]
      ## group into forecast_distribution_dt's:
      [, list(forecast_distribution_dt = list(data.table(probs=probs, quantiles=quantiles))),
       by=c("location", "forecast_date")]
    )
    validate_predcard_dt(qspace_ew_md_predcard_dt)
    qspace_ew_md_predcard_dt
}

#' Make an untrained quantile-space ensemble v1 forecaster
#'
#' @param response length-1 character vector, optionally named; the response for
#'   which to generate a forecast; this ensemble is formed from components with
#'   a response matching either the content or name of this argument
#' @param incidence_period length-1 character vector; the
#'   \code{incidence_period} for which to generate a forecast; this ensemble is
#'   formed from components with the same \code{incidence_period}
#' @param ahead length-1 integer-valued \code{is.numeric}; the \code{ahead} for
#'   which to generate a forecast; this ensemble is formed from components with
#'   the same \code{ahead}
#' @param forecast_date ignored (to completely discover whether an ensemble can
#'   be formed requires reading card files to find whether they are NA, but this
#'   makes \code{\link{quantileEnsemble::get_forecasters}} run slowly;
#'   currently, no checks are done on the "outer" \code{forecast_date} fed to
#'   this function, and only the \code{forecast_date} fed to a forecaster
#'   function output from this function is considered)
#' @param geo_type length-1 character vector; the \code{geo_type} for which to
#'   generate a forecast; this ensemble is formed from components with the same
#'   \code{geo_type}
#' @param n_locations length-1 integer-valued \code{is.numeric}; the
#'   \code{n_locations} for which to generate a forecast; this ensemble is
#'   formed from components with the same \code{n_locations}
#' @param ensemble_function a function, such as \code{\link{median}}, that
#'   combines quantile predictions from multiple forecasters for a single
#'   level&location&date&ahead into a single ensemble quantile prediction
#' @param required_forecaster_names names of forecasters that are required for
#'   the ensemble to be formed; no forecast will be generated if absent
#' @param include_all_other_forecasters \code{TRUE} or \code{FALSE}; if
#'   \code{TRUE}, all forecasters for the designated response, etc., within the
#'   3 directory trees will be used; if \code{FALSE}, only the explicitly
#'   designated required and optional forecasters will be considered
#' @param optional_forecaster_names names of forecasters that should be included
#'   if present, but not prevent ensemble formation if absent
#' @param shared_naming_scheme (re)naming scheme used as default for historical,
#'   prospective, and hub forecasters; see details
#' @param repo_root_dirpath length-1 character vector; path to root of the
#'   covidcast-forecast repository working directory from which to read
#'   component forecasts; defaults to the lowest ancestor of the \code{getwd}
#'   with a .git file/directory
#' @param historical_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for historical_components_dirpath
#' @param prospective_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for prospective_components_dirpath
#' @param hub_naming_scheme (re)naming scheme --- override for
#'   shared_naming_scheme if needed for hub_components_dirpath
#' @param historical_components_dirpath override for the path to the directory
#'   (tree) containing historical component prediction/score cards
#' @param prospective_components_dirpath override for the path to the directory
#'   (tree) containing prospective component prediction/score cards
#' @param hub_components_dirpath override for the path to the directory (tree)
#'   containing COVID-19 Forecasting Hub component prediction/score cards
#'
#' @details Each (re)naming scheme is a character vector: each named vector
#'   entry denotes renaming forecasters with the entry's name to the entry
#'   itself; each unnamed vector entry denotes a name that is expected to be
#'   found somewhere in the searched paths; each entry (named or unnamed) is
#'   checked for appearance somewhere in any of the searched paths. However, the
#'   converse is not checked, i.e., it is fine for there to be paths found that
#'   do not correspond to an entry in the naming schemes.
#'
#' @importFrom data.table is.data.table
#' @importFrom pipeR %>>%
untrained_qspace_ensemble_forecaster_v1 <- function(response, incidence_period, ahead, forecast_date, geo_type, n_locations,
                                                    ensemble_function,
                                                    required_forecaster_names, include_all_other_forecasters, optional_forecaster_names,
                                                    shared_naming_scheme,
                                                    repo_root_dirpath=get_repo_root_dirpath(),
                                                    historical_naming_scheme=shared_naming_scheme,
                                                    prospective_naming_scheme=shared_naming_scheme,
                                                    hub_naming_scheme=shared_naming_scheme,
                                                    historical_components_dirpath=get_historical_components_dirpath(repo_root_dirpath),
                                                    prospective_components_dirpath=get_prospective_components_dirpath(repo_root_dirpath),
                                                    hub_components_dirpath=get_hub_components_dirpath(repo_root_dirpath)) {
    ensemble_function <- match.fun(ensemble_function)
    if (incidence_period != "epiweek") {
        NA
    } else {
            function(df, forecast_date, observations_tbl=NULL) {
                target_locations = df[["location"]][df[["variable_name"]]=="location_to_be_forecast" & df[["value"]]==1]
                rm(df)
                ## Assume we only use matching args for now (although later, we may want
                ## to pool across geo_type's, or even combine different aheads from
                ## different forecast_date's):
                desired_components_dt_or_feedback =
                    read_backoff_ensemble_components_dt(response, incidence_period, ahead, forecast_date, geo_type, n_locations,
                                                        shared_naming_scheme,
                                                        only_read_forecaster_names =
                                                            if (include_all_other_forecasters) NULL
                                                            else c(required_forecaster_names, optional_forecaster_names),
                                                        repo_root_dirpath=repo_root_dirpath,
                                                        historical_components_dirpath=historical_components_dirpath,
                                                        prospective_components_dirpath=prospective_components_dirpath,
                                                        hub_components_dirpath=hub_components_dirpath,
                                                        historical_naming_scheme=historical_naming_scheme,
                                                        prospective_naming_scheme=prospective_naming_scheme,
                                                        hub_naming_scheme=hub_naming_scheme) %>>%
                    testtime_ensemble_components_dt(forecast_date) %>>%
                    desired_components_predcards_dt_or_feedback(
                        complete_output=FALSE,
                        required_forecaster_names, include_all_other_forecasters, optional_forecaster_names
                    ) %>>%
                    {.}
                if (!is.data.table(desired_components_dt_or_feedback)) {
                    print(desired_components_dt_or_feedback)
                    tibble(location=character(0L), probs=numeric(0L), quantiles=numeric(0L))
                } else {
                    predcard_dt =
                        desired_components_dt_or_feedback %>>%
                        untrained_qspace_ensemble_v1_predcard_dt(forecast_date, ensemble_function, required_forecaster_names, include_all_other_forecasters, optional_forecaster_names) %>>%
                        {.}
                    result = as_tibble(unnest_dt_column_in_dt(predcard_dt[,list(location,forecast_distribution_dt)],"forecast_distribution_dt"))
                    result <- result[result[["location"]] %in% target_locations,]
                    rm(desired_components_dt_or_feedback)
                    rm(predcard_dt)
                    gc()
                    result
                }
            }
    }
}
