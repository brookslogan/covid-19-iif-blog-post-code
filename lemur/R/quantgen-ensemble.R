
#' @source common-ensemble-settings.R
NULL

#' Get array of quantile forecasts from \code{forecasters} on \code{train_dates}
#'
#' Requires that prediction_cards are in directory structure like
#' Naras's.
#'
#' @param locations_considered \code{NULL} or character vector; if \code{NULL},
#'   does nothing; if a character vector, restricts output to locations in this
#'   vector
#'
#' @return 3-D  array: (date;location) x forecaster x quantile;  can be fed into
#'     Ryan's quantgen package
#'
#' @importFrom fs path
#' @importFrom dplyr bind_rows count filter mutate pull group_by slice ungroup transmute
#' @importFrom tibble deframe
#' @importFrom stringi stri_paste
get_quantile_forecasts <- function(train_dates,
                                   forecasters,
                                   observations_tbl,
                                   ahead = 1,
                                   geo_type = "county",
                                   incidence_period = "epiweek",
                                   n_locations = 200,
                                   response = "usafacts_deaths_incidence_num",
                                   repo_root_dirpath=get_repo_root_dirpath(),
                                   locations_considered = NULL) {
  .GlobalEnv[["debug.gqf.env"]] <- environment()
  historical_components_dirpath = get_historical_components_dirpath(repo_root_dirpath)
  prospective_components_dirpath = get_prospective_components_dirpath(repo_root_dirpath)
  hub_components_dirpath = get_hub_components_dirpath(repo_root_dirpath)
  forecaster_quantiles = array(NA,dim=c(0,length(forecasters),length(cdc_probs)))
  actual_values = c()
  datelocs = c()
  for (train_date in as.character(train_dates)) {
    print(train_date)
    predcard_list = list()
    for (forecaster_i in seq_along(forecasters)) {
      forecaster = forecasters[[forecaster_i]]
      forecaster_alt_names =
        if (is.null(names(forecasters)) || names(forecasters)[[forecaster_i]]=="") {
          character(0L)
        } else {
          c(names(forecasters)[[forecaster_i]])
        }
      file_basenames = c("out.RDS")
      try({
        ## get crossings of forecaster aliases and different {prediction,score}
        ## card directory tree roots, in a particular priority order:
        possible_predcard_filepaths =
          expand.grid(
            response_alias = c(response, names(response)),
            forecaster_alias = c(forecaster, forecaster_alt_names),
            components_dirpath = c(historical_components_dirpath, prospective_components_dirpath, hub_components_dirpath),
            file_basename = file_basenames
          ) %>%
          mutate(filepath = path(components_dirpath,
                                 train_date, ahead, response_alias, geo_type, incidence_period, n_locations, forecaster_alias,
                                 file_basename)) %>%
          pull(filepath)
        possible_predcard_exists = file.exists(possible_predcard_filepaths)
        if (!any(possible_predcard_exists)) {
          stop (sprintf("Could not find predcard at any of the following paths:\n%s",
                        paste(collapse="\n", capture.output(print(possible_predcard_filepaths)))))
        } else {
            predcard_filepath = possible_predcard_filepaths[[which(possible_predcard_exists)[[1L]]]]
            s = readRDS(predcard_filepath)
            predcard_list[[forecaster]] <-
              if (is.logical(s) && length(s) == 1L && is.na(s)) {
                NULL
              } else {
                s
              }
        }
      })
    }
    
    ## For now, we throw away any location that isn't predicted by every forecaster
    #if (length(predcard_list) != length(forecasters) || anyNA(predcard_list)) {
    #  next
    #}
    if (length(predcard_list) == 0) {
      next
    }
    predcards_one_day = bind_rows(predcard_list[!is.na(predcard_list)], .id="forecaster")
    ## For now, we throw away any location that isn't predicted by every forecaster
    ## good_locations = (predcards_one_day %>% count(location) %>% filter(n==length(forecasters)))[["location"]]
    ## predcards_one_day = predcards_one_day %>% filter(location %in% good_locations)
    encountered_locations = unique(predcards_one_day[["location"]])
    all_locations =
      if(is.null(locations_considered)) {
        encountered_locations
      } else {
        intersect(encountered_locations, locations_considered)
      }
    forecasts_one_day = array(
      NA_real_,
      dim=c(length(all_locations), length(forecasters), length(cdc_probs)),
      dimnames=list(`Date;Location`=stri_paste(train_date,";",all_locations), `Forecaster`=forecasters, `Quantile`=cdc_probs)
    )
    ## Better way to do this using dplyr??
    for (i in seq_len(dim(predcards_one_day)[[1L]])) {
      line = predcards_one_day[i,]
      if (line[["location"]] %in% all_locations) {
        forecasts_one_day[stri_paste(train_date, ";", line[["location"]]), line[["forecaster"]], ] = line[["forecast_distribution"]][[1]][["quantiles"]]
      }
    }
    datelocs = c(datelocs,stri_paste(train_date,";",all_locations))
    dim_fq = dim(forecaster_quantiles)
    dim_f1 = dim(forecasts_one_day)
    new_forecaster_quantiles = array(0,dim=c(dim_fq[1]+dim_f1[1],dim_fq[2],dim_fq[3]))
    if (dim_fq[1] > 0) {
      new_forecaster_quantiles[1:dim_fq[1],,] = forecaster_quantiles
    }
    new_forecaster_quantiles[(dim_fq[1]+1):(dim_fq[1]+dim_f1[1]),,] = forecasts_one_day
    dimnames(new_forecaster_quantiles) = list(`Date;Location`=datelocs,`Forecaster`=forecasters,`Quantile`=cdc_probs)
    forecaster_quantiles = new_forecaster_quantiles
  }
  dimnames(forecaster_quantiles) = list(`Date;Location`=datelocs, `Forecaster`=forecasters, `Quantile`=cdc_probs)
  result = list(forecasts = forecaster_quantiles)
  actual_values = observations_tbl %>%
    transmute(`Date;Location` = stri_paste(date-6L+1L-7L*(ahead-1L),";",location), value) %>%
    deframe() %>%
    `[`(datelocs)
  result[["actual"]] = actual_values
  return(result)
}

#' Impute missing forecast quantiles using Evan Ray's method:
#' Mean imputation for missing forecast quantiles and adjust weights so that
#' a forecaster gets 0 weight for missing forecast and its weight is distributed
#' equally to forecasters which provide forecasts for that (date;location)
#' 
#' Treats a forecast with any NAs as completely missing
impute_quantile_forecasts = function(qf) {
  missing_arr = apply(qf$forecasts,1:2,function(fcast) { any(is.na(fcast)) })
  missing_inds = which(missing_arr, arr.ind=TRUE)
  result = list()
  result$forecasts = qf$forecasts
  result$actual = qf$actual
  for (i in seq_len(nrow(missing_inds))) {
    dateloc = missing_inds[i, 1]
    fcaster = missing_inds[i, 2]
    result$forecasts[dateloc, fcaster, ] = apply(qf$forecasts[dateloc,,],2,mean,na.rm=T)
  }
  result$missing_arr = missing_arr
  
  return(result)
}

#' @param training_locations_considered \code{NULL} or character vector; if
#'   \code{NULL}, does nothing; if a character vector, limits training to
#'   locations in this vector
#' @param cheating_fit_on_test_date_instead \code{TRUE} or \code{FALSE}; if \code{TRUE},
#'   the model is fit on scorecards for the test date rather than the normal
#'   training days (cheating)
#' @importFrom tibble tibble
#' @importFrom quantgen quantile_ensemble
#' @importFrom stats predict
quantgen_ensemble_forecaster_v0 <- function(response, incidence_period, ahead,
                                            geo_type, n_locations,
                                            forecasters,
                                            tau_groups,
                                            repo_root_dirpath=get_repo_root_dirpath(),
                                            impute_missing = FALSE,
                                            fit_taus = cdc_probs,
                                            training_locations_considered = NULL,
                                            n_traindays = 4,
                                            unit_sum = TRUE,
                                            intercept = FALSE,
                                            cheating_fit_on_test_date_instead = FALSE,
                                            debug_weights_folder = NULL) {
    my_forecaster = function(df, forecast_date, observations_tbl=NULL) {
        print(ahead)
        print(forecast_date)

        ## To determine train_dates, we use ahead to avoid relying on information not available at forecast_date
        train_dates = as.character(forecast_date - 7*seq(ahead,ahead + (n_traindays-1)))
        
        component_forecasts = get_quantile_forecasts(as.character(forecast_date), forecasters, observations_tbl, ahead=ahead, geo_type=geo_type, incidence_period=incidence_period,
                                                     n_locations=n_locations,response=response,repo_root_dirpath=repo_root_dirpath)
        if (dim(component_forecasts$forecasts)[1] == 0) {
          print(paste0("Could not load any component forecasts for forecast date ",forecast_date, " and ahead ",ahead,", returning empty tibble"))
          return(tibble(location=character(),probs=numeric(),quantiles=numeric()))
        }
        
        ## Ignore all forecasters that are completely absent from training data
        #mask = apply(component_forecasts$forecasts,2,function(x) { !all(is.na(x))})
        ## Ingore all forecasters that have any NA predictions for the 50 US states
        component_locations = sapply(strsplit(dimnames(component_forecasts$forecasts)[[1]],";"), "[[", 2)
        component_states = which(as.numeric(component_locations) < 60 & as.numeric(component_locations) != 11)
        mask = apply(component_forecasts$forecasts[component_states,,],2,function(x) { !any(is.na(x))})
        if (any(!mask)) {
          warning(paste0("Some component forecasters are unavailable for forecast date ",forecast_date))
        }
        forecasters = forecasters[mask]
        component_forecasts$forecasts = component_forecasts$forecasts[,mask,]

        success = tryCatch(expr={
            qf =
              if (cheating_fit_on_test_date_instead) {
                stop ('cheating_fit_on_test_date_instead not supported')
              } else {
                get_quantile_forecasts(train_dates, forecasters, observations_tbl, ahead=ahead, geo_type=geo_type, incidence_period=incidence_period,
                                       n_locations=n_locations, response=response, repo_root_dirpath=repo_root_dirpath,
                                       locations_considered=training_locations_considered)
              }
            qf$forecasts = qf$forecasts[,,as.character(fit_taus)]
            na_mask = apply(qf$forecasts,1,function(arr) { all(is.na(arr))}) | is.na(qf$actual)
            qf$actual = qf$actual[!na_mask]
            qf$forecasts = qf$forecasts[!na_mask,,]
            if (impute_missing) {
              qf = impute_quantile_forecasts(qf)
            }
            na_preds = apply(qf$forecasts,1,function(arr) { any(is.na(arr))})
            st_obj = quantile_ensemble(qf$forecasts[!na_preds,,], qf$actual[!na_preds], fit_taus, tau_groups = tau_groups, noncross = FALSE, lp_solver = "gurobi",
                                       intercept = intercept, unit_sum = unit_sum, verbose=FALSE)
            orig_weights = st_obj[["alpha"]]
            ## Adjust weights based on number of missing forecasts for each forecaster
            if (impute_missing) {
              ## Calculate weights for each training example
              new_weights = array(0,dim=dim(qf$forecasts))
              for (i in 1:dim(qf$forecasts)[1]) {
                ## Initialize weights for training example to be the fitted weights
                if (length(unique(tau_groups)) == 1) {
                  for (k in 1:length(fit_taus)) {
                    new_weights[i,,k] = st_obj$alpha
                  }
                } else {
                  new_weights[i,,] = st_obj$alpha
                }
                
                missing_forecasters = qf$missing_arr[i,]
                if (sum(missing_forecasters) > 0) {
                  ## For missing forecasters, set weight to 0
                  new_weights[i,missing_forecasters,] = 0
                  ## For non-missing forecasters, equally distribute weight of missing forecasters
                  if (length(unique(tau_groups)) == 1) {
                    missing_weight = sum(st_obj$alpha[missing_forecasters]) / sum(!missing_forecasters)
                  } else {
                    missing_weight = apply(st_obj$alpha[missing_forecasters,,drop=FALSE],2,sum) / sum(!missing_forecasters)
                  }
                  for (j in which(!missing_forecasters)) {
                    new_weights[i,j,] = new_weights[i,j,] + missing_weight
                  }
                }
              }
              
              ## Take mean of new weights across all training examples
              if (length(unique(tau_groups)) == 1) {
                new_weights = apply(new_weights,2,mean)
              } else {
                new_weights = apply(new_weights,2:3,mean)
              }
              st_obj$alpha = new_weights
            }
            success = 0
        },error = function(c) {
            print(c$message)
            ## Couldn't train ensemble weights, return empty tibble for now
            return(NULL)
        })
        if (is.null(success)) {
            print(paste0("Could not fit ensemble weights for forecast date ",forecast_date, " and ahead ",ahead,", returning empty tibble"))
            return(tibble(location=character(),probs=numeric(),quantiles=numeric()))
        }

        if (!is.null(debug_weights_folder)) {
          debug_weights_file = sprintf("%s/%s/%s/%s/%d/%d/%s.RDS", debug_weights_folder, response, incidence_period, geo_type, n_locations, ahead, forecast_date)
          cat("Saving weights info to", debug_weights_file, "\n")
          if (!dir.exists(dirname(debug_weights_file))) {
            dir.create(dirname(debug_weights_file), recursive=TRUE)
          }
          saveRDS(list(
            weights=st_obj[["alpha"]],
            orig_weights=orig_weights,
            forecasters=forecasters
          ), debug_weights_file)
        }

        ensemble_forecasts = predict(st_obj,component_forecasts$forecasts[,,as.character(fit_taus)])
        
        full_ensemble_forecasts = array(NA,dim=c(dim(ensemble_forecasts)[1],length(cdc_probs)))
        if (length(fit_taus) != length(cdc_probs)) {
          for (i in 1:(dim(component_forecasts$forecasts)[1])) {
            if (any(is.na(ensemble_forecasts[i,]))) {
              next
            }
            middle_idx = as.integer(median(1:length(fit_taus)))
            tau_lower = min(fit_taus)
            tau_middle = fit_taus[middle_idx]
            tau_upper = max(fit_taus)
            lower_sd = (ensemble_forecasts[i,middle_idx] - ensemble_forecasts[i,1]) / (qnorm(tau_middle) - qnorm(tau_lower))
            upper_sd = (ensemble_forecasts[i,length(fit_taus)] - ensemble_forecasts[i,middle_idx]) / (qnorm(tau_upper) - qnorm(tau_middle))
            lower_fn = function(x,m) { qnorm(x,mean=m,sd=lower_sd)}
            upper_fn = function(x,m) { qnorm(x,mean=m,sd=upper_sd)}
            full_ensemble_forecasts[i,] = quantile_extrapolate(fit_taus, ensemble_forecasts[i,], qfun_left=lower_fn, qfun_right=upper_fun)
            m = ensemble_forecasts[i,middle_idx]
            left_tail = cdc_probs[cdc_probs < tau_lower]
            right_tail = cdc_probs[cdc_probs > tau_upper]
            full_ensemble_forecasts[i,cdc_probs < tau_lower] = lower_fn(left_tail,m)
            full_ensemble_forecasts[i,cdc_probs > tau_upper] = upper_fn(right_tail,m)
          }
        } else {
          full_ensemble_forecasts = ensemble_forecasts
        }

        forecast_locations = sapply(strsplit(dimnames(component_forecasts$forecasts)[[1]],";"), "[[", 2) # Split ";" in each (date;location) and extract location
        forecast_result = tibble(location = rep(forecast_locations,each=length(cdc_probs)), probs = rep(cdc_probs,length(forecast_locations)), quantiles = 0)

        ## Better way to do this using dplyr??
        for (i in 1:length(forecast_locations)) {
            forecast_result[forecast_result$location == forecast_locations[[i]],"quantiles"] = full_ensemble_forecasts[i,]
        }

        return(forecast_result)
    }

    return(my_forecaster)
}
