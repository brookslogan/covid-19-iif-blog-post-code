#' Run all forecasters using the production data
#'
#' @param forecaster_pkgs a string array of forecasting R package names
#' @param forecaster_filters a list of conjunctive criteria (implemented as functions of `type` and `name`) to filter the forecasters
#' @param output_path the output path where all output should be stored in a hierarchy
#' @param county_data_path the full path of the county data file
#' @param state_data_path the full path of the state data file
#' @param forecast_date the forecast data (yyyy-mm-dd)
#' @param git_sha the sha for the github repo from which packages where installed
#' @param ahead the value of ahead to use, default `1:4`
#' @param geo_type the geo_type values to run for, default `c("county", "state")`
#' @param responses a named list of responses for county and state
#' @param incidence_period the incidence period, default "epiweek"
#' @param n_locations the number of locations to forecast for, default 200
#' @importFrom logger log_info
#' @importFrom purrr map map_chr map_int flatten
#' @importFrom fs path dir_create dir_exists
#' @importFrom tibble tibble
#' @export run_production
run_production  <- function(forecaster_pkgs,
                            forecaster_filter = list(type = function(type) type == "standalone",
                                                     name = function(name) TRUE),
                            output_path,
                            county_data_path,
                            state_data_path,
                            forecast_date,
                            git_sha,
                            aheads = 1:4,
                            geo_types = c("county", "state"),
                            ## these variables specify the forecasting task:
                            responses,
                            incidence_period = "epiweek",
                            n_locations = 200
                            ) {

    forecast_date_chr  <- format(forecast_date, "%Y-%m-%d")
    e  <- new.env(parent = emptyenv())
    if ("county" %in% geo_types) {
        logger::log_info("Loading county data")
        load(county_data_path, envir = e)
    } else {
        e$df  <- NA
    }

    e$data_list <- list(county = e$df)

    if ("state" %in% geo_types) {
        logger::log_info("Loading state data")
        load(state_data_path, envir = e)
        e$data_list$state <- e$df
    } else {
        e$data_list$state  <- NA
    }

    if (is.na(e$data_list$county) && is.na(e$data_list$state)) {
        logger::log_info("No county or state data loaded; stopping.")
        stop("evalforecast::run_production: stopped because of no data")
    }

    manifest  <- vector(mode = "list", length = 512) ## a reasonable upper bound for us
    ## I could do better, but ok for now.
    counter <- 0

    for (ahead in aheads) {
        for (geo_type in geo_types) {
            response <- responses[[geo_type]]
            all_offerings <- purrr::flatten(
                                        purrr::map(.x = forecaster_pkgs,
                                                   .f = function(pkg) {
                                                       utils::getFromNamespace(x = "get_forecasters", ns = pkg)(
                                                           response = response,
                                                           incidence_period = incidence_period,
                                                           ahead = ahead,
                                                           geo_type = geo_type,
                                                           n_locations = n_locations,
                                                           forecast_date = forecast_date)
                                                   })
                                    )

            selection <- mapply(FUN = function(name, forecaster) forecaster_filter$name(name) && forecaster_filter$type(forecaster$type),
                                names(all_offerings), all_offerings)

            forecasters_and_type <- all_offerings[selection]
            forecasters  <- lapply(forecasters_and_type, function(x) x$forecaster)

            if (length(selection) == 0L) {
                logger::log_info("No forecasters match selection criteria!")
            } else {
                logger::log_info(sprintf("Selected forecasters: %s", paste0(names(forecasters), collapse = ", ")))
                result  <- evalforecast::run_forecasters(forecasters = forecasters,
                                                         response = response,
                                                         incidence_period = incidence_period,
                                                         ahead = ahead,
                                                         geo_type = geo_type,
                                                         n_locations = n_locations,
                                                         forecast_date = forecast_date,
                                                         data_list = e$data_list)

                logger::log_info("Saving prediction outputs")
                for (fn in names(result)) {
                    output_path  <- fs::path(forecast_date, ahead, response, geo_type, incidence_period, n_locations, fn)
                    if (!fs::dir_exists(output_path)) fs::dir_create(output_path, recurse = TRUE)
                    output_file_path  <- fs::path(output_path, "out.RDS")
                    current_result  <- result[[fn]]
                    saveRDS(current_result, file = output_file_path)
                    ## Save for manifest
                    counter  <- counter + 1
                    na_reason  <- attr(current_result, "NA_REASON") ## What was the reason for NA (NULL if not NA)
                    if (is.null(na_reason)) {
                        na_reason  <- NA
                    }

                    manifest[[counter]]  <- list(forecast_date = forecast_date_chr,
                                                 git_sha = git_sha,
                                                 ahead = ahead,
                                                 response = response,
                                                 geo_type = geo_type,
                                                 incidence_period = incidence_period,
                                                 n_locations = n_locations,
                                                 fn = fn,
                                                 output_file_path = output_file_path,
                                                 is_na = anyNA(current_result), ## is the result NA?
                                                 na_reason = na_reason)
                }
                logger::log_info("Done.")
            }
        }
    }
    manifest  <- Filter(function(x) !is.null(x), manifest)
    tibble::tibble(
                forecast_date = map_chr(manifest, function(x) x$forecast_date),
                git_repo_sha = map_chr(manifest, function(x) x$git_sha),
                ahead = map_int(manifest, function(x) as.integer(x$ahead)),
                response = map_chr(manifest, function(x) x$response),
                geo_type = map_chr(manifest, function(x) x$geo_type),
                incidence_period = map_chr(manifest, function(x) x$incidence_period),
                n_locations = map_int(manifest, function(x) as.integer(x$n_locations)),
                forecaster = map_chr(manifest, function(x) x$fn),
                output_file_path = map_chr(manifest, function(x) x$output_file_path),
                is_na = map_lgl(manifest, function(x) x$is_na),
                na_reason = map_chr(manifest,  function(x) x$na_reason)
            )
}
