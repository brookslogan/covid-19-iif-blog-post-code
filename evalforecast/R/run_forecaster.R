#' Test a forecaster if it works using the prediction harness
#' @param response the response (e.g. "usafacts_deaths_incidence_num")
#' @param incidence_period the incidence period (e.g. "epiweek" for
#'     now, for all forecasters)
#' @param ahead the ahead parameter (e.g. 1, 2, 3, 4)
#' @param forecast_date the date of the forecast
#' @param geo_type the geographic type (e.g "county" or "state" or
#'     "hrr" or "msa"... but for now only the first two),
#' @param n_locations the number of locations (for now we will use 200
#'     for this)
#' @param data_path full path to your county/state Rdata file
#' @param forecaster your forecaster function
#' @importFrom dplyr pull
#' @importFrom assertthat assert_that
#' @importFrom logger log_info
#' @export
run_forecaster <- function(response, incidence_period = c("epiweek"), ahead,
                           forecast_date,
                           geo_type = c("county", "state", "hrr", "msa"),
                           n_locations = 200,
                           data_path,
                           forecaster) {
    incidence_period <- match.arg(incidence_period)
    ## Currently we only work with "county" or "state" level forecasts
    geo_type <- match.arg(geo_type)
    assert_that(geo_type %in% c("county", "state"),
                msg = 'We only use geo_type = "county"/"state" for now!')

    logger::log_info("evalforecast::run_forecaster: Loading data")
    e  <- new.env(parent = emptyenv())
    load(data_path, envir = e)

    logger::log_info("evalforecast::run_forecaster: Choosing locations for forecasting")
    ## choose locations that we will be forecasting today:
    date_for_selection <- min(
        get_target_period(forecast_date, incidence_period, ahead) %>%
        dplyr::pull(start) - 1,
        min(forecast_date) - 1
    )
    logger::log_info("evalforecast::run_forecaster: Getting top n_locations")
    forecast_locations <- get_top_n_locations(df = e$df,
                                              response = response,
                                              latest_date = date_for_selection,
                                              n = n_locations)

    logger::log_info("evalforecast::run_forecaster: Computing predictions")
    predictions_card <- get_predictions_card(df = e$df,
                                             your_quantile_forecaster = forecaster,
                                             forecast_date = forecast_date,
                                             forecast_locations = forecast_locations,
                                             response = response,
                                             incidence_period = incidence_period,
                                             ahead = ahead,
                                             geo_type = geo_type,
                                             n_locations = n_locations)
    logger::log_info("evalforecast::run_forecaster: Computing predictions... done!")

    predictions_card
}
