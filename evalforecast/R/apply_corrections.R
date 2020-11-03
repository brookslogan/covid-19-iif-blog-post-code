#' Apply corrections, if available, to upstream data frame
#'
#' Corrections data are collection of replacement records for the
#' original data. This means that the variables appear exactly in the
#' same order as in the original record and only the `value` of a
#' variable is potentially different. The replacement process returns
#' a new tibble by removing the matching original data, matched by the
#' variables `location`, `reference_date` and `variable_name` and
#' appending the entire corrections data at the end. Ideally, this
#' function should only make corrections that a properly versioned
#' data frame cannot account for, i.e. persistent bad data rows that
#' are likely to mess up forecasting algorithms (this has the salutory
#' effect of keeping the number of corrections small). Note that
#' `issue_date` is not accounted for; this function will have to
#' modified to account for non-`NA` `issue_date`.
#'
#' @param df the upstream data frame corresponding to the geo type
#' @param geo_type the geo_type corresponding to the upstream data
#'     frame
#' @param forecast_date the forecast date as a date object to account
#'     for the response variable name change that happened on
#'     `2020-07-13`
#' @param log_info a boolean flag indicating whether to log
#'     information on changes, default `TRUE`
#' @return a df with corrections applied if the corrections are
#'     available, or same dataframe
#' @importFrom fs file_exists
#' @importFrom dplyr anti_join select
#' @importFrom logger log_info
#' @importFrom magrittr %>%
#' @export apply_corrections
#'
#' @examples
#'
#' \dontrun{
#'   e  <- new.env()
#'   load("upstream_df_state_2020-08-30.Rdata", envir = e)
#'   new_df <- apply_corrections(df = e$df, geo_type = "state", forecast_date = lubridate::ymd("2020-08-09"))
#'   nrow(e$df) == nrow(new_df)  # Same number of rows?
#' }
apply_corrections  <- function(df, geo_type = c("county", "state"), forecast_date,
                               log_info = TRUE) {
    geo_type  <- match.arg(geo_type)
    if (geo_type == "state") {
        corrections_file  <- system.file("extdata", "state_corrections.RDS", package = "evalforecast")
    } else {
        corrections_file  <- system.file("extdata", "county_corrections.RDS", package = "evalforecast")
    }
    if (fs::file_exists(corrections_file)) {
        if (log_info) logger::log_info(sprintf("Reading corrections file for %s\n", geo_type))
        corrections <- readRDS(corrections_file)
        if (log_info) logger::log_info(sprintf("Applying %d row replacements\n", nrow(corrections)))
        dplyr::anti_join(x = df, y = corrections, by = c("location", "reference_date", "variable_name")) %>%
            dplyr::bind_rows(corrections)
    } else {
        if (log_info) logger::log_info(sprintf("No corrections available for %s\n", geo_type))
        df
    }
}


