## Global variables for passing R cmd check.

## Note: start and end are generic functions, so be explicit if
## using them, i.e. stats::start, stats::end
##

## Hint: Processed from R CMD check after manual sanity checks
global_vars  <- c(
    "actual",
    "align_date",
    "alpha",
    "date0",
    "dist_from_interval",
    "end",
    "epiweek",
    "first_date",
    "forecast_date",
    "forecast_distribution",
    "issue_date",
    "location_name",
    "lower",
    "n_locations",
    "na_prediction",
    "original_value",
    "reference_date",
    "response",
    "start",
    "target_end",
    "target_period_dates",
    "target_start",
    "upper",
    "value",
    "variable_name",
    "variable_name.x",
    "variable_name.y",
    "width"
)

utils::globalVariables(global_vars)
