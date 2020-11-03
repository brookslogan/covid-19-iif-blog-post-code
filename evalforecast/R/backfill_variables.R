#' @export
#' @importFrom lubridate ymd
backfill_variables <- tribble(~variable_name,     ~first_date,
                              "hospitalizations", lubridate::ymd("2020-05-07"))

# the data frame above records...
# (i)  which variables have backfill
# (ii) date of the first data dump we have (note: we do not have access to
#      the pre-backfilled data before this date)
