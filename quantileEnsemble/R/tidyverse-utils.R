
#' @source validate.R
NULL

#' Convert predcard_dt from \code{data.table} to \code{tibble}
#'
#' @importFrom data.table copy set
#' @importFrom tibble as_tibble
predcard_dt_to_predcard_tbl <- function(predcard_dt) {
    if (is.null(predcard_dt)) {
        NA
    } else {
        validate_predcard_dt(predcard_dt)
        predcard_dt <- copy(predcard_dt)
        set(predcard_dt,, "forecast_distribution", lapply(predcard_dt[["forecast_distribution_dt"]], as_tibble))
        set(predcard_dt,, "forecast_distribution_dt", NULL)
        as_tibble(predcard_dt)
    }
}

#' Convert predcards_dt from \code{data.table} to \code{tibble}
#'
#' @importFrom data.table copy set
#' @importFrom tibble as_tibble
predcards_dt_to_predcards_tbl <- function(predcards_dt) {
    validate_predcards_dt(predcards_dt)
    predcards_dt <- copy(predcards_dt)
    set(predcards_dt,, "predcard", lapply(predcards_dt[["predcard_dt"]], predcard_dt_to_predcard_tbl))
    set(predcards_dt,, "predcard_dt", NULL)
    as_tibble(predcards_dt)
}
