
validate_character <- function(object) {
    if (!is.character(object)) {
        stop (sprintf('expected %s to satisfy is.character',
                      paste(deparse(match.call()[["object"]]), collapse="\n")))
    } else {
        return (object)
    }
}

#' @importFrom stats setNames
#' @importFrom data.table is.data.table
validate_predcard_dt <- function(object) {
    if (!is.data.table(object)) {
        stop (sprintf('expected %s to satisfy data.table::is.data.table',
                      paste(deparse(match.call()[["object"]]), collapse="\n")))
    } else {
        expected_names = c("location", "forecast_date", "forecast_distribution_dt")
        names_are_present = setNames(expected_names %in% names(object), expected_names)
        if (!all(names_are_present)) {
            stop (sprintf('Name check on %s failed with output:\n%s\nand names:\n%s',
                          paste(deparse(match.call()[["object"]]), collapse="\n"),
                          paste(capture.output(print(names_are_present)), collapse="\n"),
                          paste(capture.output(print(names(object))), collapse="\n")))
        } else {
            ## FIXME validate columns further
            return (object)
        }
    }
}

#' @importFrom data.table is.data.table
validate_predcards_dt <- function(object) {
    if (!is.data.table(object)) {
        stop (sprintf('expected %s to satisfy data.table::is.data.table',
                      paste(deparse(match.call()[["object"]]), collapse="\n")))
    } else {
        name_check = all.equal(names(object),
                               c("forecast_date", "forecaster_name", "predcard_dt"))
        if (!isTRUE(name_check)) {
            stop (sprintf('Name check on %s failed with output:\n%s\nand names:\n%s',
                          paste(deparse(match.call()[["object"]]), collapse="\n"),
                          paste(capture.output(print(name_check)), collapse="\n"),
                          paste(capture.output(print(names(object))), collapse="\n")))
        } else if (!inherits(object[["forecast_date"]], "Date")) {
            stop (sprintf('Expected (%s)$forecast_date to satisfy is.Date',
                          paste(deparse(match.call()[["object"]]), collapse="\n")))
        } else if (!is.character(object[["forecaster_name"]])) {
            stop (sprintf('Expected (%s)$forecaster_name to satisfy is.character',
                          paste(deparse(match.call()[["object"]]), collapse="\n")))
        } else if (!is.list(object[["predcard_dt"]])) {
            stop (sprintf('Expected (%s)$predcard_dt to to be a list',
                          paste(deparse(match.call()[["object"]]), collapse="\n")))
        } else if (
            !tryCatch({for (predcard_dt in object[["predcard_dt"]]) if (!is.null(predcard_dt)) validate_predcard_dt(predcard_dt); TRUE},
                     error=function(e)
                         stop (sprintf('Expected (%s)$predcard_dt to be a list with elements that are either NULL or clear validate_predcard_dt, but instead obtained error: %s',
                                       paste(deparse(match.call()[["object"]]), collapse="\n"),
                                       e))
                     )
            ) {
            stop ('internal error in validate_object')
        } else {
            return (object)
        }
    }
}
