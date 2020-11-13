
#' Get "visible" version of a data.table
#'
#' \code{data.table} objects  have an internal version  of invisibility separate
#' from  \code{base::invisible}, which  disables \code{print}  output the  first
#' time that they are printed after  certain operations (e.g., mutating to add a
#' column).  This function ensures that internal invisibility flag is unset.
#'
#' @return the data.table with its internal invisibility flag unset
visible_dt <- function(DT) {
    DT[]
}

#' @importFrom data.table set rbindlist
unnest_dt_column_in_dt <- function(DT, column.name, fill=FALSE) {
    column.values = DT[[column.name]]
    ## TODO check that is list of DT's
    if (any(vapply(column.values, is.null, logical(1L)))) {
        stop ('unnesting with NULL not yet supported')
    }
    cbind(set(DT[rep.int(seq_along(column.values), vapply(column.values, nrow, integer(1L)))],, column.name, NULL),
          rbindlist(DT[[column.name]], fill=fill))
}
