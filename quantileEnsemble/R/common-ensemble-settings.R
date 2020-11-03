
#' @source path-utils.R
NULL

get_historical_components_dirpath <- function(repo_root_dirpath=get_repo_root_dirpath()) {
    file.path(repo_root_dirpath, "smallcards", "historical_ensemble_components")
}

get_prospective_components_dirpath <- function(repo_root_dirpath=get_repo_root_dirpath()) {
    file.path(repo_root_dirpath, "forecaster_predictions")
}

get_hub_components_dirpath <- function(repo_root_dirpath=get_repo_root_dirpath()) {
    file.path(repo_root_dirpath, "smallcards", "historical_cdc_components_all")
}

#' @importFrom pipeR %>>%
get_hub_components_names_helper <- function(hub_components_dirpath) {
  pattern = "^(\\d+-\\d{2}-\\d{2})/(\\d+)/([-\\w]+)/([-\\w]+)/([-\\w]+)/(\\d+)/([-\\w]+)/.*\\.RDS$"
  unique(sub(pattern,
             "\\7",
             list.files(hub_components_dirpath, recursive=TRUE) %>>% {.[grepl(pattern,.,perl=TRUE)]},
             perl=TRUE))
}

get_hub_components_names <- function(repo_root_dirpath=get_repo_root_dirpath()) {
    hub_components_dirpath = get_hub_components_dirpath(repo_root_dirpath)
    get_hub_components_names_helper(hub_components_dirpath)
}

## FIXME has numerical differences vs. scorecards
cdc_probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
