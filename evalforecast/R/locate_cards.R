#' Locate all the predictions cards or scorecards in a directory
#' 
#' Given a structured directory tree that contains predictions cards or
#' scorecards saved as RDS files, this function prepares a convenient data frame
#' with the information contained in the directory tree structure and locates
#' the file names.  The default directory structure is
#' 
#' forecast_date/ahead/response/geo_type/incidence_period/n_locations/forecaster/scorecard.RDS
#' 
#' although this can be changed using the 
#' second argument.
#' 
#' 
#' @param cards_dir name of directory with the structure described above
#' @param dir_order specifies the order of the directories
#' @param cards_type either "scorecard.RDS", for scorecards or "out.RDS", for 
#' predictions cards.
#' @export
locate_cards <- function(cards_dir,
                         dir_order = c("forecast_date", 
                                       "ahead", 
                                       "response", 
                                       "geo_type", 
                                       "incidence_period",
                                       "n_locations",
                                       "forecaster"),
                         cards_type = c("scorecard.RDS", "out.RDS")) {
  cards_type <- match.arg(cards_type)
  files <- list.files(cards_dir, recursive = TRUE, full.names = TRUE)
  cards_files <- files %>% 
    str_subset(paste0(cards_type, "$"))
  cards_meta <- cards_files %>% 
    str_remove(paste0(cards_dir, "/")) %>% 
    str_remove(paste0("/", cards_type, "$")) %>% 
    str_split("/") %>% 
    map_dfr(~ set_names(.x, dir_order)) %>% 
    mutate(filename = cards_files)
  return(cards_meta)
}
