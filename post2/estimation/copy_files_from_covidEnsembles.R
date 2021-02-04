library(dplyr)
library(stringr)

# it is expected that this script is run with the working directory set to the
# root of the covid-19-iif-blog-post-code repository, and that the
# covid-19-iif-blog-post-code and covid-19-iif-blog-post-data repositories are
# set to the same location

# path to covidEnsembles repository -- retrospective ensemble forecasts and
# component weight estimates will be copied from there to the
# covid-19-iif-blog-post-data repository.
covidEnsembles_path <- "/home/eray/research/epi/covid/covidEnsembles/"

model_dirs <- list.dirs(
  paste0(covidEnsembles_path, 
    "code/application/retrospective-qra-comparison/retrospective-forecasts/state"),
  full.names = FALSE,
  recursive = FALSE)
model_dirs <- model_dirs[!grepl("prospective_selection", model_dirs)]

for (model_dir in model_dirs) {
  for (response_var in c("inc_case", "inc_death")) {
    if (response_var == "inc_case") {
      first_forecast_date <- "2020-09-14"
    } else {
      first_forecast_date <- "2020-06-22"
    }

    for (object_type in c("forecasts", "weights")) {
      # copy forecast files over
      files <- Sys.glob(paste0(
        covidEnsembles_path,
        "code/application/retrospective-qra-comparison/retrospective-",
        object_type,
        "/state/",
        model_dir,
        "/", response_var, "*.csv"
      ))

      for (file in files) {
        target_file <- gsub(
          paste0(
            covidEnsembles_path,
            "code/application/retrospective-qra-comparison/"),
          "../covid-19-iif-blog-post-data/post2/",
          file)
        
        dir_end_ind <- stringr::str_locate_all(target_file, "/")[[1]] %>%
          tail(1) %>%
          `[`(1)
        
        # skip forecast dates outside of range used for blog post
        file_date <- substr(
          target_file,
          dir_end_ind + nchar(response_var) + 2,
          dir_end_ind + nchar(response_var) + 11)
        
        if (file_date < first_forecast_date || file_date > "2021-01-18") {
          next
        }
        
        target_dir <- substr(target_file, 1, dir_end_ind)
        if (!dir.exists(target_dir)) {
          dir.create(target_dir, recursive = TRUE)
        }
        file.copy(from = file, to = target_file)
      }
    }
  }
}
