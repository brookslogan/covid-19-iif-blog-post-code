# When we look at evaluation reports, we like to see multiple forecast dates.
# This script takes score cards from individual forecast dates and combines 
# them into score cards that have multiple forecast dates.
# It then saves these new aggregated score cards to file so they can be read
# into evaluation reports. 

library(tidyverse)
library(lubridate)

devtools::load_all("../evalforecast")

combine_scorecards = function(subdir) {
  ## read in all the score cards:
  scorecard_meta <- locate_cards(file.path("../../covid-19-iif-blog-post-data/smallcards",subdir))
  scorecards_raw <- scorecard_meta$filename %>% map(readRDS)
  scorecards_combined <- scorecard_meta %>% mutate(scorecard = scorecards_raw)
  ## combine all the forecast dates for each (forecaster, forecast task) pair:
  scorecards_combined <- scorecards_combined %>% 
    group_by(forecaster,
             ahead, 
             response,
             geo_type,
             incidence_period,
             n_locations) %>% 
    group_modify(~ {
      sc <- bind_rows(.x$scorecard)
      attr(sc, "metadata")$name <- .y$forecaster
      return(tibble(scorecard = list(sc)))
    }) %>% 
    ungroup()
  ## now write them out in a similar structure:
  scorecards_combined %>% 
    rowwise() %>% 
    group_walk(~ {
      path <- file.path(file.path("../../covid-19-iif-blog-post-data/largecards",subdir),
                        .x$ahead,
                        .x$response,
                        .x$geo_type,
                        .x$incidence_period,
                        .x$n_locations,
                        .x$forecaster)
      dir.create(path, recursive = TRUE)
      saveRDS(.x$scorecard[[1]], file.path(path, "scorecard.RDS"))
    })
}

combine_scorecards("ensembles")
for (subset_size in c(4L,8L,16L)) {
  combine_scorecards(sprintf("cheating_subset_ensembles/%d", subset_size))
  combine_scorecards(sprintf("oos_subset_ensembles/%d", subset_size))
  for (subset_i in seq_len(30L)) {
    combine_scorecards(sprintf("subset_ensembles/%d/%d", subset_size, subset_i))
  }
}
