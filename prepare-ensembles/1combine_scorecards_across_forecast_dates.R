# When we look at evaluation reports, we like to see multiple forecast dates.
# This script takes score cards from individual forecast dates and combines 
# them into score cards that have multiple forecast dates.
# It then saves these new aggregated score cards to file so they can be read
# into evaluation reports. 

library(tidyverse)
library(lubridate)

devtools::load_all("../evalforecast")

# read in all the score cards:
scorecard_meta <- locate_cards("../../covid-19-iif-blog-post-data/smallcards/ensembles")
scorecards_raw <- scorecard_meta$filename %>% map(readRDS)
scorecards_combined <- scorecard_meta %>% mutate(scorecard = scorecards_raw)
# combine all the forecast dates for each (forecaster, forecast task) pair:
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
# now write them out in a similar structure:
scorecards_combined %>% 
  rowwise() %>% 
  group_walk(~ {
    path <- file.path("../../covid-19-iif-blog-post-data/largecards/ensembles",
                      .x$ahead,
                      .x$response,
                      .x$geo_type,
                      .x$incidence_period,
                      .x$n_locations,
                      .x$forecaster)
    dir.create(path, recursive = TRUE)
    saveRDS(.x$scorecard[[1]], file.path(path, "scorecard.RDS"))
  })

