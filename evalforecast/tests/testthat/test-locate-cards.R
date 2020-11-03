# here we create a directory structure containing many scorecards and predictions
# cards and then we check that evalforecast::locate_cards works for locating
# these cards

data(df)

response <- "jhu-csse_deaths_incidence_num"
geo_type <- "state"

forecasting_packages  <- c("strawman", "poiszero")
# devtools::install("../../../forecaster_pkgs/strawman/")
# devtools::install("../../../forecaster_pkgs/poiszero/")

output_dir  <- tempdir()

incidence_period <- "epiweek"
n_locations <- 2
forecast_dates <- as.list(lubridate::ymd("2020-05-11") - 7 * (0:1))

for (i_fdate in seq_along(forecast_dates)) {
  forecast_date  <- forecast_dates[[i_fdate]]
  for (ahead in 1:4) {
    forecasters <- do.call(c,
                           unlist(
                             lapply(forecasting_packages,
                                    function(pkg) {
                                      utils::getFromNamespace(x = "get_forecasters", ns = pkg)(
                                        response = response,
                                        incidence_period = incidence_period,
                                        ahead = ahead,
                                        geo_type = geo_type,
                                        n_locations = n_locations,
                                        forecast_date = forecast_date)
                                    }),
                             recursive = FALSE)
    )
    result  <- evalforecast::run_forecasters(forecasters = forecasters,
                                             response = response,
                                             incidence_period = incidence_period,
                                             ahead = ahead,
                                             geo_type = geo_type,
                                             n_locations = n_locations,
                                             forecast_date = forecast_date,
                                             data_list = list(state = df))
    for (fn in names(result)) {
      output_path  <- fs::path(output_dir, forecast_date, ahead, response,
                               geo_type, incidence_period, n_locations, fn)
      if (!fs::dir_exists(output_path)) fs::dir_create(output_path, recurse = TRUE)
      saveRDS(result[[fn]], file = fs::path(output_path, "out.RDS"))
      
      if (is.na(result[[fn]])) next
      scorecard <- evaluate_quantile_predictions_cards(df, list(result[[fn]]), 
                                                       backfill_buffer = 0)
      saveRDS(scorecard, file = fs::path(output_path, "scorecard.RDS"))
    }
  }
}

test_that("multiple prediction cards", {
  library(dplyr)
  pc_meta <- locate_cards(output_dir, cards_type = "out.RDS")
  sc_meta <- locate_cards(output_dir, cards_type = "scorecard.RDS")
  expect_equal(pc_meta %>% count(forecast_date, ahead) %>% pull(n), rep(5, 8))
  sc <- readRDS(sc_meta$filename[1])
  metadata <- attr(sc, "metadata")
  expect_equal(metadata$response, sc_meta$response[1])
  expect_equal(metadata$incidence_period, sc_meta$incidence_period[1])
  expect_equal(metadata$ahead, as.numeric(sc_meta$ahead[1]))
  expect_equal(metadata$geo_type, sc_meta$geo_type[1])
  expect_equal(metadata$n_locations, as.numeric(sc_meta$n_locations)[1])
  pc <- readRDS(pc_meta$filename[1])
  attributes(pc)$call_args <- NULL
  attributes(sc)$metadata <- NULL
  attributes(sc)$hidden_metadata <- NULL
  expect_equal(pc, sc %>% select(location, forecast_date, forecast_distribution))
})

unlink(output_dir, recursive = TRUE)


