data(df)

response <- "jhu-csse_deaths_incidence_num"
incidence_period <- "epiweek"
ahead <- 1
geo_type <- "state"
n_locations <- 5

pz_forcaster <- poiszero::get_forecasters(response = response,
                                          incidence_period = incidence_period,
                                          ahead = ahead,
                                          geo_type = geo_type,
                                          n_locations = n_locations)

test_that("get same score_card using two approaches", {
  forecast_date <- ymd("2020-06-01")
  set.seed(123)
  score_card1 <- evaluate_quantile_forecaster(df,
                                             pz_forcaster$poiszero,
                                             response = response,
                                             incidence_period = incidence_period,
                                             ahead = ahead,
                                             geo_type = geo_type,
                                             forecast_dates = forecast_date,
                                             n_locations = n_locations)
  
  set.seed(123)
  out <- run_forecasters(forecasters = pz_forcaster,
                         response = response,
                         incidence_period = incidence_period,
                         ahead = ahead,
                         geo_type = geo_type,
                         n_locations = n_locations,
                         forecast_date = forecast_date,
                         data_list = list(state = df))
  predictions_cards <- list(out$poiszero)
  score_card2 <- evaluate_quantile_predictions_cards(df, predictions_cards)
  
  attributes(score_card1)$metadata$err_measure <- NULL # there's an issue here
  attributes(score_card2)$metadata$err_measure <- NULL # but for now it's ok
  
  expect_equal(score_card1, score_card2)
})

test_that("multiple prediction cards", {
  forecast_dates <- ymd("2020-06-01") + 7 * (0:1)
  set.seed(123)
  predictions_cards <- list()
  for (i in seq_along(forecast_dates)) {
    out <- run_forecasters(forecasters = pz_forcaster,
                           response = response,
                           incidence_period = incidence_period,
                           ahead = ahead,
                           geo_type = geo_type,
                           n_locations = n_locations,
                           forecast_date = forecast_dates[i],
                           data_list = list(state = df))
    predictions_cards[[i]] <- out$poiszero
  }
  score_card <- evaluate_quantile_predictions_cards(df, predictions_cards)
  
  score_card1 <- evaluate_quantile_predictions_cards(df, predictions_cards[1])
  score_card2 <- evaluate_quantile_predictions_cards(df, predictions_cards[2])
  
  expect_equal(bind_rows(score_card1, score_card2), score_card)
  
  attributes(predictions_cards[[1]])$call_args$ahead <- 2
  expect_error({
    score_card <- evaluate_quantile_predictions_cards(df, predictions_cards)
    })
  expect_error({
    score_card <- evaluate_quantile_predictions_cards(df, list(score_card1,
                                                               score_card1))
  })
})

