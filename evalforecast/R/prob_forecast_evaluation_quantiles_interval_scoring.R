#library(tidyverse)

#' @export
weighted_interval_score <- function(quantile_forecasts, actual_value) {
  # computes the weighted interval score
  #
  # interval score: width + (2/alpha) * dist_from_interval
  #
  # weighted interval score:
  # (|y - median| + sum_k (alpha_k/2) * interval_score_k) / (num_intervals + 1)
  #
  # (|y - median| + sum_k [alpha_k*width_k/2 + dfi_k]) / (num_intervals + 1)
  # where dfi_k = dist_from_interval_k
  if (is.na(actual_value)) return(NA)
  num_prob <- nrow(quantile_forecasts) # 23
  stopifnot(num_prob %% 2 == 1, num_prob >= 3)
  num_intervals <- (num_prob - 1) / 2 # 11
  q <- quantile_forecasts$quantiles
  probs <- quantile_forecasts$probs
  stopifnot(abs(probs + rev(probs) - 1) < 1e-8)
  stopifnot(diff(q) >= 0 | is.na(diff(q)))
  # note: I will treat the median as a 0% predictive interval
  # (alpha = 1) of width 0.  This is equivalent to the expression above
  int <- tibble(lower = q[1:(num_intervals + 1)],
                upper = q[num_prob:(num_prob - num_intervals)],
                alpha = 2 * probs[1:(num_intervals + 1)],
                width = upper - lower,
                dist_from_interval = pmax(actual_value - upper,
                                          lower - actual_value,
                                          0),
                scaled_int_scores = alpha * width / 2 + dist_from_interval)
  return(mean(int$scaled_int_scores))
}
