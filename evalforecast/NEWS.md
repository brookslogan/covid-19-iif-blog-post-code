# Version 20200712-200328

- Updated the `get_predictions_card` to store `NA_REASON` as
  attribute in result. It will be an error message or
  `"Unimplemented"`. (naras)
- Added run_production function which imports packages `logger` and
  `fs`.
- Accounting for forecasters that may be `type = "ensemble"` in
  `run_production()`. 
