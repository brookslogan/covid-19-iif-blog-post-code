
library("pipeR")
library("data.table")

debug_weights_aheads_folder = "../../covid-19-iif-blog-post-data/debug_quantgen_weights/ensemble3_cdc_inconly/jhu-csse_deaths_incidence_num/epiweek/state/200"

debug_weights_filenames = list.files(debug_weights_aheads_folder, recursive=TRUE)

debug_weights_info = debug_weights_filenames %>>%
  setNames(paste0(dirname(.),"/",basename(.))) %>>%
  lapply(function(filename) {
    readRDS(file.path(debug_weights_aheads_folder, filename))
  })

debug_mweights_dt = debug_weights_info %>>%
  lapply(function(info) {
    data.table(forecaster = info[["forecasters"]],
               mweight = rowMeans(info[["weights"]], na.rm=TRUE))
  }) %>>%
  rbindlist(idcol="ahead/forecast_date") %>>%
  {(.
    [CJ(`ahead/forecast_date`=unique(.[["ahead/forecast_date"]]), `forecaster`=unique(.[["forecaster"]])), on=c("ahead/forecast_date", "forecaster"), nomatch=NA]
    [, mweight := `[<-`(mweight, is.na(mweight), 0)]
    [, ahead := as.integer(dirname(`ahead/forecast_date`))]
    [, forecast_date := as.Date(basename(`ahead/forecast_date`))]
    [, `ahead/forecast_date` := NULL]
    [] # (removes internal data.table invisible flag)
  )}

## debug_mweights_dt %>>%
##   ggplot2::ggplot(ggplot2::aes(forecast_date, mweight, colour=forecaster)) %>>%
##   `+`(ggplot2::geom_line())

## debug_mweights_dt[forecast_date >= as.Date("2020-09-01")][, list(mmweight=mean(mweight)), by="forecaster"][order(mmweight)]

forecaster_full_mmweights = debug_mweights_dt %>>%
  {
    list(
      df = setDF(.[, list(mmweight=mean(mweight)), by="forecaster"][order(-mmweight)]),
      max_forecast_date = max(.[["forecast_date"]])
    )
  }
saveRDS(forecaster_full_mmweights, "../../covid-19-iif-blog-post-data/debug_quantgen_weights/forecaster_full_mmweights.RDS")

forecaster_partial_mmweights =
  debug_mweights_dt[forecast_date <= median(unique(forecast_date))] %>>%
  {
    list(
      df = setDF(.[, list(mmweight=mean(mweight)), by="forecaster"][order(-mmweight)]),
      max_forecast_date = max(.[["forecast_date"]])
    )
  }
saveRDS(forecaster_partial_mmweights, "../../covid-19-iif-blog-post-data/debug_quantgen_weights/forecaster_partial_mmweights.RDS")
