
library("pipeR")
library("data.table")

## debug_weights_folder = "../../covid-19-iif-blog-post-data/debug_quantgen_weights/ensemble1_cdc_inconly/jhu-csse_deaths_incidence_num/epiweek/state/200/1/"
## debug_weights_folder = "../../covid-19-iif-blog-post-data/debug_quantgen_weights/ensemble1_cdc_inconly/jhu-csse_deaths_incidence_num/epiweek/state/200/3/"
debug_weights_folder = "../../covid-19-iif-blog-post-data/debug_quantgen_weights/ensemble3_cdc_inconly/jhu-csse_deaths_incidence_num/epiweek/state/200/3/"

debug_weights_filenames = list.files(debug_weights_folder)

debug_weights_info = debug_weights_filenames %>>%
  setNames(basename(debug_weights_filenames)) %>>%
  lapply(function(filename) {
    readRDS(file.path(debug_weights_folder, filename))
  })

debug_mweights_dt = debug_weights_info %>>%
  lapply(function(info) {
    data.table(forecaster = info[["forecasters"]],
               mweight = rowMeans(info[["weights"]], na.rm=TRUE))
  }) %>>%
  rbindlist(idcol="forecast_date") %>>%
  {(.
    [CJ("forecast_date"=unique(.[["forecast_date"]]), "forecaster"=unique(.[["forecaster"]])), on=c("forecast_date", "forecaster"), nomatch=NA]
    [, mweight := `[<-`(mweight, is.na(mweight), 0)]
    [, forecast_date := as.Date(forecast_date)]
    [] # (removes internal data.table invisible flag)
  )}

## asdf = debug_mweights_dt %>>%
##   reshape2::acast(forecast_date ~ forecaster, value.var="weight") %>>%
##   {.[is.na(.)] <- 0; .} %>>%

debug_mweights_dt %>>%
  ggplot2::ggplot(ggplot2::aes(forecast_date, mweight, colour=forecaster)) %>>%
  `+`(ggplot2::geom_line())

## debug_mweights_dt[forecast_date >= as.Date("2020-07-01")][, list(mmweight=mean(mweight)), by="forecaster"][order(mmweight)]
debug_mweights_dt[forecast_date >= as.Date("2020-09-01")][, list(mmweight=mean(mweight)), by="forecaster"][order(mmweight)]
