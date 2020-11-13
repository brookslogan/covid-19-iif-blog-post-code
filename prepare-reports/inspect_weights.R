
library("pipeR")
library("data.table")

## debug_weights_folder = "../../covid-19-iif-blog-post-data/debug_quantgen_weights/ensemble1_cdc_inconly/jhu-csse_deaths_incidence_num/epiweek/state/200/1/"
debug_weights_folder = "../../covid-19-iif-blog-post-data/debug_quantgen_weights/ensemble1_cdc_inconly/jhu-csse_deaths_incidence_num/epiweek/state/200/3/"

debug_weights_filenames = list.files(debug_weights_folder)

debug_weights_info = debug_weights_filenames %>>%
  setNames(basename(debug_weights_filenames)) %>>%
  lapply(function(filename) {
    readRDS(file.path(debug_weights_folder, filename))
  })

debug_weights_dt = debug_weights_info %>>%
  lapply(function(info) {
    data.table(forecaster = info[["forecasters"]],
               weight = info[["weights"]])
  }) %>%
  rbindlist(idcol="forecast_date") %>>%
  {(.
    [CJ("forecast_date"=unique(.[["forecast_date"]]), "forecaster"=unique(.[["forecaster"]])), on=c("forecast_date", "forecaster"), nomatch=NA]
    [, weight := `[<-`(weight, is.na(weight), 0)]
    [, forecast_date := as.Date(forecast_date)]
  )}

## asdf = debug_weights_dt %>>%
##   reshape2::acast(forecast_date ~ forecaster, value.var="weight") %>>%
##   {.[is.na(.)] <- 0; .} %>>%

debug_weights_dt %>>%
  ggplot2::ggplot(ggplot2::aes(forecast_date, weight, colour=forecaster)) %>>%
  `+`(ggplot2::geom_line())
