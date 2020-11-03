#' @export
select_dates <- function(df, forecast_date, response, incidence_period, ahead,backfill_buffer,train_forecast_date = NULL){
  # This is a helper function which tells you for each variable the issue date and reference date
  # to use for training and testing.
  # It also tells you which date to use as your training forecast date.
  # For a dictionary of date related terms, and an explanation of what this function is doing and why
  # see the README in hospitalization/pipeline.
  # Inputs:
  #   df: data frame with columns "location", "reference_date", "issue_date",
  #       as well as "variable_name" and "value".
  #
  #   forecast_date: vector of class Date, e.g., as created by
  #    lubridate::ymd("2020-04-21").
  #
  #   response: name of a variable to take as the response
  #
  #   incidence_period: one of "epiweek" or "day"
  #
  #   ahead: how many epiweeks/days ahead are you forecasting?
  #
  #   backfill_buffer: How many days until response is deemed trustworthy
  #    enough to be taken as correct?
  #
  # Outputs: a list comprised of the following elements
  #
  #   x_date_target: a dataframe specifying the reference dates and issue dates to use for each
  #                             variable in constructing x_target.
  #   train_forecast_date: the forecast date to use for training.
  #   x_date_train: analogous to x_date_target.
  #

  if(!is.null(train_forecast_date) & !(class(train_forecast_date) %in% "Date"))
    stop("train_forecast_date must be of class Date.")

  # (0) Gather the unique issue dates for each variable.
  train_issue_dates <- df %>%
    group_by(variable_name) %>%
    group_modify( ~ unique(.x$issue_date) %>% enframe(name = NULL, value = "issue_date")) %>%
    ungroup()

  # (1) Make x_date_target.
  #     For each variable, the reference date and the issue date will be as late as possible.
  x_date_target <- train_issue_dates %>%
    group_by(variable_name) %>% summarise(issue_date = max(issue_date)) %>% ungroup %>% # always take the latest issue
    mutate(reference_date =
             reference_date_from_issue_and_forecast_date(issue_date,variable_name,forecast_date))

  # (2) Pick train_forecast_date as late as possible,
  #     such that we can still hope to observe
  #     stable values of the response variable in our training data.
  if(is.null(train_forecast_date))
  {
    train_forecast_date <- latest_possible_forecast_date(df,response = response,
                                                         incidence_period = incidence_period,
                                                         ahead = ahead,
                                                         backfill_buffer)
  } else{
    latest_train_forecast_date <- latest_possible_forecast_date(df,response = response,
                                                                incidence_period = incidence_period,
                                                                ahead = ahead,
                                                                backfill_buffer)
    if(train_forecast_date > latest_train_forecast_date)
      warning(paste0("Argument train_forecast_date is later than recommended maximum train forecast date of ",
              latest_train_forecast_date))
  }

  # (3) Make x_date_train.
  #     For each variable, clearly reference date - forecast_date
  #     must be the same when training as when predicting; this determines
  #     the train reference date, since we already know the train forecast date,
  #     the target forecast date, and the target reference date.
  #
  #     Then the issue date should be the first issue date after the forecast date for that
  #     variable.
  x_date_train <- train_issue_dates %>%
    left_join(x_date_target %>% select(variable_name,reference_date), by = c("variable_name")) %>%
    mutate(reference_date = train_forecast_date + (reference_date - forecast_date)) %>%
    group_by(variable_name,reference_date) %>%
    summarise(issue_date = issue_date_from_reference_date(issue_date,reference_date,variable_name)) %>%
    ungroup

  # (4) Issue warnings if there isn't alignment between reference and issue date.
  #     We would like that reference_date - issue_date be the same in training and predicting.
  #     However, due to the irregular nature of issue times, that may be impossible.
  #     We issue a warning when it does not happen, suggesting the user
  #     should make use only of stabilized features.
  x_date <- left_join(x_date_train %>% rename(reference_date_train = reference_date,
                                              issue_date_train = issue_date),
                      x_date_target,by = "variable_name") %>%
    mutate(source_name = str_extract(variable_name,"[^_]+")) %>%
    group_by_at(vars(-contains("variable_name"))) %>% summarise %>% ungroup
  for(ii in 1:nrow(x_date))
    check_dates(x_date[ii,], backfill_buffer)

  return(list(x_date_target = x_date_target,
              train_forecast_date = train_forecast_date,
              x_date_train = x_date_train))

}

#' @export
reference_date_from_issue_and_forecast_date <- function(issue_date,variable_name,forecast_date){
  # Rule for determining what reference date to use for a given issue date.
  # A placeholder for now.
  if_else(grepl("hospitalization", variable_name), min(issue_date - 1,forecast_date),forecast_date)
}

#' @export
issue_date_from_reference_date <- function(issue_date, reference_date,variable_name){
  # Rule for determining what issue date to use for a given reference date, among the available options.
  # A placeholder for now.
  if(any(grepl("hospitalization", variable_name)))
     min(subset(issue_date,issue_date >= reference_date + 1))
  else
    min(issue_date)
}

#' @export
check_dates <- function(x,backfill_buffer){
  # Rule for determining whether the difference between
  #      issue_date - reference_date
  #  and
  #      issue_date_train - reference_date_train
  #
  # is cause for concern. If it is, we give a detailed warning telling the modeler
  # what to do.
  if(grepl("hospitalization", x$source_name)){
    if(!( (x$issue_date - x$reference_date >= backfill_buffer) |
          (x$issue_date - x$reference_date == x$issue_date_train - x$reference_date_train) ) )
    {
      warning_sentence_1 <- paste0("\nThis warning is regarding ", x$source_name, " features.\n")
      warning_sentence_2 <- paste0("Your backfill buffer is ", backfill_buffer," days.\n")
      warning_sentence_3 <- paste0(
        "For forecasting, you are using feature data which has solidified for only ",
        ifelse(x$issue_date - x$reference_date >= backfill_buffer,
               paste0(x$issue_date - x$reference_date,"(>=",backfill_buffer, ") days.\n"),
               paste0(x$issue_date - x$reference_date, " days.\n"))
      )
      warning_sentence_4 <- paste0(
        "For training, you are using feature data which has solidified for ",
        ifelse(x$issue_date_train - x$reference_date_train >= backfill_buffer,
               paste0(x$issue_date_train - x$reference_date_train,"(>=",backfill_buffer, ") days.\n"),
               paste0(x$issue_date_train - x$reference_date_train, " days.\n"))
      )
      warning_sentence_5 <- paste0(
        "We recommend that you use only lags of at least ",
        backfill_buffer, " - ", (x$issue_date - x$reference_date), " = ", backfill_buffer - (x$issue_date - x$reference_date),
        " for this variable in your model.\n"
      )
      warning(paste0(warning_sentence_1,
                     warning_sentence_2,
                     warning_sentence_3,
                     warning_sentence_4,
                     warning_sentence_5))
    }
  }
}

#' @export
#' @importFrom fuzzyjoin fuzzy_left_join
#' @importFrom lubridate today
replace_na_issue_dates <- function(df)
{
  # Placeholder function to handle NA issue dates, using a backfill_variables dataframe.
  # We don't know how to handle NA issue dates for non-hospitalization related variables
  # since no one is keeping track of backfill yet.
    fuzzy_left_join(df,backfill_variables,by = "variable_name", match_fun = str_detect) %>%
    mutate(issue_date = coalesce(issue_date,first_date)) %>%
    mutate(issue_date = coalesce(issue_date,lubridate::today())) %>%
    rename(variable_name = variable_name.x) %>%
    select(-c(variable_name.y,first_date))
}
