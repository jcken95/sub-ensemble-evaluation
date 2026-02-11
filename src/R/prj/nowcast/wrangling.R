#' @name wrangling
#' @section Version: 0.0.1
#'
#' @title Functions for wrangling SGSS test data for nowcasting
#'
#' @description Helper functions wranging SGSS data with a view to nowcasting
#'
#' @seealso
#' * [wrangling$pre_triangle()]
#' * [wrangling$construct_reporting_triangle()]
#' * [wrangling$tests_to_counts()]
#' * [wrangling$model_quantiles()]
".__module__."

box::use(box / deps_)

.on_load <- function(ns) {
  deps_$need(
    "dplyr",
    "rlang",
    "stats",
    "tidyr",
    "tsibble"
  )
}

#' Helper for constructing a reporting triangle
#'
#' @param input_data data frame to be summarise
#' @param patient_id patient id column
#' @param reported_date date at which test was reported to database
#' @param onset_date disease onset date, or suitable proxy
#' @param minimum_date minimum `reported_date` to use. Defaults to `"2023-01-01"`
#' @return tibble
#'
#' @export
pre_triangle <- function(input_data, patient_id, reported_date, onset_date, minimum_date = "2023-01-01") {
  input_data |>
    dplyr::filter({{ reported_date }} >= minimum_date) |>
    # keep first test per person-episode
    dplyr::group_by({{ patient_id }}) |>
    dplyr::arrange({{ onset_date }}, {{ reported_date }}) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      target = 1,
      days_to_reported = as.numeric({{ reported_date }} - {{ onset_date }})
    ) |>
    dplyr::summarise(target = sum(target), .by = c({{ onset_date }}, days_to_reported))
}


#' Construct a reporting triangle
#'
#' @param testing_data tibble of VPD data
#' @param date_column column which identifies the date of interest
#' @param max_delay maximum assumed reporting delay (implicit unit is days)
#' @return tibble describing the reporting triangle
#'
#' @export
construct_reporting_triangle <- function(testing_data, date_column, max_delay) {
  if (max_delay <= 0) {
    stop("max_delay must be positive")
  }

  date_name <- rlang::as_name(rlang::enquo(date_column))

  reference_date <- dplyr::pull(testing_data, {{ date_column }})

  triangle <- tidyr::expand_grid(
    "{date_name}" := seq.Date( # nolint: object_name_linter.
      from = min(reference_date),
      to = max(reference_date),
      by = "day"
    ),
    days_to_reported = seq(0, max_delay, 1)
  ) |>
    dplyr::left_join(
      testing_data,
      by = c(date_name, "days_to_reported")
    ) |>
    # convert NAs to 0s
    dplyr::mutate(
      target = dplyr::coalesce(target, 0),
      # add NAs back for specimen date + days reported after the max date, i.e. in the future
      target = dplyr::if_else({{ date_column }} + days_to_reported > max({{ date_column }}), NA, target)
    )

  triangle
}


#' Take a data frame of disease testing data and return a complete time series of counts
#'
#' @param input_df data frame of incomplete time series data
#' @param date_column column containing relevant disease onset date (or suitable proxy)
#' @param group_by_columns columns to group tests by
#' @param ... arguments to pass to `tsibble::fill_gaps()` - most notably see `.start` and `.end`
#' @return tibble with at least `nrow(input_df)` rows and columns `target` (number disease cases per unit time)
#' `date_column` and `group_by_columns`
#'
#' @export
tests_to_counts <- function(input_df, date_column, group_by_columns = NULL, ...) {
  input_df |>
    dplyr::summarise(
      target = dplyr::n(),
      .by = c({{ date_column }}, {{ group_by_columns }})
    ) |>
    tsibble::as_tsibble(index = {{ date_column }},
      key = {{ group_by_columns }}) |>
    tsibble::fill_gaps(...) |>
    dplyr::mutate(target = dplyr::coalesce(target, 0)) |>
    tsibble::as_tibble()
}

#' Compute quantiles of predictions
#'
#' @param predictions tibble of posterior samples
#' @param target_var column of known number of cases
#' @param value_var column of posterior samples
#' @param by_var column(s) to aggregate samples by, e.g. date column or t_aggregation
#' @return tibble
#'
#' @export

model_quantiles <- function(predictions, target_var, value_var, by_var) {

  if (
    anyNA(dplyr::pull(predictions, {{ target_var }}))
  ) {
    stop("There are NAs in your target variable.")
  }

  predictions |>
    dplyr::summarise(
      {{ target_var }} := unique({{ target_var }}),
      .mean = mean({{ value_var }}),
      .q50 = stats::median({{ value_var }}),
      .q05 = stats::quantile({{ value_var }}, 0.05),
      .q25 = stats::quantile({{ value_var }}, 0.25),
      .q75 = stats::quantile({{ value_var }}, 0.75),
      .q95 = stats::quantile({{ value_var }}, 0.95),
      .by = {{ by_var }}
    ) |>
    dplyr::arrange(dplyr::across({{ by_var }})) |>
    dplyr::mutate(
      dplyr::across(
        !c({{ by_var }}, {{ target_var }}), ~ .x + {{ target_var }}
      )
    )
}
