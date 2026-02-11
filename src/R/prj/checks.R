#' @name checks
#' @section Version: 0.0.3
#'
#' @title
#' Checks to ensure data is in the right format for further processing
#'
#' @seealso
#' * [checks$check_forecast_format_sample()]
#' * [checks$check_forecast_format_summary()]
".__module__."

box::use(
  box / deps_,
  box / help_
)

.on_load <- function(ns) {
  deps_$need(
    "glue",
    "rlang",
    "waldo"
  )
}

#' Check forecast data format for summaries (quantiles)
#'
#' Checks the format of data to make sure it is compliant
#' with agreed forecast model schema:
#' * All required columns are present
#' * No additional columns
#' * No duplicate rows
#'
#' @param data data frame from formatted model output
#'
#' @returns `TRUE` (invisibly) if the check was successful; otherwise,
#'  an error will be thrown with an informative message.
#'
#' @examples
#' checks$check_forecast_format_summary(calls_formatted)
#'
#' @export
check_forecast_format_summary <- function(.data) {

  check_colnames(
    colnames(.data),
    c(
      "model",
      "prediction_start_date",
      "location",
      "location_level",
      "age_group",
      "age_group_granularity",
      "population",
      "target_name",
      "target_value",
      "date",
      "forecast_horizon",
      "p_increase",
      "p_stable",
      "p_decrease",
      "pi_50",
      "pi_5",
      "pi_95",
      "pi_2.5",
      "pi_97.5",
      "pi_25",
      "pi_75",
      "pi_17",
      "pi_83"
  ))

  duplicate_rows <- .data[.data |> duplicated(), ]

  if (nrow(duplicate_rows) > 0) {
    stop(glue::glue(
      "There are {nrow(duplicate_rows)} duplicated rows in this dataframe"))
  }

  invisible(TRUE)
}



#' Check forecast data format for sample (draws from posterior)
#'
#' Checks the format of data to make sure it is compliant
#' with agreed forecast model schema:
#' * All required columns are present
#' * No additional columns
#' * No duplicate rows
#'
#' @param data data frame from formatted model output
#'
#' @returns `TRUE` (invisibly) if the check was successful; otherwise,
#'  an error will be thrown with an informative message.
#'
#' @examples
#' checks$check_forecast_format_sample(calls_formatted)
#'
#' @export
check_forecast_format_sample <- function(.data) {

  check_colnames(
    colnames(.data),
    c(
      "model",
      "prediction_start_date",
      "location",
      "location_level",
      "age_group",
      "age_group_granularity",
      "population",
      "target_name",
      "target_value",
      "date",
      "forecast_horizon",
      ".sample",
      ".value"
    )
  )

  duplicate_rows <- .data[.data |> duplicated(), ]

  if (nrow(duplicate_rows) > 0) {
    stop(glue::glue(
      "There are {nrow(duplicate_rows)} duplicated rows in this dataframe"))
  }

  if (length(unique(.data$.sample)) <= 1) {
    stop("There's only one or fewer samples in this data")
  }

  invisible(TRUE)
}



check_colnames <- function(actual, required) {
  if (all(actual == required))
    return(invisible(TRUE))

  # Note: would typically use cli::cli_abort(), but can't here because it squashes
  # important whitespace in the waldo output!
  rlang::abort(
    c(
      "x" = "Data must contain all the correct columns, in the correct order:",
      "",
      waldo::compare(
        required,
        actual,
        x_arg = "required",
        y_arg = "actual",
        max_diffs = Inf,
        quote_strings = FALSE # forces side-by-side layout
      )
    ),
    use_cli_format = FALSE
  )
}
