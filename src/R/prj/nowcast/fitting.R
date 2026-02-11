#' @name fitting
#' @section Version: 0.0.1
#'
#' @title Functions for fitting nowcasts for infectious disease mopdelling
#'
#' @description Functions to fit nowcasts to SGSS testing data
#'
#' @seealso
#' * [fitting$run_scripted_model()]
#' * [fitting$pertussis_gam()]
".__module__."

box::use(
  box / deps_,
  prj / intervals,
  . / wrangling
)

.on_load <- function(ns) {
  deps_$need(
    "dplyr",
    "furrr",
    "gratia",
    "lubridate",
    "mgcv",
    "purrr"
  )
}


# General ----

#' Wrapper function for running the selected model from script
#'
#' Takes data, parameters, and model selection to run over each lookback.
#' This wrapper can also run multiple models in parallel on different lookbacks.
#'
#' @param model_function Function defining a model to be fitted
#' @param training_data Data frame of the input data.
#' @param prediction_end_dates A sequence of dates defining the start of each look back.
#' @param required_covariates A list of strings of data's column names to keep,
#' generally already listed in the config.
#' @param model_hyperparams A named list of numerical priors for the model,
#' also generally found in the config.
#' @param n_pi_samples Number of posterior samples to calculate
#'
#'
#' @returns Combined model outputs, as a data frame.
#'
#' @export

run_scripted_model <- function(
    model_function,
    training_data,
    prediction_end_dates,
    model_formula,
    output_columns,
    model_hyperparams,
    n_pi_samples,
    model_description = ""
    ) {
  ## end date is a model hyperparameter

  hypers <- purrr::map(prediction_end_dates,
    \(end_date) purrr::list_modify(model_hyperparams, prediction_end_date = end_date))

  model_outputs <- furrr::future_map(
    hypers,
    \(model_hyperparams) {
      model_function(training_data, model_formula, model_hyperparams, output_columns, n_pi_samples)
    },
    .options = furrr::furrr_options(
      seed = NULL,
      packages = c(
        "data.table",
        "dplyr",
        "here",
        "lubridate",
        "purrr",
        "stats"
      )
    )
  )

  names(model_outputs) <- as.character(prediction_end_dates)

  return(model_outputs)

}

# Specific ----

#' Fit a gam to pertussis data
#' @param input_data camera-ready gam data
#' @param formula a model formula
#' @param model_hyperparams A named list of numerical priors for the model,
#' also generally found in the config.
#' @param n_pi_samples Number of posterior samples to calculate
#' @param prediction_end_date final date for prediction (yyyy-mm-dd)
#'
#' @export
pertussis_gam <- function(input_data, formula, model_hyperparameters, chosen_output_columns, n_pi_samples) {

  reporting_triangle <- input_data |>
    dplyr::filter(specimen_date <= model_hyperparameters$prediction_end_date) |>
    wrangling$pre_triangle(cdr_opie_id, sgss_received_date, specimen_date) |>
    wrangling$construct_reporting_triangle(specimen_date, model_hyperparameters$max_delay)
  reporting_triangle <- reporting_triangle |>
    dplyr::filter(
      specimen_date <= model_hyperparameters$prediction_end_date,
      specimen_date > lubridate::ymd(model_hyperparameters$prediction_end_date) -
        lubridate::days(model_hyperparameters$training_length)
    ) |>
    dplyr::filter(days_to_reported <= model_hyperparameters$max_delay) |>
    dplyr::mutate(
      origin = as.numeric(specimen_date - min(specimen_date)) + 1, # +1 to make model happy
      # add NAs for specimen date + days reported after the max date, i.e. in the future
      target = ifelse(specimen_date + days_to_reported > model_hyperparameters$prediction_end_date, NA, target),
      dow_specimen_date = lubridate::wday(specimen_date, week_start = 1),
      dow_report_date = lubridate::wday(specimen_date + days_to_reported, week_start = 1),
      weekend_reporting = ifelse((dow_report_date == 6 | dow_report_date == 7), 1, 0),
      dow_specimen_date_factor = as.factor(dow_specimen_date),
      dow_report_date_factor = as.factor(dow_report_date),
      weekend_reporting_factor = as.factor(weekend_reporting),
    )

  reporting_triangle_fit <- dplyr::filter(reporting_triangle, !is.na(target))

  # fitting ----

  fitted_gam <- mgcv::gam(formula, family = "nb", data = reporting_triangle_fit)

  # process fit ----

  # Generate samples from model fit coefficients for unknown data

  known_data <- reporting_triangle_fit |>
    dplyr::filter(specimen_date + days_to_reported <= model_hyperparameters$prediction_end_date)

  reporting_triangle_out <- dplyr::filter(reporting_triangle, is.na(target))

  fits_out <- intervals$generate_samples(
    .model = fitted_gam,
    .data = reporting_triangle_out,
    .n_pi_samples = n_pi_samples,
    .method = "mh"
  )

  # tidy samples
  output_data_samples <- fits_out |>
    dplyr::select(dplyr::any_of(chosen_output_columns)) |>
    # Monday first DoW ==> week_start = 1
    dplyr::mutate(week_starting = lubridate::floor_date(specimen_date, unit = "week", week_start = 1))

  # sum into daily total positive tests for each sample and add target

  current_reports <- dplyr::summarise(known_data,
    target = sum(target, na.rm = TRUE),
    .by = specimen_date)

  daily_sample_predictions <- output_data_samples |>
    dplyr::group_by(specimen_date, week_starting, .sample) |>
    dplyr::summarise(.value = sum(.value), .groups = "drop") |>
    dplyr::left_join(current_reports, by = c("specimen_date"))

  daily_preds <- daily_sample_predictions |>
    # start fn
    dplyr::summarise(
      .value = sum(.value, na.rm = TRUE),
      target = dplyr::if_else(all(is.na(target)),
        NA_real_,
        sum(target, na.rm = TRUE)),
      .by = !dplyr::all_of(c("week_starting", ".value", "target"))
    ) |>
    dplyr::mutate(t_aggregation = "daily")


  gam_preds <- daily_preds |>
    dplyr::mutate(prediction_end_date = model_hyperparameters$prediction_end_date) |>
    dplyr::rename(target_value = target)

  gam_preds
}
