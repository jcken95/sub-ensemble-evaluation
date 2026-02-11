# ============================================================================ #
# This is an example of a "model module". It can be used as a starting point
# when developing new models.
#
# The run_*() function is an example model structure, which can be adapted to
# create new models. It contains the required basic transformations used to
# produce the outputs required for model post-processing. It will need to be
# adapted for the needs of any new model.
#
# For more info about box modules, see the spikeprotein-box wiki, particularly
# the "Documenting modules" page:
#   https://github.com/REDACTED
# ============================================================================ #

#' @name example
#' @section Version: 0.0.1
#'
#' @title
#' COVID-19 admissions example model
#'
#' @description
#' Example model for COVID-19 admissions forecasting
#'
#' @details
#' # Development
#'
#' Model first developed [which season, when in season, why?].
#'
#' [more info about the model and how it works]
#'
#' # Key assumptions
#'
#' * Assumption one
#' * Assumption two
#' * More assumptions...
#'
#' # Other documentation
#'
#' * Package docs: ...
#'
#' @seealso
#' * [example$run_example()]
#'
".__module__."


box::use(
  box / deps_,
  box / help_
)


.on_load <- function(ns) {
  deps_$need(
    "dplyr",
    "lubridate",
    "mgcv",
    "tidyr"
  )
}


#' Run COVID-19 admissions example model
#'
#' @param .data Data used to train the model.
#' @param forecast_horizon Size of forecasting window, i.e. number of days of
#'   forecast values to obtain, starting with `prediction_date`.
#' @param n_pi_samples Number of replications to be simulated.
#' @param prediction_date Date of first forecast value to produce.
#' @param output_variables A vector of column names to keep in the output data
#'   frame.
#' @param hyperparams A named list of model hyperparameters.
#'
#' @returns A list with two elements: `sample_predictions` is a data frame
#'   containing model output and predictions, and `model_coefs` is a vector of
#'   model coefficients.
#'
#' @export
run_example <- function(
  .data,
  forecast_horizon = 14,
  n_pi_samples = 500,
  prediction_date,
  output_variables,
  hyperparams
) {
  # do data imputation, smoothing, feature engineering
  transformed_data <- .data

  # maintain informative name internally (need generic input for box module)
  prediction_start_date <- as.Date(prediction_date)

  # subset the data for training the model
  train_data <- transformed_data |>
    dplyr::filter(
      date < prediction_start_date,
      date >=
        prediction_start_date -
          lubridate::days(hyperparams$training_length)
    ) |>
    dplyr::mutate(
      prediction_start_date = prediction_start_date,
      wday_ = as.factor(lubridate::wday(date)),
      t_ = as.integer(date - min(date)),
      trust_code = as.factor(trust_code),
      nhs_region_name = as.factor(nhs_region_name)
    )

  # data used to forecast with (extends into future)
  train_test_data <- tidyr::expand_grid(
    date = seq(
      min(train_data$date),
      max(train_data$date) + forecast_horizon,
      by = "day"
    ),
    # spatial identifier
    trust_code = as.factor(unique(train_data$trust_code))
  ) |>
    # bring in leading indicator covariates
    dplyr::left_join(transformed_data, by = c("trust_code", "date")) |>
    dplyr::mutate(
      prediction_start_date = prediction_start_date,
      wday_ = as.factor(lubridate::wday(date)),
      t_ = as.integer(date - min(date)),
      trust_code = as.factor(trust_code),
      nhs_region_name = as.factor(nhs_region_name)
    )

  # pass as an argument?
  f <- as.formula("target ~ nhs_region_name")

  # pass as an argument?
  gamma <- 1

  # run model
  model <- mgcv::bam(
    formula = f,
    data = train_data,
    family = "nb",
    discrete = TRUE,
    method = "fREML",
    #' paraPen = list(),
    gamma = gamma
  )

  fits_out <- intervals$generate_samples(
    .model = model,
    .data = train_test_data,
    .n_pi_samples = n_pi_samples,
    model_rate = FALSE
  )

  # tidy samples
  output_data_samples <- fits_out |>
    dplyr::mutate(model = "example") |>
    dplyr::select(dplyr::any_of(output_variables))

  return(
    list(
      sample_predictions = output_data_samples,
      model = model
    )
  )
}
