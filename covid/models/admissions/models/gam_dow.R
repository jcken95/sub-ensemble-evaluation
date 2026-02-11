#' @name gam_dow
#' @section Version: 0.0.1
#'
#' @title
#' Baseline COVID-19 admissions GAM (day of week effect)
#'
#' @description
#' Generalised additive model (day of week effect) for COVID-19 admissions forecasting.
#' Forecasts are produced at an ICB level with a fixed effect for day of week reporting differences
#' and random effects for ICB.
#'
#' @details
#' # Development
#'
#' Model first developed in Winter 24/25 to with the aim of providing a baseline model for forecasting COVID-19
#' hospital admissions.
#'
#' # Key assumptions#
#'
#'  A day of week model for forecasting admissions has the following structure:
#'
#'  \deqn{Y_t \sim \text{NegBin}\, (\mu_t, \theta)}
#'  \deqn{\log \{E (Y_t) \} = \text{DoW}_t + \text{icb}}
#'
#' We also define:
#'
#'  * \eqn{Y_t} is the number of admissions at time \eqn{t}
#'  * \eqn{\text{DoW}_t} is a fixed effect for the day of week
#'  * \eqn{\text{icb}} is a random effect for the ICB
#'
#' # Other documentation
#'
#' Package docs:
#'   - <https://rdrr.io/cran/mgcv/man/smooth.terms.html>
#'
#' @md
#'
#' @seealso
#' * [dow$run_gam_dow()]
#'
#' @md
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
    "stats",
    "tidyr"
  )
}



#' Run COVID-19 admissions GAM (day of week) model.
#'
#' Fit an additive model to COVID-19 admissions data, and format the outputs.
#' Run `box::help(gam_rw)` for modelling details.
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
#'   containing model output and predictions, and `model` is the model object.
#'
#' @export
run_gam_dow <- function(
    .data,
    forecast_horizon = 14,
    n_pi_samples = 500,
    prediction_date,
    output_variables,
    hyperparams) {
  box::use(
    prj / intervals,
    box / s3
  )

  # do data imputation, smoothing, feature engineering
  transformed_data <- .data

  prediction_start_date <- as.Date(prediction_date) # maintain informative name

  # subset the data for training the model
  train_data <- transformed_data |>
    dplyr::filter(
      date < prediction_start_date,
      date >= prediction_start_date - lubridate::days(hyperparams$training_length)
    ) |>
    dplyr::mutate(
      prediction_start_date = prediction_start_date,
      wday_ = as.factor(lubridate::wday(date)),
      t_ = as.integer(date - min(date)),
      icb_name = as.factor(icb_name),
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
    icb_name = as.factor(unique(train_data$icb_name))
  ) |>
    dplyr::mutate(
      # need the same factors for the test set
      wday_ = as.factor(lubridate::wday(date)),
      t_ = as.integer(date - min(date)),
      prediction_start_date = prediction_start_date
    ) |>
    # bring in covariates
    dplyr::left_join(transformed_data, by = c("icb_name", "date")) |>
    dplyr::group_by(icb_name) |>
    dplyr::arrange(date) |>
    tidyr::fill(nhs_region_name, population) |>
    dplyr::ungroup() |>
    # need factor
    dplyr::mutate(nhs_region_name = as.factor(nhs_region_name))

  # define full formula for GAM

  model_formula <- stats::formula(target ~ wday_ + s(icb_name, bs = "re"))

  model <- mgcv::bam(
    model_formula,
    data = train_data,
    family = "nb",
    drop.unused.levels = FALSE,
    discrete = TRUE,
    method = "fREML",
    nthreads = c(2, 1),
    gc.level = 1
  )

  # Generate samples from model fit coefficients
  fits_out <- intervals$generate_samples(
    .model = model,
    .data = train_test_data,
    .n_pi_samples = n_pi_samples
  )

  # tidy samples
  output_data_samples <- fits_out |>
    dplyr::mutate(model = "gam_dow") |>
    dplyr::select(dplyr::any_of(output_variables)) |>
    # perform thinning of the model fit only (not future predictions)
    # keep some recent data for trend assessment
    dplyr::filter(date >= prediction_start_date - (forecast_horizon + 1) | .sample %% hyperparams$fit_thinning == 0)



  return(
    list(
      sample_predictions = output_data_samples,
      model = model
    )
  )
}
