#' @name gam_cr
#' @section Version: 0.0.2
#'
#' @title
#' Influenza admissions GAM (Cubic regression splines)
#'
#' @description
#' Generalised additive model (Cubic regression) for Influenza admissions forecasting.
#' Forecasts are produced at the ICB level, taking into account weekend and regional reporting effects;
#' there is also an offset term for ICB population size.
#' There are temporal trends regionally only, giving no pooling of temporal trends.
#'
#'
#' @details
#' # Development
#'
#' Model first developed in Winter 24/25 to with the aim of providing sensible trend
#' extrapolation, approximating the local growth rate. This is a version of a model
#' used since 2023.
#'
#' # Key assumptions
#'
#'  A Cubic regression spline model for forecasting admissions has the following structure:
#'
#'  \deqn{Y_t \sim \text{NegBin}\, (\mu_t, \theta)}
#'
#'  \deqn{\log \{E (Y_t) \} = \text{dow}_{t,\text{region}} + MRF(\text{icb}) +
#'  s(t_{\text{region}}) + \log(\text{population})}
#'
#'  where \eqn{s(t_{\text{region}})} is a region stratified cubic regression spline;
#'
#'  The number of knots \eqn{k} for a cubic regression spline is parameterised by \eqn{\text{region\_q}}.
#'
#'  \eqn{k = \text{round}(\text{training\_length} / \text{region\_q})}
#'
#' We also define:
#'
#'  * \eqn{Y_t} is the number of admissions at time \eqn{t}
#'  * \eqn{\text{dow}_{t,\text{region}}} is a random effect for the day of week, nested within region
#'  * \eqn{MRF(\text{icb})} is a Markov Random Field spatial smoother across ICB
#'  * \eqn{\log(\text{population})} serves as an offset term
#'
#'
#' # Other documentation
#'
#' Package docs:
#'   - <https://rdrr.io/cran/mgcv/man/smooth.terms.html>
#'
#' Reference texts:
#'  - <https://www.nature.com/articles/s43856-023-00424-4>
#'
#' @seealso
#' * [gam_cr$run_gam_cr()]
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
    "stats",
    "tidyr"
  )
}
# TODO
# use true population of ICBs for products (but not scoring)

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



#' Run influenza admissions GAM (cubic regression splines) model
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
run_gam_cr <- function(
    .data,
    forecast_horizon = 14,
    n_pi_samples = 500,
    prediction_date,
    output_variables,
    hyperparams) {

  box::use(prj / splines,
    prj / intervals)

  # do data imputation, smoothing, feature engineering
  transformed_data <- .data

  prediction_start_date <- as.Date(prediction_date) # maintain informative name

  # subset the data for training the model
  train_data <- transformed_data |>
    dplyr::filter(date < prediction_start_date,
      date >= prediction_start_date -
        lubridate::days(hyperparams$training_length)) |>
    dplyr::mutate(prediction_start_date = prediction_start_date,
      wday_ = as.factor(lubridate::wday(date)),
      t_ = as.integer(date - min(date)),
      icb_name = as.factor(icb_name),
      nhs_region_name = as.factor(nhs_region_name))

  # data used to forecast with (extends into future)
  train_test_data <- tidyr::expand_grid(
    date = seq(
      min(train_data$date),
      max(train_data$date) + forecast_horizon,
      by = "day"),
    # spatial identifier
    icb_name = as.factor(unique(train_data$icb_name))) |>
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

  nbobj <- hyperparams$spatial_object

  model_formula <- stats::as.formula(
    '
            target ~
            s(wday_, nhs_region_name, bs = "re") +
            s(icb_name, bs = "mrf", xt = list(nb = nbobj)) +
            s(nhs_region_name, bs = "re") +
            s(t_,
              by=nhs_region_name,
              k = splines$every_k(
                hyperparams$region_name_q, hyperparams$training_length),
              m = 2,
              bs = "cr") +
            stats::offset(log(population))
    ')

  # run model
  model <- mgcv::bam(
    formula = model_formula,
    data = train_data,
    family = "nb",
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
    dplyr::mutate(model = "gam_cr") |>
    dplyr::select(dplyr::any_of(output_variables)) |>
    # perform thinning of the model fit only (not future predictions)
    # keep some recent data for trend assessment
    dplyr::filter(date >= prediction_start_date - (forecast_horizon + 1) | .sample %% hyperparams$fit_thinning == 0)



  return(
    list(
      sample_predictions = output_data_samples,
      model = model
  ))

}
