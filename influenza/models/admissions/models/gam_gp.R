#' @name gam_gp
#' @section Version: 0.0.1
#'
#' @title
#' Influenza admissions GAM (Gaussian process smooths)
#'
#' @description
#' Generalised additive model (Gaussian process) for influenza admissions forecasting.
#' Forecasts are produced at the ICB level, taking into account weekend and regional reporting effects;
#' there is also an offset term for ICB population size.
#' There are both temporal trends for national and regional levels, giving a pooling by location.
#'
#'
#' @details
#' # Development
#'
#' Model first developed in Winter 24/25 to with the aim of providing sensible trend
#' extrapolation with some reverting towards the mean influenza admissions over the training period.
#'
#' # Key assumptions
#'
#'  A Gaussian process (GP) model for forecasting admissions has the following structure:
#'
#'  \deqn{Y_t \sim \text{NegBin}\, (\mu_t, \theta)}
#'  \deqn{\log \{E (Y_t) \} = \text{dow}_t + \text{icb}_{\text{region}} + GP_{\text{region}}(t)
#'  + GP(t) + \log(\text{population})}
#'
#'  where \eqn{GP(t)} is a Gaussian process;
#'
#'  The national GP is modelled with \eqn{m[1]=-2} giving a
#'  squared exponential covariance structure  (`abs(m[1]) == 2`) with reversion
#'  towards a constant mean value (`m[1] < 0`). The squared exponential covariance structure implies
#'  admissions curves are infinitely differentiable in a mean square sense; informally, the curves are smooth.
#'
#'  The regional GPs are modelled with \eqn{m[1]=2} giving extrapolation which tends
#'  to a linear trend out of the training period.
#'
#'  The GP's are parameterised by the \eqn{m[2]=\text{lengthscale}}, where
#'  \deqn{\text{lengthscale}=q*\text{training length}}
#'  giving \eqn{q} as the tuning parameter between 0 and 1.
#'
#' We also define:
#'
#'  * \eqn{Y_t} is the number of admissions at time \eqn{t}
#'  * \eqn{\text{dow}_t} is a random effect for the day of week
#'  * \eqn{\text{icb}_{\text{region}}} is a random effect by ICB, nested within regions
#'  * \eqn{GP_{\text{region}}(t)} are independent GP by NHS regions
#'  * \eqn{GP(t)} is a single national level GP
#'  * \eqn{\log (\text{population})} serves as an offset term
#'
#'
#' # Other documentation
#'
#' Package docs:
#'   - <https://rdrr.io/cran/mgcv/man/smooth.terms.html>
#'
#' Reference texts:
#'   - <https://gaussianprocess.org/gpml/chapters/RW.pdf>
#'
#' @seealso
#' * [gam_gp$run_gam_gp()]
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

#' Run influenza admissions GAM (Gaussian process smooths) model
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
run_gam_gp <- function(
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


  # transformed parameters
  nation_lengthscale <- hyperparams$training_length * hyperparams$nation_alpha
  region_lengthscale <- hyperparams$training_length * hyperparams$region_alpha

  nbobj <- hyperparams$spatial_object

  model_formula <- stats::as.formula(
    '
            target ~
            s(wday_, nhs_region_name, bs = "re") +
            s(icb_name, nhs_region_name, bs = "re") +
            s(t_, bs="gp", m=c(2, nation_lengthscale, 2)) +
            s(t_, bs="gp", by=nhs_region_name, m=c(-2, region_lengthscale, 2)) +
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
    dplyr::mutate(model = "gam_gp") |>
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
