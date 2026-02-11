#' @name gam_gp
#' @section Version: 0.0.1
#'
#' @title
#' RSV admissions GAM (Gaussian process smooths)
#'
#' @description
#' Generalised additive model (Gaussian process) for RSV admissions forecasting.
#' Forecasts are produced at the region:age_group level, taking into account weekend and regional reporting effects;
#' there is also an offset term for age:region stratified population size.
#' There is a temporal trend by age group only.
#'
#'
#' @details
#' # Development
#'
#' Model first developed in Winter 24/25 to with the aim of providing sensible trend
#' extrapolation with revertion towards the mean RSV admissions over the training period.
#' However, whether the model was mean reverting or extrapolating can be manually changed during the season.
#' This is done by changing `m=c(X, Y, Z)`. Typically changing between `X=2` or `X=-2` and retuning
#' the lengthscale `Y` over past data to give a sensible trend. `X` is hard coded, but `Y` is in config.
#' This may be useful near a peak, but should be avoided unless necessary.
#'
#' # Key assumptions
#'
#'  A Gaussian process (GP) model for forecasting admissions has the following structure:
#'
#'  \deqn{Y_t \sim \text{NegBin}\, (\mu_t, \theta)}
#'  \deqn{\log \{E (Y_t) \} = s(\text{dow}_t) + \text{region} + GP_{\text{age group}}(t)
#'  + \text{MRF}(\text{age group}) + \log(\text{population})}
#'
#'  The age stratified GP is modelled with \eqn{m[1]=-2} giving a
#'  squared exponential covariance structure  (`abs(m[1]) == 2`) with reversion
#'  towards a constant mean value (`m[1] < 0`). The squared exponential covariance structure implies
#'  admissions curves are infinitely differentiable in a mean square sense; informally, the curves are smooth.
#'
#'  The GP is parameterised by the \eqn{m[2]=\text{lengthscale}}, where
#'  \deqn{\text{lengthscale}=q*\text{training length}}
#'  giving \eqn{q} as the tuning parameter between 0 and 1.
#'
#' We also define:
#'
#'  * \eqn{Y_t} is the number of admissions at time \eqn{t}
#'  * \eqn{s(\text{dow}_t)} is a cyclical spline for the day of week with a 7 days repeated basis
#'  * \eqn{\text{region}} is a fixed effect by region
#'  * \eqn{GP_{\text{age group}}(t)} are independent GP by age group
#'  * \eqn{\text{MRF}(\text{age group})} is a markov random field smooth across the adjacent age groups
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



#' Run RSV admissions GAM (Gaussian process smooths) model
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
  box::use( # NB. doesn't work in targets if outside the function
    prj / splines,
    prj / intervals)

  # do data imputation, smoothing, feature engineering
  transformed_data <- .data
  prediction_start_date <- as.Date(prediction_date) # maintain informative name

  # subset the data for training the model
  train_data <- transformed_data |>
    dplyr::filter(
      date < prediction_start_date,
      date >= prediction_start_date -
        lubridate::days(hyperparams$training_length)) |>
    dplyr::mutate(
      prediction_start_date = prediction_start_date,
      wday_ = lubridate::wday(date),
      t_ = as.integer(date - min(date)),
      age_group = as.factor(age_group),
      nhs_region_name = as.factor(nhs_region_name)
    )

  # data used to forecast with (extends into future)
  train_test_data <- tidyr::expand_grid(
    date = seq(
      min(train_data$date),
      max(train_data$date) + forecast_horizon,
      by = "day"),
    # spatial identifier
    nhs_region_name = as.factor(unique(train_data$nhs_region_name)),
    # age identifier
    age_group = as.factor(unique(train_data$age_group))) |>
    dplyr::mutate(
      # need the same factors for the test set
      wday_ = lubridate::wday(date),
      t_ = as.integer(date - min(date)),
      prediction_start_date = prediction_start_date
    ) |>
    # bring in leading indicator covariates
    dplyr::left_join(
      transformed_data,
      by = c("nhs_region_name", "age_group", "date")) |>
    dplyr::group_by(nhs_region_name, age_group) |>
    dplyr::arrange(date) |>
    tidyr::fill(population) |>
    dplyr::ungroup() |>
    # need factor
    dplyr::mutate(nhs_region_name = as.factor(nhs_region_name))

  # define full formula for GAM

  age_lengthscale <- hyperparams$training_length * hyperparams$age_group_alpha

  f <- stats::as.formula(
    "target ~
    s(t_, by= age_group, bs='gp', m=c(-2, age_lengthscale, 2)) +
    nhs_region_name +
    s(age_group,bs='mrf',xt=list(nb=hyperparams$nb$age_group))+
    s(wday_, nhs_region_name, bs='fs',xt=list('cc'),k=4)+
    stats::offset(log(population))"
  )

  # run model
  model <- mgcv::gam(
    formula = f,
    data = train_data,
    family = "nb"
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
    dplyr::filter(
      date >= prediction_start_date - (forecast_horizon + 1) |
        .sample %% hyperparams$fit_thinning == 0)



  return(
    list(
      sample_predictions = output_data_samples,
      model = model
  ))

}
