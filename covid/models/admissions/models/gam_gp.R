#' @name gam_gp
#' @section Version: 0.0.2
#'
#' @title
#' COVID-19 admissions GAM (Gaussian process smooths)
#'
#' @description
#' Generalised additive model (Gaussian process) for COVID-19 admissions forecasting.
#' Forecasts are produced at the ICB level, taking into account weekend and regional reporting effects;
#' there is also an offset term for ICB population size.
#' There are both temporal trends for regional and icb levels, giving a pooling by location.
#'
#'
#' @details
#' # Development
#'
#' Model first developed in Winter 24/25 to with the aim of providing sensible trend
#' extrapolation, and revert towards the mean COVID admissions over the training period.
#'
#' # Key assumptions
#'
#'  A Gaussian process (GP) model for forecasting admissions has the following structure:
#'
#'  \deqn{Y_t \sim \text{NegBin}\, (\mu_t, \theta)}
#'  \deqn{\log \{E (Y_t) \} = \text{dow}_t + MRF(\text{icb}) + GP_{\text{region}} } (t)
#'  + GP_{\text{icb})(t) + \log(\text{population})}
#'
#'  where \eqn{GP(t)} is a Gaussian process;
#'
#'  The region process is modelled with \eqn{m[1]=-2} giving a squared exponential covariance structure
#'  (`abs(m[1]) == 2`) with reversion towards a constant mean value (`m[1] < 0`). The ICB process has \eqn{m[1] = 2},
#'  which gives long term reversion towards a linear trend (\eqn(m[1] >  0)). The squared exponential covariance
#'  structure implies admissions curves are infinitely differentiable in a mean square sense; informally,
#'  the curves are smooth.
#'
#'  The GPs are parameterised by the \eqn{m[2]=\text{lengthscale}},
#'  where \deqn{\text{lengthscale}=q*\text{training length}} giving \eqn{q} as the tuning parameter between 0 and 1.
#'  The value of `q` may be difference for the ICB and regional GPs.
#'
#' We also define:
#'
#'  * \eqn{Y_t} is the number of admissions at time \eqn{t}
#'  * \eqn{\text{dow}_t} is a random effect for the day of week
#'  * \eqn{\text{region}} is a random effect for the nhs region
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
#'  - <https://gaussianprocess.org/gpml/chapters/RW.pdf>
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
    "glue",
    "lubridate",
    "mgcv",
    "stats",
    "tidyr"
  )
}



#' Run COVID-19 admissions GAM (Gaussian process smooths) model
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

  box::use(
    prj / intervals,
    box / s3
  )

  # get spatial network object
  nbobj <- hyperparams$spatial_object

  # do data imputation, smoothing, feature engineering
  prediction_start_date <- as.Date(prediction_date) # maintain informative name

  # transform data to be model ready
  transformed_data <- .data |>
    # subset the data for training the model
    dplyr::filter(date < prediction_start_date,
      date >= prediction_start_date -
        lubridate::days(hyperparams$training_length)) |>
    # Create required covariates
    dplyr::mutate(prediction_start_date = prediction_start_date,
      wday_ = as.factor(lubridate::wday(date)),
      t_ = as.integer(date - min(date)),
      icb_name = as.factor(icb_name),
      nhs_region_name = as.factor(nhs_region_name))



  # data used to forecast with (extends into future)
  train_test_data <- tidyr::expand_grid(
    date = seq(
      min(transformed_data$date),
      max(transformed_data$date) + forecast_horizon, by = "day"),
    # spatial identifier
    icb_name = as.factor(unique(transformed_data$icb_name))) |>
    dplyr::mutate(
      # need the same factors for the test set
      wday_ = as.factor(lubridate::wday(date)),
      t_ = as.integer(date - min(date)),
      prediction_start_date = prediction_start_date
    ) |>
    # bring in leading indicator covariates
    dplyr::left_join(transformed_data |>
      # only want a subset to avoid name duplications
      dplyr::select("date", "icb_name", "nhs_region_name", "population"),
    by = c("icb_name", "date")) |>
    dplyr::group_by(icb_name) |>
    dplyr::arrange(date) |>
    tidyr::fill(nhs_region_name, population) |>
    dplyr::ungroup() |>
    # need factor
    dplyr::mutate(nhs_region_name = as.factor(nhs_region_name))



  # define transformed parameters,
  # we chose to select the lengthscale as a function of total data and some geographical
  # scale parameters. Broadly allows for more variation more locally
  region_lengthscale <- hyperparams$training_length / hyperparams$nhs_region_name_q
  icb_lengthscale <- hyperparams$training_length / hyperparams$icb_name_q


  # note, having a national level trend as well as the regional and ICB
  # makes the model much slower


  # define main model for GAM - model selection based on passed hyperparameters
  model_formula <- glue::glue("
        target ~
          s(wday_, bs = 're') +
          stats::offset(log(population)) +
          s(t_, by=nhs_region_name, bs='gp', m=c(-2, region_lengthscale, 2)) +
          s(t_, by=icb_name, bs='gp', m=c(2, icb_lengthscale, 2)) +
          s(icb_name, bs = 'mrf', xt = list(nb = nbobj))") |>
    stats::as.formula()



  # run model
  model <- mgcv::bam(
    formula = model_formula,
    data = transformed_data,
    family = "nb",
    discrete = TRUE,
    method = "fREML",
    nthreads = 2
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
