#' @name gam_rw
#' @section Version: 0.0.1
#'
#' @title
#' COVID-19 admissions GAM (random walk)
#'
#' @description
#' Generalised additive model (random walk) for COVID-19 admissions forecasting.
#' Forecasts are produced at the ICB level, taking into account weekend and regional reporting effects;
#' there is also an offset term for ICB population size.
#' No pooling is utililised in this model.
#'
#' @details
#' # Development
#'
#' Model first developed in Winter 24/25 to with the aim of providing additional uncertainty and a regularising effect
#' to the ensemble.
#'
#' # Key assumptions
#'
#'  A random walk (RW) model for forecasting admissions has the following structure:
#'
#'  \deqn{Y_t \sim \text{NegBin}\, (\mu_t, \theta)}
#'  \deqn{\log \{E (Y_t) \} = \text{weekend}_t + \text{region} + \text{icb} + W_t + \log(\text{population})}
#'
#'  where \eqn{W_t} is a random walk; if \eqn{Z_{\tau} \sim N(0, \sigma)} are i.i.d. random variables then:
#'
#'  \deqn{W_t = \sum_{\tau = 1}^{t} Z_{\tau} }
#' is a (Gaussian) random walk.
#'
#' We also define:
#'
#'  * \eqn{Y_t} is the number of admissions at time \eqn{t}
#'  * \eqn{\text{weekend}_t} is a fixed effect for whether the day of week is weekend of weekday
#'  * \eqn{\text{region}} is a random effect for the nhs region
#'  * \eqn{\text{icb}} is a random effect for the ICB
#'  * \eqn{\log (\text{population})} serves as an offset term
#'
#'
#' # Other documentation
#'
#' Package docs:
#'   - <https://rdrr.io/cran/mgcv/man/smooth.terms.html>
#'   - <https://github.com/eric-pedersen/MRFtools/blob/master/R/penalties.R>
#'
#' @seealso
#' * [gam_rw$run_gam_rw()]
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

  deps_$need(
    "MRFtools",
    install_cmd = "remotes::install_github(\"eric-pedersen/MRFtools\")"
  )
}



#' Run COVID-19 admissions GAM (random walk) model.
#'
#' Fit a random walk model to COVID-19 admissions data, and format the outputs.
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
#'   containing model output and predictions, and `model_coefs` is a vector of
#'   model coefficients.
#'
#' @export
run_gam_rw <- function(
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


  # subset the data for training the model
  train_data <- transformed_data |>
    dplyr::filter(
      date < prediction_start_date,
      date >= prediction_start_date -
        lubridate::days(hyperparams$training_length)) |>
    dplyr::mutate(
      prediction_start_date = prediction_start_date,
      weekend_ = lubridate::wday(date, label = TRUE) %in% c("Sat", "Sun"),
      t_ = as.integer(date - min(date)),
      icb_name = as.factor(icb_name),
      nhs_region_name = as.factor(nhs_region_name),
      t_factor = factor(t_, levels = 1:(hyperparams$training_length + forecast_horizon))
    )


  # data used to forecast with (extends into future)
  train_test_data <- tidyr::expand_grid(
    date = seq(
      min(transformed_data$date),
      max(transformed_data$date) + forecast_horizon, by = "day"),
    # spatial identifier
    icb_name = as.factor(unique(transformed_data$icb_name))) |>
    dplyr::mutate(
      # need the same factors for the test set
      weekend_ = lubridate::wday(date, label = TRUE) %in% c("Sat", "Sun"),
      t_ = as.integer(date - min(date)),
      prediction_start_date = prediction_start_date,
      t_factor = factor(t_, levels = 1:(hyperparams$training_length + forecast_horizon))
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



  # define full formula for GAM
  nbobj <- hyperparams$spatial_object
  # The penalty matrix needs to also go as high as we'd like to forecast
  rw_penalty <- MRFtools::mrf_penalty(object = 1:(hyperparams$training_length + forecast_horizon), type = "linear")

  model_formula <- stats::formula(
    target ~
      weekend_ +
      s(nhs_region_name, bs = "re") +
      # k is arbitraty but must be < training_length
      s(t_factor,
        bs = "mrf",
        xt = list(penalty = rw_penalty),
        k  = floor(hyperparams$training_length * hyperparams$alpha)
      ) +
      s(icb_name, bs = "re") +
      stats::offset(log(population))
  )

  model <- mgcv::bam(
    model_formula,
    data = train_data,
    family = "nb",
    # we will need our "future" levels defined
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
    dplyr::mutate(model = "gam_rw") |>
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
