#' @name historic_gr_median
#' @section Version: 0.0.1
#'
#' @title
#' Influenza admissions Historic Growth Rate Median model
#'
#' @description
#' Data driven model which uses historic sentinel surveillance data to guide short-term predictions.
#' Forecasts are produced at the ICB level, taking into account weekend and regional reporting effects;
#' there is also an offset term for ICB population size.
#' Observation model is a GAM RW (Random Walk), where we define the trend as some function
#' of past observed influenza admissions data.
#'
#' @details
#' # Development
#'
#' Model first developed towards the end of Winter 24/25 to with the aim of providing information about
#' past seasons into the model. Existing models extrapolate trend, but cannot capture seasonal effects.
#'
#' # Key assumptions
#' The core assumption is that there is information in past season growth rates that can help predict this season.
#' The assumption is NOT about absolute values, but rather relative changes over time.
#' Past seasons are matched to the current one by epidemic week `lubridate::epiweek`
#'
#' To incorporate this trend we use a GAM RW model first, of the form:
#'
#' #'  \deqn{Y_t \sim \text{NegBin}\, (\mu_t, \theta)}
#'  \deqn{\log \{E (Y_t) \} = \text{dow}_t + \text{icb}_{\text{region}} + W_t + \log (\text{population})}
#'
#'  where \eqn{W_t} is a random walk; if \eqn{Z_{\tau} \sim N(0, \sigma)} are i.i.d. random variables then:
#'
#'  \deqn{W_t = \sum_{\tau=1}^{t} Z_{\tau} }
#' is a (Gaussian) random walk.
#'
#' We also define:
#'
#'  * \eqn{Y_t} is the number of admissions at time \eqn{t}
#'  * \eqn{\text{dow}_t} is a fixed effect for the day of week
#'  * \eqn{\text{icb}_{\text{region}}} is a random effect by ICB, nested within regions
#'  * \eqn{\log (\text{population})} serves as an offset term
#'
#'  With this model, we take the past observed growth rates (in this case, the median of previous seasons)
#'  We accumulate the historic daily growth rate \deqn{gr_t} over the prediction horizon as \deqn{GR_t}
#'  For \deqn{t \geq \text{prediction start date}, Y_t = Y_t * GR_t}
#'  Which gives a multiplicative factor based on past data and a random walk
#'
#' # Other documentation
#'
#' Package docs:
#'   - <https://rdrr.io/cran/mgcv/man/smooth.terms.html>
#'   - <https://github.com/eric-pedersen/MRFtools/blob/master/R/penalties.R>
#'
#' @seealso
#' * [historic_gr_median$run_historic_gr_median()]
#'
".__module__."


box::use(
  box / deps_,
  box / help_,
  prj / sari_watch
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



run_historic_gr_median <- function(
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
      nhs_region_name = as.factor(nhs_region_name),
      epiweek = factor(lubridate::epiweek(date), levels = c(40:52, 1:20), ordered = TRUE),
      t_factor = factor(t_, levels = 1:(hyperparams$training_length + forecast_horizon))
    )

  # loading from within this module which isn't prod ready
  sari_data <- sari_watch$load_sari() |>
    dplyr::filter(season == hyperparams$season)

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
      prediction_start_date = prediction_start_date,
      epiweek = factor(lubridate::epiweek(date), levels = c(40:52, 1:20), ordered = TRUE),
      t_factor = factor(t_, levels = 1:(hyperparams$training_length + forecast_horizon))
    ) |>
    # bring in covariates
    dplyr::left_join(transformed_data, by = c("icb_name", "date")) |>
    dplyr::group_by(icb_name) |>
    dplyr::arrange(date) |>
    tidyr::fill(nhs_region_name, population) |>
    dplyr::ungroup() |>
    # need factor
    dplyr::mutate(nhs_region_name = as.factor(nhs_region_name)) |>
    # bring in sari, assumes only 1 season
    dplyr::left_join(sari_data |>
      dplyr::select("epiweek", "growth_rate"),
    by = "epiweek") |>
    dplyr::mutate(
      # set the growth rate as only for out of sample.
      # The /100 is to get out of %. The /7 is for daily conversion.
      # We don't want exactly zero as we will be logging.
      growth_rate = dplyr::if_else(t_ > max(train_data$t_), growth_rate / (7 * 100), 0.0001)
    ) |>
    dplyr::arrange(date) |>
    # in the case where we have joined no data, just predict flat
    dplyr::mutate(growth_rate = dplyr::coalesce(growth_rate, 0.0001)) |>
    # convert to daily growth rate, then a ratio
    dplyr::mutate(growth_rate_acc =
      dplyr::if_else(t_ > max(train_data$t_), exp(cumsum(growth_rate)), 1),
    .by = "icb_name")

  # The penalty matrix needs to also go as high as we'd like to forecast
  rw_penalty <- MRFtools::mrf_penalty(object = 1:(hyperparams$training_length + forecast_horizon),
    type = "linear")

  # define full formula for GAM


  model_formula <- stats::formula(
    target ~
      wday_ +
      s(icb_name, nhs_region_name, bs = "re") +
      s(t_factor,
        bs = "mrf",
        xt = list(penalty = rw_penalty),
        k  = floor(hyperparams$training_length * 0.9)
      ) +
      stats::offset(log(population))
  )

  # run model
  model <- mgcv::bam(
    formula = model_formula,
    data = train_data,
    family = "nb",
    discrete = TRUE,
    # we will need our "future" levels defined
    drop.unused.levels = FALSE,
    method = "fREML",
    nthreads = c(2, 1),
    gc.level = 1
  )

  # Generate samples from model fit coefficients
  fits_out <- intervals$generate_samples(
    .model = model,
    .data = train_test_data,
    .n_pi_samples = n_pi_samples
  ) |>
    # there must be a better way.
    # I would prefer to do this on the linear predictor rather
    # than the response scale.
    dplyr::mutate(.value = round(.value * growth_rate_acc))

  # tidy samples
  output_data_samples <- fits_out |>
    dplyr::mutate(model = "historic_gr_median") |>
    dplyr::select(dplyr::any_of(output_variables)) |>
    # perform thinning of the model fit only (not future predictions)
    # keep some recent data for trend assessment
    dplyr::filter(date >= prediction_start_date - (forecast_horizon + 1) |
      .sample %% hyperparams$fit_thinning == 0)



  return(
    list(
      sample_predictions = output_data_samples,
      model = model
  ))

}
