#' @name gr_gp
#' @section Version: 0.0.1
#'
#' @title
#' COVID-19 admissions growth rate model (Gaussian process smooths)
#'
#' @description
#' Growth rate model (Gaussian process smooths) for COVID-19 admissions forecasting
#'
#' @details
#' # Development
#'
#' Model first developed within winter 2024/25 with the aim of giving smooth, flexible epidemic growth rate
#' extrapolation.
#'
#' # Key assumptions
#'
#' At a high level the model:
#' - calculates a local growth rate based on rolling averages of the target
#' - estimate this local growth rate with a short training length and Gaussian process model in time
#' - apply this forecasted growth rate forward in time to the most recent target.
#'
#' Throughout we will refer to the growth rate as \eqn{GR_t = \log(s(\frac{y_t}{y_{t-1}}))}
#'
#' Where \eqn{s()} is some smoothing function - in our case a seven day rolling mean.
#' and \eqn{y_1, y_2 ...} is our admissions time series.
#'
#' We then define our regression as
#'
#'  \deqn{GR_t = \text{icb} + GP_{\text{region}}(t) + GP_{\text{nation}}(t)}
#'
#'
#'  - \eqn{\text{icb}} is a random effect for the ICB
#'  - \eqn{GP_{\text{location level}}(t)} is a Gaussian process smooth pooled at the given location level
#'
#'  Once the \eqn{GR} has been estimated we need to convert back to admissions.
#'
#'  We take the forecasted growth rate up to \eqn{GR_{t_{max}+14}}
#'
#'  The smooth estimates of admissions \eqn{s(y_{t_{max}})} are taken which can be extrapolated from.
#'
#'  The forecast for \eqn{t > t_{max}} is therefore the summed growth rate up to \eqn{t}
#'
#'  \deqn{s(y_t) = s(y_{t_{max}}) * \exp(\sum{GR_t})}
#'
#'  However, this gives the smoothed admissions, so we can incorporate uncertainty according to a \eqn{\text{NegBin}}
#'  distribution by assuming
#'
#'  \deqn{y_t \sim \text{NegBin}(s(y_t), \theta)}
#'
#'  The model is tuned primarily by:
#'  - how much data is fed into the model (training length).
#'  - varying GP hyperparameters
#'  - how long the rolling averages on the model smoothers are.
#'  - dispersion parameter \eqn{\theta}.
#'
#' # Other documentation
#'
#' * Package docs: <https://rdrr.io/cran/mgcv/man/smooth.terms.html>
#'
#' @seealso
#' * [gr_gp$run_gr_gp()]
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



#' Run COVID-19 admissions growth rate (Gaussian process smooths) model
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
run_gr_gp <- function(
    .data,
    forecast_horizon = 14,
    n_pi_samples = 500,
    prediction_date,
    output_variables,
    hyperparams) {
  box::use(
    prj / intervals
  )

  # do data imputation, smoothing, feature engineering
  prediction_start_date <- as.Date(prediction_date) # maintain informative name

  # transform data to be model ready
  transformed_data <- .data |>
    # subset the data for training the model
    dplyr::filter(
      date < prediction_start_date,
      date >= prediction_start_date -
        lubridate::days(hyperparams$training_length)
    ) |>
    # Create required covariates
    dplyr::mutate(
      prediction_start_date = prediction_start_date,
      wday_ = as.factor(lubridate::wday(date)),
      t_ = as.integer(date - min(date)),
      icb_name = as.factor(icb_name),
      nhs_region_name = as.factor(nhs_region_name)
    ) |>
    dplyr::mutate(
      y_smooth = zoo::rollmean(target, k = hyperparams$rolling_k, align = "right", na.pad = TRUE),
      y_1_smooth = dplyr::lag(y_smooth, n = 1),
      y_ratio_smooth = y_smooth / y_1_smooth,
      log_y_ratio_smooth = log(y_ratio_smooth),

      # if there are strings of zeros, we get +/- Inf or  NaN
      # set these growth rates to zero

      y_ratio_smooth = dplyr::if_else(
        is.infinite(y_ratio_smooth),
        0,
        y_ratio_smooth
      ),
      log_y_ratio_smooth = dplyr::if_else(
        is.infinite(log_y_ratio_smooth),
        0,
        log_y_ratio_smooth
      ),
      .row = dplyr::row_number(),
      .by = icb_name
    )

  # data used to forecast with (extends into future)

  train_test_data <- tidyr::expand_grid(
    date = seq(
      min(transformed_data$date),
      max(transformed_data$date) + forecast_horizon,
      by = "day"
    ),
    # spatial identifier
    icb_name = as.factor(unique(transformed_data$icb_name))
  ) |>
    dplyr::mutate(
      # need the same factors for the test set
      wday_ = as.factor(lubridate::wday(date)),
      t_ = as.integer(date - min(date)),
      prediction_start_date = prediction_start_date
    ) |>
    # bring in leading indicator covariates
    dplyr::left_join(
      transformed_data |>
        # only want a subset to avoid name duplications
        dplyr::select(
          "date", "icb_name", "nhs_region_name", "population", ".row", dplyr::starts_with("y_")
        ),
      by = c("icb_name", "date")
    ) |>
    dplyr::group_by(icb_name) |>
    dplyr::arrange(date) |>
    tidyr::fill(nhs_region_name, population) |>
    dplyr::ungroup() |>
    # need factor
    dplyr::mutate(nhs_region_name = as.factor(nhs_region_name))

  # define transformed parameters,
  # we chose to select the lengthscale as a function of total data and some geographical
  # scale parameters. Broadly allows for more variation more locally
  region_lengthscale <- hyperparams$training_length * hyperparams$region_lengthscale
  national_lengthscale <- hyperparams$training_length * hyperparams$national_lengthscale

  # note, having a national level trend as well as the regional and ICB
  # makes the model much slower


  # define main model for GAM - model selection based on passed hyperparameters
  # normally we would use "target" as the left hand side of the formula, but growth rate models are different

  model_formula <- glue::glue(
    "
    log_y_ratio_smooth ~
    s(icb_name, bs = 're') +
    s(t_, nhs_region_name, bs = 'gp', m = c(dplyr::if_else(hyperparams$region_trend, 2, -2), region_lengthscale, 2)) +
    s(t_, bs = 'gp', m = c(dplyr::if_else(hyperparams$national_trend, 2, -2), national_lengthscale, 2))
    "
  ) |>
    stats::as.formula()

  # run model
  growth_rate_model <- mgcv::gam(
    formula = model_formula,
    data = transformed_data,
    family = "gaussian",
    nthreads = 2
  )

  # Generate samples from model fit coefficients
  fits_out <- intervals$generate_samples(
    .model = growth_rate_model,
    .data = train_test_data,
    .n_pi_samples = n_pi_samples
  )


  fits_out <- gratia::fitted_samples(
    growth_rate_model,
    n = n_pi_samples,
    data = dplyr::select(train_test_data, t_, nhs_region_name, icb_name),
    method = "mh"
  ) |>
    dplyr::rename(.sample = .draw)

  formatted_results <- train_test_data |>
    # generate the row number in the raw data
    dplyr::mutate(row = seq_len(dplyr::n())) |> # better way?
    dplyr::left_join(fits_out, by = c("row" = ".row")) |>
    dplyr::select(-row)
  # tidy samples
  output_data_samples <- train_test_data |>
    # we're going to forecast into the future by icb
    # the nesting ensures we don't introduce NA icbs
    # is NA icbs are present at this point, it is the datas fault, not the post-model wrangling
    tidyr::nest(
      data = -c(icb_name, prediction_start_date)
    ) |>
    dplyr::mutate(
      data = purrr::map2(
        data, icb_name,
        \(input_data, icb) {

          input_data |>
            dplyr::mutate(
              .row = dplyr::row_number(),
              icb_name = icb
            ) |>
            convert_to_target(fits_out, hyperparams$rolling_k, hyperparams$training_length, hyperparams$size) |>
            dplyr::select(-icb_name) # is already there in nested structure, so can drop
        }
      )
    ) |>
    tidyr::unnest(cols = c(data)) |>
    dplyr::mutate(model = "gr_gp") |>
    dplyr::select(dplyr::any_of(output_variables)) |>
    # perform thinning of the model fit only (not future predictions)
    # keep some recent data for trend assessment
    dplyr::filter(date >= prediction_start_date - (forecast_horizon + 1) | .sample %% hyperparams$fit_thinning == 0)

  return(
    list(
      sample_predictions = output_data_samples,
      model = growth_rate_model
    )
  )
}


convert_to_target <- function(model_data, samples, rolling_mean_k, training_length, size) {
  model_data |>
    dplyr::left_join(samples, by = ".row") |>
    dplyr::group_by(.sample, icb_name) |>
    dplyr::arrange(t_) |>
    # convert log(ratio) to ratio
    dplyr::mutate(
      # we want to accumulate later only ratios that are forecasts
      .fitted_trunc = dplyr::if_else(t_ < training_length, 0, .fitted),
      # drag the most recent date forward in time for predictions
      y_1_smooth = zoo::na.locf(y_smooth, na.rm = FALSE),
      # replace past values with the previous days smooth value
      y_1_smooth = dplyr::coalesce(y_1_smooth, dplyr::lag(y_smooth, n = 1)),
      # cumulative sum of predicted ratios gives future (log) forecast relative to a starting value
      .accumulation = cumsum(.fitted_trunc),
      # provides a model fit for t<max_fit_t
      .accumulation = dplyr::if_else(t_ < training_length, .fitted, .accumulation),
      # convert the accumulated growth rate to y scale by multiplying most recent value
      .response = log(y_1_smooth) + .accumulation,
      .response = exp(.response),
      multiplier = stats::rbeta(dplyr::n(), 32, 8),
      # we think that weekend admissions are 80% of weekday admissions
      .response = .response * dplyr::if_else(lubridate::wday(date, label = TRUE) %in% c("Sat", "Sun"), multiplier, 1)
    ) |>
    dplyr::ungroup() |>
    # our rolling_mean_k days need to go because of our smoothing
    dplyr::filter(t_ > rolling_mean_k) |>
    # add noise around the mean to get y prediction
    dplyr::mutate(
      .value = dplyr::if_else(
        is.na(.response),
        NA_integer_,
        stats::rnbinom(dplyr::n(), mu = .response, size = size)
      )
    )
}
