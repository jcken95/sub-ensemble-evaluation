#' @name ets
#' @section Version: 0.0.2
#'
#' @title
#' COVID-19 admissions ETS
#'
#' @description
#' ETS (error-trend-seasonality) time series model for COVID-19 admissions forecasting
#'
#' @details
#' # Development
#'
#' Model first developed mid-winter 2023/24.
#'
#' This approach outperformed both log transformations & multiplicative components.
#'
#' The model takes a short time series trend and extrapolates.
#'
#' # Key assumptions
#'
#' * Admissions are Gaussian distributed (not log scaled)
#' * Continuous prediction samples are rounded, and negative values set to zero to match the count admissions data
#' * The E, T and S, are all on an additive scale, rather than multiplicative
#' * Each ICB has a separate model fit, and predictions generated; there is no pooling across locations
#'
#' # Other documentation
#'
#' * Package docs: <https://fable.tidyverts.org/reference/ETS.html>
#' * Exponential smoothing (Chapter 8, FPP3): <https://otexts.com/fpp3/expsmooth.html>
#'
#' @seealso
#' * [ets$run_ets()]
#'
".__module__."


box::use(
  box / deps_,
  box / help_
)


.on_load <- function(ns) {
  deps_$need(
    "distributional>=0.5.0",
    "dplyr",
    "fable>=0.4.1",
    "fabletools>=0.5.0",
    "purrr",
    "stats",
    "tidyr",
    "tsibble"
  )
}



#' Run COVID-19 admissions ETS model
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
run_ets <- function(
    .data,
    forecast_horizon = 14,
    n_pi_samples = 500,
    prediction_date,
    output_variables,
    hyperparams) {
  prediction_start_date <- as.Date(prediction_date) # maintain informative name

  # sum to ICB level for ETS
  ets_training_data <- .data

  train_data <- tidyr::expand_grid(
    date = seq(
      min(ets_training_data$date, na.rm = TRUE),
      prediction_start_date + (forecast_horizon - 1),
      by = "day"
    ),
    icb_name = unique(stats::na.omit(ets_training_data$icb_name))
  ) |>
    dplyr::left_join(ets_training_data, by = c("date", "icb_name")) |>
    dplyr::mutate(prediction_start_date = prediction_start_date) |>
    dplyr::filter(
      date >= prediction_start_date - hyperparams$training_length) |>
    # create tsibble
    tsibble::as_tsibble(
      index = date,
      key = c("icb_name", "nhs_region_name")) |>
    dplyr::filter(date < prediction_start_date)

  # run model for regional and national level
  ets_model <- train_data |>
    fabletools::model(
      ets = fable::ETS(
        (target + 0.01) ~ error("A") +
          trend(
            "Ad",
            alpha = !!hyperparams$alpha,
            beta = !!hyperparams$beta,
            phi = !!hyperparams$phi
          ) +
          season(
            "A",
            period = "1 week",
            gamma = !!hyperparams$gamma
          ),
        opt_crit = "amse",
        nmse = hyperparams$nmse)
    )



  ets_fcast <- ets_model |>
    fabletools::forecast(
      h = forecast_horizon, bootstrap = TRUE, times = n_pi_samples) |>
    dplyr::mutate(distributional::parameters(target), stats::family(target)) |>
    tidyr::unnest(x) |>
    dplyr::mutate(
      # Clip negative values to 0
      ".value" = round(pmax(x, 0), digits = 0),
      .keep = "unused"
    ) |>
    dplyr::group_by(icb_name, date, nhs_region_name) |>
    dplyr::mutate(
      ".sample" = dplyr::row_number(),
      "model" = "ets",
      "prediction_start_date" = prediction_start_date
    ) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::any_of(output_variables))

  # the separate forecast versus fit frames are a pain for populations,
  # so we need a lookup
  final_day_population <- train_data |>
    dplyr::as_tibble() |>
    dplyr::filter(date == max(date)) |>
    dplyr::select(c("icb_name", "population"))

  # assumes future population == last observed population
  # (consistent with other models fill forward approach)
  ets_fcast <- as.data.frame(ets_fcast) |>
    dplyr::left_join(
      # important we only use population up to the observations available.
      final_day_population,
      by = c("icb_name")
    )

  section_bounds <- unique(
    # +1 to be end inclusive of the final day of data
    c(seq(1, hyperparams$training_length + 1, by = 7), hyperparams$training_length + 1))

  # instead of looping through these, we just do them for the ones in the data

  date_sections <- cut(as.numeric(prediction_start_date - train_data$date),
    # we need `right=FALSE` to keep the first index value
    section_bounds, right = FALSE)

  ets_fit <- purrr::map(
    sort(unique(date_sections[!is.na(date_sections)])),
    ~ ets_model |>
      fabletools::generate(
        new_data = train_data |>
          dplyr::filter(
            date_sections == .x
          ),
        bootstrap = TRUE,
        times = n_pi_samples
      )
  ) |>
    purrr::list_rbind()

  ets_fit <- ets_fit |>
    dplyr::mutate(
      "model" = "ets",
      ".value" = round(pmax(.sim, 0), digits = 0),
      ".sample" = as.numeric(.rep),
      .keep = "unused"
    ) |>
    dplyr::mutate("prediction_start_date" = prediction_start_date) |>
    # we need population brought in
    dplyr::left_join(train_data |>
      dplyr::as_tibble() |>
      dplyr::select(c("icb_name", "date", "population")),
    by = c("icb_name", "date")) |>
    dplyr::select(dplyr::any_of(output_variables))


  model_coefs <- stats::coef(ets_model) |>
    dplyr::filter(term %in% c("alpha", "beta", "gamma", "phi"))

  ets_combined <- dplyr::bind_rows(ets_fcast, ets_fit) |>
    # perform thinning of the model fit only (not future predictions)
    # keep some recent data for trend assessment
    dplyr::filter(date >= prediction_start_date - (forecast_horizon + 1) | .sample %% hyperparams$fit_thinning == 0)

  return(list(sample_predictions = ets_combined,
    model_coefs = model_coefs))
}
