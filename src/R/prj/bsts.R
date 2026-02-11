#' @name bsts
#' @section Version: 1.0.4
#'
#' @title
#' Functions to run BSTS models.
#'
#' @description
#' Functions for post processing BSTS models (Bayesian structural time series),
#' originally for nowcasting.
#'
#'
#'
#' @seealso
#' * [bsts$bsts_generate_samples()]
#' * [bsts$bsts_generate_components()]
".__module__."


box::use(
  box / deps_,
  box / help_
)

.on_load <- function(ns) {
  deps_$need(
    "stats",
    "dplyr>=1.1.0",
    "tidyr",
    "stringr"
  )
}



#' Generate Samples from BSTS
#'
#' This function is used to generate predictions from the model fit and
#' also out put them in more usable format.
#'
#' @param .model The model fit object.
#' @param .newdata The new data that the model is going to base the predictions
#' off, null If uni-variate.
#' @param horizon Numeric value of the forecast horizon;
#' shows how many observations ahead will be forecast.
#' @param burnin Numeric value of the number of iterations which will be
#' ignored when starting to run the model.
#'
#' @returns The prediction samples as a dataframe.
#'
#' @examples
#' predictions <- bsts_generate_samples(
#'   .model = fit,
#'   .newdata = test_data, # if not including regresors, set to null
#'   horizon = 7,
#'   burnin = 2000) # 2000 if 50000 samples
#'
#' @export

bsts_generate_samples <- function(.model, .newdata, horizon, burnin = 0) {

  prediction_object <- stats::predict(
    .model,
    newdata = .newdata,
    horizon = horizon,
    burnin = burnin
  )

  prediction_samples <- prediction_object[["distribution"]] |>
    data.frame() |>
    dplyr::mutate(.sample = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("X"),
      names_to = ".h", # .h and H is an index for the forecast day
      values_to = ".value"
    ) |>
    dplyr::mutate(.h = as.integer(stringr::str_remove(.h, "X")))

  return(prediction_samples)
}


#' Extract model components
#'
#' Used in creating plots that look at the fit when creating bsts models.
#'
#' @param .model The model fit.
#' @param .newdata The new data that the model is going to base the predictions
#' off, null If uni-variate.
#' @param burnin Numeric value of the number of iterations which will be
#' ignored when running the model
#'
#' @examples
#' components <- bsts_generate_components(
#'   .model = fit,
#'   .newdata = test_data,
#'   burnin = 0)
#'
#' components |>
#'   dplyr::group_by(component, .t) |>
#'   generate_intervals(method = "quantile") |>
#'   ggplot2::ggplot() +
#'   ggplot2::geom_ribbon(
#'     ggplot2::aes(x = .t, ymax = pi_95, ymin = pi_5),
#'     fill = "steelblue", alpha = 0.9) +
#'   ggplot2::geom_line(ggplot2::aes(x = .t, y = pi_50)) +
#'   ggplot2::facet_wrap(~component, scales = "free")
#'
#' @export

bsts_generate_components <- function(.model, .newdata, burnin = 0) {

  state_component_samples <- .model[["state.contributions"]] |>
    as.data.frame.table()  |>
    dplyr::mutate(
      .sample = as.integer(mcmc.iteration),
      .t = as.integer(time)
    ) |>
    dplyr::filter(.sample > burnin) |>
    dplyr::mutate(.sample = .sample - min(.sample) + 1) |>
    dplyr::rename(.value = Freq) |>
    dplyr::select(-c("time", "mcmc.iteration"))

  return(state_component_samples)
}
