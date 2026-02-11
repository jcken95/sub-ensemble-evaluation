#' @name contextual_scoring
#' @section Version: 0.0.1
#'
#' @title
#' Weighted contextual interval score
#'
#' @description
#' Functions to compute the weighted contextual interval score from
#' [Marshall, Parker and Gardner (2024)](https://link.springer.com/article/10.1186/s44263-024-00098-7).
#'
#' @seealso
#' * [contextual_scoring$wcis()]
#'
".__module__."


box::use(
  box / deps_,
  box / help_
)

.on_load <- function(ns) {
  deps_$need(
    "dplyr",
    "scoringutils >= 2.0.0"
  )
}

#' Contextual relative error
#'
#' Computes and returns the contextual relative errror.

#' \eqn{CRE(x, y; \delta) = \min \{1, \delta^{-1} |\, x - y \,|  \}}.
#'
#' @param observed Numeric vector of size n with the observed values.
#' @param predicted Numeric vector of size n with the predicted values.
#' @param delta positive number. If the magnitude of the difference between the forecasted and predicted value is
#' larger than delta, the forecast is deemed useless
#'
#' @return numeric vector
contextual_relative_error <- function(observed, predicted, delta) {
  stopifnot(delta > 0)
  pmin(1, abs(observed - predicted) / delta)
}

#' Weighted Contextual Interval Score (WCIS)
#'
#'
#' WCIS incorporates contextual utility into scoring evaluation via a utilty threshold parameter \eqn{\delta}
#' \deqn{WCIS_{\alpha\{0:K\}}(F, y; \delta) = (K+1)^{-1} \left\{ CRE(m, y; \delta) +
#' \sum_{k = 1}^K CIS(F, y; \delta)  \right\} }
#' Where:
#'  - \eqn{F} is a collection of quantiles from a probabalistic forecast
#'  - \eqn{y} is an observed value
#'  - \eqn{\delta} is the utility threshold parameter
#'  - \eqn{K} is the number of intervals implied by the vector of quantiles
#'  - \eqn{CRE(x, y; \delta)} is the contextual relative error
#'  - \eqn{CIS(F, y; \delta)} is the contextual interval score
#'
#' See [Marshall, Parker and Gardner (2024)](https://link.springer.com/article/10.1186/s44263-024-00098-7) for complete
#' details.
#'
#' @param observed Numeric vector of size n with the observed values.
#' @param predicted Numeric \eqn{n \times N} matrix of predictive quantiles, \eqn{n} (number of rows) being the number
#' of forecasts (corresponding to the number of observed values) and \eqn{N} (number of columns) the number of
#'  quantiles per forecast. If observed is just a single number, then predicted can just be a vector of size \eqn{N}.
#' @param quantile_level Vector of of size \eqn{N} with the quantile levels for which predictions were made.
#' @param delta Positive number. If the magnitude of the difference between the forecasted and predicted value is
#' larger than delta, the forecast is deemed useless.
#'
#' @examples
#' library(scoringutils)
#' example_quantile |>
#'   as_forecast_quantile(
#'     forecast_unit = c(
#'       "location", "target_end_date", "target_type", "horizon", "model"
#'     )
#'   ) |>
#'   score(
#'     metrics = list(
#'       wcis = \(observed, lower, upper, interval_range) wcis(observed, predicted, quantile_level, delta = 100)
#'     )
#'   )
#'
#' @export
wcis <- function(observed,
                 predicted,
                 quantile_level,
                 delta) {
  reformatted <- scoringutils:::quantile_to_interval(
    observed,
    predicted,
    quantile_level
  )

  reformatted <- reformatted |>
    dplyr::mutate(
      interval_score = scoringutils:::interval_score(
        observed,
        lower,
        upper,
        interval_range,
        weigh = TRUE
      ),
      # can use lower or upper for CRE; when CRE matters lower == upper
      contextual_relative_error = contextual_relative_error(observed, lower, delta),
    ) |>
    dplyr::mutate(
      # if range is 0, this is a point (median) forecast
      score = dplyr::if_else(interval_range == 0, contextual_relative_error, interval_score),
      score = score / delta
    ) |>
    dplyr::mutate(contextual_is = pmin(1, score)) |>
    dplyr::summarise(contextual_is = mean(contextual_is), .by = forecast_id)

  return(reformatted$contextual_is)
}
