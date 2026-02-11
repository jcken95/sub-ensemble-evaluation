#' @name nowcast
#' @section Version: 0.0.1
#'
#' @title Functions for nowcasting of infectious diseases
#'
#' @description Helper functions for nowcasting. Includes functions to wrangle data, fit models, and plot results.
#'
#' @seealso
#' * [fitting$run_scripted_model()]
#' * [fitting$pertussis_gam()]
#' * [plotting$prediction_plot()]
#' * [wrangling$pre_triangle()]
#' * [wrangling$construct_reporting_triangle()]
#' * [wrangling$tests_to_counts()]
#' * [wrangling$model_quantiles()]
".__module__."

box::use(
  box / deps_,
  box / help_,
)

#' @export
box::use(
  . / fitting,
  . / plotting,
  . / wrangling,
  . / pertussis
)
