#' @name intervals
#' @section Version: 0.0.3
#'
#' @title Support for working at the interface between model samples and prediction intervals.
#'
#' @description
#' Module to allow conversion between `mgcv` model outputs to "posterior" style samples,
#' converting between levels of aggregation and converting to prediction intervals.
#' Broadly this module facilitates uncertainty propagation for forecasts.
#'
#' @seealso
#' * [intervals$generate_samples()]
#' * [intervals$generate_intervals()]
#' * [intervals$aggregate_samples()]
#' * [intervals$samples_to_quantiles()]
#' * [intervals$discretise_trends()]
#'
".__module__."


box::use(
  box / deps_,
  box / help_
)

.on_load <- function(ns) {
  deps_$need(
    "bayestestR",
    "data.table",
    "dplyr",
    "dtplyr",
    "gratia>=0.10.0",
    "mgcv>=1.9.0",
    "purrr",
    "stats",
    "tidyr"
  )
  # dplyr needs to be attached for some dtplyr functions to work properly
  if (!"package:dplyr" %in% search()) attachNamespace(loadNamespace("dplyr"))
}



#' Standardised set of quantiles for creating prediction intervals.
#'
#' Takes sample data and produces prediction intervals for pre-defined
#' quantiles. Note that quantiles of this form are an assumption. Data must be
#' grouped by whatever covariates or time/space identifiers *before* calling
#' this function.
#'
#' Alternative quantile approaches are available [see this reference
#' .](https://easystats.github.io/bayestestR/articles/credible_interval.html)
#'
#' @param .data Input data frame of processed prediction samples (not directly
#'   from [intervals$generate_samples()]) which contains the column `.value`,
#'   the quantity to be summarized. One row per sample per covariate group.
#' @param method Whether to calculate using standard quantile approach or HDI
#'   (`"quantile"` or `"hdi"`).
#'
#' @returns Data frame with one row per set of groupings, rather than one row
#'   per sample (input)
#'
#' @examples
#'
#' formatted_samples |>
#'   dplyr::group_by(date, location) |>
#'   intervals$generate_intervals() |>
#'   dplyr::ungroup()
#'
#' @export
generate_intervals <- function(.data, method = c("quantile", "hdi")) {

  method <- match.arg(method)

  nested <- switch(
    method,

    # allowing NA's through because this will be applied to empty data
    # where there are no predictions
    "quantile" = dplyr::summarise(
      .data,
      # TODO we could avoid list column here (and tidyr::unnest_wider() later)
      # if dtplyr::summarise() weren't slightly broken...
      # https://github.com/tidyverse/dtplyr/issues/454 [they're maybe fixing it]
      "pi" = list(
        c(0.5, 0.05, 0.95, 0.025, 0.975, 0.25, 0.75, 0.17, 0.83) |>
          stats::quantile(.value, probs = _, na.rm = TRUE) |>
          purrr::set_names(\(x) paste0("pi_", x) |> sub("%$", "", x = _))
      ),
      .groups = "drop"
    ) |>
      # TODO have to collect() as there is no dtplyr method for unnest_wider()
      dplyr::collect(), # if they add one, use that first.


    # unclear whether NA can be passed through
    # unclear what the appropriate central value should be (pi_50) so have gone
    # with median
    "hdi" = dplyr::summarise(
      .data,
      "pi_50" = stats::quantile(.value, probs = 0.5, na.rm = TRUE), # median
      "pi" = list(
        c(0.9, 0.95, 0.5, 0.66) |>
          bayestestR::hdi(.value, ci = _, na.rm = TRUE) |>
          tidyr::pivot_wider(
            names_from = CI, values_from = !CI, names_vary = "slowest") |>
          purrr::set_names(paste0(
            "pi_", 100 * c(0.05, 0.95, 0.025, 0.975, 0.25, 0.75, 0.17, 0.83)))
      ),
      .groups = "drop"
    ) |>
      dplyr::collect() # as above
  )

  tidyr::unnest_wider(nested, pi)
}



#' Take low-level prediction samples and aggregate them to coarser covariates.
#'
#' Prediction samples are produced at the lowest level possible, which gives the
#' flexibility to aggregate predictions so e.g. higher geographies (such as trust -> region).
#' This is preferred to aggregating intervals, as it preserves the uncertainty
#'  of the modelled predictions more robustly.
#'
#' This is quite opinionated with what columns it expects within the dataframe,
#'  beware. These include: `.value`, `population`, `target`, and anything supplied in
#'  `remove_identifiers`
#'
#' @param .sample_predictions Data frame with one row per sample per set of covariates.
#' @param remove_identifiers Vector of column names which appear in the model
#'  predictions, but we do not want to be aggregated by.
#'  All covariates not specified in this argument, or aggregated, will be
#'  grouped by. In the case where the data is at the correct aggregation,
#'  or no variables need to be removed, a NULL should be provided.
#'
#' @returns Dataframe of one row per sample per set of non-removed covariates.
#'
#' @examples
#'
#' trust_samples |> # Consider we want to aggregate from trust to region level,
#'   intervals$aggregate_samples( # we need to remove trust and icb identifiers
#'     remove_identifiers = c("trust_code", "icb_name"))
#'
#' @export
aggregate_samples <- function(
    .sample_predictions,
    remove_identifiers = c("trust_code", "icb_name", "nhs_region_name", "age_group"),
    include_population = TRUE
    ) {

  if (
    !"population" %in% colnames(.sample_predictions) &&
      !"population" %in% .sample_predictions$vars &&
      include_population
  ) {
    cli::cli_abort(
      c(
        "x" = "{.fn intervals$aggregate_samples} requires a {.field population} column in the {.arg .sample_predictions} input when {.code include_population = TRUE}.", # nolint: line_length_linter.
        "?" = "Did you mean to set {.code include_population = FALSE}?"
      )
    )
  }

  .sample_predictions |>
    # calculate the summed up predictions to the level we want
    dplyr::summarise(
      .value = sum(.value, na.rm = TRUE),
      population = sum(population, na.rm = TRUE),
      target = dplyr::if_else(all(is.na(target)), NA_real_, sum(target, na.rm = TRUE)),
      .by = !dplyr::all_of(c(remove_identifiers, ".value", if (isTRUE(include_population)) "population", "target"))
    )

}


#' Take prediction samples and categorise them as changes in rates.
#'
#' We want to define categories "increase", "decrease", "stable", and assign
#' them probabilities. We do this via the change from the true target data
#' `forecast_horizon` days ago. We can work in per-capita-rates to avoid
#' different sized geographies being an issue.
#'
#' Assumes data is aggregated to the required level without extra covariates.
#' Rates automatically converted to per 100k for ease of explaining thresholds.
#' `.sample_predictions` requires `date`, `.value`, `target`, `population` columns.
#'
#' @param .sample_predictions Dataframe of one row per sample per set of covariates.
#' @param upper_rate The per capita (100k) rate change above which the trend is categorised as "increase".
#' @param lower_rate The per capita (100k) rate change below which the trend is categorised as "decrease".
#' @param forecast_horizon how many days ahead a prediction counts as a change, used to calculate change in value.
#'
#' @returns .samples_predictions with the additional columns `p_increase`, `p_decrease`, `p_stable`.
#'
#' @examples
#'
#' formatted_samples |>
#'   intervals$discretise_trends(
#'     upper_rate = 0.1,
#'     lower_rate = -0.1,
#'     forecast_horizon = 14
#'   )
#'
#' @export
discretise_trends <- function(
    .sample_predictions,
    upper_rate,
    lower_rate,
    forecast_horizon) {
  discrete_trends <- .sample_predictions |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      # work out what the true values were at a time step away
      # as rates so thresholds are not dependent of geography population size
      "target_n_avg" = (target / population) |>
        dplyr::lag(n = forecast_horizon) |>
        # roll average to remove day-of-week seasonality; more robust comparison
        data.table::frollmean(7, align = "right", na.rm = TRUE),
      # Value of the projection, as a rate per population size
      ".value_avg" = (.value / population) |>
        data.table::frollmean(7, align = "right", na.rm = TRUE),
      # Equivalent value from a forecast horizon ago (generally 14 days):
      ".value_n_avg" = (.value / population) |>
        dplyr::lag(n = forecast_horizon) |>
        data.table::frollmean(7, align = "right", na.rm = TRUE),

      .by = !c(date, .value, target, population) # grouped by all but these
    ) |>
    # probability is proportion of samples above / below threshold
    # relative changes can need quite high thresholds: ~ 20-50%.
    dplyr::mutate(
      "rate_diff" = dplyr::case_when(
        # This is the primary method: compare current model to lagged model:
        .value_n_avg > 0 ~ (.value_avg - .value_n_avg) / .value_n_avg,
        # This is the backup way: compare model to lagged real world value:
        .value_n_avg == 0 & target_n_avg > 0 ~
          (.value_avg - target_n_avg) / target_n_avg,
        # Catch any stragglers; forces increase/decrease based just on model
        .value_n_avg == 0 & .value_avg > 0 ~ upper_rate + 0.01, # always p_inc
        .value_n_avg == 0 & .value_avg < 0 ~ lower_rate - 0.01, # always p_dec
        .value_n_avg == 0 & .value_avg == 0 ~ 0, # always stable
        TRUE ~ 0), # final catch-all; always stable;
      # don't use .default, it causes a `subscript out of bounds` error here
      "p_increase" = mean(rate_diff > upper_rate),
      "p_decrease" = mean(rate_diff <= lower_rate),
      "p_stable" = 1 - (p_increase + p_decrease),
      rate_diff = NULL, # we don't need this any more
      .by = !c(
        .sample, .value, target, population, model,
        .value_avg, .value_n_avg, target_n_avg),
      .keep = "unused"
    ) |>
    dplyr::relocate(p_stable, .after = p_increase)
}

#' Take prediction samples and convert to prediction intervals.
#'
#' Essentially a wrapper function of:
#'  - `aggregate_samples()`
#'  - `discretise_trends()`
#'  - `generate_intervals()`
#'  Which summarizes our prediction samples into communicable formats.
#'
#' @param .sample_predictions Dataframe of one row per sample per set of covariates.
#' @param remove_identifiers Vector of column names which appear in the model
#' predictions, but we do not want to be aggregated by. All covariates not
#' specified in this argument, or aggregated, will be grouped by.
#' In the case where the data is already correctly aggregated, or no variables
#' need to be removed, `NULL` should be provided.
#' @param overall_params List of parameters for the model / project.
#' Must contain `$threshold_params$upper_rate`, `threshold_rates$lower_rate`
#' and `$forecast_horizon`, how far we are predicting into the future, which
#' determine the discretisation. Can be `NULL` if not discretisation.
#'
#' @returns Dataframe of one row per covariate group, with quantiles and probabilities of trends.
#'
#' @examples
#' # Consider we want to aggregate from trust to region level,
#' # we need to remove trust and icb identifiers
#'
#' trust_samples |>
#'   intervals$samples_to_quantiles(
#'     remove_identifiers = c("trust_code", "icb_name"),
#'     overall_params = list(forecast_horizon = 14,
#'       threshold_rates = list(upper_rate = 0.1,
#'         lower_rate = -0.1)))
#'
#' @export
samples_to_quantiles <- function(
    .sample_predictions,
    remove_identifiers = c(
      "trust_code", "nhs_region_name", "icb_name", "age_group"),
    overall_params = NULL, # only needed for discretise_trends, with population
    method = "quantile"
    ) {

  unavailable_identifiers <- setdiff(
    remove_identifiers,
    names(.sample_predictions))
  if (length(unavailable_identifiers) != 0) {
    stop(
      "Columns named in `remove_identifiers` must be present in `.data`!\n",
      "These columns were not available: ",
      paste(unavailable_identifiers, collapse = ", "))
  }

  # dplyr needs to be attached for some dtplyr functions to work properly
  if (!("package:dplyr" %in% search()))
    attachNamespace("dplyr")

  if ("population" %in% colnames(.sample_predictions) && !is.null(overall_params)) {

    quantiles <- .sample_predictions |>
      dtplyr::lazy_dt() |>
      aggregate_samples(remove_identifiers = remove_identifiers) |>
      discretise_trends(
        upper_rate = overall_params$threshold_rates$upper_rate,
        lower_rate = overall_params$threshold_rates$lower_rate,
        forecast_horizon = overall_params$forecast_horizon
      ) |>
      # group by the unique covariates
      dplyr::group_by(dplyr::across(!c(.sample, .value, model))) |>
      # find the summary stats we want
      generate_intervals(method = method) |> # includes the collect for lazy_dt
      dplyr::ungroup()
  } else { # TODO: Remove this population-less branch:
    quantiles <- .sample_predictions |>
      dtplyr::lazy_dt() |>
      aggregate_samples(remove_identifiers = remove_identifiers) |>
      # group by the unique covariates
      dplyr::group_by(dplyr::across(!c(.sample, .value, model))) |>
      # find the summary stats we want
      generate_intervals(method = method) |>
      dplyr::ungroup()
  }

  return(quantiles)
}



#' Generate prediction samples from fit `mgcv` and new data using `gratia` package
#'
#' Thin wrapper for the `gratia::posterior_samples()` function, which gives us
#' model prediction samples.
#'
#' [gratia package version](https://gavinsimpson.github.io/gratia/reference/predicted_samples.html)
#'
#' @param .data Dataframe containing the historic (train) and future (test) data.
#' @param .model Fitted `mgcv` object.
#' @param .n_pi_samples Integer number of samples to generate from model.
#' @param method Character denoting which gratia method to generate posteriors from.
#' Typically "gaussian", though "mh" for metropolis hastings approach allowed.
#' The "mh" approach is currently not tested in production use, and may take longer time periods.
#' @param ... Allows for other arguments that will be passed to gratia::posterior_samples().
#' This is intended for the metropolis hastings other arguments, such as `burnin`, `rw_scale`,
#' and `thin`.
#' https://gavinsimpson.github.io/gratia/reference/posterior_samples.html
#'
#' @returns Matrix with one row per `.data` input, and one column per `.n_pi_samples`.
#'
#' @examples
#'
#' model_samples <- intervals$generate_samples(
#'   .model = mgcv_model,
#'   .data = training_data,
#'   .n_pi_samples = 1000)
#'
#' @export
generate_samples <- function(.data,
                             .model,
                             .n_pi_samples = 500,
                             method = c("gaussian", "mh"),
                             ...) {

  method <- match.arg(method)

  results <- gratia::posterior_samples(
    seed = 1,
    model = .model,
    data = .data,
    n = .n_pi_samples,
    method = method,
    ...
  ) |>
    dplyr::rename(
      .sample = .draw,
      .value = .response
    )

  formatted_results <- .data |>
    # generate the row number in the raw data
    dplyr::mutate(row = seq_len(dplyr::n())) |> # better way?
    dplyr::left_join(results, by = c("row" = ".row")) |>
    dplyr::select(-row)

  return(formatted_results)

}
