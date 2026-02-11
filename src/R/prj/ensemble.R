#' @name ensemble
#' @section Version: 0.0.3
#'
#' @title
#' Create ensemble model predictions
#'
#' @description
#' Combine predictions from multiple models to create ensemble model predictions.
#' This can be done via averaging prediction intervals or using individual prediction samples.
#'
#' @seealso
#' * [ensemble$ensemble_from_points()]
#' * [ensemble$ensemble_from_samples()]
#'
".__module__."

box::use(
  box / deps_,
  box / help_,
  box / ops[...],
  prj / intervals
)

# TODO
# Add more ensemble `methods`
# Add function to ensemble_from_samples using HDI

.on_load <- function(ns) {
  deps_$need(
    "dplyr",
    "glue",
    "tidyr"
  )
}

#' Ensemble models using prediction intervals
#'
#' Takes prediction intervals from multiple models and averages them.
#' Currently only supports "mean" ensemble method.
#'
#' @param .data Dataframe of rows of prediction intervals from models
#' to be ensembled - can contain historic non-predicted data,
#' though will throw warning.
#' @param method String for ensemble method used
#' @param model_name String for a model name to assign to the ensemble,
#' e.g. string containing names of models included in the ensemble
#'
#' @returns Dataframe of prediction intervals for the ensemble model
#'
#' @export
ensemble_from_points <- function(.data, method = "mean", model_name) {

  if (method == "mean") {
    ensemble <- .data |>
      # easier to average in long format rather than in wide formats
      tidyr::pivot_longer(cols = dplyr::starts_with("pi_"),
        names_to = ".quantile",
        values_to = ".value") |>
      # fixes different lengths of ensemble models
      # this will raise a warning due to the back data not having a model name
      dplyr::mutate(first_date = min(date[!is.na(.value)]), .by = c("model", "prediction_start_date")) |>
      dplyr::mutate(.value = dplyr::case_when(
        date < max(is.finite(first_date)) ~ NA_real_,
        TRUE ~ .value
      ), .by = c("prediction_start_date")) |>
      dplyr::select(-c("first_date")) |>
      # perform averaging
      dplyr::summarise(.value = mean(.value), .by = dplyr::across(-c("model", ".value"))) |>
      # transform back to wide
      tidyr::pivot_wider(names_from = ".quantile", values_from = .value)
  }

  ensemble |>
    dplyr::mutate(model = dplyr::case_when(
      !is.na(pi_50) ~ glue::glue("{method}_ensemble_{model_name}"),
      TRUE ~ NA))
}




#' Apply weighting to samples from multiple models.
#'
#' Takes individual prediction samples from multiple models to
#' produce prediction samples proportionate to weighting.
#'
#' Weighting involves a proportionate thinning (discarding) of samples
#' to match the specified proportions desired.
#'
#' Currently is used to ensemble, and apply weighting to outputs.
#'
#' @param .sample_predictions Dataframe of rows of individual prediction
#' samples from models to be ensembled
#' @param model_weights List of `model_name`: value pairs.
#' Values must be non-negative and sum to 1.
#' Defaults to `NULL`, which will provide equal weighting.
#' example model_weights:
#' model_weights = list("ets" = 0.9, "univariate" = 0.1)
#'
#' @returns Dataframe of training data weighted prediction samples.
#'
#' @export
weight_samples <- function(
    .sample_predictions,
    model_weights = NULL
    ) {
  # check all models have the same highest number of samples
  n_samples_per_model <- .sample_predictions |>
    dplyr::filter(!is.na(model)) |>
    dplyr::summarize(max_sample = max(.sample, na.rm = TRUE), .by = "model") |>
    dplyr::pull(max_sample) |>
    unique()

  if (length(n_samples_per_model) > 1) {
    stop("You must give the same number of samples for each model")
  }

  # generate equal weights if none are given (default behaviour)
  if (is.null(model_weights)) {
    # create dataframe of equal weights
    weight_df <- data.frame(
      model = unique(.sample_predictions$model, na.rm = TRUE),
      weight = 1
    ) |>
      dplyr::mutate(weight = weight / sum(weight))
    # use weights given
  } else {
    # turn list into a joinable dataframe
    weight_df <- model_weights |>
      as.data.frame() |>
      tidyr::pivot_longer(cols = dplyr::everything(),
        names_to = "model",
        values_to = "weight")
  }

  if (!isTRUE(all.equal(sum(weight_df$weight), 1))) {
    stop("Weights for all models must == 1")
  }

  # checks if weights are mispecified
  if (length(base::setdiff(
    weight_df$model, .sample_predictions$model)) != 0) {
    stop("There is a difference between models in data and weights")
  }

  # select only non-zero weighted models
  non_zero_weighed_models <- weight_df |>
    dplyr::filter(weight > 0)

  # truncate to shortest model time series
  .sample_predictions <- .sample_predictions |>
    # add in logic to only truncated (and ensemble) by models included
    # in weighting
    dplyr::filter(model %in% c(unique(non_zero_weighed_models$model), NA)) |>
    dplyr::mutate(first_date = min(date), .by = c(model, prediction_start_date)) |>
    dplyr::filter(date >= max(first_date), .by = prediction_start_date) |>
    dplyr::select(-first_date)

  # we want the number of samples per model
  # we need to use as many samples as possible
  weight_sample_df <- weight_df |>
    dplyr::mutate(n_samples = round(weight *
      (1 / max(weight)) *
      n_samples_per_model, 0))

  weighted_samples <- .sample_predictions |>
    dplyr::left_join(weight_sample_df |>
      dplyr::select(model, n_samples),
    by = "model") |>
    # apply a thinning based on the samples needed
    # note: this means we won't get "exact" proportions specified
    dplyr::filter(.sample %% round(
      n_samples_per_model / n_samples, 0) == 0 | is.na(.sample)
    ) |>
    dplyr::select(-n_samples)

  weighted_samples


}


#' Ensemble models using prediction samples
#'
#' Takes individual prediction samples from multiple models to
#' produce prediction intervals for an ensemble model.
#'
#' Currently only supports "mellor" ensemble method,
#' which uses [intervals$samples_to_quantiles()].
#'
#' @param .sample_predictions Dataframe of rows of individual prediction
#' samples from models to be ensembled
#' @param remove_identifiers Vector of column names to remove for aggregating,
#' e.g. lower spatial identifiers than those desired
#' @param model_name String for a model name to assign to the ensemble,
#' e.g. string containing names of models included in the ensemble
#' @param method Character string appended to model name.
#' Allows an extra description of the method chosed, helpful if
#' comparing different approaches to ensembling.
#' @param model_weights List of `model_name`: value pairs.
#' Values must be non-negative and sum to 1.
#' Defaults to NULL, which will provide equal weighting.
#' example model_weights:
#' model_weights = list("ets" = 0.9, "univariate" = 0.1)
#'
#' @returns Dataframe of training data and prediction intervals
#' for the ensemble model
#'
#' @export

ensemble_from_samples <- function(
    .sample_predictions,
    remove_identifiers = c("trust_code", "nhs_region_name"),
    model_name,
    method = "",
    overall_params,
    model_weights = NULL) {
  weighted_samples <- weight_samples(
    .sample_predictions = .sample_predictions,
    model_weights = model_weights)

  if (is.null(model_weights)) {
    weighting_type <- "unweighted"
  } else {
    weighting_type <- "weighted"
  }
  # do the weighting to the samples
  ensemble <- weighted_samples |>
    intervals$samples_to_quantiles(
      remove_identifiers = remove_identifiers,
      overall_params = overall_params
    )


  ensemble |>
    dplyr::mutate(
      model = dplyr::if_else(
        is.na(pi_50),
        NA_character_,
        glue::glue("{weighting_type}_{method}_ensemble_{model_name}"),
      )
    )
}
