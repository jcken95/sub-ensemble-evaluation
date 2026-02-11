#' @name format
#' @section Version: 1.0.3
#'
#' @title Helper functions to bring into the agreed forecast schema
#'
#' @description
#' Extract prediction outputs in a nicer format.
#' Create individual model sample predictions
#' Relies on structure of foreach list creation currently used.
#'
#' @seealso
#' * [format$format_outputs()]
#' * [format$extract_from_list()]
#'
".__module__."

box::use(
  box / deps_,
  box / help_,
  prj / user_check,
)

.on_load <- function(ns) {
  deps_$need(
    "purrr"
  )
}

#' Function formatted dataframes of model outputs
#'
#' Parameters should generally be found in the config file.
#'
#' @param .data Dataframe to format.
#' @param target_name String of the variable being predicted. E.g., "admissions".
#' @param forecast_horizon Number of days into the future being predicted. E.g., 14.
#' @param disease String of disease name, should be in config. E.g., "covid".
#'
#' @returns Dataframe with formatted output predictions.
#'
#' @examples
#' # Sample code from covid run_admissions.R
#' all_formatted_summary <- predictions |> # model results & ensemble
#'   purrr::map("geo_preds") |>
#'   purrr::list_rbind(names_to = "model") |>
#'   format$format_outputs(
#'     config$overall_params$target_name, # "admissions"
#'     config$overall_params$forecast_horizon, # 14
#'     config$overall_params$disease) # "covid", etc
#'
#' @export

format_outputs <- function(
    .data,
    target_name = config$overall_params$target_name,
    forecast_horizon = config$overall_params$forecast_horizon,
    disease = config$overall_params$disease
    ) {
  disease <- user_check$disease_checker(disease) # clean user input

  .data |>
    dplyr::mutate(
      # attach disease so we can use the vectorised if_else
      # this allows us to retain age group information for rsv
      disease = disease,
      age_group = ifelse(disease == "rsv", age_group, "all"),
      age_group_granularity = ifelse(disease == "rsv", age_group_granularity, "none"),
      forecast_horizon = forecast_horizon,
      target_name = paste0(disease, "_", target_name)
    ) |>
    dplyr::select(-disease) |>
    dplyr::rename(target_value = target) |>
    # We don't love having this in,
    # but unclear where the random `calls` quantile duplicates are coming from:
    dplyr::distinct(.keep_all = TRUE) |>
    dplyr::select(
      model,
      prediction_start_date,
      location,
      location_level,
      age_group,
      age_group_granularity,
      population,
      target_name,
      target_value,
      date,
      forecast_horizon,
      # Keep these columns if they exist - but don't throw an error if they don't!
      dplyr::any_of(c(
        ".sample",
        ".value"
      )),
      dplyr::any_of(c(
        "p_increase",
        "p_stable",
        "p_decrease",
        "pi_50",
        "pi_5",
        "pi_95",
        "pi_2.5",
        "pi_97.5",
        "pi_25",
        "pi_75",
        "pi_17",
        "pi_83"
      ))
    )
}

#' Function to extract formatted dataframes and model objects
#'
#' The function binds the list elements by row.
#'
#' @param .data Dataframe to format
#' @param target_name String of the variable being predicted
#' NB: add `disease` here when ready
#' @returns Dataframe with formatted outputs
#'
#' @export

format_norovirus_nowcast_outputs <- function(
    .data,
    target_name
    ) {

  .data |>
    dplyr::mutate(
      age_group = "all",
      age_group_granularity = "none",
      target_name = paste0("norovirus_", target_name)
    ) |>
    dplyr::rename(target_value = target)
}


#' Function to extract formatted dataframes and model objects
#'
#' The function binds the list elements by row,
#' and keeps the model's name for the predictions dataframe.
#'
#' @param model_outputs Individual model output from run_model
#'
#' @returns List with dataframe of formatted predictions and model objects
#'
#' @export

extract_from_list <- function(model_outputs) {
  # Different models call their output variations on maths_word_predictions:
  predictions_name <- grep("pred", names(model_outputs[[1]]), value = TRUE)[1]

  # we want to bind all prediction dates, and access model by prediction date
  predictions <- model_outputs |>
    purrr::map(predictions_name) |>
    purrr::list_rbind()

  # Yes okay, we have to iterate twice, but that's a small price to pay
  models <- purrr::map(model_outputs, "model")

  model_extract <- list(predictions, models)
  names(model_extract) <- c(predictions_name, "models") # keeps the input name
  return(model_extract)
}
