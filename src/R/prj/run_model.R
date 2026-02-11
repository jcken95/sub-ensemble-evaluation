#' @name run_model
#' @section Version: 2.1.1
#'
#' @title
#' Function to take data and model parameters and run models over each lookback/projection.
#'
#' @description
#' Wrapper that adds the parallelisation and different lookbacks.
#'
#' @seealso
#' * [run_model$run_scripted_model()]
".__module__."

box::use(
  box / deps_,
  box / help_,
  prj / user_check,
  prj / intervals
)

.on_load <- function(ns) {
  deps_$need(
    "dplyr",
    "furrr",
    "gratia>=0.10.0",
    "rlang"
  )

  deps_$min_version("dplyr", "1.1.0")
}

#' Wrapper function for running the selected model from script
#'
#' Takes data, parameters, and model selection to run over each lookback.
#' This wrapper can also run multiple models in parallel on different lookbacks.
#'
#' @param wd string of working directory, defaults to git root folder.
#' @param model_name String of the function name of the model to be run;
#' in most cases this will be the same as the script name it came from,
#' if not supply full model_path within overall_params below.
#' @param training_data Data frame of the input data.
#' @param overall_params A named list of parameters for the model.
#' This is generally taken from config, but could be manually set in a script.
#' Now requires a disease term if no full path is given.
#' Looks for a metric, admissions, occupancy, or cases based on model/folder,
#' else it will ask about its attempted mapping from target_name.
#' Can accept a model_path to pick a model totally manually: this path should
#' include everything from working directory to the final .R of the file name.
#' @param prediction_dates A vector of dates defining the start of each
#' lookback, or for some nowcasts the end dates (hence the generic name).
#' @param required_covariates A list of strings of data's column names to keep,
#' generally already listed in the config.
#' @param model_hyperparams A named list of numerical priors for the model,
#' also generally found in the config.
#'
#'
#' @returns Combined model outputs, as a data frame.
#'
#' @examples
#'
#' # Create a parallel plan if desired
#' future::plan(future::multisession, workers = length(start_dates))
#'
#' config <- yaml::read_yaml(config_path) # wherever your configurations file is
#'
#' model_outputs <- run_model$run_scripted_model(
#'   wd, # working directory
#'   model_name = "univariate", # the model function name
#'   training_data = training_data, # pipeline data
#'   overall_params = config$overall_params, # includes disease & metric
#'   prediction_dates = start_dates, # list of dates for lookbacks or end dates
#'   required_covariates = config$required_covariates, # training_data columns
#'   model_hyperparams = config$hyperparams$calls # priors for model
#' )
#'
#' @export

run_scripted_model <- function(
  wd = system("echo $(git rev-parse --show-toplevel)", intern = TRUE),
  model_name,
  training_data,
  overall_params,
  prediction_dates, # TODO may need to update more implementations elsewhere
  required_covariates,
  model_hyperparams
) {
  if (is.null(overall_params$model_path)) {
    # lookup if there isn't anything set
    if (is.null(model_hyperparams$model_path)) {
      if (is.null(overall_params$disease)) {
        stop(
          "The run_model$run_scripted_model() function needs to be told which",
          " disease folder to find the model script. Please set this within",
          " overall_params$disease; you should set up in the config too."
        )
      }
      overall_params$disease <- user_check$disease_checker(
        overall_params$disease
      )

      # Metric Checker
      metric <- ifelse(
        is.null(overall_params$metric), # can be given directly.
        overall_params$target_name,
        overall_params$metric
      )
      if (!metric %in% c("admissions", "occupancy", "cases")) {
        user_metric <- metric
        metric <- dplyr::case_when(
          # If arrival_admissions becomes separate thing put it here.
          grepl("admi", metric, ignore.case = TRUE) ~ "admissions",
          grepl("occ", metric, ignore.case = TRUE) ~ "occupancy",
          grepl("case", metric, ignore.case = TRUE) ~ "cases",
          TRUE ~ metric
        ) # for other models in future, on your own head be it
        if (metric != user_metric) {
          message(
            'Your entered metric was: "',
            user_metric,
            '".\n',
            'Known metrics are "admissions", "occupancy", or "cases".\n',
            "We're guessing you meant ",
            '"',
            metric,
            '".'
          )
          user_check$user_check("Is this correct?")
        }
      }
      # match.arg() is cleaner than these, but less flexible
      # e.g. I don't find pmatch() picks up 'flu' for 'influenza' while mine does.

      # Trim off ".R" from the end of `model_name`, if it exists
      model_name <- sub("\\.R$", "", model_name)

      model_path <- file.path(
        wd,
        overall_params$disease,
        "models",
        metric,
        "models",
        paste0(model_name, ".R")
      )
    } else {
      model_path <- model_hyperparams$model_path # for per-model pathway
    }
  } else {
    model_path <- overall_params$model_path
  }

  if (!file.exists(model_path)) {
    stop("No such model file found. Please check this path for mistakes:", model_path)
  }

  # Load model function from file
  model_fn <- local({
    source(model_path, local = TRUE)

    # TODO temporarily run .on_load() function manually, when possible - once all
    # model files are converted to modules (https://github.com/REDACTED),
    # we can improve this (https://github.com/REDACTED)
    if (exists(".on_load")) {
      .on_load(ns = NULL)
    }

    get(paste0("run_", model_name))
  })

  message("Fitting ", model_name, " model: Started")

  model_outputs <- prediction_dates |>
    rlang::set_names() |>
    # Parallelised if user has set up a parallel plan with future::plan()
    furrr::future_map(
      \(.) {
        model_fn(
          .data = training_data,
          forecast_horizon = overall_params$forecast_horizon,
          n_pi_samples = overall_params$n_pi_sample,
          prediction_date = .,
          output_variables = required_covariates,
          hyperparams = model_hyperparams
        )
      },
      .options = furrr::furrr_options(seed = TRUE)
    )

  message("Fitting ", model_name, " model: Done!")

  model_outputs
}
