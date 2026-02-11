#' @name panpipes
#' @section Version: 0.0.7
#'
#' @title
#' Utilities for working with pancasting pipelines
#'
#' @seealso
#' * [panpipes$commit_output()]
#' * [panpipes$run()]
#' * [panpipes$download_model_zip()]
#' * [panpipes$run_all()]
".__module__."

box::use(
  box / deps_,
  box / help_
)

.on_load <- function(ns) {
  deps_$need(
    "cli",
    "fs",
    "here",
    "job",
    "purrr",
    "rlang",
    "s3fs",
    "targets",
    "utils",
    "withr",
    "yaml",
    "zip"
  )
}


#' Commit model output objects to a special S3 folder
#'
#' Once a model pipeline has been run to produce "finalised" outputs, this
#' function can be used to copy those outputs to special locations in S3, so
#' that they can be used to build slide decks, perform end-of-season evaluation
#' etc.
#'
#' @param s3_uri Path(s) to the model output(s) in S3 which should be copied to
#'   special S3 locations.
#' @param season Season "label" under which outputs should be stored.
#' @param overwrite Should an object overwrite another object of the same name?
#'   Think very carefully before changing this to `TRUE`!
#'
#' @returns Invisibly, paths to new objects within special S3 locations.
#' @examples
#' Sys.setenv(TAR_PROJECT = "covid_admissions")
#' targets::tar_make()
#'
#' targets::tar_read(upload_summary) |>
#'   panpipes$commit_output()
#'
#' @export
commit_output <- function(s3_uri, season = "winter_2024", overwrite = FALSE) {
  box::use(box / s3[object_page])

  output_root <- "PATH REDACTED"

  # Make sure we're actually dealing with model output objects
  root_is_okay <- fs::path_has_parent(s3_uri, output_root)

  if (!all(root_is_okay)) {
    paths <- s3_uri[!root_is_okay]
    cli::cli_abort(c(
      "!" = "Some elements of {.arg s3_uri} don't look like model outputs!",
      "",
      "The following URIs aren't within the {.href [S3 output folder]({object_page(output_root)})}:",
      purrr::set_names("{.val {paths}}", "*")
    ))
  }

  # Any non-csv output gets copied to "evaluation" folder
  evaluation_root <- s3fs::s3_path(output_root, "evaluation", season)

  out_noncsv <- tibble::tibble("path" = stringr::str_subset(s3_uri, "\\.csv\\.gz$", negate = TRUE)) |>
    dplyr::mutate(
      "new_path" = s3fs::s3_path(
        evaluation_root,
        fs::path_rel(path, output_root)
      )
    ) |>
    purrr::pmap_chr(\(path, new_path) s3fs::s3_file_copy(path, new_path, overwrite = overwrite))

  # Any csv output gets copied to Glue landing bucket (which also makes the data
  # accessible via `pancasts_glue` schema in Redshift)
  glue_evaluation_root <- "PATH REDACTED"

  out_csv <- tibble::tibble("path" = stringr::str_subset(s3_uri, "\\.csv\\.gz$")) |>
    dplyr::mutate(
      "rel" = fs::path_rel(path, output_root),
      "disease" = stringr::str_split_i(rel, "/", 1),
      "metric" = stringr::str_split_i(rel, "/", 2),
      "filename" = stringr::str_split_i(rel, "/", -1),
      "type" = stringr::str_split_i(filename, "_", -4),
      "model_date" = stringr::str_split_i(filename, "_", -3),
      "new_path" = s3fs::s3_path(glue::glue("PATH REDACTED"))
    ) |>
    dplyr::select(path, new_path) |>
    purrr::pmap_chr(\(path, new_path) s3fs::s3_file_copy(path, new_path, overwrite = overwrite))

  cli::cli_alert_success("Copied {length(s3_uri)} object{?s} to evaluation locations!")

  invisible(c(out_csv, out_noncsv))
}


#' Run a targets pipeline
#'
#' If parameters aren't entered directly, helpful menus will be presented
#' instead.
#'
#' @param pipeline Name of pipeline to run.
#' @param ... Target names to pass to [targets::tar_make()]
#' @param clean_slate Run [targets::tar_destroy()] before starting the run.
#' @param as_job If `TRUE`, run as a background job.
#' @param print_warnings If `TRUE`, display output of `targets::tar_meta(fields = warnings, complete_only = TRUE)`.
#' @export
run <- function(
  pipeline = NULL,
  ...,
  clean_slate = FALSE,
  as_job = NULL,
  print_warnings = TRUE
) {
  valid_names <- yaml::read_yaml(here::here("_targets.yaml")) |>
    names()

  if (is.null(pipeline)) {
    cli::cat_line()
    cli::cli_text("{.emph Which pipeline would you like to run?}")

    choice <- utils::menu(valid_names)

    if (choice == 0) {
      cli::cli_abort("{.emph Fine, be that way!}", call = NULL)
    }

    pipeline <- valid_names[choice]
  } else {
    pipeline <- rlang::arg_match(pipeline, valid_names)
    cli::cli_text("{.emph Running pipeline {.val {pipeline}}}")
  }

  if (is.null(as_job)) {
    cli::cat_line()
    cli::cli_text("{.emph Where would you like to run it?}")

    choice <- utils::menu(c("Right here in this session", "As a background job"))

    if (choice == 0) {
      cli::cli_abort("{.emph Hmph, suit yourself >:(}", call = NULL)
    }

    as_job <- (choice == 2)
  }

  withr::local_envvar("TAR_PROJECT" = pipeline)

  if (isTRUE(clean_slate)) {
    cli::cat_line()
    cli::cli_text(paste0(
      "{.emph You've stated {.arg clean_slate = TRUE} - ",
      "are you {.strong {cli::col_red('ABSOLUTELY SURE')}} you want to wipe your targets store for this pipeline?}"
    ))

    choice <- utils::menu(c("Yes, run `targets::tar_destroy()` for me right now", "No no no, omg whoops"))

    switch(
      as.character(choice),
      "0" = cli::cli_abort(
        "{.emph ... well, feel free to come back when you're feeling a bit more decisive}",
        call = NULL
      ),
      "1" = targets::tar_destroy(ask = FALSE), # we already asked
      "2" = cli::cli_abort("{.emph Ufff, that could've been nasty!}", call = NULL)
    )
  }

  # Actually run the pipeline now!
  cli::cat_line()
  cli::cli_text("{.emph Okay, here we go...!}")
  targets::tar_make(..., as_job = isTRUE(as_job))

  # Print warnings if user demands
  if (isTRUE(print_warnings)) {
    cli::cat_line()
    cli::cli_text("{.emph The pipeline {.val {pipeline}} ended with the following warnings:}")
    targets::tar_meta(fields = warnings, complete_only = TRUE)
  }
}

#' Download `.zip` file of admissions or occupancy results from S3
#'
#' ... and unzip into corresponding local subdirectory.
#'
#' @param disease Disease specified as a string; see [user_check$disease_checker()] for allowed values.
#' @param metric Metric of interest.
#' @param s3_path Path to output `.zip` in S3, specified as a string.
#' By default, finds the latest file corresponding to `disease`.
#' @param local_path Directory to save unzipped output into. By default, places the outputs
#' in the "usual" local model outputs location.
#'
#' @returns Output path, invisibly.
#' @export
download_model_zip <- function(disease, metric, s3_path = NULL, local_path = NULL) {
  box::use(
    box / s3,
    prj / user_check[disease_checker]
  )

  disease <- disease_checker(disease)
  metric <- rlang::arg_match(metric, c("admissions", "occupancy"))
  # we store results for covid as "covid-19" in some places
  disease_name <- ifelse(disease == "covid", "covid-19", disease)
  # on s3 occupancy is called "occupancy rate"
  metric_name <- ifelse(metric == "occupancy", "occupancy_rate", metric)

  if (is.null(s3_path)) {
    s3_path <- s3fs::s3_path(
      "PATH REDACTED"
    ) |>
      s3$find_latest_file()
  }

  # fix up the file path for local storage
  if (is.null(local_path)) {
    local_path <- s3_path |>
      stringr::str_replace("covid-19", "covid") |>
      stringr::str_extract(glue::glue("PATH REDACTED")) |>
      stringr::str_remove("local_outputs/") |>
      stringr::str_replace(disease, glue::glue("PATH REDACTED")) |>
      stringr::str_replace(metric_name, metric)
  }
  local_directory <- stringr::str_remove(local_path, ".zip")
  # write to disk
  s3$read_using(s3_path, \(.) zip::unzip(., exdir = local_directory))

  cli::cli_alert_success("Modelling outputs available in {.file {local_directory}}")

  return(invisible(local_directory))
}


#' Run all modelling pipelines
#' @param clean_slate Should we run [targets::tar_destroy()] on each pipeline? If `TRUE` all pipelines are destroyed
#' _before_ running _any_ targets. Defaults to `FALSE`
#' @param as_job should scripts be run as (serial) background jobs? Defaults to `FALSE`
#' @return `TRUE` invisibly
#' @export
run_all <- function(clean_slate = FALSE, as_job = FALSE) {
  pipeline_names <- here::here("_targets.yaml") |>
    yaml::read_yaml() |>
    names()

  if (isTRUE(clean_slate)) {
    cli::cli_alert_warning("Destroying all targets!")

    purrr::walk(pipeline_names, destroy_pipeline)
  }

  if (isTRUE(as_job)) {
    # empty() can be used to run a line of code in the background in a "clean" environment
    # this method of running a background job is used over the `as_job` param from `run()` as `walk()` effectively runs
    # background jobs in parallel
    box::use(job[empty])
    empty(
      {
        purrr::walk(pipeline_names, \(pipeline) run(pipeline, as_job = FALSE))
      },
      import = c(pipeline_names)
    )
  } else {
    # identity function can be used to run code in foreground
    purrr::walk(pipeline_names, \(pipeline) run(pipeline, as_job = FALSE))
  }

  return(invisible(TRUE))
}

#' Destroy results of a named pipeline
#' Small helper
#'
#' @param pipeline name of targets pipeline to be destroyed
destroy_pipeline <- function(pipeline) {
  withr::local_envvar("TAR_PROJECT" = pipeline)
  targets::tar_destroy(ask = FALSE)
}
