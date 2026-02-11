#' @name pulls
#' @section Version: 0.0.1
#'
#' @title Assorted data pulls for pancasts
#'
#' @description
#' Some of these rely on authentication against your Windows login, and therefore
#' must be run on your local machine (i.e. NOT on a spikeprotein instance).
#'
#' @seealso
#' * [pulls$autopull()]
#' * [pulls$redshift_bucket_path()]
#' * [pulls$query_vpd()]
".__module__."

box::use(
  box / deps_,
  box / help_,
  box / awssession,
  box / s3
)


.on_load <- function(ns) {
  deps_$need(
    "cli",
    "dplyr",
    "fs",
    "glue",
    "purrr",
    "rlang",
    "s3fs",
    "vroom",
    "withr"
  )
}


#' @export
box::use(
  . / vpd[query_vpd],
  . / winter_tests[query_winter_tests]
)


#' Path constructor for Redshift staging bucket
#'
#' Idea borrowed from bucket_path() from [box/redshift] - we can't use that
#' function directly here, because we're using an assumed-role session!
#'
#' @param ... character vectors, if any values are `NA`, the result will also be
#'   `NA`. The paths follow the recycling rules used in the tibble package, namely
#'   that only length 1 arguments are recycled.
#' @param ext An optional extension to append to the generated path.
#'
#' @export
redshift_bucket_path <- function(..., ext = "") {
  acc_id <- awssession$role_info()$original_role$Account
  s3fs::s3_path(glue::glue("PATH REDACTED"), ..., ext = ext)
}


#' Make special S3 paths
#'
#' Useful in conjunction with [pulls$autopull()].
#'
#' @param name Nickname of special path to make.
#'
#' @returns An S3 URI.
#'
#' @export
make_s3_path <- function(name = c("vpd", "winter_tests")) {
  name <- rlang::arg_match(name)

  folder <- switch(
    name,
    "winter_tests" = redshift_bucket_path("staging_sgss", "winter_tests"),
    "vpd" = redshift_bucket_path("staging_sgss", "vpd")
  )

  s3fs::s3_path(
    folder,
    paste(
      format(Sys.time(), "%Y-%m-%dT%H_%M_%S%z") |>
        gsub("\\+", "_", x = _), # S3 misbehaves with paths containing "+"
      "autopull",
      name,
      sep = "_"
    ),
    ext = "csv.gz"
  )
}


#' Gather info about query results and allow user to check
#'
#' @param query A dbplyr lazy query.
#' @param checks A vector of check names to include.
#' @param date_column Name of the query's date column, as a string; required
#'   if `date_range` check is included.
#' @returns `NULL`, invisibly; unless the user chooses to abort, in which case
#'   an error.
check_results_info <- function(
  query,
  checks = c("row_count", "date_range", "column_names"),
  date_column = attr(query, "date_range")
) {
  checks <- rlang::arg_match(checks, multiple = TRUE)

  if (length(checks) == 0) {
    cli::cli_alert_warning("No checks selected!")
    return(invisible(NULL))
  }

  cli::cli_alert_info("Retrieving info about results...")

  check_fns <- list(
    "row_count" = \(.) {
      . |>
        dplyr::count(name = "n") |>
        dplyr::pull(n)
    },
    "date_range" = \(.) {
      . |>
        dplyr::summarise(
          "min" = min(.data[[date_column]], na.rm = TRUE),
          "max" = max(.data[[date_column]], na.rm = TRUE)
        ) |>
        dplyr::collect() |>
        glue::glue_data("{min} - {max}")
    },
    "column_names" = colnames
  )

  info <- check_fns |>
    purrr::keep_at(checks) |>
    purrr::map(\(fn) fn(query))

  # TODO review alignment? https://github.com/r-lib/cli/issues/229
  cli::cli_bullets(c(
    "!" = "Please review the following information carefully!",
    "",
    " " = if (!is.null(info$row_count)) "Total rows: {cli::col_green(info$row_count)}",
    " " = if (!is.null(info$date_range)) "Date range: {cli::col_yellow(info$date_range)}",
    " " = if (!is.null(info$column_names)) "Column names: {cli::col_blue(info$column_names)}",
    "",
    "Does this seem right?"
  ))

  response <- readline("[yes/No]: ") |>
    substr(1, 1) |>
    tolower()

  if (response == "") {
    response <- "n"
  }

  while (!response %in% c("y", "n")) {
    response <- readline("Please respond with 'yes' or 'no': ") |>
      substr(1, 1) |>
      tolower()
  }

  if (response == "n") {
    cli::cli_abort("Oh well, better luck next time!", call = NULL)
  }

  invisible(NULL)
}


#' Run a data pull and upload the results to S3
#'
#' @param query The query to run; likely created by `pulls$query_*()`.
#' @param s3_path S3 URI (`"s3://..."`) where results will be uploaded to. It
#'   must end in `.csv` or `.csv.gz`. You can use [pulls$redshift_bucket_path()]
#'   to help construct a path within the Redshift staging bucket, or see
#'   [pulls$make_s3_path()] for some useful preconstructed paths.
#' @param local_path Path where results will be saved locally; if `NULL`, a
#'   temporary file will be used.
#' @param overwrite Whether to overwrite existing files at `local_path` and/or
#'   `s3_path`, if either/both exist.
#' @param check_info Whether to gather some info about the results before
#'   running the full query.
#' @param date_column Name of the query's date column, as a string (the date
#'   range is part of the info gathered by `check_info`).
#'
#' @returns S3 URI of successfully-uploaded results file.
#'
#' @examples
#' query_vpd() |>
#'   autopull(make_s3_path("vpd"))
#'
#' @export
autopull <- function(
  query,
  s3_path,
  local_path = NULL,
  overwrite = FALSE,
  check_info = TRUE,
  date_column = attr(query, "date_column")
) {
  if (!nzchar(Sys.getenv("AWS_DEFAULT_REGION"))) {
    cli::cli_abort(c(
      "{.envvar AWS_DEFAULT_REGION} environment variable not found.",
      "",
      "{.emph Add the following line to your {.file ~/.Renviron} file, and then restart your R session:}",
      " " = "{cli::col_yellow('AWS_DEFAULT_REGION')}=\"REDACTED\""
    ))
  }

  s3sesh <- awssession$s3()

  if (!grepl("\\.csv(\\.gz)?$", s3_path)) {
    cli::cli_abort("{.arg s3_path} must end with `.csv`/`.csv.gz`")
  }

  if (is.null(local_path)) {
    local_path <- withr::local_tempfile(fileext = paste0(".", fs::path_ext(s3_path)))
  }

  if (fs::path_ext(local_path) != s3fs::s3_path_ext(s3_path)) {
    cli::cli_abort("The file extensions of {.arg s3_path} and {.arg local_path} must match!")
  }

  if (!date_column %in% colnames(query)) {
    cli::cli_abort(
      "Column {.val {date_column}} not present in query results -
      do you need to specify a different value for the {.arg date_column} argument?"
    )
  }

  # Check write location is accessible
  # TODO this actually only tests List* permission - i.e. doesn't confirm object-write permission
  # Could possibly use paws.storage::s3()$get_object_acl(...) but I think overkill for now
  if (
    inherits(
      try(s3sesh$dir_ls(s3fs::s3_path_dir(s3_path)), silent = TRUE),
      "try-error"
    )
  ) {
    cli::cli_abort(c(
      "!" = "Can't access S3 location specified by {.arg s3_path}!",
      "*" = "{.emph Is there a typo in {.arg s3_path}?}",
      "*" = paste0(
        "{.emph Did you provide credentials for the wrong AWS account? (See {.href [our wiki](",
        "REDACTED",
        ")} for more info.)}"
      )
    ))
  }

  # Check for existing objects
  if (!isTRUE(overwrite)) {
    if (fs::file_exists(local_path)) {
      cli::cli_abort(c(
        "Object already exists locally at {.file {local_path}}",
        "Set {.arg overwrite = TRUE} if you'd like to replace it."
      ))
    }

    if (s3sesh$file_exists(s3_path)) {
      cli::cli_abort(c(
        "Object already exists in S3 at {.href [{.val {s3_path}}]({s3$object_page(s3_path)})}",
        "Set {.arg overwrite = TRUE} if you'd like to replace it."
      ))
    }
  }

  # Allow user to check some info about the query results before continuing
  if (isTRUE(check_info)) {
    check_results_info(query, date_column = date_column)
  }

  cli::cli_alert_info("Running query...")

  results <- query |>
    dplyr::collect()

  cli::cli_alert_info("Writing to {.file {local_path}}...")

  vroom::vroom_write(results, local_path, delim = ",", na = "")

  cli::cli_alert_info("Uploading to {.href [{.val {s3_path}}]({s3$object_page(s3_path)})}}...")

  awssession$s3()$file_upload(local_path, s3_path, overwrite = overwrite)
}
