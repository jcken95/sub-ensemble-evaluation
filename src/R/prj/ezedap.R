#' @name ezedap
#' @section Version: 0.0.2
#'
#' @title
#' Easy EDAP data transfers
#'
#' @description
#' Interactive functions for transferring data from EDAP's S3 to our own S3.
#'
#' @seealso
#' * [ezedap$oneoneone_calls()]
#' * [ezedap$oneoneone_online()]
#'
".__module__."

box::use(
  box / deps_,
  box / help_,
  box / redshift,
  box / awssession
)


.on_load <- function(ns) {
  deps_$need(
    "cli",
    "dplyr",
    "glue"
  )
}


copy_latest <- function(edap_s3_uri, idm_s3_uri) {
  cli::cli_alert_info("Inspecting {.val {edap_s3_uri}}...")

  info <- awssession$s3()$dir_info(edap_s3_uri, type = "file") |>
    dplyr::slice_max(last_modified)

  # TODO review alignment? https://github.com/r-lib/cli/issues/229
  cli::cli_bullets(c(
    "!" = "Please review the following information carefully!",
    "",
    " " = "Most recent file: {cli::col_blue(s3fs::s3_path_file(info$uri))}",
    " " = "Last modified: {cli::col_yellow(info$last_modified)}",
    " " = "File size: {cli::col_green(info$size)}",
    "",
    "Is this definitely the file you want to transfer?"
  ))

  response <- readline("[yes/No]: ") |>
    substr(1, 1) |>
    tolower()

  if (response == "") {
    response <- "n"
  }

  while (!response %in% c("y", "n")) {
    response <- readline("Please respond with 'yes' or 'no': ")
  }

  if (response == "n") {
    cli::cli_alert_danger("Oh well, better luck next time!")
    return(invisible(NULL))
  }

  awssession$s3_copy_to_idm(info$uri, idm_s3_uri)
}


#' Copy latest 111-online data file to Redshift staging bucket
#'
#' This function is intended for interactive use.
#'
#' @param s3_path Path to folder in EDAP S3 containing NHS 111 Online data - you
#'   should very rarely need to alter this.
#'
#' @returns Path to file in IDM S3, invisibly.
#'
#' @export
oneoneone_online <- function(
  s3_path = "PATH REDACTED"
) {
  copy_latest(
    s3_path,
    redshift$bucket_path("REDACTED", "REDACTED")
  )
}


#' Copy latest 111-calls data file to Redshift staging bucket
#'
#' This function is intended for interactive use.
#'
#' @param s3_path Path to folder in EDAP S3 containing NHS 111 Calls data - you
#'   should very rarely need to alter this.
#'
#' @returns Path to file in IDM S3, invisibly.
#'
#' @export
oneoneone_calls <- function(s3_path = "PATH REDACTED") {
  copy_latest(
    s3_path,
    redshift$bucket_path("REDACTED", "REDACTED")
  )
}
