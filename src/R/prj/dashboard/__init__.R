#' @name dashboard
#' @section Version: 0.0.1
#'
#' @title Automated regional dashboard creation
#'
#' @description Functions and documents to create regional dashboard
#'
#' @seealso
#' * [dashboard$generate_dashboards()]
#' * [dashboard$generate_one_dashboard()]
#' * [dashboard$get_regions()]
#' * [helpers$plot_projection()]
#' * [helpers$tabulate_projection()]
#' * [helpers$clean_icb_names()]
".__module__."

box::use(
  box / deps_,
  box / help_,
  box / redshift
)

.on_load <- function(ns) {
  deps_$need(
    "cli",
    "dplyr",
    "fs",
    "glue",
    "purrr",
    "quarto",
    "stringr"
  )
}

#' @export
box::use(
  . / helpers
)

#' Generate regional dashboards
#'
#' @param output_directory path to where dashboards should be stored
#' @param s3_path s3 path to modelling summary
#' @return `TRUE`
#' @export
#' @examples
#' {
#'   dashboard$generate_dashboards(
#'     "regional_dashboards",
#'     "s3://path/to/summary.csv.gz"
#'   )
#' }
#'
generate_dashboards <- function(output_directory, s3_path) {
  regions <- get_regions()

  purrr::map(
    regions,

    \(chosen_region) {
      output_path <- glue::glue(
        "{output_directory}/{snakecase::to_snake_case(chosen_region)}.html"
      )

      generate_one_dashboard(
        s3_path = s3_path,
        region = chosen_region,
        output_html = output_path
      )
    },

    .progress = list(
      total = length(regions),
      format = "Generating dashboards: {cli::pb_current} / {cli::pb_total} | eta: {cli::pb_eta}"
    )
  )

  cli::cli_alert("Dashboards generated. Stored in {.code {output_directory}}.")

  return(invisible(TRUE))
}


#' Generate quarto dashboard for one regional dashboard
#'
#' @param s3_path S3 path to modelling summary
#' @param region string of geographical region
#' @param input_qmd path to `.qmd` document to be rendered
#' @param output_html path to output `.html` - defaults to `NULL`.
#'
#' @return `output_html` invisibly
generate_one_dashboard <- function(
  s3_path,
  region,
  input_qmd = box::file("dashboard.qmd"),
  output_html = NULL
) {
  quarto::quarto_render(
    input = input_qmd,
    execute_params = list(
      s3_path = s3_path,
      region = region
    )
  )

  if (!is.null(output_html)) {
    # cannot specify file paths in `output_file` of quarto::quarto_render()
    # workaround is to move a _self contained_ document
    fs::dir_create(fs::path_dir(output_html))
    fs::file_move(stringr::str_replace(input_qmd, ".qmd", ".html"), output_html)
  }

  # don't want to return the dashboard but want to know where it is
  return(invisible(output_html))
}


#' Pull list of available nhs regions from Redshift `nhs_trust` lookup
#'
#' @return character vector
get_regions <- function() {
  rs <- redshift$data_model("REDACTED")

  regions <- rs$nhs_trusts |>
    dplyr::distinct(nhser23nm) |>
    dplyr::filter(!is.na(nhser23nm)) |>
    dplyr::pull(nhser23nm)

  regions
}
