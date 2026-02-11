#' @name regions
#' @section Version: 0.0.1
#'
#' @title Automated regional quality reports creation
#'
#' @description Functions and documents to create regional quality reports
#'
".__module__."

box::use(
  box / deps_,
)

.on_load <- function(ns) {
  deps_$need(
    "fs",
    "here"
  )
}


#' Move quarto report generated in targets pipelines to a more useful place
#' @param disease string specifying disease pipeline; see `user_check$disease_checker()` for allowed values
#' @param output_directory string specifying directory to move report (`.html` file) to
move_report <- function(disease, output_directory) {

  box::use(prj / user_check[disease_checker])

  disease <- disease_checker(disease)

  html_name <- paste0(disease, "_", "regions_report.html")

  fs::file_move(
    here::here("src/R/prj/regions/quarto/regions_report.html"),
    here::here(output_directory, html_name)
  )

  return(invisible(TRUE))

}
