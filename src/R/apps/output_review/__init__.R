#' @name output_review
#' @section Version: 0.0.1
#'
#' @title Pancasting Output Review Tool
#'
#' @description
#' To assist with reviewing pipeline outputs.
#'
#' @seealso
#' * [output_review$run_app()]
".__module__."

box::use(
  box / deps_,
  box / help_
)


.on_load <- function(ns) {
  deps_$need(
    "bslib",
    "fs",
    "glue",
    "jsonlite",
    "purrr",
    "s3fs",
    "shiny",
    "shinyjs",
    "tools",
    "utils",
    "withr"
  )
}


box::use(
  . / ui[ui],
  . / server[server]
)

#' @export
run_app <- function() shiny::runApp(list(ui = ui, server = server))
