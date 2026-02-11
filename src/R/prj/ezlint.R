#' @name ezlint
#' @section Version:  0.0.1
#'
#' @title
#' Easy linting
#'
#' @description
#' Helper functions to make linting files easier.
#'
#' @seealso [ezlint$lint_modified_files()]
#'
".__module__."

box::use(
  box / deps_,
  box / help_,
)

.on_load <- function(ns) {
  deps_$need(
    "dplyr",
    "gert",
    "glue",
    "lintr",
    "purrr",
    "rstudioapi",
    "tibble"
  )
}

#' Find all files which have been modified in current git branch
#' @param ref a reference such as "HEAD", or a commit id,
#' or NULL to the diff the working directory against the
#' repository index.
#' @return vector of strings of filepaths
find_modified_files <- function(ref = "main") {
  ref |>
    gert::git_diff() |>
    # grab just the added or modified files - don't care about deletes
    dplyr::filter(status %in% c("A", "M")) |>
    dplyr::pull(new)
}

#' Lint a collections of files
#' @param files vector of filepaths
#' @return tibble of lints
lint_many_files <- function(files = find_modified_files()) {
  files |>
    purrr::map(
      \(fname) {
        fname |>
          lintr::lint() |>
          tibble::as_tibble()
      }
    ) |>
    dplyr::bind_rows() |>
    dplyr::select(
      type,
      file = filename,
      line = line_number,
      column  = column_number,
      message
    )
}

#' Show lints in the RStudio markers pane
#' @param lints tibble of lints. Must have columns `type`, `file`, `line`, `column` and `message`
show_markers <- function(lints) {
  rstudioapi::sourceMarkers(
    name = "lints",
    markers = lints
  )
}

#' Lint all files which have been modified in the current branch
#' @param ref a reference such as "HEAD", or a commit id, or NULL to the diff the working directory against the
#' repository index.
#' @param fetch logical: should we pull origin/main? Defaults to `FALSE`
#' @export
lint_modified_files <- function(ref = "origin/main", fetch = FALSE) {
  if (isTRUE(fetch)) {
    gert::git_fetch()
  } else {
    cli::cli_warn(
      "Latest changes not fetched, since {.arg fetch = FALSE} - your local repository may be out of date!"
    )
  }
  ref |>
    find_modified_files() |>
    lint_many_files() |>
    show_markers()
}
