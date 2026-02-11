#' @name data_debug
#' @section Version: 0.0.1
#'
#' @title
#' Helper functions for debugging data problems.
#'
#' @description
#' Module provides helper functions for debugging data problems.
".__module__."

#' Find duplicated columns in a data frame and return the column names
#'
#' @param input_date data frame with unique column names
#' @return tibble with columns `col_name` and `dupe`
#' @examples
#' tibble::tibble(
#'   x = 1:3,
#'   y = 3:1
#' ) |>
#'   dplyr::mutate(z = x) |>
#'   find_duplicate_columns()
#'
#' @export
find_duplicate_columns <- function(input_data) {

  cols <- colnames(input_data)

  dupes <-  t(utils::combn(cols, 2)) |>
    tibble::as_tibble(.name_repair = \(...) c("first", "second")) |>
    purrr::pmap(
      \(first, second) {
        tibble::tibble(
          col = first,
          dupe = second,
          is_identical = all_equal(input_data[[first]], input_data[[second]])
        )
      }
    ) |>
    purrr::list_rbind() |>
    dplyr::filter(is_identical) |>
    dplyr::select(!is_identical)

  dupes
}


#' Helper to check if `x == y` allowing for `NA`s. Roughly equivalent to identical()
#' @param x vector
#' @param y vector
#' @return logical
#' @examples
#'
#' all_equal(c("apple", NA), c("apple", "banana"))
#' all_equal(c("apple", "banana"), c("apple", "banana"))
#'
#' # Known behaviour - different length vectors with `NULL`s provide unusual outputs
#' # Should not be a problem as intended use is columns of a data frame
#' all_equal(c(NULL, NULL), c("apple", "banana", "cherry", "damson"))
#' # Fine for same length
#' all_equal(c(NULL, NULL), c("apple", "banana"))
all_equal <- function(x, y) {
  all(
    (
      (x == y) &
        !(
          (is.na(x) & !is.na(y)) |
            (is.na(y) & !is.na(x))
        )
    ) |
      (is.na(x) & is.na(y))
  )
}
