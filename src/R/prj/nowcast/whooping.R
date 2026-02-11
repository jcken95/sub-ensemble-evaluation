#' @name whooping
#' @section Version: 0.0.1
#'
#' @title Functions for working with Pertussis data
#'
#' @description Helper functions wranging Pertussis data
#'
#' @seealso
#' * [wrangling$pre_triangle()]
#' * [wrangling$construct_reporting_triangle()]
#' * [wrangling$tests_to_counts()]
#' * [wrangling$model_quantiles()]
".__module__."

box::use(box / deps_)

.on_load <- function(ns) {
  deps_$need(
    "dplyr",
    "glue",
    "here",
    "readr",
    "stats",
    "stringr",
    "tibble",
    "tidyr"
  )
}

#' Put test type into meaningful groups
#'
#' @param test_name name of test
#' @return string
#' @export
tidy_test_type <- function(test_name) {
  dplyr::case_when(
    stringr::str_detect(test_name, "ANTIBODY") ~ "Antibody",
    stringr::str_starts(test_name, "ANTIGEN") ~ "Antigen",
    stringr::str_starts(test_name, "CULTURE") ~ "Culture",
    stringr::str_detect(test_name, "PCR") ~ "PCR",
    .default = "Other / NA"
  )

}


# nolint start: line_length_linter
#' Allowed age brackets for Pertussis
#'
#' Brackets taken from \url{https://www.gov.uk/government/publications/pertussis-epidemiology-in-england-2024/confirmed-cases-of-pertussis-in-england-by-month}
#' Last accessed 2024-06-20
#'
#' @export
# nolint end
age_group <- function() {
  c("[0, 3) months", "[3, 6) months", "[6, 12) months", "[1, 5) years", "[5, 10) years", "15+ years")
}

#' Convert age in years and months to an age bracket
#' @param age_in_months age of patient in months (integer)
#' @param age_in_years age of patient in years (integer)
#' @return age bracket as character
#' @export

tidy_age_group <- function(age_in_months, age_in_years) {
  dplyr::case_when(
    dplyr::between(age_in_months, 0, 2) ~ "[0, 3) months",
    dplyr::between(age_in_months, 3, 5) ~  "[3, 6) months",
    dplyr::between(age_in_months, 6, 11) ~ "[6, 12) months",
    dplyr::between(age_in_years, 1, 4)  ~ "[1, 5) years",
    dplyr::between(age_in_years, 5, 9) ~ "[5, 10) years",
    dplyr::between(age_in_years, 10, 14) ~ "[10, 15) years",
    age_in_years >= 15 ~ "15+ years",
    .default = NA_character_
  )
}

#' Turn GAM output into a posterior summary and write output

#' @param predictions tibble/data frame of GAM output, must have `.value` column
#' @param final_cases tibble of updated cases (for lookbacks). Defaults to `NULL`
#' @param output_path string specifying where to write to, defaults to `here::here()`
#' @return invisibly return tibble of summarised data
#' @export
#'
write_narrative <- function(predictions, final_cases = NULL, output_path = here::here()) {

  final_date <-  max(predictions$specimen_date)
  final_prediction <- dplyr::filter(predictions, specimen_date == final_date)

  narratives <- final_prediction |>
    dplyr::summarise(
      .value |>
        stats::quantile(probs = c(0.025, 0.25, 0.5, 0.75, 0.975)) |>
        as.list() |>
        tibble::as_tibble()
    ) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits = 0))) |>
    dplyr::mutate(
      median = glue::glue("Median: {`50%`} cases"),
      pi_50 = glue::glue("50% PI: [{`25%`}, {`75%`}] cases"),
      pi_95 = glue::glue("95% PI: [{`2.5%`}, {`97.5%`}] cases"),
      .keep = "none"
    ) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "description", values_to = "narrative")

  if (!is.null(final_cases)) {
    final_cases <- dplyr::filter(final_cases, specimen_date == final_date)

    new_narratives <- tibble::tibble(
      description = "current cases",
      narrative = glue::glue("{final_cases$target} cases")
    )

    narratives <- dplyr::bind_rows(narratives, new_narratives)
  }

  readr::write_csv(narratives, output_path)

  return(invisible(narratives))
}
