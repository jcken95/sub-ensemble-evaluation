#' @name helpers
#' @section Version: 0.0.1
#'
#' @title
#' Winter dashboard helpers
#'
#' @description
#' Helper functions for generating winter dashboard content
#'
#'
#'
#' @seealso
#' * [helpers$plot_projection()]
#' * [helpers$tabulate_projection()]
#' * [helpers$clean_icb_names()]
".__module__."

box::use(
  box / deps_,
)

.on_load <- function(ns) {
  deps_$need(
    "dplyr",
    "ggplot2",
    "glue",
    "gt",
    "PrettyCols",
    "stringr",
    "utils"
  )
}

#' Plot a forecast
#'
#' @param input_data data frame of data to be plotted. Must have columns `location`, `date`, `pi_5`, `pi_50`, `pi_95`
#' @export
#' @return object of class `gg`
plot_projection <- function(input_data) {

  scheme <- PrettyCols::prettycols("Fun", 2)

  loc <- unique(input_data$location)

  input_data |>
    ggplot2::ggplot(ggplot2::aes(x = date)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = pi_5, ymax = pi_95, fill = "90% PI"), alpha = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y = pi_50, colour = "Median prediction")) +
    ggplot2::geom_line(ggplot2::aes(y = target_value, colour = "Observed admissions")) +
    ggplot2::labs(
      title = stringr::str_wrap(
        glue::glue("14 day projection for {clean_icb_names(loc)} COVID-19 hospital bed admissions"),
        80
      ),
      x = "Date",
      y = "Number COVID-19 Admissions"
    ) +
    ggplot2::scale_colour_manual(
      name = "",
      values = c("Median prediction" = scheme[2], "Observed admissions" = scheme[1])
    ) +
    ggplot2::scale_fill_manual(
      name  = "",
      values = c("90% PI" = scheme[2])
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}


#' Turn a forecast into a `gt()` table
#' @param input_datadata frame of data to be tabulated. Must have columns `location`, `date`, `pi_5`, `pi_50`, `pi_95`
#' @param number_of_days number of days to forecast into the future
#' @return object of class `gt_tbl`
#' @export
tabulate_projection <- function(input_data, number_of_days = 14) {
  input_data |>
    dplyr::summarise(
      median = unique(pi_50),
      lwr = unique(pi_5),
      upr = unique(pi_95),
      .by = date
    ) |>
    dplyr::arrange(dplyr::desc(date)) |>
    dplyr::mutate(
      lwr = round(lwr),
      upr = round(upr),
    ) |>
    dplyr::mutate(
      Date = date,
      Median = round(median),
      `90% PI` = glue::glue("[{lwr}, {upr}]"),
      .keep = "unused"
    ) |>
    utils::head(number_of_days) |>
    gt::gt()
}

#' Helper to format ICB names
#' @param icb name of an ICB
#' @return string
#' @export
clean_icb_names <- function(icb) {
  icb |>
    stringr::str_remove_all("Integrated Care Board") |>
    stringr::str_remove_all("NHS\\s") |>
    stringr::str_trim()
}
