#' @name plotting
#' @section Version: 0.0.1
#'
#' @title Functions for plotting nowcasts
#'
#' @description Helper functions for plotting nowcasts.
#'
#' @seealso
#' * [plotting$prediction_plot()]
#' * [plotting$pack_plot()]
".__module__."

box::use(box / deps_)

.on_load <- function(ns) {
  deps_$need(
    "dplyr",
    "ggdist",
    "ggnewscale",
    "ggplot2>=3.5.0",
    "glue",
    "grDevices",
    "lubridate",
    "PrettyCols",
    "RColorBrewer",
    "rlang",
    "scales",
    "stats",
    "stringr",
    "tibble",
    "tidyr"
  )
}


#' Construct a lookback plot for a nowcast with median and a 90$ prediction interval
#'
#' To keep the function general, axis labels are not specified
#' Add them yourself via `ggplot2::labs()` or `ggplot2::xlab()`, etc.
#' This function is intended for internal use, not production graphics
#'
#' @param lookback_data tibble which summarises nowcast predictions.  Must have columns `.q05`, `.q50`, `.q95`,
#' `.q50` and `date_column`
#' @param current_data tibble of known, reported cases.  Must have colummns `target` and `date_column`
#' @param date_column symbol representing date column for x axis
#' @return ggplot
#' @export
prediction_plot <- function(lookback_data, current_data, date_column) {
  colour_scheme <- RColorBrewer::brewer.pal(3, "Dark2")

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = lookback_data,
      mapping = ggplot2::aes(x = {{ date_column }}, ymin = .q05, ymax = .q95, fill = "Nowcast"),
      alpha = 0.4
    ) +
    ggplot2::geom_line(
      data = current_data,
      mapping = ggplot2::aes(x = {{ date_column }}, y = target, colour = "Confirmed cases")
    ) +
    ggplot2::geom_line(
      data = lookback_data,
      mapping = ggplot2::aes(x = {{ date_column }}, y = .q50, colour = "Nowcast")
    ) +
    # TODO: colours
    ggplot2::scale_colour_manual(
      values = c("Nowcast" = colour_scheme[1], "Confirmed cases" = colour_scheme[2]),
      name = "Lines"
    ) +
    ggplot2::scale_fill_manual(
      values = c("Nowcast" = colour_scheme[1]),
      labels = c("Nowcast" = "90% predictive interval"),
      name = "Intervals"
    )
}

#' Internal helper function. Preprocess model samples and daily case data for production ready graphics
#' @param model_samples data frame of posterior model samples. Must have columns `.value` and a date column shared with
#' `cases`
#' @param cases daily case counts.  Must have columns `.target` and a date column shared with `model_samples`
#' @param date_column the shared date column (e.g. `specimen_date`), specified as a symbol
#' @return tibble of nowcast prediction summaries; (1, 5, 50, 95, 99)% quantiles by `date_column`
process_for_production <- function(model_samples, cases, date_column) {
  by_name <- rlang::as_name(rlang::enquo(date_column))

  percents <- c(2.5, 25, 50, 75, 97.5)
  quants <- percents / 100

  model_samples |>
    dplyr::summarise(
      .value |>
        stats::quantile(probs = quants) |>
        as.list() |>
        tibble::as_tibble() |>
        dplyr::rename_with(
          \(str) {
            str_name <- str |>
              stringr::str_remove("%") |>
              stringr::str_pad(2, "left", "0")
            glue::glue(".q{str_name}")
          }
        ),
      .by = {{ date_column }}
    ) |>
    dplyr::left_join(
      cases,
      by = by_name
    ) |>
    dplyr::arrange({{ date_column }}) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("."), ~ .x + target
      )
    )
}


#' Construct nowcast graph for pack
#'
#' Inspired by https://github.com/kassteele/Nowcasting/blob/master/functions/plotNowcast.R
#'
#' @param model_samples data frame of posterior model samples. Must have columns `.value` and a date column shared with
#' `cases`
#' @param cases daily case counts.  Must have columns `.target` and a date column shared with `model_samples`
#' @param date_column the shared date column (e.g. `specimen_date`), specified as a symbol
#' @param graph_title string specifiying main graph title
#' @param xlab string specifying x axis label
#' @param ylab string specifying y axis label
#' @param median_linewidth linewidth for median prediction line, defaults to `1.3`
#' @return gg
#' @export
pack_plot <- function(model_samples,
                      cases,
                      date_column,
                      graph_title,
                      xlab,
                      ylab,
                      median_linewidth = 1.3) {
  date_name <- rlang::as_name(rlang::enquo(date_column))

  plot_data <- process_for_production(model_samples, cases, {{ date_column }})

  # a CVD friendly colour scheme
  scheme <- PrettyCols::prettycols("Sea", 4)

  subtitle_string <- glue::glue(
    "Data up until {max(plot_data[[date_name]]) + lubridate::days(1)}"
  )

  plot_data |>
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = {{ date_column }}, y = target, fill = "known"), width = 1) +
    ggplot2::scale_fill_manual(
      name = "",
      values = c("known" = scheme[1]),
      labels = c("known" = "Reported cases")
    ) +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_crossbar(
      ggplot2::aes(x = {{ date_column }}, ymin = .q2.5, ymax = .q97.5, y = .q50, fill = ".95"),
      fill = grDevices::adjustcolor(scheme[3], alpha = 0.5), colour = NA, width = 1,
    ) +
    ggplot2::geom_crossbar(
      ggplot2::aes(x = {{ date_column }}, ymin = .q25, ymax = .q75, y = .q50, fill = ".50"),
      fill = grDevices::adjustcolor(scheme[4], alpha = 0.5), colour = NA, width = 1,
    ) +
    ggplot2::geom_step(ggplot2::aes(x = {{ date_column }} + 0.5, y = .q50, colour = "median"),
      direction = "vh",
      linewidth = median_linewidth
    ) +
    ggplot2::geom_col(ggplot2::aes(x = {{ date_column }}, y = NA_real_, fill = ".95")) +
    ggplot2::geom_col(ggplot2::aes(x = {{ date_column }}, y = NA_real_, fill = ".50")) +
    ggplot2::scale_fill_manual(
      name = "Prediction intervals",
      values = c(
        ".95" = grDevices::adjustcolor(scheme[3], alpha = 0.5),
        ".50" = grDevices::adjustcolor(scheme[4], alpha = 0.8)
      ),
      labels = c(".95" = "95%", ".50" = "50%")
    ) +
    ggplot2::scale_colour_manual(
      name = "",
      values = c("median" = "black"),
      labels = c("median" = "Nowcast median prediction")
    ) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::ggtitle(
      graph_title,
      subtitle = subtitle_string
    ) +
    ggplot2::xlim(min(plot_data[[date_name]]), max(plot_data[[date_name]]) + lubridate::days(1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

#' Construct nowcast graph for pack, with a lookback
#'
#' Inspired by https://github.com/kassteele/Nowcasting/blob/master/functions/plotNowcast.R
#'
#' @param model_samples data frame of posterior model samples. Must have columns `.value` and a date column shared with
#' `cases`
#' @param cases daily case counts known today.
#' Must have columns `.target` and a date column shared with `model_samples`
#' @param cases_simulated daily case counts known at time of nowcast.
#' Must have columns `.target` and a date column shared with `model_samples`
#' @param date_column the shared date column (e.g. `specimen_date`), specified as a symbol
#' @param graph_title string specifiying main graph title
#' @param xlab string specifying x axis label
#' @param ylab string specifying y axis label
#' @param median_linewidth linewidth for median prediction line, defaults to `1.3`
#' @return gg
#' @export
pack_plot_lookback <- function(model_samples,
                               cases,
                               cases_simulated,
                               date_column,
                               graph_title,
                               xlab,
                               ylab,
                               median_linewidth = 1.3) {
  date_name <- rlang::as_name(rlang::enquo(date_column))

  # compare number cases today to those we knew at time of prediction
  case_comparison <- model_samples |>
    dplyr::left_join(
      cases,
      by = date_name
    ) |>
    dplyr::mutate(
      case_difference = target - target_value
    ) |>
    dplyr::select({{ date_column }}, target_value, case_difference) |>
    dplyr::distinct() |>
    tidyr::pivot_longer(
      cols = c("target_value", "case_difference"),
      names_to = "target_type",
      values_to = "number_cases"
    ) |>
    dplyr::mutate(target_type = as.factor(target_type))


  plot_data <- process_for_production(model_samples, cases_simulated, {{ date_column }})

  # a CVD friendly colour scheme
  scheme <- PrettyCols::prettycols("Sea", 5)

  subtitle_string <- glue::glue(
    "Data up until {max(plot_data[[date_name]]) + lubridate::days(1)}"
  )

  plot_data |>
    ggplot2::ggplot() +
    ggplot2::geom_col(
      data = case_comparison,
      mapping = ggplot2::aes(x = {{ date_column }}, y = number_cases, fill = target_type), width = 1
    ) +
    ggplot2::scale_fill_manual(
      name = "Cases",
      values = c("target_value" = scheme[1], "case_difference" = "red"),
      labels = c(
        "target_value" = "Reported cases at\ntime of nowcast",
        "case_difference" = "Updated case count"
      )
    ) +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_crossbar(
      ggplot2::aes(x = {{ date_column }}, ymin = .q2.5, ymax = .q97.5, y = .q50, fill = ".9"),
      fill = grDevices::adjustcolor(scheme[3], alpha = 0.35), colour = NA, width = 1,
    ) +
    ggplot2::geom_step(ggplot2::aes(x = {{ date_column }} + 0.5, y = .q97.5), colour = "grey", direction = "vh") +
    ggplot2::geom_step(ggplot2::aes(x = {{ date_column }} + 0.5, y = .q2.5), colour = "grey", direction = "vh") +
    ggplot2::geom_step(ggplot2::aes(x = {{ date_column }} + 0.5, y = .q50, colour = "median"),
      direction = "vh",
      linewidth = median_linewidth
    ) +
    ggplot2::geom_col(ggplot2::aes(x = {{ date_column }}, y = NA_real_, fill = ".95")) +
    ggplot2::scale_fill_manual(
      name = "",
      values = c(".95" = grDevices::adjustcolor(scheme[3], alpha = 0.5)),
      labels = c(".95" = "95% PI")
    ) +
    ggplot2::scale_colour_manual(
      name = "",
      values = c("median" = "black"),
      labels = c("median" = "Nowcast median prediction")
    ) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::ggtitle(
      graph_title,
      subtitle = subtitle_string
    ) +
    ggplot2::xlim(min(plot_data[[date_name]]), max(plot_data[[date_name]]) + lubridate::days(1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

#' Construct growth rate plot
#'
#'
#' @param model_samples data frame of posterior model samples. Must have columns `.value` and a date column shared with
#' `cases`
#' @param date_column the shared date column (e.g. `specimen_date`), specified as a symbol
#' @param quantiles chosen growth rate quantiles to plot. Defaults to `c(0.5, 0.9, 0.98)`.
#' @param graph_title string specifiying main graph title
#' @param xlab string specifying x axis label
#' @param ylab string specifying y axis label
#' @return gg
#' @export
growth_rate <- function(
    model_samples,
    date_column,
    quantiles = c(0.5, 0.9, 0.98),
    graph_title = "title",
    xlab = "x",
    ylab = "y") {
  date_name <- rlang::as_name(rlang::enquo(date_column))

  subtitle_string <- glue::glue(
    "Data up until {max(model_samples[[date_name]]) + lubridate::days(1)}"
  )

  model_samples |>
    dplyr::group_by(.sample) |>
    dplyr::arrange({{ date_column }}) |>
    dplyr::mutate(
      growth_rate = .value_lp - dplyr::lag(.value_lp)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by({{ date_column }}) |>
    ggdist::median_qi(growth_rate, .width = quantiles) |>
    ggplot2::ggplot(
      ggplot2::aes(x = {{ date_column }}, y = growth_rate, ymin = .lower, ymax = .upper, colour = "median")
    ) +
    ggdist::geom_lineribbon(alpha = 0.4) +
    ggplot2::geom_hline(
      ggplot2::aes(colour = "0%", yintercept = 0),
      linetype = 2
    ) +
    ggplot2::ggtitle(
      graph_title,
      subtitle = subtitle_string
    ) +
    ggplot2::labs(
      x = xlab,
      y = ylab
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    PrettyCols::scale_fill_pretty_d("Sea", legend_title = "PI levels") +
    ggplot2::scale_colour_manual(
      name = "Lines",
      values = c("median" = "black", "0%" = "black")
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(override.aes = list(colour = "black", alpha = 1, fill = "transparent"))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}
