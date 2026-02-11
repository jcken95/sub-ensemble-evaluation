source("evaluation/post-season-evaluation/analysis/00-depends.R")

deps_$need(
  "dplyr",
  "forcats",
  "ggplot2",
  "lubridate",
  "purrr",
  "scoringutils",
  "stringr",
  "tibble",
  "tidyr",
  "utils",
  "zoo"
)

# scoring and analysis of trend direction --------

probability_colours <- c(
  "decrease" = projection_plots$select_ukhsa_colour("dark blue"),
  "stable" = projection_plots$select_ukhsa_colour("orange"),
  "increase" = projection_plots$select_ukhsa_colour("dark pink")
)

plot_start_date <- "2024-10-01"

# grab all single-model / ensemble data

admissions_summary <- scoring_ready |>
  dplyr::filter(
    metric == "admissions",
    location_level == "nation",
    age_group == "all",
    grepl("ensemble|tp|gp|nowcast", model),
    (date >= prediction_start_date),
    model_date > "2024-09-01"
  ) |>
  dplyr::slice_max(prediction_start_date, by = c(model_date, disease, metric), na_rm = TRUE) |>
  dplyr::mutate(disease = stringr::str_remove(disease, "-19")) |>
  dplyr::collect() |>
  # grab single-model "ensembles" and ensembles - hard to do with SQL :(
  dplyr::mutate(n_models = dplyr::n_distinct(model), .by = c(model_date, disease, metric)) |>
  dplyr::filter(grepl("ensemble", model) | n_models == 1) |>
  dplyr::right_join(
    observed_by_geography |>
      dplyr::filter(
        metric == "admissions",
        age_group == "all",
        location_level == "nation",
        location == "England",
        age_group == "all",
        date >= plot_start_date
      ),
    by = c("date", "location", "location_level", "metric", "disease", "age_group")
  )

### modal trend prediction over the season ----

trend_probs <- admissions_summary |>
  dplyr::filter(model == "ensemble_stack") |>
  tidyr::pivot_wider(
    names_from = quantile_level,
    values_from = predicted,
    names_glue = "pi_{100 * quantile_level}"
  ) |>
  dplyr::group_by(prediction_start_date, disease) |>
  dplyr::slice_max(date) |>
  most_likely_trend() |>
  dplyr::ungroup() |>
  dplyr::select(
    disease, prediction_start_date, trend_probability, trend
  ) |>
  dplyr::distinct()

### data to plot incidence; even if no forecast

prediction_dates <- admissions_summary |>
  dplyr::filter(!is.na(prediction_start_date)) |>
  dplyr::pull(date)

observed_admissions <- admissions_summary |>
  dplyr::filter(
    date >= min(prediction_dates),
    date <= max(prediction_dates)
  )

### make plot

admissions_summary |>
  dplyr::filter(model == "ensemble_stack") |>
  tidyr::pivot_wider(
    names_from = quantile_level,
    values_from = predicted,
    names_glue = "pi_{100 * quantile_level}"
  ) |>
  dplyr::left_join(trend_probs, by = c("disease", "prediction_start_date")) |>
  ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      x = date,
      ymin = pi_5,
      ymax = pi_95,
      group = prediction_start_date,
      fill = trend,
      alpha = trend_probability
    ),
  ) +
  ggplot2::geom_line(
    ggplot2::aes(x = date, y = pi_50, group = prediction_start_date)
  ) +
  ggplot2::geom_point(
    data = observed_admissions,
    ggplot2::aes(x = date, y = observed_target),
    size = 0.3
  ) +
  ggplot2::scale_fill_manual(values = probability_colours) +
  ggplot2::facet_wrap(~disease, scale = "free_y", ncol = 1)


### area chart ----

empty_forecasts <- holiday_break() |>
  tidyr::expand_grid(
    disease = c("rsv", "covid", "influenza"),
    trend_direction = ordered(c("increase", "stable", "decrease"), levels = c("decrease", "stable", "increase"))
  ) |>
  dplyr::mutate(trend_probability = 0)

last_forecast_2024 <- observed_admissions |>
  dplyr::filter(prediction_start_date <= "2025-01-01") |>
  dplyr::slice_max(prediction_start_date, by = "disease") |>
  dplyr::distinct(disease, prediction_start_date)

first_forecast_2024 <- observed_admissions |>
  dplyr::slice_min(prediction_start_date, by = "disease") |>
  dplyr::distinct(disease, prediction_start_date)

holiday_period_admissions <- dplyr::bind_rows(
  dplyr::mutate(last_forecast_2024, prediction_start_date = prediction_start_date + lubridate::days(7)),
  dplyr::mutate(last_forecast_2024, prediction_start_date = prediction_start_date + lubridate::days(14))
) |>
  tibble::add_row(disease = "covid", prediction_start_date = lubridate::ymd("2024-10-14")) |>
  dplyr::left_join(
    observed_by_geography |>
      dplyr::filter(location_level == "nation", age_group == "all", metric == "admissions") |>
      dplyr::select(disease, date, observed_target),
    by = c("disease", "prediction_start_date" = "date")
  )


normalised_admissions <- observed_admissions |>
  dplyr::slice_max(date, by = c("disease", "prediction_start_date")) |>
  dplyr::select(disease, prediction_start_date, observed_target) |>
  dplyr::distinct() |>
  dplyr::bind_rows(holiday_period_admissions) |>
  dplyr::mutate(
    observed_target = (observed_target - min(observed_target)) / (max(observed_target) - min(observed_target)),
    .by = disease
  )


normalised_admissions <- observed_admissions |>
  tidyr::drop_na(prediction_start_date) |>
  dplyr::slice_max(date, by = c("disease", "prediction_start_date")) |>
  dplyr::select(disease, prediction_start_date, observed_target) |>
  dplyr::distinct() |>
  dplyr::bind_rows(holiday_period_admissions) |>
  dplyr::select(disease, prediction_start_date) |>
  dplyr::mutate(
    all_data = purrr::map2(
      disease, prediction_start_date,
      \(.disease, .psd) {
        observed_by_geography |>
          dplyr::filter(
            disease == .disease,
            metric == "admissions",
            date <= .psd,
            date > .psd - lubridate::days(14),
            age_group == "all",
            location_level == "nation"
          ) |>
          dplyr::arrange(date) |>
          utils::tail(7)
      }
    ),
    rolling_avg = purrr::map(all_data, \(zz) mean(zz$observed_target))
  ) |>
  tidyr::unnest(rolling_avg) |>
  dplyr::mutate(
    rolling_avg_normal = (rolling_avg - min(rolling_avg)) / (max(rolling_avg) - min(rolling_avg)),
    .by = disease
  )


admissions_summary |>
  dplyr::filter(is_reported_model) |>
  tidyr::pivot_wider(
    names_from = quantile_level,
    values_from = predicted,
    names_glue = "pi_{100 * quantile_level}"
  ) |>
  dplyr::group_by(disease, prediction_start_date) |>
  dplyr::slice_max(date) |>
  dplyr::ungroup() |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("p_"),
    names_to = "trend_direction",
    values_to = "trend_probability"
  ) |>
  dplyr::mutate(
    trend_direction = stringr::str_remove(trend_direction, "p_") |>
      ordered(levels = c("decrease", "stable", "increase"))
  ) |>
  dplyr::select(trend_direction, trend_probability, prediction_start_date, observed_target, disease) |>
  dplyr::bind_rows(empty_forecasts) |>
  dplyr::arrange(prediction_start_date) |>
  dplyr::mutate(trend_direction = forcats::fct_rev(trend_direction)) |>
  ggplot2::ggplot() +
  ggplot2::geom_area(
    ggplot2::aes(fill = trend_direction, y = trend_probability, x = prediction_start_date),
    position = "fill"
  ) +
  ggplot2::geom_line(
    data = normalised_admissions,
    ggplot2::aes(x = prediction_start_date, y = rolling_avg_normal, colour = "incidence")
  ) +
  ggplot2::geom_point(
    data = normalised_admissions,
    ggplot2::aes(x = prediction_start_date, y = rolling_avg_normal)
  ) +
  ggplot2::scale_fill_manual(values = probability_colours) +
  ggplot2::scale_colour_manual(values = c("incidence" = "black")) +
  ggplot2::facet_wrap(~disease, ncol = 1)

## score ordinal forecasts ----

# the forecasts don't have the value from 14-days ago easily stored
# easier to grab them here, then join on later down the line
lagged_values <- observed_by_geography |>
  dplyr::group_by(location, location_level, age_group, disease, metric) |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    rolling_average = zoo::rollmean(observed_target, k = 7, align = "right", na.pad = TRUE),
    observed_lag = dplyr::lag(observed_target, n = 14),
    rolling_average_lagged = zoo::rollmean(observed_lag, k = 7, align = "right", na.pad = TRUE),
    observed_trend = classify_trend(rolling_average, rolling_average_lagged)
  ) |>
  dplyr::ungroup()

# restructure data for ordinal forecasts; we need a long structure with the categories in one column (predicted)
# and their corresponding probability in another (predicted_probability)

ordinal_ready <- scoring_ready |>
  dplyr::select(-c(quantile_level, predicted)) |>
  dplyr::distinct() |>
  dplyr::group_by(
    model, prediction_start_date, location,
    location_level, age_group, age_group_granularity,
    disease, metric, model_date, population
  ) |>
  dplyr::slice_max(date) |>
  dplyr::left_join(
    lagged_values,
    by = dplyr::join_by(date, location, location_level, age_group, disease, metric)
  ) |>
  dplyr::mutate(observed_trend = classify_trend(observed, observed_lag)) |>
  dplyr::slice_max(date) |>
  # is a row-wise operation
  most_likely_trend() |>
  dplyr::ungroup() |>
  dplyr::select(-c(observed, target_value)) |>
  tidyr::pivot_longer(
    dplyr::starts_with("p_"),
    names_to = "raw_trend_category",
    values_to = "raw_trend_probability"
  ) |>
  dplyr::mutate(raw_trend_category = stringr::str_remove(raw_trend_category, "p_")) |>
  dplyr::rename(
    "observed" = observed_trend,
    "predicted" = raw_trend_probability,
    "predicted_label" = raw_trend_category
  ) |>
  dplyr::distinct() |>
  dplyr::mutate(
    observed = ordered(observed, levels = c("decrease", "stable", "increase")),
    predicted_label = ordered(predicted_label, levels = c("decrease", "stable", "increase"))
  ) |>
  # renormalise probabilities to 2 dimensional simplex to shake off some floating point type errors
  # see issue #996 on {scoringutils} https://github.com/epiforecasts/scoringutils/issues/996
  dplyr::mutate(predicted = predicted / sum(predicted), .by = dplyr::all_of(forecasting_unit)) |>
  scoringutils::as_forecast_ordinal(forecast_unit = c(forecasting_unit, "is_reported_model"))

ordinal_scored <- ordinal_ready |>
  scoringutils::score()

## plotting of ordinal scores ----

### admissions ----

ordinal_scored |>
  dplyr::filter(location_level == "nation", metric == "admissions") |>
  # graph is cluttered, so will just compare ensemble / baseline
  dplyr::filter(model == "gam_dow" | is_reported_model) |>
  scoringutils::summarise_scores(by = c("model", "prediction_start_date", "disease", "metric", "is_reported_model")) |>
  plot_score_over_time(rps) +
  ggplot2::facet_grid(~disease)

ordinal_scored |>
  dplyr::filter(location_level == "region", metric == "admissions") |>
  # graph is cluttered, so will just compare ensemble / baseline
  dplyr::filter(model == "gam_dow" | is_reported_model) |>
  scoringutils::summarise_scores(
    by = c("model", "prediction_start_date", "disease", "metric", "location", "is_reported_model")
  ) |>
  plot_score_over_time(rps) +
  ggplot2::facet_grid(location ~ disease)

ordinal_scored |>
  dplyr::filter(metric == "admissions") |>
  # graph is cluttered, so will just compare ensemble / baseline
  dplyr::filter(model == "gam_dow" | is_reported_model) |>
  dplyr::mutate(location_level = ordered(location_level, levels = c("nation", "region", "icb"))) |>
  scoringutils::summarise_scores(
    by = c("model", "prediction_start_date", "disease", "metric", "location_level", "is_reported_model")
  ) |>
  plot_score_over_time(rps) +
  ggplot2::facet_grid(location_level ~ disease)
