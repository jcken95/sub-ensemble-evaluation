# script to get the results for the interim data report

box::use(prj / projection_plots)

ggplot2::theme_set(projection_plots$theme_pancasts())

# choose scripts and order we want to run
scripts <- c(
  "00-depends",
  "01-load_data",
  "02-scoring_wis",
  "03-scoring_trend"
)

plot_end_date <- "2025-04-01"

source(here::here("evaluation", "helpers.R"))

for (file in here::here("evaluation", "post-season-evaluation", "analysis", paste0(scripts, ".R"))) {
  message(glue::glue("Running: {file}"))
  source(file)
}


# PLOT EPI CURVES ----
observed_subset <- observed_by_geography |>
  dplyr::filter(
    age_group == "all",
    location_level == "nation",
    location == "England",
    age_group == "all",
    date >= plot_start_date,
    date < plot_end_date
  )

observed_subset |>
  dplyr::mutate(metric = stringr::str_replace(metric, "_", " ")) |>
  ggplot() +
  geom_point(aes(x = date, y = observed_target), size = 0.5) +
  coord_cartesian(ylim = c(0, NA)) +
  facet_wrap(vars(disease, metric), scales = "free_y", ncol = 2) +
  labs(y = "observed target value")


# Models used over time ----
overall_models <- scoring_ready |>
  dplyr::mutate(target_name = glue::glue("{disease}_{metric}"), n_models_fct = factor(n_models)) |>
  dplyr::filter(
    location_level == "nation",
    age_group_granularity == "none",
    date == prediction_start_date,
    # if we keep in the primary model we still have how many
    # models were used
    isTRUE(is_reported_model),
    # one record per prediction
    quantile_level == 0.5
  ) |>
  dplyr::mutate(metric = stringr::str_replace(metric, "_", " "))

# plot of how many models we delivered over time
overall_models |>
  ggplot() +
  geom_point(aes(x = prediction_start_date, y = metric, size = n_models_fct, fill = n_models_fct), pch = 21) +
  facet_wrap(vars(disease), scales = "free_y", ncol = 1) +
  labs(x = "prediction start date", y = "target name") +
  scale_fill_brewer(direction = 1, palette = "Spectral") +
  guides(
    size = guide_legend(title = "number of models", nrow = 1),
    fill = guide_legend(title = "number of models", nrow = 1),
  )


# Plot forecasts -----
plot_ready <- summary |>
  dplyr::filter(
    location_level == "nation",
    age_group == "all",
    # TODO: better ensemble filter; early RSV models were single-model (tp|gp)
    # noro is also a single-model approach (nowcast)
    grepl("ensemble|tp|gp|nowcast", model),
    (date >= prediction_start_date) | (date >= prediction_start_date - lubridate::days(7) & disease == "norovirus"),
    model_date > "2024-09-01"
  ) |>
  dplyr::slice_max(prediction_start_date, by = c(model_date, disease, metric), na_rm = TRUE) |>
  dplyr::mutate(disease = stringr::str_remove(disease, "-19")) |>
  dplyr::collect() |>
  # grab single-model "ensembles" and ensembles - hard to do with SQL :(
  dplyr::mutate(
    n_models = dplyr::n_distinct(model),
    # the reported model was the ensemble; if no ensemble, then we only had one model, therefore it was reported
    is_reported_model = (grepl("ensemble", model) | n_models == 1),
    .by = c(model_date, disease, metric)
  ) |>
  dplyr::filter(is_reported_model) |>
  dplyr::right_join(
    observed_subset,
    by = c("date", "location", "location_level", "metric", "disease", "age_group")
  )


# show only first week
plot_ready |>
  dplyr::mutate(metric = stringr::str_replace(metric, "_", " ")) |>
  # remove anything not in the second week
  dplyr::mutate(dplyr::across(dplyr::starts_with("pi_"), \(x) {
    dplyr::if_else(
      date < prediction_start_date + 7 &
        date >= prediction_start_date,
      x,
      NA
    )
  })) |>
  plot_report_forecasts(title_str = "1 to 7 day horizon")

# show only second week
plot_ready |>
  dplyr::mutate(metric = stringr::str_replace(metric, "_", " ")) |>
  # remove anything not in the second week
  dplyr::mutate(dplyr::across(dplyr::starts_with("pi_"), \(x) {
    dplyr::if_else(date >= prediction_start_date + 7, x, NA)
  })) |>
  plot_report_forecasts(title_str = "8 to 14 day horizon")


# show only zero-th week
plot_ready |>
  dplyr::filter(disease == "norovirus") |>
  # remove anything not in the zeo-th week
  dplyr::mutate(dplyr::across(dplyr::starts_with("pi_"), \(x) dplyr::if_else(date < prediction_start_date, x, NA))) |>
  plot_report_forecasts(title_str = "-6 to 0 day horizon")


# scoring summary stats

key_columns <- c(
  "disease",
  "metric",
  "location_level",
  "horizon_week",
  "interval_coverage_90"
)


ordinal_addition <- ordinal_scored |>
  dplyr::filter(is_reported_model, metric != "occupancy_rate", age_group == "all") |>
  scoringutils::summarise_scores(
    by = c("location_level", "disease", "metric")
  )

# create baseline ordinal score..
ordinal_baseline_score <- ordinal_ready |>
  dplyr::mutate(predicted = 1 / 3) |>
  scoringutils::score() |>
  dplyr::filter(is_reported_model, metric != "occupancy_rate", age_group == "all") |>
  scoringutils::summarise_scores(
    by = c("location_level", "disease", "metric")
  ) |>
  dplyr::rename(log_score_baseline = log_score, rps_baseline = rps)

ordinal_results <- ordinal_addition |>
  dplyr::left_join(ordinal_baseline_score, by = c("location_level", "disease", "metric")) |>
  dplyr::mutate(
    rps_uplift = paste0(100 * round((rps_baseline - rps) / rps_baseline, 3), "%"),
    log_score_uplift = 100 * round((log_score_baseline - log_score) / log_score_baseline, 3)
  ) |>
  dplyr::select(c("location_level", "disease", "metric", "rps_uplift"))

forecasts_scored |>
  dplyr::filter(is_reported_model, metric != "occupancy_rate") |>
  dplyr::mutate(
    horizon_week = dplyr::if_else(
      date < prediction_start_date + 7 &
        date >= prediction_start_date,
      1,
      2
    )
  ) |>
  scoringutils::summarise_scores(
    by = c("location_level", "disease", "metric", "horizon_week", "age_group_granularity")
  ) |>
  dplyr::select(key_columns) |>
  dplyr::mutate(
    disease = factor(disease, levels = c("covid", "influenza", "rsv", "norovirus")),
    location_level = factor(location_level, levels = c("nation", "region", "icb")),
    horizon_week = paste("90% coverage - week", horizon_week),
    interval_coverage_90 = paste0(100 * round(interval_coverage_90, 3), "%")
  ) |>
  dplyr::arrange(disease, metric, location_level, horizon_week) |>
  tidyr::pivot_wider(names_from = horizon_week, values_from = interval_coverage_90) |>
  dplyr::left_join(ordinal_results, by = c("disease", "metric", "location_level")) |>
  dplyr::rename(geography = location_level, `trend direction score` = rps_uplift) |>
  gt::gt()
