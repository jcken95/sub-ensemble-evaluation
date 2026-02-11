source("evaluation/post-season-evaluation/analysis/00-depends.R")

deps_$need(
  "dplyr",
  "ggplot2",
  "scoringutils",
  "stringr",
  "tidyr"
)

# evaluate scores ----

samples <- dplyr::tbl(redshift$connect(use_existing = FALSE), I("REDACTED")) |>
  # baseline model was fit on a single day, so will retrospectively correct the model date
  # if the model was run in "real time" the model date would be the Wednesday of the week of the prediction start date
  # in addition, using SQL commands here; a {lubridate} approach isn't SQL friendly in this case
  dplyr::mutate(
    model_date = dplyr::if_else(
      model == "gam_dow", # baseline model
      as.Date(dplyr::sql("DATE_TRUNC('week', DATE(prediction_start_date) + '2 days'::interval)")),
      as.Date(model_date)
    )
  ) |>
  # chop off additional null predictions from gam_dow model
  dplyr::filter(date < prediction_start_date + lubridate::weeks(2))

summary <- dplyr::tbl(redshift$connect(use_existing = FALSE), I("REDACTED")) |>
  # applying same model_date fix as for samples for same reasons
  dplyr::mutate(
    model_date = dplyr::if_else(
      model == "gam_dow", # baseline model
      as.Date(dplyr::sql("DATE_TRUNC('week', DATE(prediction_start_date) + '2 days'::interval)")),
      as.Date(model_date)
    )
  ) |>
  dplyr::filter(
    date < prediction_start_date + lubridate::weeks(2),
    # NOTE: for now restrict analysis to no age stratification
    age_group_granularity == "none"
  ) |>
  identify_ensemble_inclusion()


observed <- redshift$data_model("REDACTED")$REDACTED
lookups <- redshift$data_model("REDACTED")

observed_by_geography <- load_evaluation_summary(summary, observed, lookups)

forecasting_unit <- c(
  "model",
  "prediction_start_date",
  "location",
  "location_level",
  "age_group",
  "age_group_granularity",
  "date",
  "disease",
  "metric",
  "model_date",
  "population"
)

scoring_ready <- summary |>
  dplyr::filter(
    date >= prediction_start_date,
    model_date > "2024-09-01"
  ) |>
  dplyr::mutate(disease = stringr::str_remove(disease, "-19")) |>
  dplyr::slice_max(prediction_start_date, by = c(model_date, disease, metric), na_rm = TRUE) |>
  dplyr::collect() |>
  dplyr::left_join(
    observed_by_geography,
    by = c("date", "location", "location_level", "metric", "disease", "age_group")
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("pi_"),
    values_to = "predicted",
    names_to = "quantile_level"
  ) |>
  dplyr::mutate(quantile_level = as.numeric(stringr::str_remove(quantile_level, "pi_")) / 100) |>
  dplyr::mutate(
    target_name = target_name |>
      stringr::str_replace("norovirus_norovirus_norovirus_cases|cases", "norovirus_cases"),
    # if production ensembles were used then they were all stacking ensembles this year
    model = dplyr::if_else(stringr::str_detect(model, "ensemble"), "ensemble_stack", model)
  ) |>
  dplyr::mutate(
    n_models = dplyr::n_distinct(model),
    # the reported model was the ensemble; if no ensemble, then we only had one model, therefore it was reported
    is_reported_model = grepl("ensemble", model) | n_models == 1,
    .by = c(model_date, disease, metric)
  ) |>
  # if gam_dow is only model, dont need as cannot be compared to anything
  dplyr::filter(!(n_models == 1 & model == "gam_dow")) |>
  dplyr::rename(observed = observed_target) |>
  tidyr::drop_na(observed) |>
  # generate the mean & median quantile ensembles
  add_quantile_ensembles(forecasting_unit = forecasting_unit)

forecasts_scored <- score_quantiles(scoring_ready, forecasting_unit) |>
  dplyr::mutate(
    n_models = dplyr::n_distinct(model),
    # the reported model was the ensemble; if no ensemble, then we only had one model, therefore it was reported
    is_reported_model = (grepl("ensemble_stack", model) | n_models == 1),
    .by = c(model_date, disease, metric)
  )

## WIS ----

# wis per model ----

forecasts_scored |>
  dplyr::filter(location_level == "nation", metric == "admissions", scale == "natural") |>
  scoringutils::summarise_scores(by = c("model", "disease", "metric")) |>
  ggplot2::ggplot(
    ggplot2::aes(x = model, y = log(wis), groups = model, fill = model)
  ) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(~disease, scales = "free_x") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))


score_over_season <- forecasts_scored |>
  dplyr::filter(location_level == "nation", metric == "admissions", scale == "natural") |>
  scoringutils::summarise_scores(by = c("model", "disease", "metric", "location"))

# WARNING: this plot is not a fair comparison as not all models were available for the entire season
# for example, RW only available for covid in the latter half of the season, when the epidemic was approximately stable
score_over_season |>
  ggplot2::ggplot(
    ggplot2::aes(x = model, y = log(wis), groups = model, fill = model)
  ) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(~disease, scales = "free") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

score_over_season_spatial <- forecasts_scored |>
  dplyr::filter(metric == "admissions", scale == "log_1p") |>
  scoringutils::summarise_scores(by = c("model", "disease", "metric", "location_level")) |>
  dplyr::mutate(location_level = factor(location_level, levels = c("nation", "region", "icb")))

score_over_season_spatial |>
  ggplot2::ggplot(
    ggplot2::aes(x = model, y = (wis), groups = model, fill = model)
  ) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(location_level ~ disease, scales = "free") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# wis over time ----
# (average per two week forecast)

## admissions ----

forecasts_scored |>
  dplyr::filter(location_level == "nation", metric == "admissions", scale == "natural") |>
  scoringutils::summarise_scores(by = c("model", "prediction_start_date", "disease", "metric", "is_reported_model")) |>
  plot_score_over_time(wis, log) +
  ggplot2::facet_grid(~disease)

forecasts_scored |>
  dplyr::filter(location_level %in% c("region", "nation"), metric == "admissions", scale == "natural") |>
  scoringutils::summarise_scores(
    by = c("model", "prediction_start_date", "disease", "metric", "location", "is_reported_model")
  ) |>
  order_regions() |>
  plot_score_over_time(wis, log) +
  ggplot2::facet_grid(disease ~ location, scales = "free_y")

# occupancy ----
forecasts_scored |>
  dplyr::filter(location_level == "nation", metric == "occupancy_rate", scale == "natural") |>
  scoringutils::summarise_scores(by = c("model", "prediction_start_date", "disease", "metric", "is_reported_model")) |>
  plot_score_over_time(wis, log) +
  ggplot2::facet_grid(~disease)

forecasts_scored |>
  dplyr::filter(location_level %in% c("region", "nation"), metric == "occupancy_rate", scale == "natural") |>
  scoringutils::summarise_scores(
    by = c("model", "prediction_start_date", "disease", "metric", "location", "is_reported_model")
  ) |>
  order_regions() |>
  plot_score_over_time(wis, log) +
  ggplot2::facet_grid(disease ~ location, scales = "free_y")

# cases (noro) ----

# likely won't be used, but included for completeness

forecasts_scored |>
  dplyr::filter(location_level == "nation", metric == "cases", scale == "natural") |>
  scoringutils::summarise_scores(by = c("model", "prediction_start_date", "disease", "metric", "is_reported_model")) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = prediction_start_date,
      y = wis
    )
  ) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::ggtitle("WIS for norovirus")

# evaluate on per capita scale ----

# wis per model ----

forecasts_scored |>
  dplyr::filter(location_level == "nation", metric == "admissions", scale == "per_capita") |>
  scoringutils::summarise_scores(by = c("model", "prediction_start_date", "disease", "metric", "is_reported_model")) |>
  plot_score_over_time(wis, log) +
  ggplot2::facet_grid(~disease)

forecasts_scored |>
  dplyr::filter(location_level %in% c("region", "nation"), metric == "admissions", scale == "per_capita") |>
  scoringutils::summarise_scores(
    by = c("model", "prediction_start_date", "disease", "metric", "location", "is_reported_model")
  ) |>
  order_regions() |>
  plot_score_over_time(wis, log) +
  ggplot2::facet_grid(disease ~ location, scales = "free_y")

# evaluate on log scale ----

# wis per model ----

forecasts_scored |>
  dplyr::filter(location_level == "nation", metric == "admissions", scale == "log_1p") |>
  scoringutils::summarise_scores(
    by = c("model", "prediction_start_date", "disease", "metric", "is_reported_model")
  ) |>
  plot_score_over_time(wis, log) +
  ggplot2::facet_grid(~disease)


forecasts_scored |>
  dplyr::filter(location_level %in% c("region", "nation"), metric == "admissions", scale == "log_1p") |>
  scoringutils::summarise_scores(
    by = c("model", "prediction_start_date", "disease", "metric", "location", "is_reported_model")
  ) |>
  order_regions() |>
  plot_score_over_time(wis, log) +
  ggplot2::facet_grid(disease ~ location, scales = "free_y")
