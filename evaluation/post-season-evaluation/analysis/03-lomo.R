source("evaluation/post-season-evaluation/analysis/00-depends.R")

deps_$need(
  "dplyr",
  "ggplot2",
  "scoringutils",
  "purrr",
  "stats",
  "stringr",
  "tibble",
  "tidyr"
)


# Model importance ----
# first lets find the unique set of models used each week for a disease and metric

lomo_ensemble <- generate_lomo_models(
  scoring_ready,
  forecasting_unit
)

lomo_ensemble |>
  dplyr::filter(
    prediction_start_date == date,
    location_level == "nation"
  ) |>
  dplyr::summarise(
    n_models = dplyr::n_distinct(model),
    .by = c(prediction_start_date, disease)
  )


lomo_scored <- lomo_ensemble |>
  scoringutils::as_forecast_quantile(forecast_unit = forecasting_unit) |>
  scoringutils::score() |>
  # LOMO requires comparison against the "full" ensemble
  scoringutils::add_relative_skill(
    baseline = "ensemble_mean",
    by = c("disease", "prediction_start_date", "location_level")
  )




# LOMO uplift over time
lomo_data <- lomo_scored |>
  scoringutils::summarise_scores(by = c(
    "model", "disease",
    "prediction_start_date", "location_level"
  )) |>
  dplyr::mutate(
    model_removed = stringr::str_remove(model, "ensemble_lomo_"),
    location_level = factor(location_level, levels = c("nation", "region", "icb"))
  ) |>
  # we don't need to show the baseline
  dplyr::filter(model != "ensemble_mean") |>
  dplyr::mutate(wis_scaled_relative_skill = log2(wis_scaled_relative_skill))


lomo_data |>
  # add a break in the graph over the holiday period
  tibble::as_tibble() |>
  tidyr::nest(data = -c(model, disease, location_level, model_removed)) |>
  dplyr::mutate(
    data = purrr::map(
      data,
      \(input_df) dplyr::bind_rows(input_df, holiday_break()) |> dplyr::arrange(prediction_start_date)
    )
  ) |>
  tidyr::unnest(cols = data) |>
  ggplot2::ggplot() +
  geom_point(aes(
    x = prediction_start_date, y = wis_scaled_relative_skill,
    group = model_removed, colour = model_removed
  )) +
  geom_line(aes(
    x = prediction_start_date, y = wis_scaled_relative_skill,
    group = model_removed, colour = model_removed
  )) +
  geom_hline(aes(yintercept = 0), linetype = 2, alpha = 0.5) +
  facet_grid(location_level ~ disease, scales = "free_y") +
  labs(
    subtitle = "Models with log2(skill) < 0 improved the ensemble with their inclusion",
    y = "log2(relative wis)"
  )

# discretise the skill to improve/not, then estimate proportion of times it improved the score
# stratified by geography. The binomial CI help express our confidence as we have more
# data at finer spatial scales.
lomo_summary <- lomo_scored |>
  scoringutils::summarise_scores(by = c(
    "model", "disease",
    "prediction_start_date", "location_level", "location"
  )) |>
  dplyr::mutate(
    model = stringr::str_remove(model, "ensemble_lomo_"),
    location_level = factor(location_level, levels = c("nation", "region", "icb"))
  ) |>
  dplyr::filter(model != "ensemble_mean") |>
  dplyr::mutate(
    season_length = dplyr::n_distinct(prediction_start_date),
    .by = c("disease", "location_level")
  ) |>
  dplyr::summarise(
    prop_improve = mean(wis_relative_skill < 1),
    ci_5 = stats::binom.test(x = sum(wis_relative_skill < 1), dplyr::n(), conf.level = 0.9)$conf.int[[1]],
    ci_95 = stats::binom.test(x = sum(wis_relative_skill < 1), dplyr::n(), conf.level = 0.9)$conf.int[[2]],
    n = dplyr::n(),
    n_forecasts = dplyr::n_distinct(prediction_start_date),
    season_length = unique(season_length),
    .by = c("disease", "model", "location_level")
  ) |>
  dplyr::mutate(
    season_prop = n_forecasts / season_length, .by = c("disease", "location_level")
  ) |>
  dplyr::arrange(disease, model)


lomo_summary |>
  ggplot() +
  geom_point(aes(x = model, y = prop_improve, group = location_level, color = location_level),
    position = position_dodge(width = 0.5)
  ) +
  geom_linerange(aes(x = model, ymin = ci_5, ymax = ci_95, group = location_level, color = location_level),
    position = position_dodge(width = 0.5)
  ) +
  facet_grid(~disease, scale = "free_x") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  labs(
    y = "proportion of forecasts with score uplift",
    title = "Leave One Model Out comparison vs mean ensemble",
    subtitle = "Was performance improved by including this specific model"
  )
