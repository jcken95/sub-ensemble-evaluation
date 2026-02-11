## Analysis of forecast performance using GAMs to understand how different ensemble component effect the resulting WIS
## GAMs are fit with effects to identify different ensembles, and an averaging method is used to identify the effects
## of individual ensemble components on model performance

box::use(
  box / redshift,
  box / s3,
  prj / projection_plots[theme_pancasts]
)

source("evaluation/post-season-evaluation/analysis/00-depends.R")

deps_$need(
  "broom",
  "dplyr",
  "geomtextpath",
  "ggplot2",
  "glue",
  "gratia",
  "furrr",
  "future",
  "mgcv",
  "purrr",
  "s3fs",
  "stats",
  "stringr",
  "tibble",
  "tidyr"
)


ggplot2::theme_set(theme_pancasts())

scoring_s3_root <- "PATH REDACTED"

individual_models <- dplyr::tbl(
  redshift$connect(use_existing = FALSE),
  I("pancasts_glue.samples_eval2425")
) |>
  dplyr::distinct(model) |>
  dplyr::filter(model != "") |>
  dplyr::collect()

models_regex <- unique(individual_models$model) |>
  stringr::str_flatten(collapse = "|")

scores <- tibble::tibble(
  s3_path = s3fs::s3_dir_ls(scoring_s3_root)
) |>
  dplyr::filter(!stringr::str_detect(s3_path, "epinow2")) |>
  dplyr::filter(grepl("influenza", s3_path)) |>
  dplyr::mutate(data = purrr::map(s3_path, \(fpath) s3$read_using(fpath, fn = readRDS))) |>
  tidyr::unnest(data) |>
  dplyr::select(-s3_path) |>
  dplyr::filter(scale == "per_capita")

models_vector <- as.vector(individual_models)$model

scores_summarised <- scores |>
  dplyr::summarise(
    mean_wis = mean(wis),
    .by = c("model", "prediction_start_date", "location_level")
  ) |>
  dplyr::mutate(
    model_components = stringr::str_extract_all(model, models_regex)
  ) |>
  tidyr::unnest(model_components) |>
  dplyr::mutate(model_included = TRUE) |>
  tidyr::pivot_wider(
    names_from = "model_components",
    values_from = model_included
  ) |>
  dplyr::mutate(
    dplyr::across(dplyr::any_of(models_vector), \(.x) dplyr::coalesce(.x, FALSE))
  )

influenza_data <- scores_summarised |>
  dplyr::mutate(
    t_ = as.numeric(prediction_start_date),
    t_ = t_ - min(t_)
  ) |>
  dplyr::mutate(
    target = log(mean_wis),
    location_level = as.factor(location_level),
    dplyr::across(
      dplyr::where(is.logical),
      as.factor
    ),
    model = as.factor(model)
  )

## IDEA: use averaging to tease effects out of a simpler model

## here, we git using the ensemble (model) name as a factor smooth
## this means we have one fit per _ensemble_
## we also have a random effect for location level - WIS has similar shape across location levels, but different scale

## Tune gam

future::plan(future::multisession, workers = 10)

gam_tuning <- tidyr::expand_grid(
  k1 = 5:15,
  k2 = 5:15,
  k3 = 5:15
) |>
  dplyr::mutate(
    gam_formula = glue::glue(
      "target ~ s(t_, k = {k1}) + s(t_, by = model, bs = 'cr', k = {k2}) + s(model, bs = 're') + ",
      "s(t_, by = location_level, k = {k3}) + s(location_level, bs = 're')"
    ),

    gam = furrr::future_map(
      gam_formula,
      \(model_spec) mgcv::gam(stats::as.formula(model_spec), data = influenza_data),
      .progress = TRUE
    ),

    glance = purrr::map(gam, broom::glance)
  )

wis_gam <- gam_tuning |>
  tidyr::unnest(glance) |>
  dplyr::slice_min(BIC) |>
  dplyr::select(gam) |>
  # double pluck because `gam` is a list column
  purrr::pluck(1) |>
  purrr::pluck(1)

gratia::appraise(wis_gam)
# partial effect for model, is the deviation from the mean WIS for each ensemble
gratia::draw(wis_gam)


wis_gam_aug <- broom::augment(wis_gam)
wis_gam_aug

average_prediction <- dplyr::summarise(
  wis_gam_aug,
  .fitted = mean(.fitted),
  .by = c(t_, location_level)
)

wis_gam_aug |>
  ggplot2::ggplot(
    ggplot2::aes(x = t_, y = .resid)
  ) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~location_level)

average_prediction |>
  ggplot2::ggplot(ggplot2::aes(x = t_, y = .fitted)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~location_level, scale = "free_y")


ensemble_components <- influenza_data |>
  dplyr::select(model, dplyr::any_of(models_vector)) |>
  dplyr::distinct() |>
  dplyr::mutate(
    dplyr::across(-model, as.logical)
  )

model_averages <- wis_gam_aug |>
  dplyr::left_join(ensemble_components) |>
  dplyr::select(.fitted, t_, model, location_level, dplyr::any_of(models_vector)) |>
  tidyr::pivot_longer(
    cols = dplyr::any_of(models_vector),
    values_to = "model_in"
  ) |>
  dplyr::filter(model_in) |>
  dplyr::summarise(
    .fitted = mean(.fitted),
    .by = c(t_, location_level, name)
  )

model_effect <- model_averages |>
  dplyr::rename(model_average = .fitted) |>
  # shape of effect will be the same regardless of location level
  dplyr::filter(location_level == "nation") |>
  dplyr::left_join(average_prediction, by = c("t_", "location_level")) |>
  dplyr::mutate(effect = model_average - .fitted)

log_mean_wis <- scores |>
  dplyr::summarise(
    y = log(mean(wis)),
    .by = c(prediction_start_date, location_level)
  ) |>
  dplyr::mutate(
    t_ = as.numeric(prediction_start_date),
    t_ = t_ - min(t_)
  )

model_effect |>
  dplyr::left_join(
    log_mean_wis,
    by = c("t_", "location_level")
  ) |>
  dplyr::mutate(
    vjust = 1,
    hjust = 0.85
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(x = prediction_start_date, y = effect, colour = name)
  ) +
  ggplot2::geom_line() +
  geomtextpath::geom_textline(
    ggplot2::aes(x = prediction_start_date, y = effect, label = name, vjust = vjust, hjust = hjust),
  ) +
  ggplot2::ggtitle(
    "Average effect on WIS induced including\na model in an ensemble of size three",
  ) +
  ggplot2::labs(
    x = "Prediction start date",
    y = "Difference from mean WIS\n(negative implies improvement)"
  ) +
  ggplot2::theme(legend.position = "none")

## conclusions
## for (roughly) first have of season (up to ~day 60) ensemble performance is approx the same
## gam_cr is the worst performing model around the peak
## in later part of season, model performance is more distinct and we have
## gam_gp > gam_cr > historic_gr_meanian > ets > gam_rw

## NOTE: it looks like there is little variation in score up until about half way through the season
## if we plot the mean score (over each prediction start date), we see this is true in the raw data too

scores |>
  dplyr::summarise(
    y = log(mean(wis)),
    .by = c(prediction_start_date, model, location_level)
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(x = prediction_start_date, y = y, colour = model)
  ) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~location_level)

## uncertainty

posterior_samples <- intervals$generate_samples(influenza_data, wis_gam, .n_pi_samples = 1000) |>
  dplyr::mutate(.value = exp(.value))

## mean per location level - model agnostic prediction
mean_prediction <- dplyr::summarise(
  posterior_samples,
  .value = mean(.value),
  .by = c(t_, location_level, .sample)
)

## mean score, conditioned on model X being a member for the ensemble (and conditional on location level)
mean_model_prediction <- posterior_samples |>
  dplyr::summarise(
    .value = mean(.value),
    .by = c(model, t_, location_level, .sample)
  ) |>
  dplyr::left_join(ensemble_components, by = "model") |>
  dplyr::select(.sample, .value, t_, model, location_level, dplyr::any_of(models_vector)) |>
  tidyr::pivot_longer(
    cols = dplyr::any_of(models_vector),
    values_to = "model_in"
  ) |>
  dplyr::filter(model_in) |>
  dplyr::summarise(
    .value = mean(.value),
    .by = c(t_, location_level, name, .sample)
  )

## compute the deviation from the mean effect to extract model performance
mean_model_effect <- mean_model_prediction |>
  dplyr::rename(model_average = .value) |>
  # shape of effect will be the same regardless of location level
  dplyr::filter(location_level == "nation") |>
  dplyr::left_join(mean_prediction, by = c("t_", "location_level", ".sample")) |>
  dplyr::left_join(
    mean_prediction |>
      dplyr::rename("overall_mean" = .value),
    by = c("t_", "location_level", ".sample")
  ) |>
  dplyr::mutate(effect = 100 * (model_average - overall_mean) / overall_mean)


mean_model_effect |>
  dplyr::filter(location_level == "nation") |>
  dplyr::select(t_, effect, name) |>
  dplyr::group_by(name, t_) |>
  ggdist::median_qi(.width = 0.9) |>
  dplyr::left_join(
    dplyr::filter(log_mean_wis, location_level == "nation"),
    by = "t_"
  ) |>
  dplyr::mutate(
    vjust = 1,
    hjust = 0.85
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = prediction_start_date,
      y = effect,
      ymin = .lower,
      ymax = .upper,
      group = name,
      colour = name
    )
  ) +
  ggdist::geom_lineribbon() +
  geomtextpath::geom_textline(
    ggplot2::aes(x = prediction_start_date, y = effect, label = name, vjust = vjust, hjust = hjust),
  ) +
  ggplot2::scale_fill_brewer() +
  ggplot2::ggtitle(
    "Average effect on WIS induced including\na model in an ensemble of size three",
    "Median effect with 90% credible interval"
  ) +
  ggplot2::labs(
    x = "Prediction start date",
    y = "Percentage change from mean WIS\n(negative implies improvement)"
  ) +
  ggplot2::theme(legend.position = "none")
