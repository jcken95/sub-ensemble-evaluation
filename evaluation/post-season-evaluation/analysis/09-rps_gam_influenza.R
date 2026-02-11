## Analysis of forecast performance using GAMs to understand how different ensemble component effect the resulting rps
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
  dplyr::select(-s3_path)

models_vector <- as.vector(individual_models)$model

zero_offset <- 1e-10 # to avoid log(0) in any gam pre-processing

scores_summarised <- scores |>
  dplyr::summarise(
    mean_rps = mean(rps),
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
    target = log(mean_rps + zero_offset),
    location_level = as.factor(location_level),
    dplyr::across(
      dplyr::where(is.logical),
      as.factor
    ),
    model = as.factor(model)
  )

future::plan(future::multisession, workers = 10)

gam_tuning <- tidyr::expand_grid(
  k1 = 10:20,
  k2 = 3:10,
  k3 = 3:10
) |>
  dplyr::mutate(
    gam_formula = glue::glue(
      "target ~ s(t_, k = {k1}) + s(t_, by = model, k = {k2}) + s(model, bs = 're') +",
      "s(t_, by = location_level, k = {k3}) + s(location_level, bs = 're')"
    ),

    gam = furrr::future_map(
      gam_formula,
      \(model_spec) try(mgcv::gam(stats::as.formula(model_spec), data = influenza_data)),
      .progress = TRUE
    )
  ) |>
  dplyr::filter(
    # wrapping in any() to deal with variable-length class description
    !any(is(gam) == "try_error")
  ) |>
  dplyr::mutate(
    glance = purrr::map(gam, broom::glance)
  )

rps_gam <- gam_tuning |>
  tidyr::unnest(glance) |>
  dplyr::slice_min(BIC) |>
  dplyr::select(gam) |>
  # double pluck because `gam` is a list column
  purrr::pluck(1) |>
  purrr::pluck(1)
#

gratia::appraise(rps_gam)
# partial effect for model, is the deviation from the mean rps for each ensemble
gratia::draw(rps_gam)


rps_gam_aug <- broom::augment(rps_gam)
rps_gam_aug

average_prediction <- dplyr::summarise(
  rps_gam_aug,
  .fitted = mean(.fitted),
  .by = c(t_, location_level)
)

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


model_averages <- rps_gam_aug |>
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

log_mean_rps <- scores |>
  dplyr::summarise(
    y = log(mean(rps)),
    .by = c(prediction_start_date, location_level)
  ) |>
  dplyr::mutate(
    t_ = as.numeric(prediction_start_date),
    t_ = t_ - min(t_)
  )

model_effect <- model_averages |>
  dplyr::rename(model_average = .fitted) |>
  # shape of effect will be the same regardless of location level
  dplyr::filter(location_level == "nation") |>
  dplyr::left_join(average_prediction, by = c("t_", "location_level")) |>
  dplyr::mutate(effect = model_average - .fitted)


model_effect |>
  dplyr::left_join(
    log_mean_rps,
    by = c("t_", "location_level")
  ) |>
  dplyr::mutate(
    vjust = dplyr::if_else(name == "ets", 1, 1),
    hjust = dplyr::if_else(name == "ets", 0.2, 0)
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(x = prediction_start_date, y = effect, colour = name)
  ) +
  ggplot2::geom_line() +
  geomtextpath::geom_textline(
    ggplot2::aes(x = prediction_start_date, y = effect, label = name, vjust = vjust, hjust = hjust),
  ) +
  ggplot2::ggtitle(
    "Average effect on rps induced including\na model in an ensemble of size three",
    subtitle = "Note: ETS and gr_mean are layed on top of each other"
  ) +
  ggplot2::labs(
    x = "Prediction start date",
    y = "Difference from mean rps\n(negative implies improvement)"
  ) +
  ggplot2::theme(legend.position = "none")

## conclusions
## order of model performance is (remember smaller rps => better performance)
## gam_gp > gam_cr > gr_gp > gam_rw > gr_mean ~ ets
## models never overlap, for any time point
## ets and gr_mean have numerically similar, but non-identical scores

## uncertainty

posterior_samples <- intervals$generate_samples(influenza_data, rps_gam, .n_pi_samples = 5000, mvn_method = "mgcv") |>
  dplyr::mutate(.value = exp(.value) - zero_offset)

# mean per location level - model agnostic prediction
mean_prediction <- dplyr::summarise(
  posterior_samples,
  .value = mean(.value),
  .by = c(t_, location_level, .sample)
)
# mean effect (as samples!)
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

mean_model_effect <- mean_model_prediction |>
  dplyr::rename(model_average = .value) |>
  # shape of effect will be the same regardless of location level
  dplyr::left_join(
    mean_prediction |>
      dplyr::rename("overall_mean" = .value),
    by = c("t_", "location_level", ".sample")
  ) |>
  dplyr::mutate(effect = 100 * (model_average - overall_mean) / overall_mean)

mean_model_effect |>
  dplyr::select(t_, effect, name, location_level) |>
  dplyr::group_by(name, t_, location_level) |>
  ggdist::median_qi(.width = 0.9) |>
  dplyr::left_join(
    log_mean_rps,
    by = c("t_", "location_level")
  ) |>
  dplyr::mutate(
    vjust = 1,
    hjust = 0.85,
    location_level = ordered(location_level, levels = c("nation", "region", "icb"))
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = prediction_start_date,
      y = effect,
      ymin = .lower,
      ymax = .upper,
      group = name
    )
  ) +
  ggdist::geom_lineribbon() +
  geomtextpath::geom_textline(
    ggplot2::aes(x = prediction_start_date, y = effect, label = name, vjust = vjust, hjust = hjust),
  ) +
  ggplot2::scale_fill_brewer() +
  ggplot2::ggtitle(
    "Average effect on rps induced including\na model in an ensemble of size three",
    subtitle = "Median effect with 90% credible interval\nNote: ETS and gr_mean are layed on top of each other"
  ) +
  ggplot2::labs(
    x = "Prediction start date",
    y = "Percentage difference from mean rps\n(negative implies improvement)"
  ) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::facet_wrap(~location_level)
