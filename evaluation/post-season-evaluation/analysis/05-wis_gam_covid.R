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

scoring_s3_root <- "REDACTED"

individual_models <- dplyr::tbl(
  redshift$connect(use_existing = FALSE),
  I("REDACTED")
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
  dplyr::filter(grepl("covid", s3_path)) |>
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

covid_data <- scores_summarised |>
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

# fit splines by whether or not a given ensemble component is in or out of the ensemble

wis_gam <- mgcv::gam(
  target ~
    s(t_, by = gam_cr, bs = "tp") +
    s(t_, by = ets, bs = "tp") +
    s(t_, by = gr_mean, bs = "tp") +
    s(t_, by = gam_gp, bs = "tp") +
    s(t_, by = gam_rw, bs = "tp") +
    s(t_, by = gr_gp, bs = "tp") +
    location_level +
    model,
  data = covid_data
)

gratia::appraise(wis_gam)
gratia::draw(wis_gam)


scores_summarised2 <- scores |>
  dplyr::summarise(
    mean_wis = mean(wis),
    .by = c("model", "prediction_start_date", "location_level")
  ) |>
  dplyr::mutate(model_components = stringr::str_extract_all(model, models_regex)) |>
  tidyr::unnest(model_components)

covid_data2 <- scores_summarised2 |>
  dplyr::mutate(
    t_ = as.numeric(prediction_start_date),
    model_components = as.factor(model_components),
    location_level = as.factor(location_level),
    model = as.factor(model),
    target = log(mean_wis)
  )


## fit similar model as above, but have the component as a factor
## not ideal as each target value is repeated (3 instances; one for each component)
wis_gam2 <- mgcv::gam(
  target ~
    s(t_, bs = "tp") +
    s(t_, by = model_components, bs = "tp") +
    s(location_level, bs = "re") +
    model,
  data = covid_data2
)

gratia::appraise(wis_gam2)
gratia::draw(wis_gam2)


## IDEA: use averaging to tease effects out of a simpler model

## here, we git using the ensemble (model) name as a factor smooth
## this means we have one fit per _ensemble_
## we also have a random effect for location level - WIS has similar shape across location levels, but different scale

wis_gam <- mgcv::gam(
  target ~ s(t_, bs = "gp") +
    s(t_, by = model, bs = "gp") +
    s(model, bs = "re") +
    s(t_, by = location_level, bs = "gp") +
    s(location_level, bs = "re"),
  data = covid_data
)


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

average_prediction |>
  ggplot2::ggplot(ggplot2::aes(x = t_, y = .fitted)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~location_level, scale = "free_y")


ensemble_components <- covid_data |>
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

log_mean_wis <- scores |>
  dplyr::summarise(
    y = log(mean(wis)),
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
    log_mean_wis,
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
    "Average effect on WIS induced including\na model in an ensemble of size three",
    subtitle = "Note: ETS and gr_mean are layed on top of each other"
  ) +
  ggplot2::labs(
    x = "Prediction start date",
    y = "Difference from mean WIS\n(negative implies improvement)"
  ) +
  ggplot2::theme(legend.position = "none")

## conclusions
## order of model performance is (remember smaller wis => better performance)
## gam_gp > gam_cr > gr_gp > gam_rw > gr_mean ~ ets
## models never overlap, for any time point
## ets and gr_mean have numerically similar, but non-identical scores

## uncertainty

posterior_samples <- intervals$generate_samples(covid_data, wis_gam, .n_pi_samples = 1000) |>
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
    vjust = dplyr::if_else(name == "ets", 1, 1),
    hjust = dplyr::if_else(name == "ets", 0.2, 0)
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
    "Average effect on WIS induced including\na model in an ensemble of size three",
    subtitle = "Median effect with 90% credible interval\nNote: ETS and gr_mean are layed on top of each other"
  ) +
  ggplot2::labs(
    x = "Prediction start date",
    y = "Percent change from mean WIS\n(negative implies improvement)"
  ) +
  ggplot2::theme(legend.position = "none")
