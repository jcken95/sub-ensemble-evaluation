#  analysis of national covid-19 and influenza forecasts

set.seed(9876)
box::use(
  box / deps_,
  box / redshift,
  box / s3,
  box / themes,
  prj / projection_plots,
  prj / intervals
)

deps_$need(
  "dplyr",
  "ggplot2",
  "patchwork",
  "purrr",
  "rPref",
  "s3fs",
  "stringr",
  "tibble",
  "tidyr"
)

library(patchwork)

ggplot2::theme_set(
  projection_plots$theme_pancasts() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12),
      text = ggplot2::element_text(size = 10)
    )
)

source(here::here("evaluation/helpers.R"))
plot_output_dir <- fs::dir_create(here::here("publication/plots"))
plot_output_dir_tiff <- fs::dir_create(here::here(plot_output_dir, "tiff"))
plot_supplement_dir_tiff <- fs::dir_create(here::here(plot_output_dir_tiff, "supplement"))

# data output dir
fs::dir_create(here::here("publication/data"))
# Load data ----
use_downloaded_data <- TRUE

## production forecasts ----

samples <- dplyr::tbl(redshift$connect(use_existing = FALSE), I("REDACTED")) |>
  dplyr::filter(disease %in% c("influenza", "covid-19")) |>
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
  dplyr::filter(
    date < prediction_start_date + lubridate::weeks(2),
    !(model %in% c("epinow2_rw", "hisotric_gr"))
  )

summary <- dplyr::tbl(redshift$connect(use_existing = FALSE), I("REDACTED")) |>
  dplyr::select(-c("pi_2.5", "pi_97.5", "pi_17", "pi_83")) |>
  dplyr::filter(disease %in% c("influenza", "covid-19")) |>
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
    !(model %in% c("epinow2_rw", "historic_gr")),
    age_group_granularity == "none"
  ) |>
  identify_ensemble_inclusion()


observed <- redshift$data_model("pancasts")$pancasts_combined
observed <- redshift$data_model("REDACTED")$REDACTED
lookups <- redshift$data_model("REDACTED")

## retrospective evaluation forecasts ----

# these objects are the "retrospective" forecasts - produced post-season for hypothetical evaluation purposes

samples_retrospective <- dplyr::tbl(redshift$connect(use_existing = FALSE), I("REDACTED")) |>
  dplyr::filter(disease %in% c("influenza", "covid-19"))
summary_retrospective <- dplyr::tbl(redshift$connect(use_existing = FALSE), I("REDACTED")) |>
  dplyr::select(-c("pi_2.5", "pi_97.5", "pi_17", "pi_83")) |>
  dplyr::filter(disease %in% c("influenza", "covid-19"))


# useful dates ----

season_start_date <- summary |>
  dplyr::filter(model != "gam_dow", metric == "admissions") |>
  dplyr::summarise(start_date = min(model_date, na.rm = TRUE), .by = "disease") |>
  dplyr::collect()

plot_start_date <- "2024-10-01"

plotting_end_date <- as.Date("2025-04-01")

## Combine all metrics ----

observed_by_geography <- load_evaluation_summary(summary, observed, lookups)

# scoring ready forecasts ----

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
    model_date > "2024-09-01",
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
    # if production ensembles were used then they were all stacking ensembles this year
    model = dplyr::if_else(stringr::str_detect(model, "ensemble"), "ensemble_stack", model)
  ) |>
  dplyr::mutate(
    n_models = dplyr::n_distinct(model),
    # the reported model was the ensemble; if no ensemble, then we only had one model, therefore it was reported
    is_reported_model = grepl("ensemble_stack", model) | n_models == 1,
    .by = c(model_date, disease, metric)
  ) |>
  # if gam_dow is only model, don't need as cannot be compared to anything
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

forecast_score_over_season <- forecasts_scored |>
  dplyr::filter(is_reported_model) |>
  scoringutils::summarise_scores(by = c("prediction_start_date", "model", "disease", "metric", "location", "scale")) |>
  dplyr::mutate(disease = dplyr::if_else(disease == "covid", "influenza", disease))

# Faceted plots of all diseases ----

probability_colours <- c(
  "decrease" = projection_plots$select_ukhsa_colour("dark blue"),
  "stable" = projection_plots$select_ukhsa_colour("orange"),
  "increase" = projection_plots$select_ukhsa_colour("dark pink")
)

# grab all single-model / ensemble data

admissions_summary <- scoring_ready |>
  dplyr::filter(
    metric == "admissions",
    location_level == "nation",
    age_group == "all",
    grepl("ensemble|tp|gp|nowcast", model),
    date >= prediction_start_date,
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
  dplyr::filter(
    model == "ensemble_stack" | n_models == 1,
    date <= plotting_end_date
  ) |>
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
    disease,
    prediction_start_date,
    trend_probability,
    trend
  ) |>
  dplyr::distinct()

### data to plot incidence; even if no forecast ----

prediction_dates <- admissions_summary |>
  dplyr::filter(
    !is.na(prediction_start_date),
    prediction_start_date <= plotting_end_date
  ) |>
  dplyr::pull(date)

observed_admissions <- admissions_summary |>
  dplyr::filter(
    date >= min(prediction_dates),
    date <= max(prediction_dates)
  )

### make plot  ----

## mini helper for labelling

disease_facet_labels <- function(x) {
  dplyr::case_when(
    x == "covid-19" ~ "COVID-19",
    x == "covid" ~ "COVID-19",
    x == "influenza" ~ "Influenza",
    .default = x
  )
}

admissions_summary_plot <- admissions_summary |>
  dplyr::filter(model == "ensemble_stack" | n_models == 1) |>
  dplyr::filter(
    disease %in% c("covid", "influenza"),
    date <= plotting_end_date
  ) |>
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
      fill = as.factor(prediction_start_date)
    ),
    alpha = 0.25
  ) +
  ggplot2::geom_line(
    ggplot2::aes(x = date, y = pi_50, group = prediction_start_date, colour = as.factor(prediction_start_date))
  ) +
  ggplot2::geom_point(
    data = dplyr::filter(observed_admissions, disease %in% c("covid", "influenza")),
    ggplot2::aes(x = date, y = observed_target),
    size = 0.3
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(disease),
    scale = "free_y",
    ncol = 2,
    labeller = ggplot2::labeller(disease = disease_facet_labels)
  ) +
  ggplot2::labs(
    x = "Prediction start date",
    y = "Admissions"
  ) +
  ggplot2::ggtitle(
    "National forecasts for daily admissions of respiratory diseases",
    subtitle = "Colour indicates prediction start date"
  ) +
  ggplot2::theme(legend.position = "none")

admissions_summary_plot

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "admissions_summary.png"),
  plot = admissions_summary_plot,
  width = 16,
  height = 12
)

## plot at region level

admissions_summary_region <- scoring_ready |>
  dplyr::filter(
    metric == "admissions",
    location_level == "region",
    age_group == "all",
    grepl("ensemble|tp|gp|nowcast", model),
    date >= prediction_start_date,
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
        location_level == "region",
        age_group == "all",
        date >= plot_start_date
      ),
    by = c("date", "location", "location_level", "metric", "disease", "age_group")
  )

observed_admissions_region <- admissions_summary_region |>
  dplyr::filter(
    date >= min(prediction_dates),
    date <= max(prediction_dates),
    disease %in% c("covid", "influenza")
  ) |>
  dplyr::mutate(
    location_level = dplyr::case_when(
      location_level == "icb" ~ "ICB",
      location_level == "region" ~ "Region"
    ),
    disease = disease_facet_labels(disease)
  )

admissions_region_plot <- admissions_summary_region |>
  dplyr::filter(model == "ensemble_stack" | n_models == 1) |>
  dplyr::filter(
    disease %in% c("covid", "influenza"),
    date <= plotting_end_date
  ) |>
  tidyr::pivot_wider(
    names_from = quantile_level,
    values_from = predicted,
    names_glue = "pi_{100 * quantile_level}"
  ) |>
  dplyr::left_join(trend_probs, by = c("disease", "prediction_start_date")) |>
  dplyr::mutate(
    location_level = dplyr::case_when(
      location_level == "icb" ~ "ICB",
      location_level == "region" ~ "Region"
    ),
    disease = disease_facet_labels(disease)
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      x = date,
      ymin = pi_5,
      ymax = pi_95,
      group = prediction_start_date,
      fill = as.factor(prediction_start_date)
    ),
    alpha = 0.25
  ) +
  ggplot2::geom_line(
    ggplot2::aes(x = date, y = pi_50, group = prediction_start_date, colour = as.factor(prediction_start_date))
  ) +
  ggplot2::geom_point(
    data = observed_admissions_region,
    mapping = ggplot2::aes(x = date, y = observed_target),
    size = 0.1
  ) +
  ggplot2::facet_grid(
    ggplot2::vars(disease),
    ggplot2::vars(location),
    scale = "free_y",
    labeller = ggplot2::label_wrap_gen(20)
  ) +
  ggplot2::labs(
    x = "Prediction start date",
    y = "Admissions"
  ) +
  ggplot2::ggtitle(
    "Regional forecasts for daily admissions of respiratory diseases",
    subtitle = "Colour indicates prediction start date."
  ) +
  ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(size = 8, angle = 90, vjust = 1))

admissions_region_plot

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "SUPPLEMENT_admissions_summary_region.png"),
  plot = admissions_region_plot,
  width = 16,
  height = 8
)

## plot at icb level
n_icb <- 4
unique_icbs <- scoring_ready |>
  dplyr::filter(location_level == "icb") |>
  dplyr::distinct(location) |>
  dplyr::pull(location)

chosen_icbs <- withr::with_seed(seed = "REDACTED", code = {
  sample(unique_icbs, n_icb, replace = FALSE)
})

icb_labels <- tibble::tibble(icb_label = chosen_icbs) |>
  dplyr::mutate(
    .row = dplyr::row_number(),
    letter = LETTERS[.row],
    icb_anon = glue::glue("ICB {letter}")
  ) |>
  dplyr::select(icb_label, icb_anon)

admissions_summary_icb <- scoring_ready |>
  dplyr::filter(
    metric == "admissions",
    location_level == "icb",
    age_group == "all",
    grepl("ensemble|tp|gp|nowcast", model),
    date >= prediction_start_date,
    model_date > "2024-09-01",
    location %in% chosen_icbs
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
        location_level == "icb",
        age_group == "all",
        date >= plot_start_date,
        location %in% chosen_icbs
      ),
    by = c("date", "location", "location_level", "metric", "disease", "age_group")
  )

observed_admissions_icb <- admissions_summary_icb |>
  dplyr::filter(
    date >= min(prediction_dates),
    date <= max(prediction_dates),
    disease %in% c("covid", "influenza")
  ) |>
  dplyr::mutate(
    location_level = dplyr::case_when(
      location_level == "icb" ~ "ICB",
      location_level == "region" ~ "Region"
    ),
    disease = disease_facet_labels(disease)
  )

admissions_icb_plot <- admissions_summary_icb |>
  dplyr::filter(model == "ensemble_stack" | n_models == 1) |>
  dplyr::filter(
    disease %in% c("covid", "influenza"),
    date <= plotting_end_date
  ) |>
  tidyr::pivot_wider(
    names_from = quantile_level,
    values_from = predicted,
    names_glue = "pi_{100 * quantile_level}"
  ) |>
  dplyr::left_join(trend_probs, by = c("disease", "prediction_start_date")) |>
  dplyr::mutate(
    location_level = dplyr::case_when(
      location_level == "icb" ~ "ICB",
      location_level == "region" ~ "Region"
    ),
    disease = disease_facet_labels(disease)
  ) |>
  dplyr::left_join(icb_labels, dplyr::join_by(location == icb_label)) |>
  ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      x = date,
      ymin = pi_5,
      ymax = pi_95,
      group = prediction_start_date,
      fill = as.factor(prediction_start_date)
    ),
    alpha = 0.25
  ) +
  ggplot2::geom_line(
    ggplot2::aes(x = date, y = pi_50, group = prediction_start_date, colour = as.factor(prediction_start_date))
  ) +
  ggplot2::geom_point(
    data = observed_admissions_icb |>
      dplyr::left_join(icb_labels, dplyr::join_by(location == icb_label)),
    mapping = ggplot2::aes(x = date, y = observed_target),
    size = 0.3
  ) +
  ggplot2::facet_grid(
    ggplot2::vars(disease),
    ggplot2::vars(icb_anon),
    scale = "free_y"
  ) +
  ggplot2::labs(
    x = "Prediction start date",
    y = "Admissions"
  ) +
  ggplot2::ggtitle(
    "ICB level forecasts for daily admissions of respiratory diseases",
    subtitle = "Colour indicates prediction start date."
  ) +
  ggplot2::theme(legend.position = "none")

admissions_icb_plot

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "SUPPLEMENT_admissions_summary_icb.png"),
  plot = admissions_icb_plot,
  width = 12,
  height = 8
)

## scores ----

### obtain population for pcWIS ----

pre_score <- summary_retrospective |>
  dplyr::filter(
    metric == "admissions",
    disease %in% c("covid-19", "influenza"),
    date >= prediction_start_date,
    date < prediction_start_date + lubridate::days(14)
  ) |>
  dplyr::rename("observed" = target_value) |>
  dplyr::select(-dplyr::starts_with("p_")) |>
  dplyr::filter(
    grepl("admissions", target_name),
    grepl("ensemble", model)
  ) |>
  dplyr::distinct() |>
  dplyr::collect() |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("pi_"),
    names_to = "quantile",
    values_to = "predicted"
  ) |>
  dplyr::mutate(
    quantile = quantile |>
      stringr::str_remove("pi_") |>
      as.numeric(),

    quantile_level = quantile / 100
  ) |>
  dplyr::select(-quantile)

scoring_population <- pre_score |>
  dplyr::filter(
    disease %in% c("influenza", "influenza"),
    metric == "admissions",
    model == "ensemble_stack"
  ) |>
  dplyr::select(
    disease,
    metric,
    date,
    population,
    target_name,
    location,
    location_level,
    age_group,
    age_group_granularity
  ) |>
  dplyr::distinct()

divide <- function(x, y) {
  x / y
}

wis_ready <- scoring_ready |>
  dplyr::filter(
    disease %in% c("covid", "influenza"),
    metric == "admissions",
    model == "ensemble_stack"
  ) |>
  dplyr::mutate(disease = dplyr::if_else(disease == "covid", "covid-19", disease)) |>
  dplyr::left_join(
    scoring_population,
    by = c(
      "prediction_start_date" = "date",
      "disease",
      "target_name",
      "population",
      "metric",
      "location",
      "location_level",
      "age_group",
      "age_group_granularity"
    )
  )

## Score pcWIS ----

forecast_score_over_season <- wis_ready |>
  dplyr::filter(
    model == "ensemble_stack",
    disease %in% c("covid-19", "influenza")
  ) |>
  dplyr::mutate(disease = dplyr::if_else(disease == "covid", "covid-19", disease)) |>
  scoringutils::as_forecast_quantile(forecasting_unit) |>
  scoringutils::transform_forecasts(fun = divide, label = "per_capita", y = wis_ready$population) |>
  scoringutils::score(metrics = list(wis = scoringutils::wis)) |>
  scoringutils::summarise_scores(by = c("prediction_start_date", "model", "disease", "metric", "location"))


all_wis_over_season <- scoring_ready |>
  dplyr::filter(
    disease %in% c("covid", "influenza"),
    metric == "admissions"
  ) |>
  dplyr::mutate(disease = dplyr::if_else(disease == "covid", "covid-19", disease)) |>
  dplyr::left_join(
    scoring_population,
    by = c(
      "prediction_start_date" = "date",
      "disease",
      "target_name",
      "population",
      "metric",
      "location",
      "location_level",
      "age_group",
      "age_group_granularity"
    )
  ) |>
  dplyr::filter(disease %in% c("covid-19", "influenza")) |>
  dplyr::mutate(disease = dplyr::if_else(disease == "covid", "covid-19", disease)) |>
  scoringutils::as_forecast_quantile(forecasting_unit)
all_wis_over_season <- all_wis_over_season |>
  scoringutils::transform_forecasts(fun = divide, label = "per_capita", y = all_wis_over_season$population) |>
  scoringutils::score(metrics = list(wis = scoringutils::wis)) |>
  scoringutils::summarise_scores(by = c("prediction_start_date", "model", "disease", "metric", "location", "scale"))

### Wrangling for RPS ----

lagged_values <- observed_by_geography |>
  dplyr::group_by(location, location_level, age_group, disease, metric) |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    rolling_average = zoo::rollmean(observed_target, k = 7, align = "right", na.pad = TRUE),
    observed_lag = dplyr::lag(observed_target, n = 14),
    rolling_average_lagged = zoo::rollmean(observed_lag, k = 7, align = "right", na.pad = TRUE),
    observed_trend = classify_trend(rolling_average, rolling_average_lagged)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(disease = dplyr::if_else(disease == "covid", "covid-19", disease))

ordinal_ready <- scoring_ready |>
  dplyr::mutate(disease = dplyr::if_else(disease == "covid", "covid-19", disease)) |>
  dplyr::select(-c(quantile_level, predicted)) |>
  dplyr::distinct() |>
  dplyr::group_by(
    model,
    prediction_start_date,
    location,
    location_level,
    age_group,
    age_group_granularity,
    disease,
    metric,
    model_date,
    population
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
  scoringutils::score(by = forecasting_unit)

ordinal_score_over_season <- ordinal_scored |>
  dplyr::filter(
    model == "ensemble_stack",
    disease %in% c("covid-19", "influenza")
  ) |>
  scoringutils::summarise_scores(by = c("prediction_start_date", "model", "disease", "metric", "location"))

## stack up to facet

ensemble_scored <- dplyr::bind_rows(
  all_wis_over_season |>
    dplyr::filter(
      scale == "per_capita",
      location == "England",
      model == "ensemble_stack",
      disease %in% c("covid-19", "influenza")
    ) |>
    dplyr::mutate(score_name = "pcWIS") |>
    dplyr::rename("score_value" = wis) |>
    dplyr::mutate(score_value = log(score_value)),

  ordinal_score_over_season |>
    dplyr::filter(
      location == "England",
      model == "ensemble_stack",
      disease %in% c("covid-19", "influenza"),
      metric == "admissions"
    ) |>
    dplyr::mutate(score_name = "rps") |>
    dplyr::rename("score_value" = rps)
)

# [plot of scores over season for national forecast] ----

holiday_break_tibble <- tidyr::expand_grid(
  prediction_start_date = as.Date("2024-12-25"),
  score_name = c("RPS", "log(pcWIS)"),
  disease = c("COVID-19", "Influenza"),
  score_value = NA_real_
)
ensemble_scored_plot <- ensemble_scored |>
  dplyr::filter(prediction_start_date <= plotting_end_date) |>
  dplyr::mutate(
    disease = dplyr::case_when(
      disease == "covid-19" ~ "COVID-19",
      disease == "influenza" ~ "Influenza",
      .default = NULL
    ),

    score_name = dplyr::if_else(score_name == "rps", "RPS", score_name),

    score_name = dplyr::if_else(score_name == "pcWIS", "log(pcWIS)", score_name)
  ) |>
  dplyr::bind_rows(holiday_break_tibble) |>
  dplyr::arrange(prediction_start_date) |>
  ggplot2::ggplot(
    ggplot2::aes(x = prediction_start_date, y = score_value)
  ) +
  ggplot2::geom_line() +
  ggplot2::labs(
    x = "Prediction Start Date",
    y = "Score value"
  ) +
  ggplot2::facet_grid(ggplot2::vars(score_name), ggplot2::vars(disease), scales = "free_y") +
  ggplot2::ggtitle("Scores of Operational Forecasts at National Geography") +
  projection_plots$theme_pancasts()

ensemble_scored_plot

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "scores_published.png"),
  plot = ensemble_scored_plot,
  width = 16,
  height = 12
)

## which models when ----

forecasts <- summary |> # pull known stored results
  dplyr::filter(
    disease %in% c("covid-19", "influenza")
    # Could add more filters,
    # but we only have "admissions" & "occupancy_rate" here
  ) |>
  dplyr::distinct(disease, metric, model_date) |>
  dplyr::arrange(disease, metric, model_date) |>
  dplyr::collect() |>
  dplyr::rename(upload_date = model_date) |> # for later alignment
  dplyr::rowwise() |>
  dplyr::mutate(
    # add database requests (collecting every slice took 30 min)
    "samples" = list(dplyr::filter(
      samples,
      disease == local(disease),
      metric == local(metric),
      model_date == upload_date
    )),
    "summary" = list(dplyr::filter(
      summary,
      disease == local(disease),
      metric == local(metric),
      model_date == upload_date
    )),
    upload_date = round_to_wednesday(upload_date) # had to match to dbt before
  ) |>
  dplyr::ungroup()

model_ensemble_name_path <- here::here("publication/data/models_ensembles.rds")

# conditionally run query - can easily take > 1 hr so a local copy speeds up

if (isTRUE(fs::file_exists(model_ensemble_name_path)) && isTRUE(use_downloaded_data)) {
  models_and_ensembles <- readr::read_rds(model_ensemble_name_path)
} else {
  models_and_ensembles <- forecasts |>
    dplyr::filter(metric == "admissions") |>
    dplyr::mutate(
      individual_models = purrr::map(
        samples,
        \(x) {
          x |>
            dplyr::distinct(model) |>
            dplyr::pull(model)
        },
        .progress = "samples"
      ),
      ensemble_string = purrr::map(
        summary,
        \(x) {
          x |>
            dplyr::distinct(model) |>
            dplyr::filter(stringr::str_detect(model, "ensemble")) |>
            dplyr::pull(model)
        },
        .progress = "summary"
      ),
      .by = c("disease", "metric")
    ) |>
    dplyr::select(
      upload_date,
      individual_models,
      ensemble_string,
      disease,
      metric
    )

  readr::write_rds(models_and_ensembles, model_ensemble_name_path)
}


models_used <- models_and_ensembles |>
  tidyr::unnest(c(individual_models, ensemble_string)) |>
  dplyr::mutate(
    individual_models = stringr::str_trim(individual_models),
    individual_models = dplyr::na_if(individual_models, "")
  ) |>
  tidyr::drop_na(individual_models) |> # now removes empty strings
  dplyr::mutate(
    model_in_ensemble = stringr::str_detect(
      ensemble_string,
      individual_models
    )
  ) |>
  tidyr::complete(
    upload_date,
    individual_models,
    metric,
    disease,
    fill = list(model_in_ensemble = FALSE)
  ) |>
  dplyr::group_by(individual_models, metric, disease) |>
  # drop when not all false
  dplyr::filter(any(model_in_ensemble)) |>
  dplyr::ungroup() |>
  dplyr::arrange(upload_date) |>
  # working out the first time a model was used in an ensemble
  dplyr::mutate(
    is_first_use = dplyr::row_number() == 1,
    .by = c("metric", "disease", "individual_models", "model_in_ensemble")
  ) |>
  dplyr::mutate(
    is_first_use = as.character(is_first_use & model_in_ensemble),
    upload_date = lubridate::ymd(upload_date)
  )

upload_date_breaks <- models_used |>
  dplyr::distinct(upload_date) |>
  dplyr::filter(dplyr::row_number() %% 2 == 1) |>
  dplyr::pull(upload_date)

# No false is being shown?
models_used_plot <- models_used |>
  dplyr::filter(upload_date <= plotting_end_date) |>
  dplyr::mutate(tile_width = 6) |>
  dplyr::mutate(model_in_ensemble = dplyr::if_else(model_in_ensemble, "Yes", "No")) |>
  dplyr::left_join(model_code_to_name, by = dplyr::join_by(individual_models == name)) |>
  tidyr::drop_na(individual_models, full_name) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = upload_date,
      y = full_name,
      fill = model_in_ensemble
    )
  ) +
  ggplot2::geom_tile(
    ggplot2::aes(width = tile_width),
    colour = "white",
    linewidth = 0.5
  ) +
  ggplot2::labs(
    y = "Model name",
    x = "Forecast production date",
    fill = "Model in operation ensemble?",
    alpha = "Is first use?"
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(disease),
    ncol = 2,
    scales = "free_y",
    labeller = ggplot2::labeller(disease = disease_facet_labels)
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) +
  ggplot2::scale_x_date(
    breaks = upload_date_breaks,
    labels = scales::label_date_short()
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Yes" = projection_plots$select_ukhsa_colour("dark blue"),
      "No" = projection_plots$select_ukhsa_colour("orange")
    )
  ) +
  ggplot2::ggtitle(
    "Which models were used when?"
  ) +
  ggplot2::scale_y_discrete(labels = \(.str) stringr::str_wrap(.str, width = 10)) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(hjust = 0.5, size = 6),
    legend.title = ggplot2::element_text(size = 14),
    legend.text = ggplot2::element_text(size = 12),
    legend.position = "bottom"
  )

models_used_plot

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "models_used.png"),
  plot = models_used_plot,
  width = 16,
  height = 12
)

models_used_plot

combined_plots <- admissions_summary_plot / models_used_plot

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "admissions_models_combined.png"),
  plot = combined_plots,
  width = 16,
  height = 12
)

ggplot2::ggsave(
  filename = here::here(plot_output_dir_tiff, "fig_1.tiff"),
  plot = combined_plots,
  width = 19,
  height = 19,
  dpi = 300,
  units = "cm"
)


## retrospective score plots ----

## load in scored ensembles

scoring_s3_root <- "REDACTED"
scoring_s3_root_ordinal <- "REDACTED"

ensembles_scored <- tibble::tibble(
  s3_path = s3fs::s3_dir_ls(scoring_s3_root)
) |>
  dplyr::mutate(
    data = purrr::map(
      s3_path,
      \(fpath) s3$read_using(fpath, fn = readRDS) |> dplyr::mutate(model_date = as.Date(model_date))
    )
  ) |>
  tidyr::unnest(data) |>
  dplyr::select(-s3_path) |>
  # was created post-season, and trained on 24/25 season, biases results
  dplyr::filter(!stringr::str_detect(model, "epinow2")) |>
  dplyr::filter(scale == "per_capita")

rps_scored <- tibble::tibble(
  s3_path = s3fs::s3_dir_ls(scoring_s3_root_ordinal)
) |>
  dplyr::mutate(
    data = purrr::map(
      s3_path,
      \(fpath) s3$read_using(fpath, fn = readRDS) |> dplyr::mutate(model_date = as.Date(model_date))
    )
  ) |>
  tidyr::unnest(data) |>
  dplyr::select(-s3_path) |>
  # was created post-season, and trained on 24/25 season, biases results
  dplyr::filter(!stringr::str_detect(model, "epinow2"))
# scoring of retrospecive ENSEMBLES
retrospective_ensemble_scores <- rps_scored |>
  dplyr::left_join(
    ensembles_scored,

    by = c(
      "model",
      "prediction_start_date",
      "location",
      "location_level",
      "age_group",
      "age_group_granularity",
      "date",
      "disease",
      "metric"
    ),
    suffix = c("_rps", "_wis")
  ) |>
  dplyr::mutate(population = population_rps) |>
  dplyr::filter(
    disease %in% c("covid-19", "influenza"),
    location == "England"
  ) |>
  tidyr::pivot_longer(
    cols = c(wis, rps),
    names_to = "metric_name",
    values_to = "metric_value"
  ) |>
  dplyr::mutate(model_type = dplyr::if_else(model == "ensemble_matched", "Matched Ensemble", "Sub-Ensemble"))

individual_retrospective_scores <- dplyr::bind_rows(
  s3$read_using(
    "REDACTED",
    readRDS
  ) |>
    dplyr::filter(scale == "per_capita") |>
    dplyr::mutate(metric_name = "wis") |>
    dplyr::rename("metric_value" = wis),

  s3$read_using(
    "REDACTED",
    readRDS
  ) |>
    dplyr::mutate(metric_name = "rps") |>
    dplyr::rename("metric_value" = rps),
) |>
  dplyr::filter(
    disease %in% c("covid-19", "influenza"),
    location == "England"
  ) |>
  dplyr::mutate(model_type = "Individual")

published_model_scores <- dplyr::bind_rows(
  dplyr::filter(all_wis_over_season, scale == "per_capita"),
  ordinal_scored
) |>
  dplyr::filter(
    disease %in% c("covid-19", "influenza"),
    location == "England"
  ) |>
  dplyr::filter(model == "ensemble_stack") |>
  tidyr::pivot_longer(
    cols = c("wis", "rps"),
    names_to = "metric_name",
    values_to = "metric_value"
  ) |>
  tidyr::drop_na(metric_value) |>
  dplyr::mutate(model_type = "Operational Ensemble")


## gam scores plots ----

#### covid - pcwis ----

scoring_s3_root <- "REDACTED"

individual_models <- dplyr::tbl(
  redshift$connect(use_existing = FALSE),
  I("REDACTED")
) |>
  dplyr::distinct(model) |>
  dplyr::filter(model != "") |>
  dplyr::collect()

# string for regex matching of models
models_regex <- unique(individual_models$model) |>
  stringr::str_flatten(collapse = "|")

# grab scores for pathogen / score
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

# pcwis covid modelling data
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

# fit model with a standard seed
k <- 0.5 * floor(max(covid_data$t_ / 7))

wis_gam_covid <- withr::with_seed(
  seed = 404,
  {
    mgcv::gam(
      target ~ s(t_, bs = "ts", k = k) +
        s(t_, by = model, bs = "ts", k = k) +
        s(model, bs = "re") +
        s(t_, by = location_level, bs = "ts", k = k) +
        s(location_level, bs = "re"),
      data = covid_data
    )
  }
)

# gam diagnostics plot

wis_gam_covid_diagnostics <- wis_gam_covid |>
  generate_gam_diagnostics() +
  patchwork::plot_annotation(title = "Diagnostics plots: pcWIS COVID-19")

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "SUPPLEMENT_wis_gam_covid.png"),
  plot = wis_gam_covid_diagnostics,
  width = 16,
  height = 12
)


posterior_samples <- generate_samples_fitted(covid_data, wis_gam_covid, .n_pi_samples = 1000, method = "mh") |>
  dplyr::mutate(.value = exp(.value))

wis_gam_covid_aug <- broom::augment(wis_gam_covid)

average_prediction <- dplyr::summarise(
  wis_gam_covid_aug,
  .fitted = mean(.fitted),
  .by = c(t_, location_level)
)

ensemble_components <- covid_data |>
  dplyr::select(model, dplyr::any_of(models_vector)) |>
  dplyr::distinct() |>
  dplyr::mutate(
    dplyr::across(-model, as.logical)
  )

# compute mean score across all models per time and location level
model_averages <- wis_gam_covid_aug |>
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

log_mean_wis_covid <- scores |>
  dplyr::summarise(
    y = log(mean(wis)),
    .by = c(prediction_start_date, location_level)
  ) |>
  dplyr::mutate(
    t_ = as.numeric(prediction_start_date),
    t_ = t_ - min(t_),
    disease = "covid-19",
    score = "pcWIS"
  )

# compute the effect of having a model in an ensemble

model_effect <- model_averages |>
  dplyr::rename(model_average = .fitted) |>
  # shape of effect will be the same regardless of location level
  dplyr::filter(location_level == "nation") |>
  dplyr::left_join(average_prediction, by = c("t_", "location_level")) |>
  dplyr::mutate(effect = model_average - .fitted)

# geneerate uncertainty
posterior_samples <- generate_samples_fitted(covid_data, wis_gam_covid, .n_pi_samples = 1000, method = "mh") |>
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
covid_pcwis_mean_model_effect <- mean_model_prediction |>
  dplyr::rename(model_average = .value) |>
  # shape of effect will be the same regardless of location level
  dplyr::left_join(
    mean_prediction |>
      dplyr::rename("overall_mean" = .value),
    by = c("t_", "location_level", ".sample")
  ) |>
  dplyr::mutate(
    effect = 100 * (model_average - overall_mean) / overall_mean,
    disease = "covid-19",
    score = "pcWIS"
  )

pct_string <- glue::glue(
  "({x}, {y})%",
  x = round(min(covid_pcwis_mean_model_effect$effect), 2),
  y = round(max(covid_pcwis_mean_model_effect$effect), 2)
)

#### covid - rps ----

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
  s3_path = s3fs::s3_dir_ls(scoring_s3_root_ordinal)
) |>
  dplyr::filter(!stringr::str_detect(s3_path, "epinow2")) |>
  dplyr::filter(grepl("covid", s3_path)) |>
  dplyr::mutate(data = purrr::map(s3_path, \(fpath) s3$read_using(fpath, fn = readRDS))) |>
  tidyr::unnest(data) |>
  dplyr::select(-s3_path)

models_vector <- as.vector(individual_models)$model

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

covid_data <- scores_summarised |>
  dplyr::mutate(
    t_ = as.numeric(prediction_start_date),
    t_ = t_ - min(t_)
  ) |>
  dplyr::mutate(
    target = log(mean_rps),
    location_level = as.factor(location_level),
    dplyr::across(
      dplyr::where(is.logical),
      as.factor
    ),
    model = as.factor(model)
  )

k <- 0.5 * max(covid_data$t_ / 7)

rps_gam_covid <- mgcv::gam(
  target ~
    s(t_, bs = "ts", k = k) +
    s(t_, by = model, bs = "ts", k = k) +
    s(model, bs = "re") +
    s(t_, by = location_level, bs = "ts", k = k) +
    s(location_level, bs = "re"),
  data = covid_data
)

rps_gam_covid_diagnostics <- rps_gam_covid |>
  generate_gam_diagnostics() +
  patchwork::plot_annotation(title = "Diagnostics plots: RPS COVID-19")

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "SUPPLEMENT_rps_gam_covid.png"),
  plot = rps_gam_covid_diagnostics,
  width = 16,
  height = 12
)

rps_gam_covid_aug <- broom::augment(rps_gam_covid)

average_prediction <- dplyr::summarise(
  rps_gam_covid_aug,
  .fitted = mean(.fitted),
  .by = c(t_, location_level)
)

ensemble_components <- covid_data |>
  dplyr::select(model, dplyr::any_of(models_vector)) |>
  dplyr::distinct() |>
  dplyr::mutate(
    dplyr::across(-model, as.logical)
  )


model_averages <- rps_gam_covid_aug |>
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

log_mean_rps_covid <- scores |>
  dplyr::summarise(
    y = log(mean(rps)),
    .by = c(prediction_start_date, location_level)
  ) |>
  dplyr::mutate(
    t_ = as.numeric(prediction_start_date),
    t_ = t_ - min(t_),
    disease = "covid-19",
    score = "RPS"
  )

model_effect <- model_averages |>
  dplyr::rename(model_average = .fitted) |>
  # shape of effect will be the same regardless of location level
  dplyr::filter(location_level == "nation") |>
  dplyr::left_join(average_prediction, by = c("t_", "location_level")) |>
  dplyr::mutate(effect = model_average - .fitted)

## conclusions ----
## order of model performance is (remember smaller rps => better performance)
## gam_gp > gam_cr > gr_gp > gam_rw > gr_mean ~ ets
## models never overlap, for any time point
## ets and gr_mean have numerically similar, but non-identical scores

## uncertainty

posterior_samples <- generate_samples_fitted(covid_data, rps_gam_covid, .n_pi_samples = 5000) |>
  dplyr::mutate(.value = exp(.value))

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

covid_rps_mean_model_effect <- mean_model_prediction |>
  dplyr::rename(model_average = .value) |>
  # shape of effect will be the same regardless of location level
  dplyr::left_join(
    mean_prediction |>
      dplyr::rename("overall_mean" = .value),
    by = c("t_", "location_level", ".sample")
  ) |>
  dplyr::mutate(
    effect = 100 * (model_average - overall_mean) / overall_mean,
    disease = "covid-19",
    score = "RPS"
  )

#### influenza - pcwis ----

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

k <- floor(0.5 * max(influenza_data$t_ / 7))

wis_gam_influenza <- mgcv::gam(
  target ~ s(t_, bs = "ts", k = k) +
    s(t_, by = model, bs = "ts", k = k) +
    s(model, bs = "re") +
    s(t_, by = location_level, bs = "ts", k = k) +
    s(location_level, bs = "re"),
  data = influenza_data
)

wis_gam_influenza_diagnostics <- wis_gam_influenza |>
  generate_gam_diagnostics() +
  patchwork::plot_annotation(title = "Diagnostics plots: pcWIS Influenza")

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "SUPPLEMENT_wis_gam_influenza.png"),
  plot = wis_gam_influenza_diagnostics,
  width = 16,
  height = 12
)

wis_gam_influenza_aug <- broom::augment(wis_gam_influenza)
average_prediction <- dplyr::summarise(
  wis_gam_influenza_aug,
  .fitted = mean(.fitted),
  .by = c(t_, location_level)
)

ensemble_components <- influenza_data |>
  dplyr::select(model, dplyr::any_of(models_vector)) |>
  dplyr::distinct() |>
  dplyr::mutate(
    dplyr::across(-model, as.logical)
  )


model_averages <- wis_gam_influenza_aug |>
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

log_mean_wis_influenza <- scores |>
  dplyr::summarise(
    y = log(mean(wis)),
    .by = c(prediction_start_date, location_level)
  ) |>
  dplyr::mutate(
    t_ = as.numeric(prediction_start_date),
    t_ = t_ - min(t_),
    disease = "Influenza",
    score = "pcWIS"
  )

model_effect <- model_averages |>
  dplyr::rename(model_average = .fitted) |>
  # shape of effect will be the same regardless of location level
  dplyr::filter(location_level == "nation") |>
  dplyr::left_join(average_prediction, by = c("t_", "location_level")) |>
  dplyr::mutate(effect = model_average - .fitted)

## uncertainty

posterior_samples <- withr::with_seed(
  seed = 40401,
  code = {
    generate_samples_fitted(influenza_data, wis_gam_influenza, .n_pi_samples = 1000) |>
      dplyr::mutate(.value = exp(.value))
  }
)

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

influenza_pcwis_mean_model_effect <- mean_model_prediction |>
  dplyr::rename(model_average = .value) |>
  # shape of effect will be the same regardless of location level
  dplyr::left_join(
    mean_prediction |>
      dplyr::rename("overall_mean" = .value),
    by = c("t_", "location_level", ".sample")
  ) |>
  dplyr::mutate(
    effect = 100 * (model_average - overall_mean) / overall_mean,
    disease = "Influenza",
    score = "pcWIS"
  )

#### infuenza - rps ----

zero_offset <- 1e-6 # to avoid log(0) in any gam pre-processing

scores <- tibble::tibble(
  s3_path = s3fs::s3_dir_ls(scoring_s3_root_ordinal)
) |>
  dplyr::filter(!stringr::str_detect(s3_path, "epinow2")) |>
  dplyr::filter(grepl("influenza", s3_path)) |>
  dplyr::mutate(data = purrr::map(s3_path, \(fpath) s3$read_using(fpath, fn = readRDS))) |>
  tidyr::unnest(data) |>
  dplyr::select(-s3_path)

models_vector <- as.vector(individual_models)$model

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

k <- floor(0.5 * max(influenza_data$t_ / 7))

rps_gam_influenza <- mgcv::gam(
  target ~
    s(t_, bs = "ts", k = k) +
    s(t_, by = model, bs = "ts", k = k) +
    s(model, bs = "re") +
    s(t_, by = location_level, bs = "ts", k = k) +
    s(location_level, bs = "re"),
  data = influenza_data
)

rps_gam_influenza_diagnostics <- rps_gam_influenza |>
  generate_gam_diagnostics() +
  patchwork::plot_annotation(title = "Diagnostics plots: RPS Influenza")

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "SUPPLEMENT_rps_gam_influenza.png"),
  plot = rps_gam_influenza_diagnostics,
  width = 16,
  height = 12
)

rps_gam_influenza_aug <- broom::augment(rps_gam_influenza)

average_prediction <- dplyr::summarise(
  rps_gam_influenza_aug,
  .fitted = mean(.fitted),
  .by = c(t_, location_level)
)

ensemble_components <- influenza_data |>
  dplyr::select(model, dplyr::any_of(models_vector)) |>
  dplyr::distinct() |>
  dplyr::mutate(
    dplyr::across(-model, as.logical)
  )


model_averages <- rps_gam_influenza_aug |>
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

log_mean_rps_influenza <- scores |>
  dplyr::summarise(
    y = log(mean(rps)),
    .by = c(prediction_start_date, location_level)
  ) |>
  dplyr::mutate(
    t_ = as.numeric(prediction_start_date),
    t_ = t_ - min(t_),
    disease = "Influenza",
    score = "RPS"
  )

model_effect <- model_averages |>
  dplyr::rename(model_average = .fitted) |>
  # shape of effect will be the same regardless of location level
  dplyr::filter(location_level == "nation") |>
  dplyr::left_join(average_prediction, by = c("t_", "location_level")) |>
  dplyr::mutate(effect = model_average - .fitted)

## conclusions
## order of model performance is (remember smaller rps => better performance)
## gam_gp > gam_cr > gr_gp > gam_rw > gr_mean ~ ets
## models never overlap, for any time point
## ets and gr_mean have numerically similar, but non-identical scores

## uncertainty

posterior_samples <- generate_samples_fitted(
  influenza_data,
  rps_gam_influenza,
  .n_pi_samples = 5000,
  mvn_method = "mgcv"
) |>
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

influenza_rps_mean_model_effect <- mean_model_prediction |>
  dplyr::rename(model_average = .value) |>
  # shape of effect will be the same regardless of location level
  dplyr::left_join(
    mean_prediction |>
      dplyr::rename("overall_mean" = .value),
    by = c("t_", "location_level", ".sample")
  ) |>
  dplyr::mutate(
    effect = 100 * (model_average - overall_mean) / overall_mean,
    disease = "Influenza",
    score = "RPS"
  )

# stack up all model effects

all_mean_model_effect <- dplyr::bind_rows(
  covid_pcwis_mean_model_effect,
  covid_rps_mean_model_effect,
  influenza_pcwis_mean_model_effect,
  influenza_rps_mean_model_effect
)

# save for later use
data_dir <- fs::dir_create("publication/data")

saveRDS(
  all_mean_model_effect,
  here::here(data_dir, "all_mean_model_effect.rds")
)

mean_effect_plots <- all_mean_model_effect |>
  dplyr::select(t_, effect, name, location_level, disease, score) |>
  dplyr::group_by(name, t_, location_level, disease, score) |>
  ggdist::median_qi(.width = 0.9) |>
  dplyr::left_join(
    dplyr::bind_rows(
      log_mean_wis_covid,
      log_mean_rps_covid,
      log_mean_wis_influenza,
      log_mean_rps_influenza
    ),
    by = c("t_", "location_level", "disease", "score")
  ) |>
  dplyr::filter(location_level == "nation") |>
  # copying to retain disease after nesting
  dplyr::mutate(disease_copy = disease) |>
  tidyr::nest(data = -c(disease)) |>
  dplyr::mutate(
    plt = purrr::map(
      data,
      \(df) {
        plot_title <- glue::glue(
          "Average effect scoring rules induced including a model in an ensemble of size three for {chosen_disease}",
          chosen_disease = dplyr::if_else(unique(df$disease_copy) == "covid-19", "COVID-19", "Influenza")
        ) |>
          stringr::str_wrap(70)

        df |>
          dplyr::left_join(model_code_to_name, by = dplyr::join_by(name)) |>
          ggplot2::ggplot(
            ggplot2::aes(
              x = prediction_start_date,
              y = effect / 100,
              ymin = .lower / 100,
              ymax = .upper / 100,
              group = name
            )
          ) +
          ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 2) +
          ggplot2::geom_line() +
          ggplot2::geom_ribbon(alpha = 0.2) +
          ggplot2::scale_fill_brewer() +
          ggplot2::ggtitle(
            plot_title,
            subtitle = "Median effect with 90% credible interval"
          ) +
          ggplot2::labs(
            x = "Prediction start date",
            y = "Percentage difference from mean score\n(negative implies improvement)"
          ) +
          ggplot2::scale_y_continuous(labels = scales::percent) +
          ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(size = 6)) +
          ggplot2::facet_grid(ggplot2::vars(score), ggplot2::vars(full_name), scales = "free_y")
      }
    ),
    .by = "disease"
  ) |>
  dplyr::mutate(
    #  hardcoded - will need to change if figures change
    figure_number = 3:4
  )

purrr::pwalk(
  mean_effect_plots,
  \(disease, data, plt, figure_number) {
    ggplot2::ggsave(
      filename = here::here(plot_output_dir, glue::glue("{disease}_percent_change.png")),
      plot = plt,
      width = 16,
      height = 12
    )

    fname <- glue::glue("fig_{figure_number}.tiff")

    ggplot2::ggsave(
      filename = here::here(plot_output_dir_tiff, fname),
      plot = plt,
      width = 19,
      height = 14.25,
      dpi = 300,
      units = "cm"
    )
  }
)


## SUPPLEMENTARY PLOTS ----

covid_influenza_summary <- summary_retrospective |>
  dplyr::filter(
    location_level == "nation",
    disease %in% c("covid-19", "influenza"),
    date >= plot_start_date
  ) |>
  dplyr::collect()

covid_individual <- covid_influenza_summary |>
  dplyr::filter(
    disease == "covid-19",
    date >= prediction_start_date,
    date < prediction_start_date + 14,
    !grepl("ensemble", model),
    model != "gam_dow"
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      x = date,
      ymin = pi_5,
      ymax = pi_95,
      group = prediction_start_date,
      fill = as.factor(prediction_start_date)
    ),
    alpha = 0.25
  ) +
  ggplot2::geom_line(
    ggplot2::aes(x = date, y = pi_50, group = prediction_start_date, colour = as.factor(prediction_start_date))
  ) +
  ggplot2::geom_point(ggplot2::aes(x = date, y = target_value)) +
  ggplot2::facet_wrap(ggplot2::vars(model), ncol = 3) +
  ggplot2::coord_cartesian(ylim = c(0, 600)) +
  ggplot2::labs(
    x = "Admission date",
    y = "COVID-19 hosptial admissions"
  ) +
  ggplot2::ggtitle(
    "Retrospective COVID-19 forecasts",
    "Individual COVID-19 admissions forecasts at national geography by prediction start date"
  ) +
  ggplot2::theme(
    legend.position = "none",
    strip.text.x = ggplot2::element_text(size = 16),
    plot.title = ggplot2::element_text(size = 18),
    plot.subtitle = ggplot2::element_text(size = 16)
  )

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "SUPPLEMENT_covid_retrospective.png"),
  plot = covid_individual,
  width = 16,
  height = 12
)

influenza_individual <- covid_influenza_summary |>
  dplyr::filter(
    disease == "influenza",
    date >= prediction_start_date,
    date < prediction_start_date + 14,
    !grepl("ensemble", model),
    model != "gam_dow"
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      x = date,
      ymin = pi_5,
      ymax = pi_95,
      group = prediction_start_date,
      fill = as.factor(prediction_start_date)
    ),
    alpha = 0.25
  ) +
  ggplot2::geom_line(
    ggplot2::aes(x = date, y = pi_50, group = prediction_start_date, colour = as.factor(prediction_start_date))
  ) +
  ggplot2::geom_point(ggplot2::aes(x = date, y = target_value)) +
  ggplot2::facet_wrap(ggplot2::vars(model), ncol = 3) +
  ggplot2::coord_cartesian(ylim = c(0, 1600)) +
  ggplot2::labs(
    x = "Admission date",
    y = "Influenza hosptial admissions"
  ) +
  ggplot2::ggtitle(
    "Retrospective influenza forecasts",
    "Individual Influenza admissions forecasts at national geography by prediction start date"
  ) +
  ggplot2::theme(
    legend.position = "none",
    strip.text.x = ggplot2::element_text(size = 16),
    plot.title = ggplot2::element_text(size = 18),
    plot.subtitle = ggplot2::element_text(size = 16)
  )

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "SUPPLEMENT_influenza_retrospective.png"),
  plot = influenza_individual,
  width = 16,
  height = 12
)
# Quick calculation: predictive interval coverage 90% and 90%

coverage_statistics <- summary_retrospective |>
  dplyr::filter(
    date >= prediction_start_date,
    date < prediction_start_date + lubridate::days(14),
    grepl("ensemble", model),
    disease %in% c("covid-19", "influenza")
  ) |>
  dplyr::select(
    disease,
    target_name,
    target_value,
    pi_5,
    pi_95,
    pi_25,
    pi_75,
    date,
    prediction_start_date,
    location_level
  ) |>
  dplyr::mutate(
    in_interval_90 = dplyr::if_else(target_value >= pi_5 & target_value <= pi_95, 1, 0),
    in_interval_50 = dplyr::if_else(target_value >= pi_25 & target_value <= pi_75, 1, 0)
  ) |>
  # collect before summarise: sql rounding/type conversion
  dplyr::collect() |>
  dplyr::summarise(
    coverage_90 = mean(in_interval_90, na.rm = TRUE),
    coverage_50 = mean(in_interval_50, na.rm = TRUE),
    .by = c(disease, target_name, location_level)
  )
coverage_statistics

## mean scores over season

retro_scores_comparison <- dplyr::bind_rows(
  individual_retrospective_scores |>
    dplyr::mutate(model_date = as.Date(model_date)),
  retrospective_ensemble_scores,
  published_model_scores
) |>
  dplyr::group_by(disease, model, metric_name) |>
  dplyr::arrange(prediction_start_date) |>
  dplyr::filter(
    prediction_start_date <= "2025-05-01",
    lubridate::wday(prediction_start_date, label = TRUE) != "Wed"
  ) |>
  dplyr::mutate(diff = prediction_start_date - dplyr::lag(prediction_start_date)) |>
  dplyr::rowwise() |>
  dplyr::filter(isFALSE(diff < 2) | is.na(diff)) |>
  dplyr::ungroup() |>
  # add a gap for no forecasts over christmas/new year period
  dplyr::bind_rows(
    tibble::tibble(
      model_type = rep("Operational Ensemble", 4),
      prediction_start_date = rep(as.Date("2024-12-25"), 4),
      model = rep("ensemble_stack", 4),
      metric_value = rep(NA_real_, 4),
      metric_name = c("rps", "rps", "wis", "wis"),
      disease = c("covid-19", "influenza", "covid-19", "influenza")
    )
  )

first_psd_disease <- retro_scores_comparison |>
  dplyr::summarise(min_psd = min(prediction_start_date), .by = disease) |>
  dplyr::mutate(disease = tolower(disease))

reported_mean_scores <- retro_scores_comparison |>
  dplyr::left_join(first_psd_disease, by = "disease") |>
  dplyr::filter(
    model == "ensemble_stack" | model_type == "Operational",
    prediction_start_date >= min_psd,
    metric == "admissions"
  ) |>
  dplyr::mutate(scale = dplyr::coalesce(scale, "natural")) |>
  tidyr::drop_na(metric_value) |>
  dplyr::filter(
    if (unique(metric_name) == "wis") scale == "per_capita" else TRUE,
    .by = metric_name
  ) |>
  dplyr::summarise(
    mean_score = mean(metric_value),
    .by = c(metric_name, disease, scale)
  )

operational_dates <- retro_scores_comparison |>
  dplyr::left_join(first_psd_disease, by = "disease") |>
  dplyr::filter(
    model == "ensemble_stack" | model_type == "Operational",
    prediction_start_date >= min_psd
  ) |>
  dplyr::mutate(scale = dplyr::coalesce(scale, "natural")) |>
  tidyr::drop_na(metric_value) |>
  dplyr::filter(
    if (unique(metric_name) == "wis") scale == "per_capita" else TRUE,
    .by = metric_name
  ) |>
  dplyr::distinct() |>
  dplyr::pull(prediction_start_date)

reported_mean_scores

retro_mean_scores <- retro_scores_comparison |>
  dplyr::left_join(first_psd_disease, by = "disease") |>
  dplyr::filter(
    model_type == "Operational Ensemble",
    prediction_start_date >= min_psd
  ) |>
  dplyr::mutate(scale = dplyr::coalesce(scale, "natural")) |>
  tidyr::drop_na(metric_value) |>
  dplyr::filter(
    if (unique(metric_name) == "wis") scale == "per_capita" else TRUE,
    .by = metric_name
  ) |>
  dplyr::filter(prediction_start_date %in% operational_dates) |>
  dplyr::summarise(
    mean_score = mean(metric_value),
    .by = c(model, metric_name, disease, scale)
  )

# summary stats of retro_mean_scores
retro_mean_scores |>
  dplyr::summarise(
    lo_mean = min(mean_score),
    overall_mean_score = mean(mean_score),
    hi_mean = max(mean_score),

    .by = c(metric_name, disease, scale)
  )

# quanitles (eCDF)

retro_mean_scores |>
  dplyr::summarise(ecdf = list(ecdf(mean_score)), .by = c(metric_name, disease, scale)) |>
  dplyr::left_join(dplyr::select(reported_mean_scores, -scale)) |>
  dplyr::mutate(
    percentile = purrr::map2(
      ecdf,
      mean_score,
      \(.f, .x) {
        .f(.x)
      }
    )
  )


## analysis of on a week-by-week basis, how often the published ensemble better than 50% of models

retrospective_ensemble_scores |>
  dplyr::group_by(metric_name, disease, prediction_start_date, scale) |>
  dplyr::summarise(
    median_score = median(metric_value)
  )


empty_forecasts <- holiday_break() |>
  tidyr::expand_grid(
    disease = c("covid", "influenza"),
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

forecast_start_dates <- admissions_summary |>
  dplyr::filter(disease %in% c("covid", "influenza")) |>
  dplyr::slice_min(prediction_start_date, by = "disease", with_ties = FALSE) |>
  dplyr::select(disease, prediction_start_date) |>
  dplyr::mutate(disease = disease_facet_labels(disease)) |>
  dplyr::rename("min_psd" = prediction_start_date)


normalised_admissions <- observed_admissions |>
  tidyr::drop_na(prediction_start_date) |>
  dplyr::slice_max(date, by = c("disease", "prediction_start_date")) |>
  dplyr::select(disease, prediction_start_date, observed_target) |>
  dplyr::distinct() |>
  dplyr::bind_rows(holiday_period_admissions) |>
  dplyr::select(disease, prediction_start_date) |>
  dplyr::mutate(
    all_data = purrr::map2(
      disease,
      prediction_start_date,
      \(.disease, .psd) {
        observed_by_geography |>
          dplyr::filter(
            disease == .disease,
            metric == "admissions",
            date <= .psd,
            date > .psd - lubridate::days(7),
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
  ) |>
  dplyr::mutate(disease = disease_facet_labels(disease)) |>
  dplyr::left_join(forecast_start_dates, by = "disease") |>
  dplyr::filter(prediction_start_date >= min_psd)

trend_direction_data <- admissions_summary |>
  dplyr::filter(is_reported_model | n_models == 1) |>
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
  dplyr::filter(
    prediction_start_date <= max(normalised_admissions$prediction_start_date)
  ) |>
  dplyr::mutate(
    trend_direction = stringr::str_remove(trend_direction, "p_") |>
      ordered(levels = c("decrease", "stable", "increase"))
  ) |>
  dplyr::select(trend_direction, trend_probability, prediction_start_date, observed_target, disease) |>
  dplyr::bind_rows(empty_forecasts) |>
  dplyr::arrange(prediction_start_date) |>
  dplyr::mutate(
    trend_direction = forcats::fct_rev(trend_direction),
    disease = disease_facet_labels(disease)
  )

trend_dates <- trend_direction_data |>
  dplyr::summarise(
    start = min(prediction_start_date),
    end = max(prediction_start_date),
    .by = "disease"
  )
trend_direction_plot <- trend_direction_data |>
  ggplot2::ggplot() +
  ggplot2::geom_area(
    ggplot2::aes(fill = trend_direction, y = trend_probability, x = prediction_start_date),
    position = "fill"
  ) +
  ggplot2::scale_fill_manual(values = probability_colours) +
  ggplot2::facet_wrap(ggplot2::vars(disease), ncol = 2) +
  ggplot2::labs(
    x = "Prediction start date",
    y = "Trend direction probability",
    fill = "Trend direction",
    colour = ""
  ) +
  ggplot2::ggtitle(
    "Trend direction probabilities",
    subtitle = "Colours indicate probability of a given direction."
  ) +
  ggplot2::theme(legend.box = "vertical") +
  ggplot2::scale_x_date(
    limits = as.Date(c("2024-10-07", "2025-03-17"))
  )

# calculate true trend directions

observed_trend <- ordinal_ready |>
  tibble::tibble() |>
  dplyr::left_join(published_min_date, by = "disease") |>
  dplyr::filter(
    date <= "2025-04-01", # arbitrary date after the season
    date >= min_psd,
    .by = "disease"
  ) |>
  dplyr::mutate(disease = disease_facet_labels(disease)) |>
  dplyr::filter(model == "ensemble_mean", location_level == "nation") |>
  dplyr::filter(
    date == max(date),
    .by = c("prediction_start_date", "disease")
  ) |>
  dplyr::distinct(observed, prediction_start_date, disease) |>
  dplyr::arrange(prediction_start_date, .by = "disease")


observed_trend <- ordinal_ready |>
  tibble::tibble() |>
  dplyr::mutate(disease = disease_facet_labels(disease)) |>

  dplyr::left_join(trend_dates, by = "disease") |>
  dplyr::filter(
    prediction_start_date <= end, # arbitrary date after the season
    prediction_start_date >= start,
    .by = "disease"
  ) |>
  dplyr::mutate(disease = disease_facet_labels(disease)) |>
  dplyr::filter(model == "ensemble_mean", location_level == "nation") |>
  dplyr::filter(
    date == max(date),
    .by = c("prediction_start_date", "disease")
  ) |>
  dplyr::distinct(observed, prediction_start_date, disease) |>
  dplyr::arrange(prediction_start_date, .by = "disease")


observed_trend_plot <- observed_trend |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(
    ggplot2::aes(fill = observed, y = 1, x = prediction_start_date, width = 8),
    position = "fill"
  ) +
  ggplot2::scale_fill_manual(values = probability_colours) +
  ggplot2::facet_wrap(ggplot2::vars(disease), ncol = 2) +
  ggplot2::labs(
    x = "Prediction start date",
    y = "Trend direction",
    fill = "Trend direction"
  ) +
  ggplot2::ggtitle("", subtitle = "Observed trend direction") +
  ggplot2::theme(
    legend.position = "none",
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank()
  ) +
  ggplot2::scale_x_date(
    limits = as.Date(c("2024-10-07", "2025-03-17"))
  )

combined_trend_plot <- patchwork::wrap_plots(
  trend_direction_plot,
  observed_trend_plot,
  ncol = 1
) +
  patchwork::plot_layout(
    guides = "collect",
    axis_titles = "collect",
    heights = c(3, 2)
  )

ggplot2::ggsave(
  here::here(plot_output_dir, "SUPPLEMENT_trend_direction_probs.png"),
  plot = combined_trend_plot,
  width = 16,
  height = 12
)

retrospective_mean_scores <- retrospective_ensemble_scores |>
  dplyr::summarise(
    mean_retrospective_score = mean(metric_value, na.rm = TRUE),
    .by = c(metric_name, disease, scale)
  )


operational_mean_scores <- retro_scores_comparison |>
  dplyr::left_join(first_psd_disease, by = "disease") |>
  dplyr::filter(
    model == "ensemble_stack" | model_type == "Operational",
    prediction_start_date >= min_psd
  ) |>
  dplyr::mutate(scale = dplyr::coalesce(scale, "natural")) |>
  tidyr::drop_na(metric_value) |>
  dplyr::filter(
    if (unique(metric_name) == "wis") scale == "per_capita" else TRUE,
    .by = metric_name
  ) |>
  dplyr::filter(metric == "admissions") |>
  dplyr::select(
    model,
    prediction_start_date,
    metric_value,
    metric_name,
    metric,
    disease
  ) |>
  dplyr::summarise(
    mean_operational_score = mean(metric_value),
    .by = c(metric_name, disease)
  )

scores_percent_change <- dplyr::left_join(
  operational_mean_scores,
  retrospective_mean_scores,

  by = c(
    "metric_name",
    "disease"
  )
) |>
  dplyr::mutate(
    percent_change = 100 * (mean_operational_score - mean_retrospective_score) / mean_retrospective_score
  )
scores_percent_change

# Calibration plots

interval_90 <- function(...) {
  scoringutils::interval_coverage(..., interval_range = 90)
}

coverage_statistics <- wis_ready |>
  dplyr::mutate(disease = disease_facet_labels(disease)) |>
  dplyr::left_join(trend_dates, by = "disease") |>
  dplyr::filter(
    prediction_start_date >= start,
    prediction_start_date <= end
  ) |>
  dplyr::filter(
    model == "ensemble_stack",
    disease %in% c("COVID-19", "Influenza")
  ) |>
  scoringutils::as_forecast_quantile(forecasting_unit) |>
  scoringutils::score(metrics = list(int_90 = interval_90)) |>
  scoringutils::summarise_scores(
    by = c("prediction_start_date", "model", "disease", "metric", "location", "location_level")
  )

coverage_plot <- coverage_statistics |>
  dplyr::bind_rows(
    holiday_break_tibble |>
      tidyr::expand_grid(
        dplyr::distinct(coverage_statistics, location, location_level)
      ) |>
      dplyr::rename("int_90" = score_value)
  ) |>
  dplyr::arrange(prediction_start_date) |>
  dplyr::summarise(
    upr = max(int_90),
    mean = mean(int_90),
    lwr = min(int_90),
    .by = c("prediction_start_date", "disease", "location_level")
  ) |>
  dplyr::mutate(
    location_level = dplyr::case_when(
      location_level == "nation" ~ "Nation",
      location_level == "region" ~ "Region",
      location_level == "icb" ~ "ICB"
    ),

    location_level = ordered(location_level, c("Nation", "Region", "ICB"))
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    ggplot2::aes(x = prediction_start_date, ymin = lwr, ymax = upr),
    alpha = 0.5,
    fill = themes$select_ukhsa_colour("orange")
  ) +
  ggplot2::geom_line(
    ggplot2::aes(x = prediction_start_date, y = mean, colour = "Observed")
  ) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0.9, colour = "Nominal")) +
  ggplot2::labs(x = "Prediction start date", y = "Coverage", colour = "Coverage") +
  ggplot2::scale_colour_manual(
    values = c(
      "Nominal" = themes$select_ukhsa_colour("teal"),
      "Observed" = themes$select_ukhsa_colour("orange")
    )
  ) +
  ggplot2::facet_grid(ggplot2::vars(location_level), ggplot2::vars(disease)) +
  ggplot2::ggtitle(
    "Observed vs Empircal 90% Coverage Statistics",
    subtitle = "Orange lines are mean coverage statistics per prediction start date, the bands show minimum and maximum values across a spatial granularity level"
  )

ggplot2::ggsave(
  here::here(plot_output_dir, "SUPPLEMENT_coverage_statistics.png"),
  plot = coverage_plot,
  width = 16,
  height = 12
)

## scoring all operational models

pre_score_operational <- summary |>
  dplyr::filter(
    disease %in% c("covid-19", "influenza"),
    metric == "admissions",
    model_in_ensemble %in% c("included", "ensemble"),
    date >= prediction_start_date,
    date <= prediction_start_date + lubridate::days(13),
    date <= "2025-04-01"
  ) |>
  dplyr::mutate(
    model = dplyr::if_else(
      grepl("unweighted_mellor_ensemble_", model),
      "Operational ensemble",
      model
    ),

    model = dplyr::if_else(model == "", "null", model)
  ) |>
  dplyr::collect() |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("pi_"),
    values_to = "predicted",
    names_to = "quantile_level"
  ) |>
  dplyr::mutate(
    quantile_level = stringr::str_remove(quantile_level, "pi_"),
    quantile_level = as.numeric(quantile_level) / 100,
    disease = disease_facet_labels(disease)
  ) |>
  dplyr::select(-target_value) |>
  dplyr::left_join(
    observed_by_geography |> dplyr::mutate(disease = disease_facet_labels(disease)),
    by = c("date", "location", "location_level", "metric", "disease", "age_group")
  ) |>
  dplyr::select(
    !dplyr::starts_with("p_"),
    "observed" = observed_target
  ) |>
  dplyr::mutate(target_name = "admissions") |>
  dplyr::filter(
    model_date == min(model_date, na.rm = TRUE),
    .by = c("model", "prediction_start_date", "disease")
  ) |>
  dplyr::distinct()

scored_operational <- pre_score_operational |>
  dplyr::select(!dplyr::starts_with("p_")) |>
  dplyr::mutate(target_name = "admissions") |>
  dplyr::distinct() |>
  scoringutils::as_forecast_quantile(
    forecast_unit = forecasting_unit
  ) |>
  scoringutils::transform_forecasts(fun = divide, label = "per_capita", y = pre_score_operational$population) |>
  scoringutils::score(metrics = c(wis = scoringutils::wis)) |>
  dplyr::mutate(score_name = "pcWIS") |>
  dplyr::rename("score" = wis)


rps_pre_score_operational <- summary |>
  dplyr::select(
    model,
    date,
    prediction_start_date,
    location,
    location_level,
    age_group,
    age_group_granularity,
    target_value,
    target_name,
    p_increase,
    p_stable,
    p_decrease,
    disease,
    metric,
    model_date,
    model_in_ensemble,
    population
  ) |>
  dplyr::filter(
    disease %in% c("covid-19", "influenza"),
    metric == "admissions",
    model_in_ensemble %in% c("included", "ensemble"),
    date >= prediction_start_date,
    date <= prediction_start_date + lubridate::days(13),
    date <= "2025-04-01"
  ) |>
  dplyr::mutate(
    model = dplyr::if_else(
      grepl("unweighted_mellor_ensemble_", model),
      "Operational ensemble",
      model
    ),
    is_reported_model = (model == "Operational ensemble")
  ) |>
  dplyr::distinct() |>
  dplyr::collect()

rps_operational_scoring_ready <- rps_pre_score_operational |>
  dplyr::group_by(
    model,
    prediction_start_date,
    location,
    location_level,
    age_group,
    age_group_granularity,
    disease,
    metric,
    model_date,
    population
  ) |>
  dplyr::slice_max(date) |>
  dplyr::left_join(
    lagged_values,
    by = dplyr::join_by(date, location, location_level, age_group, disease, metric)
  ) |>
  #dplyr::mutate(observed_trend = classify_trend(observed, observed_lag)) |>
  dplyr::slice_max(date) |>
  # is a row-wise operation
  most_likely_trend() |>
  dplyr::ungroup() |>
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
  dplyr::mutate(predicted = predicted / sum(predicted), .by = dplyr::all_of(forecasting_unit))

rps_operational_scored <- rps_operational_scoring_ready |>
  scoringutils::as_forecast_ordinal(forecast_unit = c(forecasting_unit, "is_reported_model")) |>
  scoringutils::score() |>
  dplyr::mutate(score_name = "RPS") |>
  dplyr::rename("score" = rps)


summary_unit <- c(
  "model",
  "prediction_start_date",
  "location",
  "location_level",
  "age_group",
  "age_group_granularity",
  "disease",
  "metric",
  "score_name"
)

op_summary <- dplyr::bind_rows(
  scored_operational |> dplyr::filter(scale == "per_capita"),
  rps_operational_scored |> dplyr::mutate(scale = "per_capita")
) |>
  dplyr::mutate(disease = disease_facet_labels(disease)) |>
  tidyr::drop_na(model) |>
  dplyr::summarise(score = mean(score, na.rm = TRUE), .by = dplyr::all_of(summary_unit))

gg_colour_hue <- function(variable_names) {
  n <- length(variable_names)

  hues <- seq(15, 375, length = n + 1)
  colours <- hcl(h = hues, l = 65, c = 100)[1:n]

  names(colours) <- variable_names

  colours
}

model_names <- model_code_to_name |>
  dplyr::filter(name != "gam_tp") |>
  dplyr::pull(full_name)

pal <- RColorBrewer::brewer.pal(length(model_names) + 1, "Dark2")
names(pal) <- c(model_names, "Operational ensemble")
pal[names(pal) == "Operational ensemble"] = "black"


operational_scores_plot_nation <- op_summary |>
  tibble::tibble() |>
  dplyr::filter(prediction_start_date %in% operational_dates) |>
  dplyr::filter(location_level == "nation") |>
  tidyr::complete(prediction_start_date, tidyr::nesting(disease, score_name), fill = list(score = NA_real_)) |>
  dplyr::group_by(disease, model, score_name) |>
  dplyr::arrange(prediction_start_date) |>
  dplyr::mutate(lead_time = dplyr::lead(prediction_start_date) - prediction_start_date) |>
  dplyr::filter(dplyr::lead(prediction_start_date) - prediction_start_date > 5) |>
  dplyr::ungroup() |>
  dplyr::bind_rows(
    holiday_break_tibble |>
      dplyr::select(prediction_start_date, disease) |>
      dplyr::distinct() |>
      tidyr::expand_grid(score_name = c("pcWIS", "RPS")) |>
      dplyr::right_join(
        scored_operational |>
          dplyr::distinct(model, disease),
        by = "disease",
        relationship = "many-to-many"
      )
  ) |>
  tidyr::drop_na(model) |>
  dplyr::left_join(
    model_code_to_name |>
      dplyr::add_row(name = "Operational ensemble", full_name = "Operational ensemble"),
    by = dplyr::join_by(model == name)
  ) |>
  dplyr::mutate(
    is_ensemble = (model == "Operational ensemble"),
    score = log(score)
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(
    ggplot2::aes(x = prediction_start_date, y = (score), colour = full_name, linewidth = as.character(is_ensemble))
  ) +
  ggplot2::scale_linewidth_manual(
    values = c("TRUE" = 1.5, "FALSE" = 0.8),
    guide = "none"
  ) +
  ggplot2::scale_colour_manual(values = pal) +
  ggplot2::facet_grid(ggplot2::vars(score_name), ggplot2::vars(disease), scale = "free_y") +
  ggplot2::ggtitle(
    "log(pcWIS) and log(RPS) for operational ensemble and models used in real-time"
  ) +
  ggplot2::labs(
    x = "Prediction start date",
    y = "log(Score)",
    colour = "Model"
  ) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 14), legend.text = ggplot2::element_text(size = 11))

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "national_score_comparison.png"),
  plot = operational_scores_plot_nation,
  width = 16,
  height = 12
)

ggplot2::ggsave(
  filename = here::here(plot_output_dir_tiff, "fig_2.tiff"),
  plot = operational_scores_plot_nation,
  width = 19,
  height = 14.25,
  dpi = 300,
  units = "cm"
)

operational_scores_plot_facet <- op_summary |>
  dplyr::filter(prediction_start_date %in% operational_dates) |>
  tibble::tibble() |>
  dplyr::summarise(
    score = mean(score),
    .by = c(disease, prediction_start_date, score_name, location_level, model)
  ) |>
  tidyr::complete(prediction_start_date, tidyr::nesting(disease, score_name), fill = list(score = NA_real_)) |>
  dplyr::group_by(disease, model, score_name, location_level) |>
  dplyr::arrange(prediction_start_date) |>
  dplyr::mutate(lead_time = dplyr::lead(prediction_start_date) - prediction_start_date) |>
  dplyr::filter(dplyr::lead(prediction_start_date) - prediction_start_date > 5) |>
  dplyr::ungroup() |>
  dplyr::bind_rows(
    holiday_break_tibble |>
      dplyr::select(prediction_start_date, disease) |>
      dplyr::distinct() |>
      tidyr::expand_grid(score_name = c("pcWIS", "RPS"), location_level = c("icb", "region", "nation")) |>
      dplyr::right_join(
        scored_operational |>
          dplyr::distinct(model, disease),
        by = "disease",
        relationship = "many-to-many"
      )
  ) |>
  dplyr::mutate(
    location_level = dplyr::case_when(
      location_level == "icb" ~ "ICB",
      location_level == "region" ~ "Region",
      location_level == "nation" ~ "Nation"
    ),
    location_level = ordered(location_level, levels = c("ICB", "Region", "Nation")),
    facet_name = glue::glue("{disease} | {score_name}")
  ) |>
  dplyr::left_join(
    model_code_to_name |> tibble::add_row(name = "Operational ensemble", full_name = "Operational ensemble"),
    by = c("model" = "name")
  ) |>
  dplyr::select(-model) |>
  dplyr::rename("model" = full_name) |>
  tidyr::drop_na(model) |>
  dplyr::mutate(
    is_ensemble = (model == "Operational ensemble"),
    score = log(score)
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(
    ggplot2::aes(x = prediction_start_date, y = (score), colour = model, linewidth = as.character(is_ensemble))
  ) +
  ggplot2::scale_linewidth_manual(
    values = c("TRUE" = 1.5, "FALSE" = 0.8),
    guide = "none"
  ) +
  ggplot2::scale_colour_manual(values = pal) +
  ggplot2::facet_wrap(ggplot2::vars(facet_name, location_level), scale = "free_y", ncol = 3) +
  ggplot2::ggtitle(
    "log scores for operational ensemble and models used in real-time",
    "Scores per prediction start date are averaged across all locations at a given spatial granularity"
  ) +
  ggplot2::labs(
    x = "Prediction start date",
    y = "log(Score)",
    colour = "Model"
  )

operational_scores_plot_facet

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "facet_score_comparison.png"),
  plot = operational_scores_plot_facet,
  width = 16,
  height = 12
)

### summary of scores

matched_ensemble_scores <- rps_scored |>
  dplyr::left_join(
    ensembles_scored,

    by = c(
      "model",
      "prediction_start_date",
      "location",
      "location_level",
      "age_group",
      "age_group_granularity",
      "date",
      "disease",
      "metric"
    ),
    suffix = c("_rps", "_wis")
  ) |>
  dplyr::mutate(population = population_rps) |>
  dplyr::filter(
    disease %in% c("covid-19", "influenza"),
    model == "ensemble_matched"
  ) |>
  tidyr::pivot_longer(
    cols = c(wis, rps),
    names_to = "metric_name",
    values_to = "metric_value"
  ) |>
  # model_type = ensemble_matched
  dplyr::mutate(model_type = dplyr::if_else(model == "ensemble_matched", "Matched Ensemble", "Sub-Ensemble")) |>
  dplyr::rename("score" = metric_value, "score_name" = metric_name) |>
  dplyr::select(dplyr::all_of(colnames(op_summary))) |>
  dplyr::filter(model == "ensemble_matched") |>
  dplyr::mutate(
    model_type = "Matched ensemble",
    disease = disease_facet_labels(disease),
    score_name = dplyr::case_when(
      score_name == "wis" ~ "pcWIS",
      score_name == "rps" ~ "RPS",
      .default = NA_character_
    )
  )

min_date_disease <- matched_ensemble_scores |>
  dplyr::summarise(
    min_psd = min(prediction_start_date),
    .by = disease
  )

matched_dates <- unique(matched_ensemble_scores$prediction_start_date)
published_min_date <- retro_scores_comparison |>
  dplyr::filter(model == "ensemble_stack") |>
  dplyr::summarise(min_psd = min(prediction_start_date), .by = "disease")


upstream_format <- op_summary |>
  dplyr::rowwise() |>
  dplyr::filter(
    # check this line ok
    if (model == "Operational ensemble") prediction_start_date %in% matched_dates else TRUE
  ) |>
  dplyr::ungroup() |>
  dplyr::left_join(min_date_disease, by = "disease") |>
  dplyr::filter(prediction_start_date >= min_psd) |>
  tibble::tibble() |>
  dplyr::mutate(
    model_type = dplyr::if_else(model == "Operational ensemble", "Operational ensemble", "Individual models")
  ) |>
  dplyr::bind_rows(matched_ensemble_scores) |>
  tibble::tibble() |>
  dplyr::mutate(
    # add in subensemble type later
    model_type = dplyr::case_when(
      model == "Operational ensemble" ~ "Operational Ensemble",
      model == "ensemble_matched" ~ "Matched Ensemble",
      .default = "Individual Models"
    )
  ) |>
  # looks like overkill
  #dplyr::bind_rows(matched_ensemble_scores) |>
  dplyr::left_join(published_min_date, by = "disease") |>
  #dplyr::filter(prediction_start_date >= min_psd) |>
  dplyr::summarise(
    score = mean(score, na.rm = TRUE),
    .by = c("model", "prediction_start_date", "model_type", "disease", "score_name", "location_level")
  ) |>
  # can only have one prediction per week, there are some dupes - use the first one
  dplyr::mutate(
    # monday of prediction week
    prediction_week = round_to_wednesday(prediction_start_date) - lubridate::days(2)
  ) |>
  dplyr::filter(prediction_start_date == min(prediction_start_date), .by = c(prediction_week, disease)) |>
  dplyr::select(-prediction_week)

operational_score_summary <- upstream_format |>
  dplyr::summarise(
    mean_score = mean(score, na.rm = TRUE),
    .by = c("model_type", "disease", "score_name", "location_level")
  )

operational_score_summary |>
  dplyr::summarise(
    mean_score = mean(mean_score, na.rm = TRUE),
    .by = c("model_type", "disease", "score_name", "location_level")
  ) |>
  dplyr::mutate(
    mean_score = signif(mean_score, 4),
    location_level = dplyr::case_when(
      location_level == "icb" ~ "ICB",
      location_level == "region" ~ "Region",
      location_level == "nation" ~ "Nation"
    ),
    location_level = ordered(location_level, levels = c("ICB", "Region", "Nation"))
  ) |>
  dplyr::arrange(
    disease,
    dplyr::desc(score_name),
    dplyr::desc(location_level),
    model_type
  ) |>
  gt::gt() |>
  gt::fmt_scientific(exp_style = "x10n") |>
  gt::gtsave(here::here(plot_output_dir, "ensemble_score_summary.docx"))

# fig 6 - retrospective scores

retro_plot_data <- upstream_format |>
  dplyr::filter(
    location_level == "nation",
    # the indiv models here' aren't the retrospective ones!
    model_type != "Individual Models"
  ) |>
  dplyr::select(-c(location_level)) |>
  dplyr::bind_rows(
    retrospective_ensemble_scores |>
      dplyr::filter(model != "ensemble_matched", location_level == "nation") |>
      dplyr::summarise(
        score = mean(metric_value),
        .by = c(model, disease, metric_name, prediction_start_date)
      ) |>
      dplyr::mutate(
        model_type = "Sub-Ensemble",
        disease = disease_facet_labels(disease),
        score_name = dplyr::if_else(
          metric_name == "rps",
          "RPS",
          "pcWIS"
        )
      )
  ) |>
  dplyr::bind_rows(
    individual_retrospective_scores |>
      dplyr::filter(location_level == "nation") |>
      dplyr::mutate(model_date = as.Date(model_date)) |>
      dplyr::summarise(
        score = mean(metric_value),
        .by = c(model, disease, metric_name, prediction_start_date)
      ) |>
      dplyr::mutate(
        model_type = "Individual",
        disease = disease_facet_labels(disease),
        score_name = dplyr::if_else(
          metric_name == "rps",
          "RPS",
          "pcWIS"
        )
      )
  ) |>
  dplyr::mutate(
    score = log(score),
    scale = "per_capita",
    model_type = ordered(
      model_type,
      levels = c("Individual", "Sub-Ensemble", "Matched Ensemble", "Operational Ensemble")
    )
  ) |>
  dplyr::mutate(
    # monday of prediction week
    prediction_week = round_to_wednesday(prediction_start_date) - lubridate::days(2)
  ) |>
  dplyr::filter(
    prediction_start_date == min(prediction_start_date),
    .by = c(prediction_week, disease, model_type, model)
  ) |>
  dplyr::select(-prediction_week)

retro_scores_comparison_plot <- retro_plot_data |>
  dplyr::group_split(score_name) |>
  purrr::map(
    \(input_df) {
      min_date <- input_df |>
        dplyr::filter(model_type == "Operational Ensemble") |>
        dplyr::summarise(first_date = min(prediction_start_date, na.rm = TRUE), .by = "disease")

      chosen_metric <- unique(input_df$score_name)
      metric_lab <- glue::glue("log({chosen_metric})")

      plot_data <- input_df |>
        dplyr::bind_rows(
          tidyr::expand_grid(
            prediction_start_date = as.Date("2024-12-25"),
            model_type = c("Matched Ensemble"),
            model = "ensemble_matched",
            disease = c("COVID-19", "Influenza")
          )
        ) |>
        dplyr::bind_rows(
          tidyr::expand_grid(
            prediction_start_date = as.Date("2024-12-25"),
            model_type = c("Operational Ensemble"),
            model = "Operational ensemble",
            disease = c("COVID-19", "Influenza")
          )
        ) |>
        dplyr::left_join(min_date, by = "disease") |>
        dplyr::filter(prediction_start_date >= first_date) |>
        dplyr::arrange(prediction_start_date, model)

      plot_data |>
        ggplot2::ggplot(
          ggplot2::aes(
            x = prediction_start_date,
            y = score,
            colour = model_type,
            group = model,
            linewidth = model_type
          )
        ) +
        ggplot2::geom_line() +
        ggplot2::geom_line(
          data = dplyr::filter(plot_data, model_type == "Operational Ensemble"),
          mapping = ggplot2::aes(x = prediction_start_date, y = score, colour = model_type, group = model)
        ) +
        ggplot2::geom_line(
          data = dplyr::filter(plot_data, model_type == "Matched Ensemble"),
          mapping = ggplot2::aes(x = prediction_start_date, y = score, colour = model_type, group = model)
        ) +
        ggplot2::scale_colour_manual(
          values = c(
            "Sub-Ensemble" = "#134074",
            "Individual" = "#bfab25",
            "Operational Ensemble" = "#df2935",
            "Matched Ensemble" = "cyan"
          )
        ) +
        ggplot2::ylab(metric_lab) +
        ggplot2::xlab("Prediction start date") +
        ggplot2::ggtitle("") +
        ggplot2::scale_linewidth_manual(
          values = c(
            "Sub-Ensemble" = 0.25,
            "Individual" = 0.25,
            "Operational Ensemble" = 1.2,
            "Matched Ensemble" = 1.2
          ),
          guide = "none"
        ) +
        ggplot2::labs(colour = "Model type") +
        ggplot2::facet_wrap(ggplot2::vars(disease), scales = "free", ncol = 2) +
        ggplot2::theme(plot.title = ggplot2::element_blank())
    }
  ) |>
  rev() |> # puts pcWIs on top
  patchwork::wrap_plots(ncol = 1) +
  patchwork::plot_annotation(
    "Comparison of retrospective models and ensembles to operational forecasts",
    "All scores are for national georaphy"
  ) +
  patchwork::plot_layout(
    guides = "collect",
    axis = "collect_x"
  ) &
  ggplot2::theme(legend.title = ggplot2::element_text(size = 14), legend.text = ggplot2::element_text(size = 10))


ggplot2::ggsave(
  here::here(plot_output_dir, "retrospective_to_ensemble_comparison.png"),
  retro_scores_comparison_plot,
  width = 16,
  height = 12
)

ggplot2::ggsave(
  filename = here::here(plot_output_dir_tiff, "fig_6.tiff"),
  plot = retro_scores_comparison_plot,
  width = 19,
  height = 14.25,
  dpi = 300,
  units = "cm"
)


# percent errors

operational_score_summary |>
  dplyr::mutate(
    mean_score = signif(mean_score, 4),
    location_level = dplyr::case_when(
      location_level == "icb" ~ "ICB",
      location_level == "region" ~ "Region",
      location_level == "nation" ~ "Nation"
    ),
    location_level = ordered(location_level, levels = c("ICB", "Region", "Nation")),
    age_group_granularity = "None"
  ) |>
  dplyr::arrange(
    dplyr::desc(score_name),
    disease,
    dplyr::desc(location_level),
    model_type
  ) |>
  dplyr::filter(location_level == "Nation") |>
  tidyr::pivot_wider(
    names_from = "model_type",
    values_from = "mean_score"
  ) |>
  janitor::clean_names() |>
  dplyr::mutate(pc_err = 100 * (operational_ensemble - individual_models) / individual_models)

mean_population <- summary |>
  dplyr::filter(disease == "influenza") |>
  dplyr::summarise(
    population = mean(population),
    .by = "location"
  ) |>
  dplyr::collect()

population_score_data <- op_summary |>
  dplyr::filter(model == "Operational ensemble") |>
  tidyr::pivot_wider(
    names_from = "score_name",
    values_from = "score"
  ) |>
  janitor::clean_names() |>
  dplyr::summarise(
    pc_wis = mean(pc_wis),
    rps = mean(rps),
    .by = c("disease", "location", "location_level")
  ) |>
  dplyr::left_join(mean_population, by = "location") |>
  dplyr::mutate(
    location_level = dplyr::case_when(
      location_level == "icb" ~ "ICB",
      location_level == "region" ~ "Region",
      location_level == "nation" ~ "Nation"
    ),
    location_level = ordered(location_level, levels = c("ICB", "Region", "Nation"))
  )

mean_location_level_scores <- population_score_data |>
  dplyr::summarise(
    pc_wis = mean(pc_wis),
    rps = mean(rps),
    .by = c("disease", "location_level")
  )

scientific_labels <- function(x) {
  parse(text = gsub("e\\+", " %*% 10^", scales::scientific_format()(x)))
}

population_score_plot <- population_score_data |>
  dplyr::mutate(pc_wis = log(pc_wis), rps = log(rps)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(
    ggplot2::aes(x = pc_wis, y = rps, colour = location_level, size = population),
    alpha = 0.6
  ) +
  ggplot2::facet_wrap(ggplot2::vars(disease), scales = "free") +
  ggplot2::labs(
    x = "log(pcWIS)",
    y = "log(RPS)",
    colour = "Location level",
    size = "Population"
  ) +
  ggplot2::scale_size_continuous(labels = scientific_labels)

population_score_plot

population_range_plot <- population_score_data |>
  dplyr::distinct(location_level, location, population) |>
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(
    ggplot2::aes(x = location_level, y = population)
  ) +
  ggplot2::labs(
    x = "Location level",
    y = "Population"
  ) +
  ggplot2::scale_y_log10(labels = scientific_labels) +
  ggplot2::coord_flip()

population_combined_plot <- patchwork::wrap_plots(
  population_score_plot,
  population_range_plot
) +
  patchwork::plot_layout(ncol = 1, heights = c(3, 1)) +
  patchwork::plot_annotation(
    title = "Operational ensemble predictive performance with population",
    subtitle = "pcWIS, RPS and population values averaged over entire season. Boxplot shows range of population values for each location level."
  )

population_combined_plot

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "score_population.png"),
  plot = population_combined_plot,
  width = 16,
  height = 12
)

# how often was the reported ordinal forecast correct?

ordinal_ready |>
  dplyr::filter(model == "ensemble_stack", metric == "admissions") |>
  dplyr::group_by(prediction_start_date, location, location_level, disease) |>
  dplyr::slice_max(predicted) |>
  dplyr::mutate(correct_prediction = (observed == predicted_label)) |>
  dplyr::ungroup() |>
  dplyr::summarise(
    pct_correct = mean(correct_prediction),
    .by = c("disease", "location_level")
  )
