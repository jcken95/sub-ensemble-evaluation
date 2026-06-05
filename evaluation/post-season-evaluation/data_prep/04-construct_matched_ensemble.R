# construct an ensemble (mean method) using retrospective model runs.
# the ensemble is "matched" to the operational ensemble.
# Each week, we use the same models in the matched ensemble as we did in the live operational ensemble to create a fair comparison
source("evaluation/post-season-evaluation/data_prep/01-load_data.R")
deps_$need(
  "aws.s3",
  "dplyr",
  "glue",
  "purrr",
  "s3fs",
  "stringr",
  "tidyr",
  "tidytable"
)

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

individual_models <- summary |>
  dplyr::filter(
    disease %in% c("covid-19", "influenza"),
    metric == "admissions",
    !grepl("ensemble", model),
    #  only need one location level, use the one with least rows
    location_level == "nation",
    !(model %in% c("epinow2_rw", "gam_dow", ""))
  ) |>
  dplyr::distinct(model) |>
  dplyr::pull(model)

models_regex <- stringr::str_c(individual_models, collapse = "|")

ensembles <- summary |>
  dplyr::filter(disease %in% c("covid-19", "influenza"), metric == "admissions", grepl("ensemble", model)) |>
  dplyr::select(model, prediction_start_date, model_date, disease) |>
  dplyr::distinct() |>
  dplyr::collect()


models_used <- ensembles |>
  dplyr::filter(prediction_start_date <= "2025-04-01") |>
  dplyr::rename("ensemble_name" = model) |>
  dplyr::mutate(
    models = list(tibble::tibble(model = individual_models)),

    models_in_ensemble = purrr::map2(
      ensemble_name,
      models,
      \(.ensemble, .models) {
        .models |>
          dplyr::mutate(
            in_ensemble = stringr::str_detect(.ensemble, model)
          )
      }
    )
  ) |>
  tidyr::unnest(cols = c(models_in_ensemble)) |>
  dplyr::filter(in_ensemble) |>
  dplyr::select(disease, prediction_start_date, model) |>
  dplyr::distinct()

scoring_ready <- summary_retrospective |>
  dplyr::filter(
    date >= prediction_start_date,
    model_date > "2024-09-01",
    disease %in% c("covid-19", "influenza"),
    model != "gam_dow",
    !grepl("ensemble", model),
    date <= "2025-04-01"
  ) |>
  dplyr::collect() |>
  dplyr::left_join(
    observed_by_geography |>
      dplyr::mutate(disease = dplyr::if_else(disease == "covid", "covid-19", disease)),
    by = c("date", "location", "location_level", "metric", "disease", "age_group")
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("pi_"),
    values_to = "predicted",
    names_to = "quantile_level"
  ) |>
  dplyr::mutate(quantile_level = as.numeric(stringr::str_remove(quantile_level, "pi_")) / 100) |>
  dplyr::mutate(
    n_models = dplyr::n_distinct(model),
    # the reported model was the ensemble; if no ensemble, then we only had one model, therefore it was reported
    .by = c(model_date, disease, metric)
  ) |>
  dplyr::rename(observed = observed_target) |>
  tidyr::drop_na(observed)


scoring_ready_production_only <- scoring_ready |>
  dplyr::right_join(
    models_used,
    by = dplyr::join_by(disease, prediction_start_date, model)
  ) |>
  tidyr::drop_na(prediction_start_date)

# construct a forecasting unit for the ensemble - essentially the same as the standard forecasting unit used in the analysis
# used for grouped computations
ensemble_unit <- c(
  forecasting_unit[!forecasting_unit == "model"],
  "quantile_level",
  "observed"
)

ensemble_mean <- scoring_ready_production_only |>
  dplyr::summarise(
    predicted = mean(predicted),
    p_increase = mean(p_increase),
    p_stable = mean(p_stable),
    p_decrease = mean(p_decrease),
    .by = dplyr::all_of(ensemble_unit)
  ) |>
  # note: need to normalise probabilities after averaging
  # as probabilities may not sum to 1
  dplyr::mutate(
    normalise_factor = (1 / (p_increase + p_stable + p_decrease)),
    p_increase = p_increase * normalise_factor,
    p_stable = p_stable * normalise_factor,
    p_decrease = p_decrease * normalise_factor,
    .by = dplyr::all_of(ensemble_unit)
  ) |>
  dplyr::select(-normalise_factor) |>
  tidyr::drop_na(quantile_level) |>
  dplyr::mutate(model = "ensemble_matched")

ensemble_s3_ready <- ensemble_mean |>
  dplyr::mutate(quantile_level = glue::glue("pi_{100 * quantile_level}")) |>
  tidyr::pivot_wider(
    names_from = "quantile_level",
    values_from = "predicted"
  ) |>
  dplyr::mutate(
    target_name = glue::glue("{stringr::str_remove(disease, '-19')}_admissions"),
    forecast_horizon = 14
  ) |>
  dplyr::relocate(
    c(
      "model",
      "prediction_start_date",
      "location",
      "location_level",
      "age_group",
      "age_group_granularity",
      "population",
      "target_name",
      "observed",
      "date",
      "forecast_horizon",
      "p_increase",
      "p_stable",
      "p_decrease",
      "pi_50",
      "pi_5",
      "pi_95",
      "pi_2.5",
      "pi_97.5",
      "pi_25",
      "pi_75",
      "pi_17",
      "pi_83",
      "disease",
      "metric",
      "model_date"
    )
  ) |>
  dplyr::filter(
    # retain only the models within the operational forecasting dates - i.e.e drop over christmas period
    prediction_start_date <= "2024-12-17" | prediction_start_date >= "2025-01-03",
    # after 13+ days, forecasts are null
    date <= prediction_start_date + lubridate::days(13)
  ) |>
  dplyr::collect()

ensemble_s3_ready |>
  dplyr::group_split(disease) |>
  purrr::walk(
    \(input_data) {
      disease <- unique(input_data$disease)
      model_name <- "ensemble_matched"
      s3_path <- glue::glue("REDACTED/{disease}_{model_name}.rds")

      out <- input_data |>
        tidytable::as_tidytable() |>
        tidytable::filter(date <= prediction_start_date + lubridate::days(13)) |>
        tidytable::pivot_longer(
          cols = dplyr::starts_with("pi_"),
          names_to = "quantile_level",
          values_to = "predicted"
        ) |>
        tidytable::mutate(
          quantile_level = as.numeric(stringr::str_remove(quantile_level, "pi_")),
          model = model_name
        ) |>
        #using dplyr not tidytable; tidytable causes some naming issues?
        dplyr::summarise(
          predicted = mean(predicted),
          p_increase = mean(p_increase),
          p_stable = mean(p_stable),
          p_decrease = mean(p_decrease),
          observed = unique(observed),
          .by = dplyr::all_of(ensemble_unit)
        ) |>
        tidytable::rowwise() |>
        tidytable::mutate(
          p_total = p_increase + p_stable + p_decrease,
          p_increase = p_increase / p_total,
          p_stable = p_stable / p_total,
          p_decrease = p_decrease / p_total
        ) |>
        tidytable::ungroup() |>
        dplyr::relocate(
          "prediction_start_date",
          "location",
          "location_level",
          "age_group",
          "age_group_granularity",
          "date",
          "disease",
          "metric",
          "model_date",
          "population",
          "quantile_level",
          "predicted",
          "p_increase",
          "p_stable",
          "p_decrease",
          "observed",
          "p_total"
        )

      aws.s3::s3write_using(out, saveRDS, object = s3_path)
    }
  )
# primitive plot as a sanity check

ensemble_s3_ready |>
  dplyr::filter(location_level == "nation") |>
  dplyr::mutate(prediction_start_date = as.factor(prediction_start_date)) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = date, y = observed)) +
  ggplot2::geom_line(ggplot2::aes(x = date, y = pi_50, colour = prediction_start_date)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = date, ymin = pi_5, ymax = pi_95, fill = prediction_start_date), alpha = 0.3) +
  ggplot2::facet_wrap(~disease) +
  ggplot2::coord_cartesian(ylim = c(0, 1000))
