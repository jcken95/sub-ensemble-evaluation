#  RSV and norovirus plots
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

# Load data ----
use_downloaded_data <- TRUE

## production forecasts ----

samples <- dplyr::tbl(redshift$connect(use_existing = FALSE), I("REDACTED")) |>
  dplyr::filter(disease %in% c("norovirus", "rsv")) |>
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
    date >= prediction_start_date,
    date < prediction_start_date + lubridate::weeks(2),
    !(model %in% c("epinow2_rw", "hisotric_gr")),
    disease %in% c("norovirus", "rsv")
  )

summary <- dplyr::tbl(redshift$connect(use_existing = FALSE), I("REDACTED")) |>
  dplyr::select(-c("pi_2.5", "pi_97.5", "pi_17", "pi_83")) |>
  dplyr::filter(disease %in% c("norovirus", "rsv")) |>
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
    date >= prediction_start_date,
    !(model %in% c("epinow2_rw", "historic_gr")),
    age_group_granularity == "none"
  ) |>
  identify_ensemble_inclusion()


observed <- redshift$data_model("REDACTED")$REDACTED
lookups <- redshift$data_model("REDACTED")

# useful dates ----

season_start_date <- summary |>
  dplyr::filter(model != "gam_dow", metric %in% c("admissions", "cases")) |>
  dplyr::summarise(start_date = min(model_date, na.rm = TRUE), .by = "disease") |>
  dplyr::collect()

plot_start_date <- "2024-10-01"

plotting_end_date <- as.Date("2025-04-01")

## which models when

forecasts <- summary |> # pull known stored results
  dplyr::filter(
    disease == "rsv"
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
    "summary" = list(dplyr::filter(
      summary,
      disease == local(disease),
      metric == local(metric),
      model_date == upload_date,
      age_group == "all",
      location_level == "nation"
    )),
    upload_date = round_to_wednesday(upload_date) # had to match to dbt before
  ) |>
  dplyr::ungroup()


# conditionally run query - can easily take > 1 hr so a local copy speeds up

models_and_ensembles <- forecasts |>
  dplyr::filter(metric == "admissions" | metric == "cases") |>
  dplyr::mutate(
    individual_models = purrr::map(
      summary,
      \(x) {
        x |>
          dplyr::distinct(model) |>
          dplyr::pull(model)
      },
      .progress = "individual models"
    ),
    ensemble_string = purrr::map(
      summary,
      \(x) {
        models <- x |>
          dplyr::distinct(model) |>
          dplyr::filter(
            model != "gam_dow",
          ) |>
          dplyr::pull(model)

        if (length(models) == 1) {
          return(models)
        }

        models[stringr::str_detect(models, "ensemble")]
      },
      .progress = "ensemble string"
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
  ) |>
  # treat single model runs as the ensemble
  dplyr::mutate(
    n_models = dplyr::n_distinct(individual_models),
    .by = "upload_date"
  ) |>
  dplyr::mutate(
    ensemble_string = dplyr::if_else(n_models == 1, individual_models, ensemble_string),
    model_in_ensemble = dplyr::if_else(n_models == 1, TRUE, model_in_ensemble)
  )

upload_date_breaks <- models_used |>
  dplyr::distinct(upload_date) |>
  dplyr::filter(dplyr::row_number() %% 2 == 1) |>
  dplyr::pull(upload_date)

# No false is being shown?
models_used_plot <- models_used |>
  dplyr::filter(upload_date <= plotting_end_date) |>
  dplyr::mutate(tile_width = 4) |>
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
    linewidth = 3
  ) +
  ggplot2::labs(
    y = "Model name",
    x = "Forecast production date",
    fill = "Model in operation ensemble?",
    alpha = "Is first use?"
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
    axis.text.y = ggplot2::element_text(angle = 45, hjust = 0.5),
    legend.position = "bottom"
  )

models_used_plot

## Combine all metrics ----

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
    model_date > "2024-09-01",
    date <= "2025-04-01"
  ) |>
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


# grab all single-model / ensemble data

admissions_summary <- scoring_ready |>
  dplyr::filter(
    location_level == "nation",
    age_group == "all",
    grepl("ensemble|tp|gp|nowcast", model),
    date >= prediction_start_date,
    model_date > "2024-09-01"
  ) |>
  dplyr::slice_max(prediction_start_date, by = c(model_date, disease, metric), na_rm = TRUE) |>
  dplyr::collect() |>
  # grab single-model "ensembles" and ensembles - hard to do with SQL :(
  dplyr::mutate(n_models = dplyr::n_distinct(model), .by = c(model_date, disease, metric)) |>
  dplyr::filter(grepl("ensemble", model) | n_models == 1) |>
  dplyr::right_join(
    observed_by_geography |>
      dplyr::filter(
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
    x == "norovirus" ~ "Norovirus",
    x == "rsv" ~ "RSV",
    .default = x
  )
}

admissions_summary_plots <- admissions_summary |>
  dplyr::filter(model == "ensemble_stack" | n_models == 1) |>
  dplyr::filter(
    disease %in% c("rsv", "norovirus"),
    date <= plotting_end_date
  ) |>
  tidyr::pivot_wider(
    names_from = quantile_level,
    values_from = predicted,
    names_glue = "pi_{100 * quantile_level}"
  ) |>
  dplyr::left_join(trend_probs, by = c("disease", "prediction_start_date")) |>
  tidyr::nest(data = -c(disease)) |>
  dplyr::mutate(
    plot = purrr::map2(
      data,
      disease,

      \(.data, .disease) {
        metric <- dplyr::if_else(.disease == "rsv", "admissions", "cases")

        plt_title <- glue::glue(
          "National forecasts for daily {metric} of {disease_facet_labels(.disease)}"
        )

        .data |>
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
            data = dplyr::filter(observed_admissions, disease == .disease),
            ggplot2::aes(x = date, y = observed_target),
            size = 0.3
          ) +
          ggplot2::labs(
            x = "Prediction start date",
            y = "Admissions"
          ) +
          ggplot2::ggtitle(
            plt_title,
            subtitle = "Colour indicates prediction start date"
          ) +
          ggplot2::theme(legend.position = "none")
      }
    )
  )


admissions_summary_plots |>
  dplyr::filter(disease == "norovirus") |>
  dplyr::pull(plot) |>
  ggplot2::ggsave(
    filename = here::here(plot_output_dir, "SUPPLEMENT_admissions_summary_noro.png"),
    plot = _,
    width = 16,
    height = 12
  )

admissions_summary_plots |>
  dplyr::filter(disease == "norovirus") |>
  dplyr::pull(plot) |>
  ggplot2::ggsave(
    filename = here::here(plot_supplement_dir_tiff, "fig_l.tiff"),
    plot = _,
    width = 19,
    height = 14.25,
    dpi = 300,
    units = "cm"
  )


combined_rsv_forecast_plot <- patchwork::wrap_plots(
  admissions_summary_plots |>
    dplyr::filter(disease == "rsv") |>
    dplyr::pull(plot) |>
    purrr::pluck(1),
  models_used_plot,
  ncol = 1
)

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "SUPPLEMENT_admissions_summary_rsv.png"),
  plot = combined_rsv_forecast_plot,
  width = 16,
  height = 12
)

ggplot2::ggsave(
  filename = here::here(plot_supplement_dir_tiff, "fig_m.tiff"),
  plot = combined_rsv_forecast_plot,
  width = 19,
  height = 14.25,
  dpi = 300,
  units = "cm"
)


# Note: in 24/25 season, noro was a nation-only model, so no regional analysis
admissions_summary_region <- scoring_ready |>
  dplyr::filter(
    metric == "admissions",
    disease == "rsv",
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
    disease == "rsv"
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
    size = 0.3
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(location),
    scale = "free_y",
    labeller = ggplot2::label_wrap_gen(20)
  ) +
  ggplot2::labs(
    x = "Prediction start date",
    y = "Admissions"
  ) +
  ggplot2::ggtitle(
    "Regional forecasts for daily RSV admissions",
    subtitle = "Colour indicates prediction start date."
  ) +
  ggplot2::theme(legend.position = "none")

admissions_region_plot

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "SUPPLEMENT_admissions_summary_region_rsv.png"),
  plot = admissions_region_plot,
  width = 16,
  height = 12
)

ggplot2::ggsave(
  filename = here::here(plot_supplement_dir_tiff, "fig_n.tiff"),
  plot = combined_rsv_forecast_plot,
  width = 19,
  height = 14.25,
  dpi = 300,
  units = "cm"
)

# RSV Age
summary_age <- dplyr::tbl(redshift$connect(use_existing = FALSE), I("REDACTED")) |>
  dplyr::select(-c("pi_2.5", "pi_97.5", "pi_17", "pi_83")) |>
  dplyr::filter(
    date <= "2025-04-01",
    disease == "rsv"
  ) |>
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
  ) |>
  identify_ensemble_inclusion()

scoring_ready_age <- summary_age |>
  dplyr::filter(
    date >= prediction_start_date,
    model_date > "2024-09-01",
    date <= "2025-04-01",
    disease == "rsv",
  ) |>
  dplyr::slice_max(prediction_start_date, by = c(model_date, disease, metric), na_rm = TRUE) |>
  dplyr::collect() |>
  dplyr::left_join(
    observed_by_geography |> dplyr::filter(age_group != "all"),
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

admissions_summary_age <- scoring_ready_age |>
  dplyr::filter(
    metric == "admissions",
    disease == "rsv",
    location_level == "nation",
    grepl("ensemble|tp|gp|nowcast", model),
    date >= prediction_start_date,
    model_date > "2024-09-01"
  ) |>
  dplyr::slice_max(prediction_start_date, by = c(model_date, disease, metric), na_rm = TRUE) |>
  dplyr::collect() |>
  # grab single-model "ensembles" and ensembles - hard to do with SQL :(
  dplyr::mutate(n_models = dplyr::n_distinct(model), .by = c(model_date, disease, metric)) |>
  dplyr::filter(grepl("ensemble", model) | n_models == 1) |>
  dplyr::right_join(
    observed_by_geography |>
      dplyr::filter(
        metric == "admissions",
        location_level == "nation",
        age_group != "all",
        date >= plot_start_date
      ),
    by = c("date", "location", "location_level", "metric", "disease", "age_group")
  )

observed_admissions_age <- admissions_summary_age |>
  dplyr::filter(
    date >= min(prediction_dates),
    date <= max(prediction_dates),
    disease == "rsv"
  ) |>
  dplyr::mutate(
    location_level = dplyr::case_when(
      location_level == "icb" ~ "ICB",
      location_level == "region" ~ "Region"
    ),
    disease = disease_facet_labels(disease)
  )


rsv_age_bands <- tibble::tribble(
  # machine readable recoding
  ~age_group , ~age_band ,
  "[0,2)"    , "00_02"   ,
  "[2,5)"    , "02_05"   ,
  "[5,18)"   , "05_18"   ,
  "[18,65)"  , "18_65"   ,
  "[65,75)"  , "65_75"   ,
  "[75,120)" , "75_plus"
)

admissions_age_plot <- admissions_summary_age |>
  dplyr::filter(model == "ensemble_stack" | n_models == 1) |>
  dplyr::filter(
    date <= plotting_end_date
  ) |>
  tidyr::pivot_wider(
    names_from = quantile_level,
    values_from = predicted,
    names_glue = "pi_{100 * quantile_level}"
  ) |>
  dplyr::mutate(
    location_level = dplyr::case_when(
      location_level == "icb" ~ "ICB",
      location_level == "region" ~ "Region"
    ),
    disease = disease_facet_labels(disease),
    age_group = ordered(age_group, levels = rsv_age_bands$age_group)
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
    data = dplyr::mutate(observed_admissions_age, age_group = ordered(age_group, levels = rsv_age_bands$age_group)),
    mapping = ggplot2::aes(x = date, y = observed_target),
    size = 0.3
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(age_group),
    scale = "free_y",
    labeller = ggplot2::label_wrap_gen(20)
  ) +
  ggplot2::labs(
    x = "Prediction start date",
    y = "Admissions"
  ) +
  ggplot2::ggtitle(
    "Age breakdown forecasts for daily RSV admissions",
    subtitle = "Colour indicates prediction start date."
  ) +
  ggplot2::theme(legend.position = "none")

admissions_age_plot

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "SUPPLEMENT_admissions_summary_age_rsv.png"),
  plot = admissions_age_plot,
  width = 16,
  height = 12
)

ggplot2::ggsave(
  filename = here::here(plot_supplement_dir_tiff, "fig_o.tiff"),
  plot = combined_rsv_forecast_plot,
  width = 19,
  height = 14.25,
  dpi = 300,
  units = "cm"
)

# Score forecasts

holiday_break_tibble <- tidyr::expand_grid(
  prediction_start_date = c(as.Date("2024-12-25"), as.Date("2025-01-01")),
  score_name = c("RPS", "log(pcWIS)"),
  disease = c("rsv", "norovirus"),
  score_value = NA_real_
)

divide <- function(x, y) {
  x / y
}

scored <- scoring_ready |>
  scoringutils::as_forecast_quantile(forecast_unit = forecasting_unit) |>
  scoringutils::transform_forecasts(fun = divide, label = "per_capita", y = scoring_ready$population) |>
  dplyr::filter(scale == "per_capita") |>
  scoringutils::score(metrics = list(pcwis = scoringutils::wis))

score_summary <- scored |>
  dplyr::summarise(
    pcwis = log(mean(pcwis)),
    .by = c(disease, prediction_start_date, location_level, location, age_group, age_group_granularity)
  ) |>
  dplyr::mutate(score_name = "log(pcWIS)") |>
  dplyr::bind_rows(
    holiday_break_tibble |>
      dplyr::rename("log(pcWIS)" = score_value) |>
      dplyr::mutate(age_group = "all", location_level = "nation")
  ) |>
  dplyr::arrange(prediction_start_date)

rsv_noro_score_plot <- score_summary |>
  dplyr::mutate(disease = disease_facet_labels(disease)) |>
  dplyr::filter(location_level == "nation", age_group == "all") |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = prediction_start_date, y = pcwis)) +
  ggplot2::ggtitle(
    "log(pcWIS) for Norovirus cases and RSV admissions forecasts",
    subtitle = "Mean pcWIS per prediction start date (log scale)"
  ) +
  ggplot2::facet_wrap(ggplot2::vars(disease), scale = "free_y") +
  ggplot2::labs(x = "Prediction Start Date", y = "log(pcWIS)") +
  ggplot2::theme(
    strip.text.x = ggplot2::element_text(size = 16),
    plot.title = ggplot2::element_text(size = 18),
    plot.subtitle = ggplot2::element_text(size = 16)
  )


ggplot2::ggsave(
  filename = here::here(plot_output_dir, "SUPPLEMENT_rsv_noro_score_nation.png"),
  plot = rsv_noro_score_plot,
  width = 16,
  height = 12
)

ggplot2::ggsave(
  filename = here::here(plot_supplement_dir_tiff, "fig_p.tiff"),
  plot = combined_rsv_forecast_plot,
  width = 19,
  height = 14.25,
  dpi = 300,
  units = "cm"
)

rsv_score_regions_plot <- score_summary |>
  dplyr::filter(disease == "rsv", location_level != "nation", age_group == "all") |>
  tidyr::nest(data = -location) |>
  dplyr::mutate(
    data = purrr::map(
      data,
      \(dd) {
        dd |>
          dplyr::bind_rows(holiday_break_tibble) |>
          dplyr::arrange(prediction_start_date)
      }
    )
  ) |>
  tidyr::unnest(cols = c(data)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = prediction_start_date, y = pcwis)) +
  ggplot2::ggtitle(
    "pcWIS for regional RSV forecasts",
    subtitle = "Mean pcWIS per prediction start date (log scale)"
  ) +
  ggplot2::labs(x = "Prediction Start Date", y = "log(pcWIS)") +
  ggplot2::facet_wrap(ggplot2::vars(location))

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "SUPPLEMENT_rsv_score_regions.png"),
  plot = rsv_score_regions_plot,
  width = 16,
  height = 12
)

ggplot2::ggsave(
  filename = here::here(plot_supplement_dir_tiff, "fig_q.tiff"),
  plot = rsv_score_regions_plot,
  width = 19,
  height = 14.25,
  dpi = 300,
  units = "cm"
)


# score and plot age
scored_age <- scoring_ready_age |>
  scoringutils::as_forecast_quantile(forecast_unit = forecasting_unit) |>
  scoringutils::transform_forecasts(fun = divide, label = "per_capita", y = scoring_ready_age$population) |>
  dplyr::filter(scale == "per_capita") |>
  scoringutils::score(metrics = list(pcwis = scoringutils::wis))


score_summary_age <- scored_age |>
  dplyr::summarise(
    pcwis = log(mean(pcwis)),
    .by = c(disease, prediction_start_date, location_level, location, age_group, age_group_granularity)
  ) |>
  dplyr::mutate(score_name = "log(pcWIS)") |>
  dplyr::bind_rows(
    holiday_break_tibble |>
      dplyr::rename("log(pcWIS)" = score_value) |>
      dplyr::mutate(location_level = "nation")
  ) |>
  dplyr::arrange(prediction_start_date)


rsv_score_age_plot <- score_summary_age |>
  tidyr::drop_na(age_group) |>
  dplyr::filter(disease == "rsv", location_level == "nation") |>
  tidyr::nest(data = -age_group) |>
  dplyr::mutate(
    data = purrr::map(
      data,
      \(dd) {
        dd |>
          dplyr::bind_rows(holiday_break_tibble) |>
          dplyr::arrange(prediction_start_date)
      }
    )
  ) |>
  tidyr::unnest(cols = c(data)) |>
  dplyr::left_join(rsv_age_bands, by = dplyr::join_by(age_group)) |>
  dplyr::mutate(age_group = ordered(age_group, levels = rsv_age_bands$age_group)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = prediction_start_date, y = pcwis)) +
  ggplot2::ggtitle(
    "pcWIS for age breakdown RSV forecasts",
    subtitle = "Mean pcWIS per prediction start date (log scale)"
  ) +
  ggplot2::labs(x = "Prediction Start Date", y = "log(pcWIS)") +
  ggplot2::facet_wrap(ggplot2::vars(age_group))

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "SUPPLEMENT_rsv_score_age.png"),
  plot = rsv_score_age_plot,
  width = 16,
  height = 12
)

ggplot2::ggsave(
  filename = here::here(plot_supplement_dir_tiff, "fig_r.tiff"),
  plot = rsv_score_age_plot,
  width = 19,
  height = 14.25,
  dpi = 300,
  units = "cm"
)
##  Ordinal forecasts

lagged_values <- observed_by_geography |>
  dplyr::filter(metric %in% c("admissions", "cases"), disease %in% c("norovirus", "rsv")) |>
  dplyr::group_by(location, location_level, age_group, disease, metric) |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    rolling_average = zoo::rollmean(observed_target, k = 7, align = "right", na.pad = TRUE),
    observed_lag = dplyr::lag(observed_target, n = 14),
    rolling_average_lagged = zoo::rollmean(observed_lag, k = 7, align = "right", na.pad = TRUE),
    observed_trend = classify_trend(rolling_average, rolling_average_lagged)
  ) |>
  dplyr::ungroup()

ordinal_ready <- scoring_ready |>
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
    is_reported_model | disease == "norovirus"
  ) |>
  scoringutils::summarise_scores(
    by = c("prediction_start_date", "model", "disease", "metric", "location", "location_level", "is_reported_model")
  )

ordinal_score_plot <- ordinal_score_over_season |>
  dplyr::rowwise() |>
  dplyr::filter(
    if (disease == "rsv") {
      location_level == "nation"
    } else {
      TRUE
    }
  ) |>
  dplyr::ungroup() |>
  dplyr::bind_rows(
    holiday_break_tibble
  ) |>
  dplyr::arrange(prediction_start_date) |>
  dplyr::mutate(log_rps = log(rps), disease = disease_facet_labels(disease)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(
    ggplot2::aes(x = prediction_start_date, y = log_rps)
  ) +
  ggplot2::ggtitle(
    "log(RPS) for national Norovirus cases and RSV admissions"
  ) +
  ggplot2::labs(
    x = "Prediction start date",
    y = "log(RPS)"
  ) +
  ggplot2::facet_wrap(ggplot2::vars(disease))

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "SUPPLEMENT_ordinal_score_rsv_noro.png"),
  plot = ordinal_score_plot,
  width = 16,
  height = 12
)

ggplot2::ggsave(
  filename = here::here(plot_supplement_dir_tiff, "fig_s.tiff"),
  plot = ordinal_score_plot,
  width = 19,
  height = 14.25,
  dpi = 300,
  units = "cm"
)

rsv_ordinal_region <- ordinal_score_over_season |>
  dplyr::filter(
    location_level == "region",
    disease == "rsv"
  ) |>
  tidyr::nest(data = -c(location)) |>
  dplyr::mutate(
    data = purrr::map(
      data,
      \(dd) dplyr::bind_rows(dd, holiday_break_tibble)
    )
  ) |>
  tidyr::unnest(cols = c(data)) |>
  dplyr::arrange(prediction_start_date) |>
  dplyr::mutate(log_rps = log(rps), disease = disease_facet_labels(disease)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(
    ggplot2::aes(x = prediction_start_date, y = log_rps)
  ) +
  ggplot2::ggtitle(
    "log(RPS) RSV admissions by region"
  ) +
  ggplot2::labs(
    x = "Prediction start date",
    y = "log(RPS)"
  ) +
  ggplot2::facet_wrap(ggplot2::vars(location))


ggplot2::ggsave(
  filename = here::here(plot_output_dir, "SUPPLEMENT_rsv_ordinal_region_score.png"),
  plot = rsv_ordinal_region,
  width = 16,
  height = 12
)

ggplot2::ggsave(
  filename = here::here(plot_supplement_dir_tiff, "fig_t.tiff"),
  plot = rsv_ordinal_region,
  width = 19,
  height = 14.25,
  dpi = 300,
  units = "cm"
)

## ordinal rsv by age

ordinal_ready_age <- scoring_ready_age |>
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


ordinal_scored_age <- ordinal_ready_age |>
  scoringutils::score(by = forecasting_unit)

ordinal_score_over_season_age <- ordinal_scored_age |>
  scoringutils::summarise_scores(
    by = c("prediction_start_date", "model", "disease", "metric", "is_reported_model", "age_group")
  )

ordinal_score_age <- ordinal_score_over_season_age |>
  dplyr::filter(is_reported_model) |>
  dplyr::left_join(rsv_age_bands, by = dplyr::join_by(age_group)) |>
  dplyr::mutate(age_group = ordered(age_group, levels = rsv_age_bands$age_group)) |>
  tidyr::nest(data = -c(age_group)) |>
  dplyr::mutate(data = purrr::map(data, \(dd) dplyr::bind_rows(dd, holiday_break_tibble))) |>
  tidyr::unnest(cols = c(data)) |>
  dplyr::arrange(prediction_start_date) |>
  dplyr::mutate(log_rps = log(rps), disease = disease_facet_labels(disease)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(
    ggplot2::aes(x = prediction_start_date, y = log_rps)
  ) +
  ggplot2::ggtitle(
    "log(RPS) RSV admissions by age group"
  ) +
  ggplot2::labs(
    x = "Prediction start date",
    y = "log(RPS)"
  ) +
  ggplot2::facet_wrap(ggplot2::vars(age_group))

ggplot2::ggsave(
  filename = here::here(plot_output_dir, "SUPPLEMENT_rsv_ordinal_age_score.png"),
  plot = ordinal_score_age,
  width = 16,
  height = 12
)

ggplot2::ggsave(
  filename = here::here(plot_supplement_dir_tiff, "fig_u.tiff"),
  plot = ordinal_score_age,
  width = 19,
  height = 14.25,
  dpi = 300,
  units = "cm"
)

## numerical summary of scores
combined_score_summary <-
  dplyr::bind_rows(
    scored |>
      dplyr::filter(!is.nan(pcwis), age_group_granularity == "none") |>
      dplyr::group_by(disease, location_level, age_group_granularity) |>
      dplyr::summarise(mean_score = mean(pcwis, na.rm = TRUE)) |>
      dplyr::mutate(score_name = "pcWIS"),

    scored_age |>
      dplyr::filter(!is.nan(pcwis)) |>
      dplyr::filter(age_group_granularity == "fine") |>
      dplyr::group_by(disease, location_level, age_group_granularity) |>
      dplyr::summarise(mean_score = mean(pcwis, na.rm = TRUE)) |>
      dplyr::mutate(score_name = "pcWIS"),

    ordinal_scored |>
      dplyr::filter(!is.nan(rps), age_group_granularity == "none") |>
      dplyr::group_by(disease, location_level, age_group_granularity) |>
      dplyr::summarise(mean_score = mean(rps, na.rm = TRUE)) |>
      dplyr::mutate(score_name = "RPS"),

    ordinal_scored_age |>
      dplyr::filter(!is.nan(rps)) |>
      dplyr::group_by(disease, location_level, age_group_granularity) |>
      dplyr::summarise(mean_score = mean(rps, na.rm = TRUE)) |>
      dplyr::mutate(score_name = "RPS")
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    model_type = dplyr::if_else(disease == "rsv", "Operational ensemble", "Single operational model"),
    disease = disease_facet_labels(disease)
  ) |>
  dplyr::mutate(
    mean_score = signif(mean_score, 4),
    location_level = dplyr::case_when(
      location_level == "icb" ~ "ICB",
      location_level == "region" ~ "Region",
      location_level == "nation" ~ "Nation"
    ),
    location_level = ordered(location_level, levels = c("ICB", "Region", "Nation")),
    age_group_granularity = stringr::str_to_title(age_group_granularity)
  ) |>
  dplyr::arrange(
    disease,
    dplyr::desc(score_name),
    dplyr::desc(location_level),
    model_type
  ) |>
  dplyr::relocate(
    model_type,
    location_level,
    age_group_granularity,
    disease,
    score_name,
    mean_score
  )

combined_score_summary |>
  gt::gt() |>
  gt::fmt_scientific(exp_style = "x10n") |>
  gt::gtsave(here::here(plot_output_dir, "ensemble_score_summary_rsv_noro.docx"))
