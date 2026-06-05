## scoring of ordinal forecasts (trend direction)
## not analysis, creates the data and upload to s3
box::use(
  box / redshift,
  box / s3,
  prj / projection_plots[theme_pancasts]
)
source("evaluation/post-season-evaluation/analysis/00-depends.R")
deps_$need(
  "aws.s3",
  "dplyr",
  "ggplot2",
  "glue",
  "purrr",
  "s3fs",
  "scoringutils",
  "stringr",
  "tibble",
  "tidyr",
  "zoo"
)
# Script is ram-heavy; split by disease

chosen_disease <- "influenza"

source("evaluation/helpers.R")

ggplot2::theme_set(theme_pancasts())

# read/write params

read_data_from_s3 <- TRUE

force_write <- TRUE # replace to TRUE if you would like to completely restart the analysis!

if (read_data_from_s3) {
  s3_paths <- tibble::tibble(
    path = s3fs::s3_dir_ls(path = "REDACTED")
  ) |>
    dplyr::filter(!stringr::str_detect(path, "ensembles.rds")) |>
    # only want to read in .rds files
    dplyr::filter(stringr::str_ends(path, ".rds"))

  ensembles <- s3_paths |>
    dplyr::mutate(
      data = purrr::map(
        path,
        \(.path) {
          aws.s3::s3readRDS(.path) |> dplyr::mutate(model_date = as.Date(model_date))
        },
        .progress = "reading in RDS files"
      )
    ) |>
    tidyr::unnest(data) |>
    dplyr::filter(disease == chosen_disease) |>
    dplyr::select(-p_total) |>
    dplyr::filter(
      date >= prediction_start_date,
      date < prediction_start_date + lubridate::days(14)
    )
}

ensemble_names <- ensembles |>
  dplyr::distinct(path, disease) |>
  dplyr::mutate(
    model = path |>
      stringr::str_extract(
        # get everything after the match for a disease
        glue::glue("(?<={disease}_).*")
      ) |>
      # drop file extension
      stringr::str_remove(".rds")
  ) |>
  dplyr::select(-disease)

ensembles <- ensembles |>
  dplyr::left_join(
    ensemble_names,
    by = "path"
  ) |>
  dplyr::select(-path)

summary <- dplyr::tbl(
  redshift$connect(use_existing = FALSE),
  I("REDACTED")
)

observed <- redshift$data_model("REDACTED")$REDACTED
lookups <- redshift$data_model("REDACTED")


## conbine metrics ----

observed_by_geography <- load_evaluation_summary(summary, observed, lookups)

lagged_values <- observed_by_geography |>
  # covid-19 is coded as `covid` in this object
  dplyr::mutate(disease = dplyr::if_else(disease == "covid", "covid-19", disease)) |>
  dplyr::filter(disease == chosen_disease) |>
  dplyr::group_by(location, location_level, age_group, disease, metric) |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    rolling_average = zoo::rollmean(observed_target, k = 7, align = "right", na.pad = TRUE),
    observed_lag = dplyr::lag(observed_target, n = 14),
    rolling_average_lagged = zoo::rollmean(observed_lag, k = 7, align = "right", na.pad = TRUE),
    observed_trend = classify_trend(rolling_average, rolling_average_lagged)
  ) |>
  dplyr::ungroup()

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

samples_retrospective <- dplyr::tbl(redshift$connect(use_existing = FALSE), I("REDACTED"))

ensemble_model_combinations <- samples_retrospective |>
  # don't want to ensemble the ensembles!
  dplyr::filter(!stringr::str_detect(model, "ensemble")) |>
  # don't care about the DoW model
  dplyr::filter(model != "gam_dow") |>
  dplyr::filter(disease == chosen_disease) |>
  dplyr::distinct(model, prediction_start_date, disease) |>
  dplyr::collect()

models_regex <- unique(ensemble_model_combinations$model) |>
  c("ensemble_matched") |>
  stringr::str_flatten(collapse = "|")


ensembles_long <- ensembles |>
  # predicted here is the value from the raw forecast; we don't need this for the ordinal part
  dplyr::select(-c(predicted, observed, quantile_level)) |>
  dplyr::distinct() |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("p_"),
    values_to = "predicted",
    names_to = "predicted_label"
  ) |>
  dplyr::left_join(
    lagged_values |>
      dplyr::select(
        date,
        age_group,
        location,
        location_level,
        disease,
        metric,
        observed = observed_trend
      ),
    by = c("date", "age_group", "location", "location_level", "disease", "metric")
  ) |>
  dplyr::mutate(predicted_label = stringr::str_remove(predicted_label, "p_"))

message("scoring forecasts")

scoring_s3_root <- "REDACTED"

s3fs::s3_dir_create(scoring_s3_root)

ensemble_identifiers <- ensembles_long |>
  dplyr::distinct(model, disease) |>
  # was created post-season, and trained on 24/25 season, biases results
  dplyr::filter(!stringr::str_detect(model, "epinow2"))

ensembles_s3_scoring <- ensemble_identifiers |>
  dplyr::mutate(
    out = purrr::map2(
      model,
      disease,

      \(.model, .disease) {
        s3_path <- glue::glue("{scoring_s3_root}/{.disease}-{.model}.rds")

        do_not_compute <- s3fs::s3_file_exists(s3_path) & isFALSE(force_write)

        if (do_not_compute) {
          return(s3_path)
        } # don't want to rerun analysis

        ensembles_ordinal <- ensembles_long |>
          dplyr::filter(
            model == .model,
            disease == .disease,
            !is.na(observed)
          ) |>
          dplyr::mutate(
            predicted_label = ordered(predicted_label, levels = c("decrease", "stable", "increase")),
            observed = ordered(observed, levels = c("decrease", "stable", "increase"))
          ) |>
          scoringutils::as_forecast_ordinal(forecast_unit = forecasting_unit)

        scoring_data <- ensembles_ordinal |>
          dplyr::filter(model == .model, disease == .disease)

        scored_forecast <- scoring_data |>
          scoringutils::score(metrics = list(rps = scoringutils::rps_ordinal))

        aws.s3::s3write_using(scored_forecast, saveRDS, object = s3_path)

        s3_path
      },
      .progress = "scoring top-n models"
    )
  )
