box::use(
  box / redshift,
  box / s3,
  prj / projection_plots[theme_pancasts]
)
source("evaluation/helpers.R")
source("evaluation/post-season-evaluation/analysis/00-depends.R")

deps_$need(
  "aws.s3",
  "dplyr",
  "ggplot2",
  "ggrepel",
  "glue",
  "purrr",
  "rPref",
  "s3fs",
  "scoringutils",
  "stringr",
  "tibble",
  "tidyr"
)


library(rPref) # for pareto fronts


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
        \(.path) aws.s3::s3readRDS(.path) |> dplyr::filter(quantile_level %in% c(5.0, 25.0, 50.0, 75.0, 95.0)),
        .progress = "reading in RDS files"
      )
    )
}

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
summary_retrospective <- dplyr::tbl(redshift$connect(use_existing = FALSE), I("REDACTED"))

ensemble_model_combinations <- summary_retrospective |>
  # don't want to ensemble the ensembles!
  dplyr::filter(!stringr::str_detect(model, "ensemble")) |>
  # don't care about the DoW model
  dplyr::filter(model != "gam_dow") |>
  dplyr::filter(disease != "rsv") |>
  dplyr::distinct(model, prediction_start_date, disease) |>
  dplyr::collect()

models_regex <- unique(ensemble_model_combinations$model) |>
  c("ensemble_matched") |>
  stringr::str_flatten(collapse = "|")

divide <- function(x, y) {
  x / y
} # small helper for scoring

ensembles_unnest <- ensembles |>
  dplyr::mutate(
    data = purrr::map(data, \(.d) dplyr::mutate(.d, model_date = as.Date(model_date)))
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    model = stringr::str_extract_all(path, models_regex) |>
      unlist() |>
      stringr::str_flatten("_"),
    .keep = "unused"
  ) |>
  dplyr::ungroup() |>
  tidyr::unnest(data) |>
  dplyr::filter(
    date >= prediction_start_date,
    date < prediction_start_date + lubridate::days(14)
  )

ensembles_quantile <- ensembles_unnest |>
  dplyr::mutate(quantile_level = quantile_level / 100) |> # must be expressed as 0<=q<=1)
  scoringutils::as_forecast_quantile(forecasting_unit)


message("scoring forecasts")

scoring_s3_root <- "REDACTED"

ensemble_identifiers <- ensembles_quantile |>
  tibble::as_tibble() |>
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

        if (s3fs::s3_file_exists(s3_path) && isFALSE(force_write)) {
          return(s3_path)
        } # don't want to rerun analysis

        scoring_data <- ensembles_quantile |>
          dplyr::filter(model == .model, disease == .disease)

        scoring_population <- ensembles_unnest |>
          dplyr::filter(model == .model, disease == .disease) |>
          dplyr::pull(population)

        scored_forecast <- scoring_data |>
          scoringutils::transform_forecasts(fun = divide, label = "per_capita", y = scoring_population) |>
          scoringutils::score(metrics = list(wis = scoringutils::wis))

        aws.s3::s3write_using(scored_forecast, saveRDS, object = s3_path)

        s3_path
      },
      .progress = "scoring top-n models"
    )
  )

ensembles_scored <- ensembles_s3_scoring |>
  dplyr::select(s3_path = out) |>
  dplyr::mutate(data = purrr::map(s3_path, \(fpath) s3$read_using(fpath, fn = readRDS))) |>
  tidyr::unnest(data) |>
  dplyr::select(-s3_path) |>
  # was created post-season, and trained on 24/25 season, biases results
  dplyr::filter(!stringr::str_detect(model, "epinow2"))

score_summary <- ensembles_scored |>
  dplyr::summarise(
    .mean = mean(wis),
    .sd = sd(wis),
    .by = c("model", "location_level", "scale", "disease")
  )

location_level_weights <- tibble::tribble(
  ~location_level , ~weight ,
  "nation"        , 0.5     ,
  "region"        , 0.25    ,
  "icb"           , 0.25
)

## calc pareto front

### per capita

weighted_score_summary <- score_summary |>
  dplyr::left_join(location_level_weights, by = "location_level") |>
  tidyr::nest(data = -c(disease, scale)) |>
  dplyr::mutate(
    summary = purrr::map(
      data,
      \(dd) {
        dd |>
          dplyr::summarise(
            .mean = sum(.mean * weight),
            .sd = sqrt(sum(.sd^2 * weight)), # equivalent to weight average of variances
            .by = c(model)
          )
      }
    ),
    .keep = "unused"
  )

# Going to construct a pareto front, this is a collection of decisions (models) which, in some sense,
#  cannot be bettered. This is an idea from multi-objective optimisation
# low() means a smaller value is better
# we want small (mean) wis (high performance), and we also want small variability in performance, i.e. consistency
preference <- rPref::low(.mean) * rPref::low(.sd)
pareto_front <- weighted_score_summary |>
  dplyr::mutate(
    front = purrr::map(summary, \(.summary) rPref::psel(.summary, preference)),
    .keep = "unused"
  ) |>
  tidyr::unnest(front)

# NOTE: Due to the density of the text, it is advisable to view plots in a separate window, not in RStudio pane

plot_front(weighted_score_summary, pareto_front, "covid-19", "natural")
plot_front(weighted_score_summary, pareto_front, "covid-19", "per_capita")
plot_front(weighted_score_summary, pareto_front, "influenza", "natural")
plot_front(weighted_score_summary, pareto_front, "influenza", "per_capita")

# verify scores are on a comparable scale across geographic granularities

score_summary |>
  dplyr::filter(scale == "per_capita", disease == "influenza") |>
  ggplot2::ggplot(ggplot2::aes(x = .mean, y = .sd)) +
  ggplot2::geom_point() +
  ggrepel::geom_text_repel(ggplot2::aes(label = model)) +
  ggplot2::facet_wrap(~location_level, scale = "free")
