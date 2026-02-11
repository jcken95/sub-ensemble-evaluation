source("evaluation/post-season-evaluation/analysis/00-depends.R")
deps_$need(
  "dplyr",
  "ggplot2",
  "lubrdate",
  "stringr"
)

# load data ----

## production forecasts ----

samples <- dplyr::tbl(redshift$connect(use_existing = FALSE), I("REDACTED")) |>
  # baseline model was fit on a single day, so will restrospectively correct the model date
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

## retrospective evaluation forecasts ----

samples_retrospective <- dplyr::tbl(redshift$connect(use_existing = FALSE), I("REDACTED"))
summary_retrospective <- dplyr::tbl(redshift$connect(use_existing = FALSE), I("REDACTED"))


## useful dates ----

season_start_date <- summary |>
  dplyr::filter(model != "gam_dow", metric == "admissions") |>
  dplyr::summarise(start_date = min(model_date, na.rm = TRUE), .by = "disease") |>
  dplyr::collect()

# Combine all metrics ----

observed_by_geography <- load_evaluation_summary(summary, observed, lookups)

## lookback plot (national) ----

plot_start_date <- summary |>
  dplyr::summarise(x = min(model_date)) |>
  dplyr::pull(x)

summary |>
  dplyr::filter(
    location_level == "nation",
    age_group == "all",
    # TODO: better ensemble filter; early RSV models were single-model (tp|gp)
    # noro is also a single-model approach (nowcast)
    grepl("ensemble|tp|gp|nowcast", model),
    (date >= prediction_start_date) | (date >= prediction_start_date - lubridate::days(7) & disease == "norovirus"),
    model_date > "2024-09-01"
  ) |>
  dplyr::slice_max(prediction_start_date, by = c(model_date, disease, metric), na_rm = TRUE) |>
  dplyr::mutate(disease = stringr::str_remove(disease, "-19")) |>
  dplyr::collect() |>
  # grab single-model "ensembles" and ensembles - hard to do with SQL :(
  dplyr::mutate(
    n_models = dplyr::n_distinct(model),
    # the reported model was the ensemble; if no ensemble, then we only had one model, therefore it was reported
    is_reported_model = (grepl("ensemble", model) | n_models == 1),
    .by = c(model_date, disease, metric)
  ) |>
  dplyr::filter(is_reported_model) |>
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
    alpha = 0.5
  ) +
  ggplot2::geom_line(
    ggplot2::aes(x = date, y = pi_50, group = prediction_start_date)
  ) +
  ggplot2::geom_point(
    ggplot2::aes(x = date, y = observed_target),
    size = 0.3
  ) +
  ggplot2::facet_wrap(metric ~ disease, scale = "free_y") +
  ggplot2::theme(legend.position = "none")
