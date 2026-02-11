# Plot gam_dow over entire season

# load data ------------------------------------------------------------------------------------------------------------

box::use(
  box / redshift,
  box / s3,
  prj / projection_plots
)


ggplot2::theme_set(projection_plots$theme_pancasts())

source("evaluation/helpers.R")

# load data ----

summary <- dplyr::tbl(redshift$connect(use_existing = FALSE), I("REDACTED")) |>
  # applying same model_date fix as for samples for same reasons
  dplyr::mutate(
    model_date = dplyr::if_else(
      model == "gam_dow", # baseline model
      as.Date(dplyr::sql("DATE_TRUNC('week', DATE(prediction_start_date) + '2 days'::interval)")),
      as.Date(model_date)
    )
  ) |>
  dplyr::filter(date < prediction_start_date + lubridate::weeks(2))

observed <- redshift$data_model("REDACTED")$REDACTED
lookups <- redshift$data_model("REDACTED")


## conbine metrics ----

observed_by_geography <- load_evaluation_summary(summary, observed, lookups)


# plot ----

plot_start_date <- summary |>
  dplyr::summarise(x = min(model_date)) |>
  dplyr::pull(x)


summary |>
  dplyr::filter(
    location_level == "nation",
    age_group == "all",
    model == "gam_dow",
    date >= prediction_start_date,
    metric == "admissions",
    model_date > "2024-09-01"
  ) |>
  dplyr::slice_max(prediction_start_date, by = c(model_date, disease, metric), na_rm = TRUE) |>
  dplyr::mutate(disease = stringr::str_remove(disease, "-19")) |>
  dplyr::collect() |>
  # grab single-model "ensembles" and ensembles - hard to do with SQL :(
  dplyr::right_join(
    observed_by_geography |>
      dplyr::filter(
        age_group == "all",
        location_level == "nation",
        location == "England",
        age_group == "all",
        metric == "admissions",
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
  ggplot2::facet_wrap(~disease, scale = "free_y") +
  ggplot2::theme(legend.position = "none")
