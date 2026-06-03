## Pareto analysis of WIS and RPS for influenxa and covid-19 (retrospective) forecasts based on 3-model ensemebles

box::use(
  box / redshift,
  box / s3,
  prj / projection_plots[theme_pancasts]
)

source("evaluation/helpers.R")

source("evaluation/post-season-evaluation/analysis/00-depends.R")

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


ggplot2::theme_set(theme_pancasts())


message("scoring forecasts")

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

all_scores <- rps_scored |>
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
      "disease"
    )
  )

score_summary <- all_scores |>
  dplyr::summarise(
    rps = mean(rps),
    wis = mean(wis),
    .by = c("model", "location_level", "disease")
  ) |>
  dplyr::mutate(wis_per_100k = wis * 1e5)

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
  tidyr::nest(data = -c(disease)) |>
  dplyr::mutate(
    summary = purrr::map(
      data,
      \(dd) {
        dd |>
          dplyr::summarise(
            rps = sum(rps * weight),
            wis = sum(wis * weight),
            .by = model
          ) |>
          dplyr::mutate(wis_per_100k = wis * 1e5)
      }
    ),
    .keep = "unused"
  )

# Going to construct a pareto front, this is a collection of decisions (models) which, in some sense,
#  cannot be bettered. This is an idea from multi-objective optimisation
# low() means a smaller value is better
# we want small (mean) wis (high performance), and we also want small variability in performance, i.e. consistency
preference <- rPref::low(wis_per_100k) * rPref::low(rps)
pareto_front <- weighted_score_summary |>
  dplyr::mutate(
    front = purrr::map(summary, \(.summary) rPref::psel(.summary, preference)),
    .keep = "unused"
  ) |>
  tidyr::unnest(front)

# NOTE: Due to the density of the text, it is advisable to view plots in a separate window, not in RStudio pane

plot_score_front(weighted_score_summary, pareto_front, "covid-19")
plot_score_front(weighted_score_summary, pareto_front, "influenza")

## in both cases, fronts are a single point!

pareto_front

## What if we don't weight the scores?
## we might not know, for example, which weights are really sensible

score_summary_geography <- score_summary |>
  dplyr::select(-wis) |>
  tidyr::pivot_wider(
    names_from = "location_level",
    values_from = c("rps", "wis_per_100k")
  )

preference <- stringr::str_c(
  "rPref::low(rps_nation) * rPref::low(rps_region) * rPref::low(rps_icb)",
  "rPref::low(wis_per_100k_nation) * rPref::low(wis_per_100k_region) * rPref::low(wis_per_100k_icb)",
  sep = " * "
) |>
  # evaluate content of the string as an R expression
  str2expression() |>
  eval()

fronts <- tibble::tibble(
  disease = c("covid-19", "influenza")
) |>
  dplyr::mutate(
    front = purrr::map(
      disease,
      \(.disease) {
        score_summary_geography |>
          dplyr::filter(disease == .disease) |>
          rPref::psel(pref = preference) |>
          dplyr::select(-disease)
      }
    )
  ) |>
  tidyr::unnest(cols = c(front))

## which models are present in the fronts?

fronts |>
  dplyr::distinct(disease, model) |>
  gt::gt() |>
  gt::tab_caption("Pareto fronts per disease")

## for all fronts, we have gam_cr and gam_gp in every model
## note: we should consider the flu and covid gam_cr as entirely different models (same for gam_gp)
## flu has historic_gr_median or ets as the third component
## covid has gr_gp as the third model

## See how components compare against each other (flu only, as this has a non trivial pareto front)

fronts |>
  dplyr::filter(disease == "influenza") |>
  dplyr::select(-wis) |>
  tidyr::pivot_longer(cols = -c("disease", "model")) |>
  dplyr::mutate(score = stringr::str_extract(name, "rps|wis")) |>
  tidyr::nest(data = -score) |>
  dplyr::mutate(
    plot = purrr::map2(
      data,
      score,

      \(.data, .score) {
        .data |>
          ggplot2::ggplot(
            ggplot2::aes(x = name, y = value, colour = model, fill = model)
          ) +
          ggplot2::geom_col(position = "dodge")
      }
    )
  ) |>
  dplyr::select(plot) |>
  purrr::pluck(1) |>
  patchwork::wrap_plots() +
  patchwork::plot_layout(guides = "collect")
