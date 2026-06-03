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

## Pareto plots ----

all_mean_model_effect <- readRDS(here::here("publication/data/all_mean_model_effect.rds"))

scoring_s3_root <- "REDACTED"
scoring_s3_root_ordinal <- "REDACTED"

ensembles_scored <- tibble::tibble(
  s3_path = s3fs::s3_dir_ls(scoring_s3_root)
) |>
  dplyr::mutate(data = purrr::map(s3_path, \(fpath) s3$read_using(fpath, fn = readRDS))) |>
  tidyr::unnest(data) |>
  dplyr::select(-s3_path) |>
  # was created post-season, and trained on 24/25 season, biases results
  dplyr::filter(!stringr::str_detect(model, "epinow2")) |>
  dplyr::filter(scale == "per_capita")

rps_scored <- tibble::tibble(
  s3_path = s3fs::s3_dir_ls(scoring_s3_root_ordinal)
) |>
  dplyr::mutate(data = purrr::map(s3_path, \(fpath) s3$read_using(fpath, fn = readRDS))) |>
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
  ) |>
  dplyr::filter(model != "ensemble_matched")

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

pareto_front <- pareto_front |>
  dplyr::mutate(
    disease = dplyr::case_when(
      disease == "covid-19" ~ "COVID-19",
      disease == "influenza" ~ "Influenza",
      .default = NA_character_
    )
  )

weighted_score_summary <- weighted_score_summary |>
  dplyr::mutate(
    disease = dplyr::case_when(
      disease == "covid-19" ~ "COVID-19",
      disease == "influenza" ~ "Influenza",
      .default = NA_character_
    )
  )

# create a model nickname for tidyness in plot
model_code <- all_mean_model_effect |>
  dplyr::distinct(name) |>
  dplyr::mutate(name_short = LETTERS[seq_len(n())])


all_mean_model_effect_distinct <- all_mean_model_effect |>
  dplyr::mutate(
    disease = tolower(disease),
    disease = dplyr::case_when(
      disease == "covid-19" ~ "COVID-19",
      disease == "influenza" ~ "Influenza",
      .default = NA
    )
  ) |>
  dplyr::distinct(name, disease)

model_code_lookup <- weighted_score_summary |>
  tidyr::unnest(summary) |>
  dplyr::distinct(model, disease) |>
  dplyr::rename("ensemble" = model) |>
  tidyr::nest(ensemble_names = ensemble) |>
  dplyr::left_join(
    all_mean_model_effect_distinct,
    by = "disease"
  ) |>
  tidyr::unnest(cols = ensemble_names) |>
  dplyr::mutate(model_present = stringr::str_detect(ensemble, name)) |>
  dplyr::left_join(model_code, by = "name") |>
  dplyr::group_by(ensemble) |>
  dplyr::filter(model_present) |>
  dplyr::summarise(code_name = stringr::str_flatten(unique(name_short)))


weighted_pareto_plot <-
  withr::with_seed(
    seed = 4321,
    code = {
      weighted_score_summary |>
        tidyr::unnest(summary) |>
        dplyr::mutate(is_front = dplyr::if_else(model %in% pareto_front$model, "PO", "NPO")) |>
        dplyr::left_join(model_code_lookup, by = c("model" = "ensemble")) |>
        ggplot2::ggplot(ggplot2::aes(x = log(wis_per_100k), y = log(rps))) +
        ggplot2::geom_point(ggplot2::aes(colour = is_front, fill = is_front), size = 0.75) +
        ggplot2::geom_line(data = pareto_front, colour = projection_plots$select_ukhsa_colour("teal")) +
        ggrepel::geom_text_repel(
          ggplot2::aes(label = code_name, colour = is_front),
          size = 3
        ) +
        ggplot2::scale_colour_manual(
          values = c("PO" = projection_plots$select_ukhsa_colour("teal"), "NPO" = "grey30")
        ) +
        ggplot2::scale_fill_manual(values = c("PO" = projection_plots$select_ukhsa_colour("teal"), "NPO" = "black")) +
        ggplot2::labs(
          x = "log(pcWIS*)",
          y = "log(RPS*)"
        ) +
        projection_plots$theme_pancasts() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::facet_wrap(~disease, scales = "free", ncol = 1) +
        ggplot2::ggtitle(
          "Sub-Ensemble Pareto front",
          "RPS* and pcWIS* are computed as a weighted average across geographies"
        )
    }
  )


layout <- c(
  patchwork::area(t = 1, l = 1, b = 4, r = 5),
  patchwork::area(t = 5, l = 1, b = 5.2, r = 5)
)

model_code_table <- model_code |>
  dplyr::left_join(model_code_to_name) |>
  dplyr::select(full_name, name_short) |>
  gt::gt() |>
  gt::tab_header("Model abbreviations") |>
  gt::cols_label(
    full_name = "Model",
    name_short = "Abbreviation"
  )

model_code_table

weighted_pareto_combo <- weighted_pareto_plot + model_code_table + patchwork::plot_layout(widths = c(3, 2))

weighted_pareto_combo

ggplot2::ggsave(
  here::here(plot_output_dir, "pareto_front.png"),
  plot = weighted_pareto_combo,
  width = 16,
  height = 12
)

ggplot2::ggsave(
  filename = here::here(plot_output_dir_tiff, "fig_5.tiff"),
  plot = weighted_pareto_combo,
  width = 19,
  height = 14.25,
  dpi = 300,
  units = "cm"
)

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
  gt::tab_caption("Pareto fronts per disease") |>
  gt::gtsave(
    filename = here::here(plot_output_dir, "pareto_front.docx")
  )

## for all fronts, we have gam_cr and gam_gp in every model
## note: we should consider the flu and covid gam_cr as entirely different models (same for gam_gp)
## flu has historic_gr_median or ets as the third component
## covid has gr_gp as the third model

## See how components compare against each other

models_regex <- model_code_to_name |>
  dplyr::pull(name) |>
  stringr::str_flatten(collapse = "|")


pareto_contributions <- fronts |>
  tidyr::pivot_longer(cols = -c("disease", "model")) |>
  dplyr::mutate(
    score = toupper(stringr::str_extract(name, "rps|wis")),
    score = dplyr::if_else(score == "WIS", "PCWIS per 100K", score),
    name = stringr::str_extract(name, "icb|nation|region"),
    y_text = 0,
    model = stringr::str_extract_all(model, models_regex),
    model = unlist(purrr::map(model, \(m) stringr::str_flatten(m, " "))),
    disease = dplyr::case_when(
      disease == "covid-19" ~ "COVID-19",
      disease == "influenza" ~ "Influenza",
      .default = NA_character_
    ),
    name = factor(name, levels = c("icb", "region", "nation")),
    name = forcats::fct_relabel(
      name,
      \(fct_lvl) dplyr::if_else(fct_lvl == "icb", "ICB", stringr::str_to_sentence(fct_lvl))
    )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(x = name, y = value, colour = model, fill = model)
  ) +
  ggplot2::geom_col(
    position = "dodge"
  ) +
  ggplot2::geom_text(
    ggplot2::aes(x = name, y = y_text, label = model),
    colour = "black",
    angle = 90,
    hjust = 0,
    position = ggplot2::position_dodge(width = 0.9)
  ) +
  ggplot2::facet_grid(
    ggplot2::vars(score),
    ggplot2::vars(disease)
  ) +
  ggplot2::labs(
    x = "Spatial granularity",
    y = "Score value"
  ) +
  ggplot2::ggtitle(
    "Component scores for ensembles on the PF",
    subtitle = "Colour indicates ensemble"
  ) +
  ggplot2::theme(legend.position = "none")


ggplot2::ggsave(
  plot = pareto_contributions,
  filename = here::here(plot_output_dir, "SUPPLEMENT_pareto_contributions.png"),
  width = 16,
  height = 12
)

ggplot2::ggsave(
  filename = here::here(plot_supplement_dir_tiff, "fig_c.tiff"),
  plot = weighted_pareto_combo,
  width = 19,
  height = 14.25,
  dpi = 300,
  units = "cm"
)
