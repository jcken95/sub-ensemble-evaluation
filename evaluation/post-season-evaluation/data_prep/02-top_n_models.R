# construct sub-ensembles based on retrospective model runs (and save to s3)
# use a quantile (and probability) averaging method to construct ensembles cheaply
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

# construct the forecasts for the top-n models approach
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

n_models <- 3

ensemble_model_combinations <- samples_retrospective |>
  # don't want to ensemble the ensembles!
  dplyr::filter(!stringr::str_detect(model, "ensemble")) |>
  # don't care about the DoW model
  dplyr::filter(model != "gam_dow") |>
  # was created post-season, and trained on 24/25 season, biases results
  dplyr::filter(!stringr::str_detect(model, "epinow2")) |>
  dplyr::filter(disease != "rsv") |>
  dplyr::distinct(model, prediction_start_date, disease) |>
  dplyr::collect()


model_combinations <- ensemble_model_combinations |>
  tidyr::drop_na(prediction_start_date) |>
  tidyr::nest(models = model) |>
  dplyr::mutate(
    # contruct all possible model combinations of size n
    models = purrr::map(
      models,

      \(x) {
        # combn() constructs all (unorderd) subsets of size n_models from the set unlist(x)
        x <- combn(unlist(x), m = n_models) |>
          t() |>
          as.data.frame()

        names(x) <- paste0("model_", seq_along(x))

        x
      }
    )
  ) |>
  tidyr::unnest(models) |>
  dplyr::mutate(id = dplyr::row_number()) |> # useful to help nest the model
  tidyr::nest(models = dplyr::starts_with("model_"), .by = c(id, disease, prediction_start_date)) |>
  dplyr::distinct(disease, models)
summary_local <- summary_retrospective |>
  # don't want to ensemble the ensembles!
  dplyr::filter(!stringr::str_detect(model, "ensemble")) |>
  # don't care about the DoW model
  dplyr::filter(model != "gam_dow") |>
  dplyr::rename("observed" = target_value) |>
  dplyr::collect()

ensembles <- model_combinations |>
  # find the quantiles, predictions, etc corresponding to each model subset (for each disease)
  dplyr::mutate(
    summary_subset = purrr::map2(
      disease,
      models,

      \(.disease, .models) {
        summary_local |>
          dplyr::filter(
            disease == .disease,
            model %in% unlist(.models)
          )
      },

      .progress = "computing subset"
    )
  )

## Use a quantile / probability averaging approach for simplicity
# useing {tidytable} for (almost) free speed in the wrangling

ensembles_s3 <- ensembles |>
  dplyr::mutate(
    ensemble = purrr::map(
      summary_subset,

      \(chosen_summary) {
        # side effect: write data

        disease <- unique(chosen_summary$disease)
        models <- chosen_summary$model |>
          unique() |>
          paste0(collapse = "_")

        s3_path <- glue::glue("REDACTED/{disease}_{models}.rds")

        s3_exists <- s3fs::s3_file_exists(s3_path)

        if (s3_exists) {
          return(TRUE)
        }

        ensemble_unit <- c(
          forecasting_unit[!forecasting_unit == "model"],
          "quantile_level"
        )
        out <- chosen_summary |>
          tidytable::as_tidytable() |>
          tidytable::pivot_longer(
            cols = dplyr::starts_with("pi_"),
            names_to = "quantile_level",
            values_to = "predicted"
          ) |>
          tidytable::mutate(
            quantile_level = as.numeric(stringr::str_remove(quantile_level, "pi_"))
          ) |>
          tidytable::summarise(
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
          tidytable::ungroup()

        aws.s3::s3write_using(out, saveRDS, object = s3_path)

        TRUE
      },
      .progress = "constructing ensembles / writing to s3"
    )
  )
