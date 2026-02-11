box::use(box / deps_)

deps_$need(
  "crew",
  "dplyr",
  "fs",
  "glue",
  "here",
  "parallel",
  "purrr",
  "rlang",
  "s3fs",
  "stringr",
  "tarchetypes",
  "targets",
  "lubridate",
  "tidyr",
  "vroom",
  "withr",
  "yaml",
  "zoo"
)

# Set up static-branched targets (one branch per model) ========================

# Fetch model hyperparams from the config file
config_file_path <- here::here("rsv", "models", "admissions", "rsv_admissions_config.yaml")

config_models <- config::get("models", file = config_file_path) |>
  purrr::keep(\(.) isTRUE(.$include_run))


# Set parallel configuration
targets::tar_option_set(
  controller = crew::crew_controller_local(
    workers = max(
      config::get(
        "target_workers",
        file = here::here(
          "rsv",
          "models",
          "admissions",
          "rsv_admissions_config.yaml"
        )
      ),
      floor(parallel::detectCores() / 5)
    )
  )
)

# Create a target list
model_targets <- tarchetypes::tar_map(
  values = config_models |>
    # Have to use expressions: https://books.ropensci.org/targets/static.html#limitations
    purrr::map(\(.) rlang::call2("list", !!!.)) |>
    tibble::enframe(name = "MODEL_NAME", value = "HYPERPARAMS"),

  # Use MODEL_NAME for target-name suffixes
  names = MODEL_NAME,

  # No need for a custom description
  descriptions = NULL,

  # Don't flatten returned structure - useful when we extract the separate parts
  # later when building the "samples" and "preds" targets with tarchetypes::tar_combine()
  unlist = FALSE,
  targets::tar_target(
    model_fn,
    cue = targets::tar_cue("always"), # in case model file has been tweaked
    command = {
      # We can't quite use a "normal" call to box::use(), since we need to
      # replace certain parts of the module path
      do.call(
        box::use,
        list(substitute(
          . / rsv / models / admissions / models / MODEL["fn" = FN],
          list(
            "MODEL" = as.name(MODEL_NAME),
            "FN" = as.name(paste0("run_", MODEL_NAME))
          )
        ))
      )

      # we cannot update a target like a list, so making a local copy
      local_hyperparams <- HYPERPARAMS
      local_hyperparams$nb$age_group <- mrf_structures$age_nb

      # Create a function based on `fn`, but with some arguments pre-filled
      # because we already know what their values should always be!
      #
      # In fact, the only argument we *aren't* pre-filling is `prediction_date`,
      # which we'll provide when we call this function within the `model` target
      #
      # More info: https://purrr.tidyverse.org/reference/partial.html
      purrr::partial(
        fn,
        ... = ,
        .data = data,
        forecast_horizon = config_overall_params$forecast_horizon,
        n_pi_samples = config_overall_params$n_pi_sample,
        output_variables = config_required_covariates,
        hyperparams = local_hyperparams
      )
    }
  ),

  # Create a "model_*" target for each model - each will be suffixed by MODEL_NAME
  targets::tar_target(
    model,

    # Create a separate "dynamic branch" for each value in start_dates
    # See docs for targets::tar_pattern() - note that map() here is a "pattern"
    # which can be interpreted by {targets} i.e. it is NOT purrr::map()
    pattern = map(start_dates),

    # Again don't flatten returned structure - useful when we extract the
    # separate parts in the "samples_*" and "preds_*" targets we make shortly
    iteration = "list",
    command = {
      box::use(prj / intervals)

      sample_preds <- start_dates |>
        rlang::set_names() |>
        # For each start date, call the function we constructed a moment ago
        # Parallelised if user has set up a parallel plan with future::plan()
        furrr::future_map(model_fn, .options = furrr::furrr_options(seed = TRUE)) |>
        purrr::map("sample_predictions") |>
        purrr::list_rbind() |>
        # For identification later, when all results are combined into one big data frame
        dplyr::mutate("model" = MODEL_NAME, .before = 1) |>
        dplyr::full_join(
          data |>
            dplyr::filter(date >= "2022-09-01") |>
            dplyr::select(date, nhs_region_name, age_group, target, population),
          by = c("nhs_region_name", "age_group", "date", "population")
        ) |>
        # we are converting from the estimated rate to estimated counts using
        #  the true population; bring in the true (total) denominator,
        #   not reported denominator.
        dplyr::left_join(
          trust_age_population |>
            dplyr::summarise(
              total_population = sum(population),
              .by = c("nhs_region_name", "age_group")
            ),
          by = c("nhs_region_name", "age_group")
        ) |>
        # We adjust up the predictions because rsv admissions uses a subset of trusts
        # The adjustment maps the prediction across the subset of trusts to a regional prediction
        dplyr::mutate(
          # adjust up predictions
          .value = round(total_population * (.value / population)),
          # adjust up the true values for the plots:
          target = round(total_population * (target / population)),
          age_group_granularity = "full",
          location_level = "region"
        ) |>
        dplyr::select(-total_population)

      all_preds <- list(
        "geo_preds" = list(
          "nation" = intervals$samples_to_quantiles(
            .sample_predictions = sample_preds,
            remove_identifiers = c("age_group", "nhs_region_name"),
            overall_params = config_overall_params,
            method = "quantile"
          ) |>
            dplyr::mutate(
              model = MODEL_NAME,
              age_group = "all",
              age_group_granularity = "none",
              location = "England",
              location_level = "nation"
            ),
          "region" = intervals$samples_to_quantiles(
            .sample_predictions = sample_preds,
            remove_identifiers = c("age_group"),
            overall_params = config_overall_params,
            method = "quantile"
          ) |>
            dplyr::mutate(
              model = MODEL_NAME,
              age_group = "all",
              age_group_granularity = "none",
              location_level = "region"
            ) |>
            dplyr::rename(location = nhs_region_name)
        ) |>
          purrr::list_rbind(names_to = "location_level"),
        "age_preds" = list(
          "fine" = intervals$samples_to_quantiles(
            .sample_predictions = sample_preds |>
              dplyr::mutate(
                age_group = dplyr::case_when(
                  age_group %in% c("[0,1)", "[1,2)") ~ "[0,2)",
                  age_group %in% c("[75,85)", "[85,120)") ~ "[75,120)",
                  .default = age_group
                )
              ),
            remove_identifiers = c("nhs_region_name"),
            overall_params = config_overall_params,
            method = "quantile"
          ) |>
            dplyr::mutate(
              model = MODEL_NAME,
              location = "England",
              location_level = "nation",
              age_group_granularity = "fine"
            ),
          "coarse" = intervals$samples_to_quantiles(
            .sample_predictions = sample_preds |>
              dplyr::mutate(
                age_group = dplyr::case_when(
                  age_group %in% c("[0,1)", "[1,2)", "[2,5)") ~ "[0,5)",
                  age_group %in% c("[65,75)", "[75,85)", "[85,120)") ~ "[65,120)",
                  .default = age_group
                )
              ),
            remove_identifiers = NULL,
            overall_params = config_overall_params,
            method = "quantile"
          ) |>
            dplyr::rename(location = nhs_region_name) |>
            dplyr::mutate(
              model = MODEL_NAME,
              location = "England",
              location_level = "region",
              age_group_granularity = "coarse"
            )
        ) |>
          purrr::list_rbind(names_to = "age_group_granularity")
      ) |>
        dplyr::bind_rows() |>
        dplyr::mutate("model" = MODEL_NAME)

      list(
        "sample_preds" = sample_preds,
        "all_preds" = all_preds
      )
    }
  ),

  # Create "samples_*" and "preds_*" targets
  targets::tar_target(samples, model |> purrr::map("sample_preds") |> purrr::list_rbind()),
  targets::tar_target(preds, model |> purrr::map("all_preds") |> purrr::list_rbind())
)


list(
  # Setup ====================================================================
  targets::tar_target(
    mrf_structures,
    {
      source(
        here::here("rsv", "models", "src", "mrf_structures.R"),
        local = TRUE
      )
      list(
        "age_nb" = age_nb,
        "nhs_nb" = nhs_nb,
        "age_breakdowns" = age_breakdowns
      )
    }
  ),
  targets::tar_target(
    trust_age_population,
    {
      age_breakdowns <- mrf_structures$age_breakdowns # save re-running that
      source(
        here::here("rsv", "models", "src", "population.R"),
        local = TRUE
      )
      trust_age_population
    }
  ),

  # Config ===================================================================

  targets::tar_target(
    config_file,
    here::here("rsv", "models", "admissions", "rsv_admissions_config.yaml"),
    format = "file"
  ),
  targets::tar_target(
    config,
    {
      input_file <- config::get(file = config_file)
      # add MRF structures to hyperparameters from the sourced script above.
      model_configs <- purrr::map(
        names(input_file$models),
        \(chosen_model_name) {
          chosen_model_config <- input_file$models[[chosen_model_name]]

          chosen_model_config$nb$age_group <- mrf_structures$age_nb
          chosen_model_config$nhs_region_name <- mrf_structures$nhs_nb

          chosen_model_config
        }
      )

      names(model_configs) <- names(input_file$models)

      out <- input_file
      out$models <- model_configs
      out
    }
  ),
  targets::tar_target(
    config_overall_params,
    config$overall_params
  ),
  targets::tar_target(
    config_required_covariates,
    config$required_covariates
  ),
  targets::tar_target(
    config_files,
    config$files
  ),
  targets::tar_target(
    excluded_trusts,
    config$exclude_trusts
  ),
  targets::tar_target(
    included_model_names,
    config |>
      purrr::pluck("models") |>
      purrr::keep(\(.) isTRUE(.$include_run)) |>
      names()
  ),
  targets::tar_target(
    ensemble_model_names,
    config |>
      purrr::pluck("ensemble") |>
      purrr::keep(\(.) isTRUE(.)) |>
      names()
  ),
  targets::tar_target(
    check_ensemble_choice,
    {
      model_diff <- setdiff(ensemble_model_names, included_model_names)

      if (length(model_diff) != 0) {
        models_not_run <- stringr::str_flatten_comma(model_diff)

        stop(paste("Models are being ensembled that have not been run:", models_not_run))
      }

      model_diff
    }
  ),

  # Data Preprocessing =======================================================

  targets::tar_target(
    raw_data,
    cue = targets::tar_cue("always"), # we might have fresh data upstream - we should always check!
    command = {
      box::use(box / redshift)

      rs <- redshift$data_model(c("REDACTED", "REDACTED"))

      rs$REDACTED |>
        dplyr::select(
          date,
          trust_code,
          # we don't want all RSV columns, only those corresponding to admissions
          dplyr::starts_with("rsv_n")
        ) |>
        dplyr::rename_with(\(.) sub("^rsv_n", "n", .)) |>
        dplyr::filter(!(trust_code %in% excluded_trusts)) |>
        # We must arrange here to ensure we always have the same row order
        # Otherwise, random row order -> different result hash -> unnecessary downstream execution!
        dplyr::arrange(date, trust_code) |>
        dplyr::collect() |>
        dplyr::left_join(
          rs$REDACTED |>
            dplyr::select(code, name, nhser23nm) |>
            dplyr::rename(
              "trust_code" = code,
              "trust_name" = name,
              "nhs_region_name" = nhser23nm
            ) |>
            dplyr::collect(),
          by = c("trust_code")
        )
    }
  ),
  targets::tar_target(
    data_max_dates,
    dplyr::summarise(
      raw_data,
      dplyr::across(
        !(c("date", "trust_code", "trust_name", "nhs_region_name")),
        \(col) {
          dplyr::pick(date, col) |>
            dplyr::filter(!is.na(col)) |>
            dplyr::summarise(max(date)) |>
            dplyr::pull(1)
        }
      )
    ) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "source", values_to = "max_date")
  ),
  targets::tar_target(
    data,
    {
      sgss_clean <- raw_data |>
        tidyr::pivot_longer(
          cols = dplyr::starts_with("n_age_"),
          names_to = "age",
          values_to = "count"
        ) |>
        dplyr::mutate(age = as.integer(stringr::str_remove(age, "n_age_"))) |>
        dplyr::filter(!is.na(age)) |> # we cannot bin missing ages
        # age_breakdowns defined in mrf_structures.R;
        #  N.B. some combined after model
        dplyr::mutate(
          age_group = cut(age, mrf_structures$age_breakdowns, right = FALSE)
        ) |>
        dplyr::filter(!is.na(age_group)) |>
        dplyr::summarise(
          count = sum(count),
          .by = c(date, trust_code, nhs_region_name, age_group)
        ) |>
        dplyr::filter(
          date <= max(date)
        ) |>
        dplyr::left_join(
          trust_age_population,
          by = c("trust_code", "age_group", "nhs_region_name")
        ) |>
        # truncate to max date where we have all RSV values
        # due to combined_data table appraoch
        dplyr::filter(date <= min(data_max_dates$max_date)) |>
        # NOTE: RSV tests are backfilled. A crude 2 day cut off is applied
        # however, the best approach would be more data informed.
        dplyr::filter(date <= max(date) - lubridate::days(2))

      # generate all combinations of date, location, and age;
      # which we need to predict with
      regional_spine <- expand.grid(
        date = seq(min(sgss_clean$date), max(sgss_clean$date), by = "day"),
        nhs_region_name = unique(sgss_clean$nhs_region_name),
        age_group = unique(sgss_clean$age_group)
      )

      training_data <- sgss_clean |>
        # remove impact of trusts with only zero counts:
        # first assume all missing are zero:
        dplyr::mutate(count = dplyr::coalesce(count, 0)) |>
        # calculate the rolling sum by age group and trust (to work out if zero)
        dplyr::group_by(trust_code, age_group) |>
        dplyr::arrange(date) |>
        dplyr::mutate(
          trust_age_level_rolling_sum_count = zoo::rollapplyr(
            count,
            90,
            sum,
            partial = TRUE,
            align = "right"
          )
        ) |>
        dplyr::ungroup() |>
        # get overall trust counts,
        # given we think misreporting is at a trust not trust:age level
        dplyr::group_by(trust_code, date) |>
        dplyr::mutate(
          trust_level_rolling_sum_count = sum(
            trust_age_level_rolling_sum_count,
            na.rm = TRUE
          )
        ) |>
        dplyr::ungroup() |>
        # let's remove any counts for trusts that don't meet our criteria at
        # a point in time so they don't contribute to the regional numerator.
        # NOTE: NAs and zeros are treated the same in this approach
        dplyr::mutate(
          count = dplyr::case_when(
            trust_level_rolling_sum_count == 0 ~ NA_real_,
            .default = count
          ),
          # Then make their populations low (but not zero) so they don't
          # contribute to the regional denominator:
          population = dplyr::case_when(
            trust_level_rolling_sum_count == 0 ~ 1, # 0 messes offset up
            .default = population
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(date, nhs_region_name, age_group) |>
        dplyr::summarise(
          target = sum(count, na.rm = TRUE),
          population = sum(population, na.rm = TRUE)
        ) |>
        dplyr::ungroup() |>
        dplyr::left_join(
          regional_spine,
          .,
          by = c("date", "nhs_region_name", "age_group")
        ) |>
        # fill in missing (ie no tests) with 0
        dplyr::mutate(target = dplyr::coalesce(target, 0))

      training_data
    }
  ),
  targets::tar_target(
    max_lookback,
    max(data$date) + 1
  ),
  targets::tar_target(
    # define lookback start dates
    start_dates,
    {
      box::use(box / redshift)

      summary <- dplyr::tbl(redshift$connect(use_existing = FALSE), I("REDACTED"))

      summary |>
        dplyr::filter(grepl("rsv", disease, ignore.case = TRUE)) |>
        dplyr::filter(!is.na(prediction_start_date)) |>
        dplyr::distinct(prediction_start_date) |>
        dplyr::select(prediction_start_date) |>
        dplyr::collect() |>
        # the rsv job really struggles with the `samples` target
        # we will break the job up into pre-post xmas to help with this
        # change year as appropriate
        dplyr::filter(lubridate::year(prediction_start_date) == "2025")
    }
  ),
  # Models ================================================================

  ## Components --------------------------------------------------------------

  # The model_*, samples_*, and preds_* targets defined earlier
  model_targets,

  # Combine the relevant subsets of targets
  tarchetypes::tar_combine(samples, model_targets[["samples"]]),
  tarchetypes::tar_combine(preds, model_targets[["preds"]]),

  ## Ensemble ----------------------------------------------------------------

  targets::tar_target(
    preds_ensemble,
    {
      box::use(prj / ensemble)

      chosen_samples <- samples |>
        dplyr::filter(model %in% ensemble_model_names)

      list(
        "nation" = ensemble$ensemble_from_samples(
          .sample_predictions = chosen_samples,
          remove_identifiers = c("nhs_region_name", "age_group"),
          method = "mellor",
          model_name = paste(ensemble_model_names, collapse = "_"),
          overall_params = config_overall_params
        ) |>
          dplyr::mutate(
            age_group = "all",
            age_group_granularity = "none",
            location = "England",
            location_level = "nation"
          ),
        "region" = ensemble$ensemble_from_samples(
          .sample_predictions = chosen_samples,
          remove_identifiers = "age_group",
          method = "mellor",
          model_name = paste(ensemble_model_names, collapse = "_"),
          overall_params = config_overall_params
        ) |>
          dplyr::mutate(
            age_group = "all",
            age_group_granularity = "none",
            location_level = "region"
          ) |>
          dplyr::rename("location" = nhs_region_name),
        "age" = ensemble$ensemble_from_samples(
          .sample_predictions = chosen_samples |>
            dplyr::mutate(
              age_group = dplyr::case_when(
                age_group %in% c("[0,1)", "[1,2)") ~ "[0,2)",
                age_group %in% c("[75,85)", "[85,120)") ~ "[75,120)",
                .default = age_group
              )
            ),
          remove_identifiers = "nhs_region_name",
          method = "mellor",
          model_name = paste(ensemble_model_names, collapse = "_"),
          overall_params = config_overall_params
        ) |>
          dplyr::mutate(
            location = "England",
            location_level = "nation",
            age_group_granularity = "fine"
          )
      ) |>
        dplyr::bind_rows()
    }
  ),

  # Results ==================================================================

  targets::tar_target(
    model_names,
    result_summary |>
      dplyr::distinct(model) |>
      dplyr::pull(model)
  ),

  # aggregate each model to the required geography
  targets::tar_target(
    result_samples,
    {
      box::use(
        prj / intervals,
        prj / format,
        prj / checks
      )

      out <- list(
        "nation" = intervals$aggregate_samples(
          samples,
          remove_identifiers = c("nhs_region_name", "age_group")
        ) |>
          dplyr::mutate(location = "England"),
        "region" = intervals$aggregate_samples(
          samples,
          remove_identifiers = "age_group"
        ) |>
          dplyr::rename("location" = nhs_region_name),
        "age" = intervals$aggregate_samples(
          samples,
          remove_identifiers = "nhs_region_name"
        ) |>
          dplyr::mutate(location = "England")
      ) |>
        dplyr::bind_rows() |>
        # purrr::list_rbind(names_to = "location_level") |>
        format$format_outputs(
          config_overall_params$target_name,
          config_overall_params$forecast_horizon,
          config_overall_params$disease
        )

      checks$check_forecast_format_sample(out)

      out
    }
  ),

  # do some formatting
  targets::tar_target(
    result_summary,
    {
      box::use(
        prj / format,
        prj / checks
      )

      out <- dplyr::bind_rows(
        preds,
        preds_ensemble
      ) |>
        format$format_outputs(
          config_overall_params$target_name,
          config_overall_params$forecast_horizon,
          config_overall_params$disease
        )

      checks$check_forecast_format_summary(out)

      out
    }
  ),

  # Outputs ==================================================================
  targets::tar_target(
    make_timestamp,
    {
      force(result_summary) # Have to declare dependency
      format(Sys.time(), "%Y-%m-%d_%H:%M:%S_%a")
    }
  ),
  targets::tar_target(
    output_path,
    here::here("rsv", "outputs", "admissions", make_timestamp) |>
      fs::dir_create()
  ),

  ## Plotting ----------------------------------------------------------------
  # use c("lookbacks", "rag") in plots_include to show coloured projections

  # RSV peaks are temperamental, so in case they're used
  # I've separated out the regional, national, and age grouped plotting.

  targets::tar_target(
    plot_projections_nation,
    {
      box::use(prj / outputs)

      if (config$overall_params$show_peaks) {
        rsv_peaks_data <- outputs$rsv_peaks_handler(
          target_name = "estimated admissions",
          training_data = data,
          trust_age_population = trust_age_population,
          geography = "nation",
          age_granularity = "none"
        )
      } else {
        rsv_peaks_data <- FALSE
      }

      plots <- purrr::map(
        model_names,
        \(model_name) {
          outputs$projections_plotter(
            plots_include = list(c("rag", "multiple_cis")),
            data = result_summary,
            target_name = "estimated admissions",
            model_name = model_name,
            geography = "nation",
            age_granularity = "none", # the default
            y_limit = 1000,
            peaks_data = rsv_peaks_data,
            should_nudge_x = TRUE,
            plot_historic_fit = config$overall_params$show_fits,
            output_path = output_path,
            disease = config$overall_params$disease
          )
        }
      )

      names(plots) <- model_names

      plots
    }
  ),
  targets::tar_target(
    plot_projections_region,
    {
      box::use(prj / outputs)

      if (config$overall_params$show_peaks) {
        rsv_peaks_data <- outputs$rsv_peaks_handler(
          target_name = "estimated admissions",
          training_data = data,
          trust_age_population = trust_age_population,
          geography = "region",
          age_granularity = "none"
        )
      } else {
        rsv_peaks_data <- FALSE
      }

      plots <- purrr::map(
        model_names,
        \(model_name) {
          outputs$projections_plotter(
            plots_include = list(c("rag", "multiple_cis")),
            data = result_summary,
            target_name = "estimated admissions",
            model_name = model_name,
            geography = "region",
            age_granularity = "none",
            y_limit = 300,
            peaks_data = rsv_peaks_data,
            should_nudge_x = TRUE,
            plot_historic_fit = config$overall_params$show_fits,
            output_path = output_path,
            disease = config$overall_params$disease
          )
        }
      )

      names(plots) <- model_names

      plots
    }
  ),
  targets::tar_target(
    plot_rag,
    {
      box::use(prj / outputs)

      purrr::walk(
        model_names,
        \(model_name) {
          outputs$rag_plotter(
            data = result_summary,
            target_name = rlang::sym(config$overall_params$target_name),
            model_name = model_name,
            geography = "nation",
            output_path = output_path,
            disease = config$overall_params$disease
          )
        }
      )
    }
  ),
  targets::tar_target(
    plot_projections_nation_age_groups,
    {
      box::use(prj / outputs)

      if (config$overall_params$show_peaks) {
        rsv_peaks_data <- outputs$rsv_peaks_handler(
          target_name = "estimated admissions",
          training_data = data,
          trust_age_population = trust_age_population,
          geography = "nation",
          age_granularity = "fine"
        )
      } else {
        rsv_peaks_data <- FALSE
      }
      plots <- purrr::map(
        model_names,
        \(model_name) {
          outputs$projections_plotter(
            plots_include = list(c("rag", "multiple_cis")),
            data = result_summary,
            target_name = "estimated admissions",
            model_name = model_name,
            geography = "nation",
            age_granularity = "fine",
            y_limit = 600,
            peaks_data = rsv_peaks_data,
            should_nudge_x = TRUE,
            plot_historic_fit = config$overall_params$show_fits,
            output_path = output_path,
            disease = config$overall_params$disease
          )
        }
      )
      names(plots) <- model_names

      plots
    }
  ),
  ## Copy plots with lookbacks for QA

  targets::tar_target(
    output_path_qa,
    fs::dir_create(output_path, "qa")
  ),
  targets::tar_target(
    plot_projections_nation_qa,
    {
      box::use(prj / outputs)

      if (config$overall_params$show_peaks) {
        rsv_peaks_data <- outputs$rsv_peaks_handler(
          target_name = "estimated admissions",
          training_data = data,
          trust_age_population = trust_age_population,
          geography = "nation",
          age_granularity = "none"
        )
      } else {
        rsv_peaks_data <- FALSE
      }

      plots <- purrr::map(
        model_names,
        \(model_name) {
          outputs$projections_plotter(
            plots_include = list(c("lookbacks", "rag", "multiple_cis")),
            data = result_summary,
            target_name = "estimated admissions",
            model_name = model_name,
            geography = "nation",
            age_granularity = "none", # the default
            y_limit = 1000,
            peaks_data = rsv_peaks_data,
            should_nudge_x = TRUE,
            plot_historic_fit = config$overall_params$show_fits,
            output_path = output_path_qa,
            disease = config$overall_params$disease
          )
        }
      )

      names(plots) <- model_names

      plots
    }
  ),
  targets::tar_target(
    plot_projections_region_qa,
    {
      box::use(prj / outputs)

      if (config$overall_params$show_peaks) {
        rsv_peaks_data <- outputs$rsv_peaks_handler(
          target_name = "estimated admissions",
          training_data = data,
          trust_age_population = trust_age_population,
          geography = "region",
          age_granularity = "none"
        )
      } else {
        rsv_peaks_data <- FALSE
      }

      plots <- purrr::map(
        model_names,
        \(model_name) {
          outputs$projections_plotter(
            plots_include = list(c("lookbacks", "rag", "multiple_cis")),
            data = result_summary,
            target_name = "estimated admissions",
            model_name = model_name,
            geography = "region",
            age_granularity = "none",
            y_limit = 300,
            peaks_data = rsv_peaks_data,
            should_nudge_x = TRUE,
            plot_historic_fit = config$overall_params$show_fits,
            output_path = output_path_qa,
            disease = config$overall_params$disease
          )
        }
      )

      names(plots) <- model_names

      plots
    }
  ),
  targets::tar_target(
    plot_rag_qa,
    {
      box::use(prj / outputs)

      purrr::walk(
        model_names,
        \(model_name) {
          outputs$rag_plotter(
            data = result_summary,
            target_name = rlang::sym(config$overall_params$target_name),
            model_name = model_name,
            geography = "nation",
            output_path = output_path_qa,
            disease = config$overall_params$disease
          )
        }
      )
    }
  ),
  targets::tar_target(
    plot_projections_nation_age_groups_qa,
    {
      box::use(prj / outputs)

      if (config$overall_params$show_peaks) {
        rsv_peaks_data <- outputs$rsv_peaks_handler(
          target_name = "estimated admissions",
          training_data = data,
          trust_age_population = trust_age_population,
          geography = "nation",
          age_granularity = "fine"
        )
      } else {
        rsv_peaks_data <- FALSE
      }
      plots <- purrr::map(
        model_names,
        \(model_name) {
          outputs$projections_plotter(
            plots_include = list(c("lookbacks", "rag", "multiple_cis")),
            data = result_summary,
            target_name = "estimated admissions",
            model_name = model_name,
            geography = "nation",
            age_granularity = "fine",
            y_limit = 600,
            peaks_data = rsv_peaks_data,
            should_nudge_x = TRUE,
            plot_historic_fit = config$overall_params$show_fits,
            output_path = output_path_qa,
            disease = config$overall_params$disease
          )
        }
      )
      names(plots) <- model_names

      plots
    }
  ),
  ## Narratives --------------------------------------------------------------

  targets::tar_target(
    output_narratives_national,
    {
      box::use(prj / narratives)

      # National narrative

      narratives <- purrr::map(
        model_names,
        \(model_name) {
          narratives$narrative_txt_output(
            data = result_summary,
            target_name = "estimated admissions",
            model_name = model_name,
            geography = "nation",
            age_granularity = "none",
            disease = "RSV",
            output_path = output_path
          )
        }
      )

      narratives
    }
  ),
  targets::tar_target(
    output_narratives_regional,
    {
      box::use(prj / narratives)

      # Regional narrative

      model <- model_names |>
        tibble::tibble() |>
        dplyr::filter(grepl("ensemble", model_names)) |>
        unlist()

      regions <- result_summary |>
        dplyr::filter(location_level == "region") |>
        dplyr::distinct(location) |>
        dplyr::pull()

      region_model_combinations <- tidyr::expand_grid(
        model = model,
        regions = regions
      )

      regional_narratives <- purrr::map2(
        region_model_combinations$model,
        region_model_combinations$regions,
        \(chosen_model, chosen_region) {
          narratives$narrative_txt_output(
            data = dplyr::filter(result_summary, location == chosen_region),
            target_name = rlang::sym(config_overall_params$target_name),
            model_name = chosen_model,
            geography = "region",
            age_granularity = "none",
            disease = "RSV",
            output_path = output_path,
            as_tibble = TRUE
          )
        }
      ) |>
        dplyr::bind_rows()

      regional_narratives
    }
  ),
  targets::tar_target(
    output_tables,
    {
      box::use(prj / narratives)

      ensemble_name <- result_summary$model |>
        unique() |>
        stringr::str_subset(pattern = "ensemble")

      formatted_output_table_no_age <- narratives$create_narrative_tables(
        result_summary,
        target_name = config_overall_params$target_name,
        model_name = ensemble_name,
        disease = config_overall_params$disease,
        location_levels = c("nation", "region"),
        age_granularity = "none",
        output_path = output_path
      )

      formatted_output_table_age <- narratives$create_narrative_tables(
        result_summary,
        target_name = config_overall_params$target_name,
        model_name = ensemble_name,
        disease = config_overall_params$disease,
        location_levels = c("nation"),
        age_granularity = "fine",
        output_path = output_path
      )

      dplyr::bind_rows(
        formatted_output_table_no_age,
        formatted_output_table_age
      )
    }
  ),

  ## Upload artefacts---------------------------------------------------------

  targets::tar_target(
    output_zip,
    {
      # These must have run already at this point
      force(c(
        output_narratives_national,
        output_narratives_regional,
        plot_projections_nation,
        plot_projections_nation_age_groups,
        plot_projections_region,
        plot_rag
      ))

      z <- fs::file_temp(ext = "zip")
      withr::with_dir(output_path, utils::zip(z, fs::dir_ls(".", recurse = TRUE)))
      z
    }
  ),
  targets::tar_target(
    upload_zip,
    s3fs::s3_file_upload(
      output_zip,
      s3fs::s3_path(
        "PATH REDACTED",
        ext = "zip"
      ),
      overwrite = TRUE
    )
  ),
  targets::tar_target(
    upload_summary,
    {
      box::use(box / s3)

      s3$write_to_s3(
        result_summary,
        s3fs::s3_path(
          "PATH REDACTED",
          ext = "csv.gz"
        ),
        overwrite = TRUE
      )
    }
  ),
  targets::tar_target(
    upload_samples,
    {
      box::use(box / s3)

      s3$write_to_s3(
        result_samples,
        s3fs::s3_path(
          "PATH REDACTED",
          ext = "csv.gz"
        ),
        overwrite = TRUE
      )
    }
  ),

  ## regional team output --------------------------------------------------------

  targets::tar_target(
    output_regional_team_summary,
    {
      box::use(
        prj / projection_plots,
        prj / outputs
      )

      ### grab plots ----

      regional_plots <- plot_projections_nation[grepl(
        "ensemble",
        names(plot_projections_nation)
      )] |>
        purrr::imap(\(x, id_x) dplyr::mutate(x, name = id_x)) |>
        projection_plots$tabulate_plot_list(region, "RSV")

      ### grab narratives ----
      narratives_national <- output_narratives_national |>
        tibble::tibble() |>
        tidyr::unnest(cols = output_narratives_national) |>
        dplyr::filter(grepl("ensemble", output_narratives_national)) |>
        dplyr::mutate(
          location_level = "nation",
          location = "England",
          icb = NA
        ) |>
        dplyr::rename("narrative" = output_narratives_national)

      narratives_regional <- output_narratives_regional |>
        dplyr::mutate(location_level = "region", icb = NA)

      all_narratives <- dplyr::bind_rows(narratives_national, narratives_regional) |>
        dplyr::mutate(narrative = stringr::str_remove(narrative, "Summary:.*?\\n"))

      ### grab probabilities

      probabilities <- result_summary |>
        dplyr::slice_max(tibble::tibble(prediction_start_date, date)) |>
        dplyr::filter(
          grepl("ensemble", model),
          # icb probabilities can be very unstable, so will not report
          location_level %in% c("region", "nation"),
          # we don't provide age-based forecasts to regions team
          age_group == "all"
        ) |>
        dplyr::select(
          location,
          location_level,
          dplyr::starts_with("p_")
        ) |>
        outputs$round_probabilities()

      ### bring together -----

      regional_output <- regional_plots |>
        dplyr::left_join(
          probabilities,
          dplyr::join_by(region == location, geography == location_level)
        ) |>
        dplyr::left_join(
          all_narratives,
          dplyr::join_by(region == location, geography == location_level)
        ) |>
        dplyr::mutate(
          disease = "rsv",
          target = "admissions",
          prediction_generated_date = lubridate::today()
        ) |>
        dplyr::relocate(
          disease,
          target,
          geography,
          region,
          plot,
          prediction_generated_date
        )

      regional_output
    }
  ),
  targets::tar_target(
    upload_regional_team_summary,
    {
      box::use(box / s3)

      datetime <- format(Sys.time(), "%Y-%m-%d_%H:%M:%S%Z")

      disease <- config_overall_params$disease

      metric <- config_overall_params$metric

      s3_uri <- glue::glue(
        "PATH REDACTED"
      )

      s3$write_using(
        x = output_regional_team_summary,
        s3_uri = s3_uri,
        fn = saveRDS,
        overwrite = TRUE
      )
    }
  ),
  tarchetypes::tar_quarto(
    name = regional_team_summary_report,
    path = here::here("src/R/prj/regions/quarto/regions_report.qmd"),
    working_directory = rprojroot::find_rstudio_root_file()
  ),
  targets::tar_target(
    move_regions_report,
    {
      # this target does not create any new outputs
      # it instead moves the regional QA report to a more
      # convenient location

      force(regional_team_summary_report)

      box::use(prj / regions)

      fs::dir_create(output_path, "regional_team")

      regions$move_report(
        disease = config_overall_params$disease,
        output_directory = fs::path(output_path, "regional_team")
      )
    }
  )
)
