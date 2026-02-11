# COVID-19 admissions targets pipeline
box::use(box / deps_)

deps_$need(
  "cli",
  "config",
  "crew",
  "dplyr",
  "fs",
  "glue",
  "here",
  "parallel",
  "purrr",
  "rlang",
  "s3fs",
  "tarchetypes",
  "targets",
  "tibble",
  "tidyr",
  "withr"
)


# Set up static-branched targets (one branch per model) ========================

# Fetch model hyperparams from the config file
config_file_path <- here::here(
  "covid",
  "models",
  "admissions",
  "covid_admissions_config.yaml"
)

config_models <- config::get("models", file = config_file_path) |>
  purrr::keep(\(.) isTRUE(.$include_run))


# Set parallel configuration
targets::tar_option_set(
  # set the storage and retrieval to avoid putting all memory in the central node.
  # note: experimental
  storage = "worker",
  retrieval = "worker",
  controller = crew::crew_controller_local(
    workers = max(
      config::get("target_workers", file = config_file_path),
      floor(parallel::detectCores() / 5)
    )
  )
)


# Create a target list
model_targets <- tarchetypes::tar_map(
  values = config_models |>
    # Have to use expressions:
    #  https://books.ropensci.org/targets/static.html#limitations
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
          . / covid / models / admissions / models / MODEL["fn" = FN],
          list(
            "MODEL" = as.name(MODEL_NAME),
            "FN" = as.name(paste0("run_", MODEL_NAME))
          )
        ))
      )

      # we cannot update a target like a list, so making a local copy
      local_hyperparams <- HYPERPARAMS
      local_hyperparams$spatial_object <- spatial_object

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
        furrr::future_map(
          model_fn,
          .options = furrr::furrr_options(seed = TRUE)
        ) |>
        purrr::map("sample_predictions") |>
        purrr::list_rbind() |>
        # For identification later, when all results are combined into one big data frame
        dplyr::mutate("model" = MODEL_NAME, .before = 1)

      # Attach ICB level data to predictions
      sample_preds <- data |>
        # we need to specify ICBs or else we end up creating duplicate records when we join.
        # Which becomes a problem for later aggregation of target values ahead of plotting
        tidyr::expand_grid(
          prediction_start_date = unique(sample_preds$prediction_start_date)
        ) |>
        dplyr::full_join(
          sample_preds |>
            # discard the models population as unreliable
            dplyr::select(-population),
          dplyr::join_by(
            date,
            icb_name,
            nhs_region_name,
            prediction_start_date
          )
        ) |>
        # remove the population for the forecasted time period
        dplyr::mutate(population = dplyr::na_if(population, date >= prediction_start_date)) |>
        dplyr::arrange(date) |>
        dplyr::group_by(icb_name) |>
        # fill in the forecast period population with the most recent observation
        # to simulate unknown future, and ensure consistency across models.
        tidyr::fill(population, .direction = "down") |>
        dplyr::ungroup() |>
        # ensure model name carried forward as historic data does not have a numeric .sample
        dplyr::mutate(model = MODEL_NAME)

      list(
        "sample_preds" = sample_preds,
        "geo_preds" = list(
          "nation" = intervals$samples_to_quantiles(
            .sample_predictions = sample_preds,
            remove_identifiers = c("icb_name", "nhs_region_name"),
            overall_params = config_overall_params
          ) |>
            dplyr::mutate("location" = "England"),
          "region" = intervals$samples_to_quantiles(
            .sample_predictions = sample_preds,
            remove_identifiers = "icb_name",
            overall_params = config_overall_params
          ) |>
            dplyr::rename("location" = nhs_region_name),
          "icb" = intervals$samples_to_quantiles(
            .sample_predictions = sample_preds,
            remove_identifiers = "nhs_region_name",
            overall_params = config_overall_params
          ) |>
            dplyr::rename("location" = icb_name)
        ) |>
          purrr::list_rbind(names_to = "location_level") |>
          dplyr::mutate("model" = MODEL_NAME)
      )
    }
  ),

  # Create "samples_*" and "preds_*" targets
  targets::tar_target(
    samples,
    model |> purrr::map("sample_preds") |> purrr::list_rbind()
  ),
  targets::tar_target(
    preds,
    model |> purrr::map("geo_preds") |> purrr::list_rbind()
  )
)


# The main target list
list(
  # Config ===================================================================
  targets::tar_target(
    parent_config_file,
    here::here("covid", "models", "covid_config.yaml"),
    format = "file"
  ),
  targets::tar_target(
    config_file,
    config_file_path,
    format = "file"
  ),
  targets::tar_target(
    config,
    {
      out <- config::merge(
        config::get(file = parent_config_file),
        config::get(file = config_file)
      )

      out$overall_params[["threshold_rates"]] <- list(
        "upper_rate" = out$thresholds[[out$overall_params$target_name]],
        "lower_rate" = -out$thresholds[[out$overall_params$target_name]]
      )
      out
    }
  ),

  # We need these intermediate targets so that changes to one part of `config`
  # don't invalidate ALL downstream targets

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
    excluded_trusts_filter,
    config$excluded_trusts$filter
  ),
  targets::tar_target(
    excluded_trusts_zero,
    config$excluded_trusts$zero
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

        stop(paste(
          "Models are being ensembled that have not been run:",
          models_not_run
        ))
      }

      model_diff
    }
  ),

  # Data =====================================================================

  targets::tar_target(
    data_raw,
    cue = targets::tar_cue("always"), # we might have fresh data upstream - we should always check!
    command = {
      box::use(box / redshift)
      rs <- redshift$data_model("REDACTED")

      rs$REDACTED |>
        dplyr::filter(date >= max(date) - 356L) |> # TODO make this better - based on max training length etc
        dplyr::select(
          date,
          population,
          trust_code,
          dplyr::starts_with(c("icb_", "nhs_region_")),
          paste0("covid_", config_overall_params$target_name)
        ) |>
        dplyr::rename_with(\(.) sub("^covid_", "", .)) |>
        dplyr::rename("target" = config_overall_params$target_name) |>
        # We must arrange here to ensure we always have the same row order
        # Otherwise, random row order -> different result hash -> unnecessary downstream execution!
        dplyr::arrange(date, icb_name) |>
        dplyr::collect()
    }
  ),
  targets::tar_target(
    data_max_dates,
    dplyr::summarise(
      data_raw,
      dplyr::across(
        # this is implicitly only selecting `target` now
        !(date:population),
        \(col) {
          dplyr::pick(date, col) |>
            dplyr::filter(!is.na(col)) |>
            dplyr::summarise(max(date)) |>
            dplyr::pull(1)
        }
      )
    ) |>
      tidyr::pivot_longer(
        dplyr::everything(),
        names_to = "source",
        values_to = "max_date"
      )
  ),
  targets::tar_target(
    data,
    {
      # We will only be able to make ensemble predictions if *all* component
      # models have been able to make predictions; which means we have to
      # truncate data to the "earliest max date" of all our feature columns
      cli::cli_warn(
        "Data will be truncated to earliest max date: {.val {min(data_max_dates$max_date)}}"
      )

      data_raw |>
        dplyr::filter(date <= min(data_max_dates$max_date)) |>
        # DATA QUALITY EXCLUSIONS
        # these need to happen after the time filtering or we replace with zeros non
        # existant data.
        dplyr::filter(!(trust_code %in% excluded_trusts_filter)) |>
        # trusts we need to replace with zeros: usually done when all trusts in an ICB have been removed
        dplyr::mutate(target = dplyr::if_else(trust_code %in% excluded_trusts_zero, 0, target)) |>
        # we don't want the trust identifier in the later processing
        dplyr::select(-c("trust_code")) |>
        # aggregate to ICBs
        dplyr::group_by(date, icb_name, nhs_region_name) |>
        # reporting population consists of trusts that report on a given day,
        # or NA if no trusts
        dplyr::summarise(
          population = dplyr::if_else(
            all(is.na(target)),
            NA,
            sum(population[!is.na(target)], na.rm = TRUE)
          ),
          dplyr::across(
            !population,
            \(.) ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))
          ),
          .groups = "keep"
        ) |>
        dplyr::ungroup() |>
        # Backward filling to remove any remaining NA
        dplyr::arrange(date) |>
        dplyr::group_by(icb_name) |>
        tidyr::fill(c(target, population), .direction = "up") |>
        # impute target to zero if we have no population, then replace
        # any remaining population with a token non-zero value (1)
        # since the numerator/target is zero, this has no impact on per-capita rates
        dplyr::mutate(
          target = dplyr::if_else(is.na(population), dplyr::coalesce(target, 0), target),
          population = dplyr::coalesce(population, 1)
        ) |>
        dplyr::ungroup()
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
        dplyr::filter(grepl("influenza", disease, ignore.case = TRUE)) |>
        dplyr::filter(!is.na(prediction_start_date)) |>
        dplyr::distinct(prediction_start_date) |>
        dplyr::select(prediction_start_date) |>
        dplyr::collect()
    }
  ),
  targets::tar_target(
    spatial_object,
    {
      box::use(box / s3)
      s3$read_using(
        config_files$spatial_network_path,
        readRDS
      )
    }
  ),

  # Models ===================================================================

  ## Components --------------------------------------------------------------

  # The model_* , samples_*, and preds_* targets defined earlier
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
          remove_identifiers = c("nhs_region_name", "icb_name"),
          method = "mellor",
          model_name = paste(ensemble_model_names, collapse = "_"),
          overall_params = config_overall_params
        ) |>
          dplyr::mutate(location = "England"),
        "region" = ensemble$ensemble_from_samples(
          .sample_predictions = chosen_samples,
          remove_identifiers = "icb_name",
          method = "mellor",
          model_name = paste(ensemble_model_names, collapse = "_"),
          overall_params = config_overall_params
        ) |>
          dplyr::rename("location" = nhs_region_name),
        "icb" = ensemble$ensemble_from_samples(
          .sample_predictions = chosen_samples,
          remove_identifiers = "nhs_region_name",
          method = "mellor",
          model_name = paste(ensemble_model_names, collapse = "_"),
          overall_params = config_overall_params
        ) |>
          dplyr::rename("location" = icb_name)
      ) |>
        purrr::list_rbind(names_to = "location_level")
    }
  ),

  # Results ==================================================================

  targets::tar_target(
    model_names,
    result_summary |>
      dplyr::distinct(model) |>
      dplyr::pull(model)
  ),
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
          remove_identifiers = c("nhs_region_name", "icb_name")
        ) |>
          dplyr::mutate(location = "England"),
        "region" = intervals$aggregate_samples(
          samples,
          remove_identifiers = "icb_name"
        ) |>
          dplyr::rename("location" = nhs_region_name)
      ) |>
        purrr::list_rbind(names_to = "location_level") |>
        format$format_outputs(
          config_overall_params$target_name,
          config_overall_params$forecast_horizon,
          config_overall_params$disease
        )

      checks$check_forecast_format_sample(out)

      out
    }
  ),
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
      # Have to declare dependency
      force(result_summary)
      format(Sys.time(), "%Y-%m-%d_%H:%M:%S_%a")
    }
  ),
  targets::tar_target(
    output_path,
    here::here("covid", "outputs", "admissions", make_timestamp) |>
      fs::dir_create()
  ),

  ## Plots -------------------------------------------------------------------

  targets::tar_target(
    plot_projections_nation_region,
    {
      box::use(prj / outputs)

      plots <- purrr::map(
        model_names,
        \(model_name) {
          outputs$projections_plotter(
            plots_include = list(c("multiple_cis", "rag")),
            data = result_summary,
            target_name = rlang::sym(config_overall_params$target_name),
            model_name = model_name,
            geography = c("nation", "region"),
            output_path = output_path,
            y_limit = c("nation" = 300, "region" = 100),
            x_limit_lower = 112,
            disease = config_overall_params$disease,
            plot_historic_fit = config_overall_params$show_fits,
            peaks_data = config_overall_params$show_peaks,
            should_nudge_x = TRUE
          )
        }
      )
      names(plots) <- model_names

      plots
    }
  ),

  # make a new version with lookbacks for QA
  targets::tar_target(
    plot_projections_nation_region_qa,
    {
      box::use(prj / outputs)

      fs::dir_create(fs::path(output_path, c("qa")))
      plots <- purrr::map(
        model_names,
        \(model_name) {
          outputs$projections_plotter(
            plots_include = list(c("multiple_cis", "lookbacks")),
            data = result_summary,
            target_name = rlang::sym(config_overall_params$target_name),
            model_name = model_name,
            geography = c("nation", "region"),
            output_path = fs::path(output_path, "qa"),
            y_limit = c("nation" = 300, "region" = 100),
            x_limit_lower = 112,
            disease = config_overall_params$disease,
            plot_historic_fit = config_overall_params$show_fits,
            peaks_data = config_overall_params$show_peaks,
            should_nudge_x = TRUE
          )
        }
      )
      names(plots) <- model_names

      plots
    }
  ),
  targets::tar_target(
    plot_projections_icb,
    {
      box::use(
        box / redshift,
        prj / outputs
      )

      fs::dir_create(fs::path(output_path, "icb"))

      rs <- redshift$data_model("REDACTED")

      nhs_trust <- rs$REDACTED |>
        dplyr::select(icb23nm, nhser23nm) |>
        dplyr::collect()

      # Add on region as additional column
      icb_plotting <- result_summary |>
        dplyr::filter(location_level == "icb") |>
        dplyr::left_join(
          nhs_trust,
          by = c("location" = "icb23nm"),
          relationship = "many-to-many"
        )

      region_list <- unique(icb_plotting$nhser23nm[
        !is.na(icb_plotting$nhser23nm)
      ])

      plots <- purrr::map(
        region_list,
        \(chosen_region) {
          one_region <- icb_plotting |>
            dplyr::filter(nhser23nm == chosen_region)

          outputs$projections_plotter(
            plots_include = list(c("multiple_cis")),
            data = one_region,
            target_name = rlang::sym(config_overall_params$target_name),
            model_name = grep("ensemble", model_names, value = TRUE), # selects ensemble
            geography = "icb",
            output_path = output_path,
            disease = config_overall_params$disease,
            plot_historic_fit = config_overall_params$show_fits,
            peaks_data = NULL, # Don't add peaks
            x_limit_lower = 112,
            y_limit = NA
          )
        }
      )
      names(plots) <- region_list

      plots
    }
  ),
  targets::tar_target(
    plot_rag,
    {
      box::use(prj / outputs)

      for (model_name in model_names) {
        outputs$rag_plotter(
          data = result_summary,
          target_name = rlang::sym(config_overall_params$target_name),
          model_name = model_name,
          geography = c("nation", "region", "icb"),
          output_path = output_path,
          disease = config_overall_params$disease
        )
      }
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
            target_name = config_overall_params$target_name,
            model_name = model_name,
            geography = "nation",
            disease = "COVID-19",
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

      regions <- preds_ensemble |>
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
            target_name = config_overall_params$target_name,
            model_name = chosen_model,
            geography = "region",
            disease = "COVID-19",
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
    output_narratives_icb,
    {
      box::use(prj / narratives)

      # ICB narrative
      narratives <- purrr::map(
        model_names,
        \(model_name) {
          narratives$icb_map_narrative_txt_output(
            data = result_summary,
            model_name = model_name,
            disease = "COVID-19",
            output_path = output_path
          )
        }
      )

      narratives
    }
  ),

  # Added new target with ICB narratives for the regional team
  targets::tar_target(
    regional_team_icb_narratives,
    {
      box::use(prj / narratives)

      # ICB narrative
      model <- model_names |>
        tibble::tibble() |>
        dplyr::filter(grepl("ensemble", model_names)) |>
        unlist()

      icb <- result_summary |>
        dplyr::filter(location_level == "icb") |>
        dplyr::distinct(location) |>
        dplyr::pull()

      icb_model_combinations <- tidyr::expand_grid(
        model = model,
        icb = icb
      )

      icb_narratives <- purrr::map2(
        icb_model_combinations$model,
        icb_model_combinations$icb,
        \(chosen_model, chosen_icb) {
          narratives$narrative_txt_output(
            data = dplyr::filter(result_summary, location == chosen_icb),
            target_name = config_overall_params$target_name,
            model_name = chosen_model,
            geography = "icb",
            disease = "COVID-19",
            output_path = output_path,
            as_tibble = TRUE
          )
        }
      ) |>
        dplyr::bind_rows()

      icb_narratives
    }
  ),
  targets::tar_target(
    output_tables,
    {
      box::use(prj / narratives)

      ensemble_name <- result_summary$model |>
        unique() |>
        stringr::str_subset(pattern = "ensemble")

      formatted_output_table <- narratives$create_narrative_tables(
        data = result_summary,
        target_name = config_overall_params$target_name,
        model_name = ensemble_name,
        disease = config_overall_params$disease,
        location_levels = c("nation", "region"),
        age_granularity = "none",
        output_path = output_path
      )

      formatted_output_table
    }
  ),

  ## Upload artefacts --------------------------------------------------------

  targets::tar_target(
    output_zip,
    {
      # These must have run already at this point
      force(c(
        output_narratives_national,
        output_narratives_icb,
        output_narratives_regional,
        regional_team_icb_narratives,
        plot_projections_icb,
        plot_projections_nation_region,
        plot_rag
      ))

      z <- fs::file_temp(ext = "zip")
      withr::with_dir(
        output_path,
        utils::zip(z, fs::dir_ls(".", recurse = TRUE))
      )
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
  targets::tar_target(
    icb_lookup,
    {
      box::use(box / redshift)

      rs <- redshift$data_model("REDACTED")
      icb_lookup <- rs$REDACTED |>
        dplyr::select(
          nhser23nm,
          icb23nm
        ) |>
        dplyr::distinct() |>
        dplyr::collect() |>
        dplyr::rename(
          region = nhser23nm,
          icb = icb23nm
        )
      icb_lookup
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

      icb_plots <- plot_projections_icb |>
        purrr::imap(\(x, id_x) dplyr::mutate(x, region = id_x)) |>
        projection_plots$tabulate_plot_list(icb, "COVID-19")

      regional_plots <- plot_projections_nation_region[grepl(
        "ensemble",
        names(plot_projections_nation_region)
      )] |>
        purrr::imap(\(x, id_x) dplyr::mutate(x, name = id_x)) |>
        projection_plots$tabulate_plot_list(region, "COVID-19")

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

      narratives_icb <- regional_team_icb_narratives |>
        dplyr::mutate(location_level = "icb", icb = location) |>
        dplyr::left_join(icb_lookup, by = "icb") |>
        dplyr::select(-location) |>
        dplyr::rename("location" = region)

      all_narratives <- dplyr::bind_rows(
        narratives_national,
        narratives_regional,
        narratives_icb
      ) |>
        dplyr::mutate(
          icb = icb |>
            stringr::str_remove("NHS ") |>
            stringr::str_remove(" Integrated Care Board"),
          narrative = stringr::str_remove(narrative, "Summary:.*?\\n")
        )

      # TODO: get narrative into output tibble
      # See https://github.com/REDACTED

      ### grab probabilities

      probabilities <- result_summary |>
        dplyr::slice_max(tibble::tibble(prediction_start_date, date)) |>
        dplyr::filter(
          grepl("ensemble", model),
          location_level %in% c("region", "nation") # icb probs can be very unstable, so will not report
        ) |>
        dplyr::select(
          location,
          location_level,
          dplyr::starts_with("p_")
        ) |>
        outputs$round_probabilities()

      ### bring together -----

      regional_output <- dplyr::bind_rows(
        icb_plots,
        regional_plots
      ) |>
        dplyr::left_join(
          probabilities,
          dplyr::join_by(
            geography == location_level,
            region == location
          )
        ) |>
        dplyr::left_join(
          all_narratives,
          dplyr::join_by(
            region == location,
            geography == location_level,
            icb == icb
          )
        ) |>
        dplyr::mutate(
          disease = "covid",
          target = "admissions",
          prediction_generated_date = lubridate::today()
        ) |>
        dplyr::relocate(
          disease,
          target,
          geography,
          region,
          icb,
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
        "PATH REDACTED",
        "{datetime}_{disease}_{metric}_regional_team.rds"
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
