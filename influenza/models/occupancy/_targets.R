# targets file to run influenza occupancy model

#### Description ####
#
# Take influenza admissions model and convert to occupancy model
#
# Method
#   Take forecasted samples for admissions in the next two weeks
#   Take posterior samples for fixed discharge probability
#   Combine forecast samples and posterior samples to calculate occupancy levels

box::use(box / deps_)

deps_$need(
  "bayesplot",
  "config",
  "dplyr",
  "fs",
  "ggplot2",
  "here",
  "lubridate",
  "parallel",
  "s3fs",
  "targets",
  "tidyr",
  "vroom",
  "withr",
  "yaml"
)

# Set parallel configuration
targets::tar_option_set(
  controller = crew::crew_controller_local(
    workers = max(
      config::get(
        "target_workers",
        file = here::here(
          "influenza",
          "models",
          "occupancy",
          "influenza_occupancy_config.yaml"
        )
      ),
      floor(parallel::detectCores() / 11)
    )
  )
)

# Return target list
list(
  # Config ===================================================================
  targets::tar_target(
    parent_config_file,
    here::here("influenza", "models", "influenza_config.yaml"),
    format = "file"
  ),
  targets::tar_target(
    config_file,
    here::here("influenza", "models", "occupancy", "influenza_occupancy_config.yaml"),
    format = "file"
  ),
  targets::tar_target(
    config,
    config::merge(
      config::get(file = parent_config_file),
      config::get(file = config_file)
    )
  ),
  targets::tar_target(
    ensemble_model_names,
    config |>
      purrr::pluck("ensemble") |>
      purrr::keep(\(.) isTRUE(.)) |>
      names()
  ),
  targets::tar_target(
    excluded_trusts_filter,
    config$excluded_trusts$filter
  ),
  targets::tar_target(
    excluded_trusts_zero,
    config$excluded_trusts$zero
  ),

  # Data =====================================================================

  targets::tar_target(
    admissions_forecast_path,
    cue = targets::tar_cue("always"),
    command = {
      box::use(box / s3)

      admissions_forecast_path <- if (
        config$files$admissions_forecast_data %in%
          c(
            "",
            "latest",
            "new",
            "newest",
            "most recent"
          ) ||
          is.null(config$files$admissions_forecast_data)
      ) {
        s3$find_latest_file(
          uri = config$files$admissions_forecast_path,
          pattern = config$files$admissions_forecast_pattern
        )
      } else {
        config$files$admissions_forecast_data
      }
    }
  ),

  # Load in data from admissions run
  targets::tar_target(
    admissions_forecast,
    {
      box::use(box / s3)

      s3$read_using(admissions_forecast_path, vroom::vroom) |>
        dplyr::filter(age_group == "all", location_level == "region") |>
        # assumption: we do not store the ensemble model, only the constituent models
        # because of our blending approach, so we want only the models in the ensemble
        dplyr::filter(model %in% ensemble_model_names)

      # TODO: target compatible check on dates here?
    }
  ),
  targets::tar_target(
    overall_params,
    {
      overall_params <- config$overall_params
      # some of our overall params are defined upstream from the admissions model
      overall_params$forecast_horizon <- unique(
        admissions_forecast$forecast_horizon
      )
      overall_params$n_lookbacks <- length(unique(na.omit(
        admissions_forecast$prediction_start_date
      )))
      overall_params$max_lookback <- max(unique(na.omit(
        admissions_forecast$prediction_start_date
      )))

      overall_params
    }
  ),

  # Load in the most recent training data
  targets::tar_target(
    training_data,
    cue = targets::tar_cue("always"), # to make sure we have latest data from upstream
    command = {
      box::use(box / redshift)

      rs <- redshift$data_model("REDACTED")

      rs$REDACTED |>
        dplyr::mutate(
          "total_beds" = dplyr::coalesce(total_adult_beds_general_acute, 0) +
            dplyr::coalesce(total_adult_beds_critical_care, 0) +
            dplyr::coalesce(total_paediatric_beds_general_acute, 0) +
            dplyr::coalesce(total_paediatric_beds_critical_care, 0)
        ) |>
        dplyr::select(
          date,
          trust_code,
          icb_name,
          nhs_region_name,
          population,
          total_beds,
          "admissions" = influenza_admissions,
          "occupancy" = influenza_occupancy
        ) |>
        # DATA QUALITY EXCLUSIONS
        # easy to filter trusts
        dplyr::filter(!(trust_code %in% excluded_trusts_filter)) |>
        # trusts that need to be replaces with 0 - usually due to all trusts in an ICB being removed
        dplyr::mutate(
          occupancy = dplyr::if_else(trust_code %in% excluded_trusts_zero, 0, occupancy),
          admissions = dplyr::if_else(trust_code %in% excluded_trusts_zero, 0, admissions)
        ) |>
        # Ensure consistent row order (otherwise random row order from database
        # may lead to unnecessary rerunning of downstream targets!)
        dplyr::arrange(date, trust_code) |>
        dplyr::select(!trust_code) |>
        dplyr::collect() |>
        dplyr::filter(date < overall_params$max_lookback) |>
        # ad hoc fixes for data would go here, e.g. Frimley

        # TODO temporary?: Model breaks occupancy being zero:
        dplyr::mutate(occupancy = pmax(occupancy, 0.001)) |>
        # TODO temporary/permanent removal of bad quality data:
        # We want to believe in them, but we can't :(
        dplyr::mutate(
          admissions = ifelse(
            icb_name == "NHS Frimley Integrated Care Board",
            0,
            admissions
          ),
          occupancy = ifelse(
            icb_name == "NHS Frimley Integrated Care Board",
            0,
            occupancy
          )
        )

      # TODO data check for how recent the data is
    }
  ),

  # Discharge Model ==========================================================

  targets::tar_target(
    discharge_model_fn,
    cue = targets::tar_cue("always"),
    command = {
      box::use(. / influenza / models / occupancy / models / discharge_region)
      discharge_region$run_discharge_region
    }
  ),
  targets::tar_target(
    discharge_results,
    {
      # SET GLOBAL SEED for reproducibility
      set.seed(8675309)
      discharge_model_fn(training_data, overall_params)
    }
  ),

  ## Regional Discharge Outputs ----------------------------------------------

  targets::tar_target(
    los_regional_samples,
    {
      expand.grid(
        # explore posterior parameters
        t = seq(0, overall_params$max_lag, 0.1),
        nhs_region_name = unique(
          discharge_results$training_data$nhs_region_name
        )
      ) |>
        dplyr::left_join(
          discharge_results$parameters,
          by = "nhs_region_name",
          relationship = "many-to-many"
        ) |> # hopefully silence warning
        dplyr::mutate(
          p = dlnorm(t, lognormal_mu, lognormal_sigma), # probability density
          cd = plnorm(t, lognormal_mu, lognormal_sigma) # cumulative p.d."
        ) # TODO: a better name than t and p; cd isn't all that clear either
    }
  ),
  targets::tar_target(
    los_regional_summary,
    {
      los_regional_samples |>
        dplyr::group_by(t, nhs_region_name) |>
        dplyr::summarise(
          p_50 = quantile(p, 0.5),
          p_5 = quantile(p, 0.05),
          p_95 = quantile(p, 0.95),
          c_50 = quantile(cd, 0.5),
          c_5 = quantile(cd, 0.05),
          c_95 = quantile(cd, 0.95)
        ) |>
        dplyr::ungroup()
    }
  ),

  # TODO: Could move all of these to the output section; to be more consistent
  # But some of these are also used as checks so... it's tricky.

  targets::tar_target(
    los_regional_plot,
    {
      box::use(prj / outputs)

      # Some discharge distribution parameters do depend on the region,
      #  so we expect variation.
      # Should be a right skewed distribution, ideally in a nice /‾\_ shape
      # There is some pooling, but mostly each region will be independently fit
      #  and differ in distribution for length of stay.

      outputs$los_plotter(
        los_regional_summary,
        discharge_results,
        geography = "region",
        output_path = output_path,
        disease = config$overall_params$disease
      )
    }
  ),
  targets::tar_target(
    los_regional_narratives,
    {
      box::use(prj / narratives)

      narratives$los_narrative_txt_output(
        data = los_regional_samples,
        geography = "region",
        disease = "influenza",
        output_path = output_path
      )
    }
  ),

  ## National ----------------------------------------------------------------

  # For each region:
  # find mean population across training period
  # calculate number of samples proportional to this to use in national average
  targets::tar_target(
    regional_n_samples,
    {
      training_data |>
        dplyr::filter(
          date <= max(discharge_results$predictions$date),
          date >= min(discharge_results$predictions$date),
          !is.na(admissions),
          !is.na(occupancy),
          !is.na(total_beds),
          !is.na(admissions)
        ) |>
        dplyr::group_by(nhs_region_name) |>
        dplyr::summarise(population = median(population)) |>
        dplyr::mutate(
          n_samples = round(
            population / max(population) * overall_params$n_pi_sample
          )
        )
    }
  ),
  targets::tar_target(
    los_national_samples,
    {
      expand.grid(
        t = seq(0, overall_params$max_lag, 0.1),
        nhs_region_name = unique(
          discharge_results$training_data$nhs_region_name
        )
      ) |>
        dplyr::left_join(
          discharge_results$parameters,
          by = "nhs_region_name",
          relationship = "many-to-many" # silence warning
        ) |>
        # number of samples weighted by region
        dplyr::left_join(regional_n_samples, by = "nhs_region_name") |>
        dplyr::filter(.draw <= n_samples) |>
        dplyr::mutate(
          p = dlnorm(t, lognormal_mu, lognormal_sigma), # probability density
          cd = plnorm(t, lognormal_mu, lognormal_sigma)
        ) # cumulative p.d.
    }
  ),
  targets::tar_target(
    los_national_summary,
    {
      los_national_samples |>
        dplyr::group_by(t) |>
        dplyr::summarise(
          p_50 = quantile(p, 0.5),
          p_5 = quantile(p, 0.05),
          p_95 = quantile(p, 0.95),
          c_50 = quantile(cd, 0.5),
          c_5 = quantile(cd, 0.05),
          c_95 = quantile(cd, 0.95)
        ) |>
        dplyr::ungroup()
    }
  ),
  targets::tar_target(
    los_national_plot,
    {
      box::use(prj / outputs)

      outputs$los_plotter(
        # National Length of Stay Plot
        los_national_summary,
        discharge_results,
        geography = "nation",
        output_path = output_path,
        disease = config$overall_params$disease
      )
    }
  ),
  targets::tar_target(
    los_national_narratives,
    {
      box::use(prj / narratives)

      narratives$los_narrative_txt_output(
        # National length of stay narrative
        data = los_national_samples,
        geography = "nation",
        disease = "influenza",
        output_path = output_path
      )
    }
  ),

  # TODO: # enforce a user check on the stan model?
  # Do the QA plots (in the plots pane) for the fit stan model look sensible?

  # Occupancy Model ==========================================================

  targets::tar_target(
    admissions_to_occupancy_model_fn,
    cue = targets::tar_cue("always"),
    command = {
      box::use(. / influenza / models / occupancy / models / admissions_to_occupancy)
      admissions_to_occupancy$run_occupancy_region
    }
  ),
  targets::tar_target(
    occupancy_results,
    admissions_to_occupancy_model_fn(
      admissions_data = admissions_forecast,
      discharge_data = discharge_results,
      overall_params = overall_params
    )
  ),

  # Format ===================================================================

  # National samples
  targets::tar_target(
    occupancy_samples,
    {
      discharge_results$training_data |>
        dplyr::select(-admissions) |>
        dplyr::full_join(
          occupancy_results$forecast_occupancy,
          by = c("date", "nhs_region_name")
        ) |>
        dplyr::rename(
          .value = .pred,
          target = occupancy,
          population = total_beds
        ) |>
        dplyr::group_by(nhs_region_name) |>
        dplyr::arrange(date) |>
        dplyr::select(-r) |>
        tidyr::fill(population) |>
        dplyr::ungroup()
    }
  ),
  targets::tar_target(
    nation_occupancy_formatted_summary,
    {
      box::use(prj / ensemble)

      occupancy_samples |>
        ensemble$ensemble_from_samples(
          remove_identifiers = c("nhs_region_name"),
          overall_params = overall_params,
          model_name = overall_params$model_name
        ) |>
        dplyr::mutate(
          # convert to a percentage rate:
          dplyr::across(dplyr::starts_with("pi_"), ~ 100 * . / population),
          target = 100 * target / population
        ) |>
        dplyr::rename(target_value = target) |>
        dplyr::mutate(
          location = "England",
          location_level = "nation",
          age_group = "all",
          model = overall_params$model_name,
          age_group_granularity = "none",
          target_name = "influenza_bed_occupancy_rate",
          forecast_horizon = overall_params$forecast_horizon
        )
    }
  ),
  targets::tar_target(
    region_occupancy_formatted_summary,
    {
      box::use(prj / ensemble)

      occupancy_samples |>
        ensemble$ensemble_from_samples(
          remove_identifiers = c(),
          overall_params = overall_params,
          model_name = overall_params$model_name
        ) |>
        dplyr::mutate(
          # convert to a percentage rate
          dplyr::across(dplyr::starts_with("pi_"), ~ 100 * . / population),
          target = 100 * target / population
        ) |>
        dplyr::rename(
          target_value = target,
          location = nhs_region_name
        ) |>
        dplyr::mutate(
          location_level = "region",
          age_group = "all",
          model = overall_params$model_name,
          age_group_granularity = "none",
          target_name = "influenza_bed_occupancy_rate",
          forecast_horizon = overall_params$forecast_horizon
        )
    }
  ),
  targets::tar_target(
    occupancy_formatted_summary,
    {
      box::use(
        prj / checks,
        prj / format
      )

      out <- dplyr::bind_rows(
        nation_occupancy_formatted_summary,
        region_occupancy_formatted_summary
      ) |>
        dplyr::rename(target = target_value) |>
        format$format_outputs(
          target_name = overall_params$target_name,
          forecast_horizon = overall_params$forecast_horizon,
          disease = overall_params$disease
        )

      checks$check_forecast_format_summary(out)

      out
    }
  ),

  # regional samples
  targets::tar_target(
    occupancy_formatted_samples,
    {
      box::use(
        prj / checks,
        prj / format
      )

      out <- occupancy_samples |>
        dplyr::rename(location = nhs_region_name) |>
        dplyr::mutate(
          location_level = "region",
          age_group = "all",
          model = overall_params$model_name,
          age_group_granularity = "none",
          target_name = "influenza_bed_occupancy_rate",
          forecast_horizon = overall_params$forecast_horizon
        ) |>
        format$format_outputs(
          target_name = overall_params$target_name,
          forecast_horizon = overall_params$forecast_horizon,
          disease = overall_params$disease
        )

      # takes a while as checking each
      checks$check_forecast_format_sample(out)

      out
    }
  ),

  # Outputs =================================================================
  # If there weren't some discharge plots & narratives above, the folder and its
  # timestamp would go here.
  # If there weren't some discharge plots & narratives above the folder and its
  # timestamp would go here.
  targets::tar_target(
    make_timestamp,
    {
      force(discharge_results)
      format(Sys.time(), "%Y-%m-%d_%H:%M:%S_%a")
    }
  ),

  # moved output earlier in pipeline to help with QA check outputting
  targets::tar_target(
    output_path,
    {
      here::here("influenza", "outputs", "occupancy", make_timestamp) |>
        fs::dir_create()
    }
  ),

  ## QA Discharge model #####

  targets::tar_target(
    discharge_qa,
    {
      # DISCHARGE MODEL QA #
      # Code to sense check the model outputs
      # lognormal_mu and lognormal_sigma define the discharge probability
      # alpha is the conversion multiplier between:
      #  arrival admissions and admissions.
      # beta is the error term on the gamma distribution.
      # the y_hat variables are the forecasted values historically to check fit

      path <- fs::dir_create(fs::path(output_path, "qa"))

      # check convergence
      sigma_plot <- bayesplot::mcmc_trace(
        discharge_results$model$draws(),
        pars = c("lognormal_sigma")
      ) +
        ggplot2::theme_bw() +
        ggplot2::scale_color_brewer(palette = "Dark2")
      ggplot2::ggsave(sigma_plot, filename = fs::path(path, "lognormal_sigma_trace.png"))
      mu_plot <- bayesplot::mcmc_trace(
        discharge_results$model$draws(),
        regex_pars = c("lognormal_mu")
      ) +
        ggplot2::theme_bw() +
        ggplot2::scale_color_brewer(palette = "Dark2")
      ggplot2::ggsave(mu_plot, filename = fs::path(path, "lognormal_mu_trace.png"))
      beta_plot <- bayesplot::mcmc_trace(
        discharge_results$model$draws(),
        regex_pars = c("beta")
      ) +
        ggplot2::theme_bw() +
        ggplot2::scale_color_brewer(palette = "Dark2")
      ggplot2::ggsave(beta_plot, filename = fs::path(path, "beta_trace.png"))

      # show how the model fit to the data
      regional_fit_plot <- discharge_results$predictions |>
        dplyr::mutate(
          pi_50 = dplyr::if_else(max(t) - t >= overall_params$training_length, NA, pi_50),
          pi_5 = dplyr::if_else(max(t) - t >= overall_params$training_length, NA, pi_5),
          pi_95 = dplyr::if_else(max(t) - t >= overall_params$training_length, NA, pi_95),
        ) |>
        ggplot2::ggplot() +
        ggplot2::geom_line(
          ggplot2::aes(x = date, y = pi_50, color = "median occupancy estimate")
        ) +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            x = date,
            ymin = pi_5,
            ymax = pi_95,
            fill = "occupancy 90% posterior"
          ),
          alpha = 0.3
        ) +
        ggplot2::geom_line(
          ggplot2::aes(x = date, y = occupancy, color = "true occupancy")
        ) +
        ggplot2::geom_line(
          ggplot2::aes(x = date, y = admissions, color = "true admissions")
        ) +
        ggplot2::ylab("metric") +
        ggplot2::theme_bw() +
        ggplot2::facet_wrap(~nhs_region_name, scales = "free_y") +
        ggplot2::scale_color_manual(
          values = c(
            "true occupancy" = "red",
            "true admissions" = "orange",
            "median occupancy estimate" = "blue"
          )
        ) +
        ggplot2::scale_fill_manual(
          values = c(
            "occupancy 90% posterior" = "royalblue"
          )
        ) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::guides(color = ggplot2::guide_legend(nrow = 3))

      ggplot2::ggsave(
        regional_fit_plot,
        width = 10,
        height = 8,
        filename = fs::path(path, "regional_fit.png")
      )

      # plot the fit parameters by region
      # are the ones we expect to vary varying?
      #  At the moment, beta and lognormal_mu are varying (05/10/2023)
      fit_parameters <- discharge_results$parameters |>
        tidyr::pivot_longer(
          cols = c("lognormal_mu", "lognormal_sigma", "beta"),
          names_to = "parameter_name",
          values_to = "parameter_value"
        ) |>
        dplyr::summarise(
          cri_50 = quantile(parameter_value, 0.5),
          cri_95 = quantile(parameter_value, 0.95),
          cri_05 = quantile(parameter_value, 0.05),
          .by = c(nhs_region_name, parameter_name)
        ) |>
        ggplot2::ggplot() +
        ggplot2::geom_point(ggplot2::aes(
          y = parameter_name,
          x = cri_50,
          color = parameter_name
        )) +
        ggplot2::geom_linerange(ggplot2::aes(
          xmin = cri_05,
          xmax = cri_95,
          y = parameter_name,
          color = parameter_name
        )) +
        ggplot2::facet_wrap(~nhs_region_name, ncol = 1) +
        ggplot2::xlim(c(0, NA)) +
        ggplot2::xlab("parameter value") +
        ggplot2::ylab("") +
        ggplot2::theme_bw()
      ggplot2::ggsave(filename = fs::path(path, "los_parameters.png"))

      # should be monotonically increasing,
      #  reaching 1 near the end of the max_lag
      cumulative_region_fit <-
        los_regional_summary |>
        ggplot2::ggplot() +
        ggplot2::geom_line(ggplot2::aes(x = t, y = c_50)) +
        ggplot2::geom_ribbon(
          ggplot2::aes(x = t, ymin = c_5, ymax = c_95),
          alpha = 0.4,
          fill = "darkgreen"
        ) +
        ggplot2::ylab("cumulative p discharge") +
        ggplot2::xlab("days since admission") +
        ggplot2::ggtitle("Likelihood of discharge over time") +
        ggplot2::ylim(0, 1) +
        ggplot2::theme_bw() +
        ggplot2::facet_wrap(~nhs_region_name)

      ggplot2::ggsave(cumulative_region_fit, filename = fs::path(path, "cumulative_regional_fit.png"))
    }
  ),

  ## Plots -------------------------------------------------------------------
  targets::tar_target(
    plot_projections_and_rag,
    {
      box::use(prj / outputs)

      outputs$projections_plotter(
        plots_include = list(c("multiple_cis", "rag")),
        data = occupancy_formatted_summary,
        target_name = "bed_occupancy_rate_(%)",
        model_name = overall_params$model_name,
        geography = c("nation", "region"),
        output_path = output_path,
        peaks_data = config$overall_params$show_peaks,
        plot_historic_fit = config$overall_params$show_fits,
        disease = config$overall_params$disease,
        y_limit = c("nation" = 6, "region" = 8), # bring down if graphs look silly
        should_nudge_x = TRUE
      )

      outputs$rag_plotter(
        data = occupancy_formatted_summary,
        target_name = "bed_occupancy_rate_(%)",
        model_name = overall_params$model_name,
        geography = "nation", # Only national needed for this
        output_path = output_path,
        disease = config$overall_params$disease
      )
    }
  ),

  # same as above with lookbacks for QA
  targets::tar_target(
    plot_projections_and_rag_qa,
    {
      box::use(prj / outputs)

      outputs$projections_plotter(
        plots_include = list(c("lookbacks", "multiple_cis", "rag")),
        data = occupancy_formatted_summary,
        target_name = "bed_occupancy_rate_(%)",
        model_name = "ensemble_conversion",
        geography = c("nation", "region"),
        output_path = here::here(output_path, "qa"),
        peaks_data = config$overall_params$show_peaks,
        plot_historic_fit = config$overall_params$show_fits,
        disease = config$overall_params$disease,
        y_limit = c("nation" = 6, "region" = 8), # bring down if graphs look silly
        should_nudge_x = TRUE
      )

      outputs$rag_plotter(
        data = occupancy_formatted_summary,
        target_name = "bed_occupancy_rate_(%)",
        model_name = "ensemble_conversion",
        geography = "nation", # Only national needed for this
        output_path = here::here(output_path, "qa"),
        disease = config$overall_params$disease
      )
    }
  ),
  ## Narrative ---------------------------------------------------------------

  targets::tar_target(
    output_narratives,
    {
      box::use(prj / narratives)

      narratives$narrative_txt_output(
        occupancy_formatted_summary,
        geography = "nation",
        age_granularity = "none",
        target_name = "bed_occupancy_rate",
        model_name = overall_params$model_name,
        disease = "influenza",
        rounding_level = 1,
        is_percent = TRUE,
        output_path = output_path
      )
    }
  ),
  targets::tar_target(
    output_tables,
    {
      box::use(prj / narratives)

      formatted_output_table <- narratives$create_narrative_tables(
        occupancy_formatted_summary,
        target_name = overall_params$target_name,
        model_name = "ensemble_conversion",
        disease = overall_params$disease,
        location_levels = c("nation", "region"),
        age_granularity = "none",
        output_path = output_path
      )

      formatted_output_table
    }
  ),

  ## Upload artefacts --------------------------------------------------------

  targets::tar_target(
    local_data_output_path,
    fs::dir_create(fs::path(output_path, "data"))
  ),
  targets::tar_target(
    upload_summary,
    {
      box::use(box / s3)

      results_summary_name <- paste0(
        "all_models_predictions_summary_",
        make_timestamp,
        ".csv.gz"
      )

      s3$write_to_s3(
        occupancy_formatted_summary,
        s3fs::s3_path(
          "PATH REDACTED"
        ),
        local_path = paste0(
          local_data_output_path,
          "/",
          results_summary_name
        ),
        overwrite = TRUE
      )
    }
  ),
  targets::tar_target(
    upload_samples,
    {
      box::use(box / s3)

      results_samples_name <- paste0(
        "all_models_predictions_samples_",
        make_timestamp,
        ".csv.gz"
      )

      s3$write_to_s3(
        occupancy_formatted_samples,
        s3fs::s3_path(
          "PATH REDACTED"
        ),
        local_path = paste0(
          local_data_output_path,
          "/",
          results_samples_name
        ),
        overwrite = TRUE
      )
    }
  ),
  targets::tar_target(
    output_zip,
    {
      # These must have run already at this point
      force(c(
        discharge_qa,
        los_national_narratives,
        los_national_plot,
        los_regional_narratives,
        los_regional_plot,
        output_narratives,
        plot_projections_and_rag
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
  )
)
