#' @name outputs
#' @section Version: 0.2.10
#'
#' @title
#' Combined Pack Output Functions.
#'
#' @description
#' Output producing functions
#'
#' We have tried to combine and bundle as much of the necessary functionality
#' away from the many old separate outputs scripts so users can use one function
#' for any disease for consistency.
#' For everyday use these should be flexible enough to cover everything we do.
#'
#' @details
#' The main function is `projections_plotter()`, which acts as a wrapper around
#' the `projection_plots$plot_projection()` function (itself wrapped in
#' `projection_plot_builder()` to handle loops), with user input tidying,
#' looping through geographies and included plots, as well as saving the result
#' (wrapped into `projection_plots_writer()`).
#'
#' There is also the `rag_plotter()` which is for showing a model's
#' probabilistic outputs, and is a wrapper around the two projection_plots
#' functions: `plot_probability()` for national use, and `plot_maps()` for
#' regional & ICB use. You can force it to use the map on national, and vice
#' versa, but you're less likely to need them. This function also does some
#' tidying as it goes, loops through given geographies, and saves the results.
#'
#' The last function you might use is the `los_plotter()`, for showing the
#' length of stay for hospital occupancy. This currently doesn't loop over
#' geographies as that's not typically required.
#'
#' There's also the `peaks_handler()` function for dealing with some complicated
#' issues that arise from the peaks of certain diseases; notably RSV.
#'
#' @examples
#' # setup box module:
#' box::use(prj / outputs)
#'
#' # National & regional projection plots:
#' outputs$projections_plotter( # pull from box module
#'   plots_include = list(c("lookbacks", "rag")), # the kind of plots you want
#'   data = all_formatted_summary, # the summarised model results
#'   target_name = target_name_sym, # usually admissions or occupancy.
#'   model_name = model_name, # either individual or ensemble model name.
#'   geography = c("nation", "region"), # if two supplied, will process both.
#'   output_path = output_path, # full path to dated outputs folder.
#'   y_limit = NA, # can supply different limits with geography-named vector.
#'   disease = config$overall_params$disease, # all config files have now.
#'   peaks_data = config$overall_params$show_peaks # in config, this is T|F
#' ) # peaks data may be supplied manually, or looked up internally if T.
#'
#' # RAG probabilities
#' outputs$rag_plotter(
#'   data = all_formatted_summary, # same data as above; like other inputs here.
#'   target_name = target_name_sym,
#'   model_name = model_name,
#'   geography = c("nation", "region", "icb"), # will do bar for national only,
#'   output_path = output_path,                # and maps for region & ICBs.
#'   disease = config$overall_params$disease,
#' )
#'
#' # Length of Stay plot
#' outputs$los_plotter(
#'   data = los_regional_summary, # summarised discharge_results with quantiles.
#'   discharge_results = discharge_results, # direct model result
#'   geography = "region", # only one geographic level at a time
#'   output_path = output_path, # timestamped output folder path
#'   disease = config$overall_params$disease # no in all config files here.
#' )
#'
#' @seealso
#' * [outputs$los_plotter()]
#' * [outputs$projections_plotter()]
#' * [outputs$rag_plotter()]
#'
".__module__."

#### Outstanding TODOs ####
# In decreasing importance:
# Break down projection plots by NHS region (as ICB is already done).
# Add ICB level breakdowns to more plots & peaks codes (if modelling expands).
# Consider Trust level outputs (if modelling expands).
# See if making los_projection loop over geography is worthwhile. - unlikely.
# Add the devolved nations to this UKHSA code.
# If trough values are added to Norovirus peaks, use an average of peak & trough
#   for the total beds value.
# Embed RSV population script, or turn into function call somewhere.
# Check RSV population contribution factors again.

#### Setup ####
# Sys.setenv( # sometimes needed if box struggling to find path; fix in Rprofile
#   "R_BOX_PATH" = fs::path(rprojroot::find_root(rprojroot::is_git_root),
#                           "src", "R")) # ensures box always works

box::use(
  box / deps_,
  box / help_,
  box / s3,
  prj / peaks,
  prj / projection_plots,
  prj / user_check
)
.on_load <- function(ns) {
  deps_$need( # ensures needed libraries are available
    "aws.s3",
    "dplyr",
    "ggplot2",
    "lubridate",
    "purrr",
    "rlang",
    "tidyr",
    "tidyselect"
  )

  deps_$min_version("dplyr", "1.1.0")
}


#### Exported Functions ####
#' Projections Plotter
#'
#' Create and save projection plots
#'
#' Operates as a wrapper around `projection_plots$plot_projection()` function,
#' plus some tidying up and checking of the user input, looping through
#' geographies and included plots, as well as saving the result.
#'
#' @param plots_include A list of the kinds of plots you want to produce.
#' Will loop over each item, though multiple parts can be added into one item to
#' be plotted together, for example, this will give two kinds of plots:
#'  list("multiple_cis", c("lookbacks", "rag", "map"))
#' @param data A data frame, the formatted summary of the model outputs.
#' @param target_name A symbol string of the kind of model being run;
#' current options are one of: `admissions`, `arrival_admissions`, or `occupancy`.
#' @param model_name String of the type of model being plotted.
#' Can accept a vector where the last entry specifies whether the model fit is
#' a nowcast, or not (default)
#' @param geography String of the geographic level, or levels, to be plotted.
#' Can be given a list like this to loop through: c("nation", "region", "icb").
#' @param age_granularity Optional string of the age grouping fidelity to use.
#' Should be the default "none" for all but RSV (also an option for RSV).
#' RSV can also have "fine", "course", anything else will be treated as "full".
#' @param output_path String of the full path the the timestamped output folder.
#' @param disease String picking the disease being plotted; always in config.
#' @param y_limit numeric value for the maximum y-axis value to show on plots.
#' If you're happy to let the script pick this, leave it as NA.
#' If you have multiple geographies, with different maximum values, you can
#' supply a named list like this: c("nation" = NA, "region" = 150).
#' If names are not supplied they will be filled in in the geography order.
#' If more geographies than y_limit values are given, the remainder will be NA.
#' @param x_limit_upper Date to provide upper limit on plot's x-axis.
#' @param x_limit_lower Date to provide lower limit on plot's x-axis.
#'  Can also be a number of days before x_limit_upper.
#' @param data_source Optional string of the data's source.
#'  Defaults based on disease read in from list at the end of this script.
#' @param plot_historic_fit Optional Boolean for whether to plot the model fit
#'  and ribbon for the historical fit; the forecast is always shown. Default = T
#' @param peaks_data Optional Boolean or data frame of peaks values.
#' If TRUE it will look up the peaks based on disease, and use stored values.
#' Will deal with changes needed for specific diseases, see peaks_handler below.
#' @param should_nudge_x Optional Boolean denoting whether to align labels on
#' peak text left (`TRUE`) or right (`FALSE`, default).
#'
#' @return tibble
#'
#' @examples
#' # National & regional projection plots:
#' outputs$projections_plotter( # pull from box module
#'   plots_include = list(c("lookbacks", "rag")), # the kind of plot you want
#'   data = all_formatted_summary, # the summarised model results
#'   target_name = target_name_sym, # usually admissions or occupancy.
#'   model_name = model_name, # either individual or ensemble model name.
#'   geography = c("nation", "region"), # two supplied, will make both.
#'   output_path = output_path, # full path to dated outputs folder.
#'   y_limit = NA, # can supply different limits with geography-named vector.
#'   disease = config$overall_params$disease, # all config files have now.
#'   peaks_data = config$overall_params$show_peaks # config has TRUE/FALSE
#' ) # peaks data may be supplied manually, or looked up internally if TRUE.
#'
#' @export

projections_plotter <- function( # nolint: cyclocomp_linter.
    plots_include = list("multiple_cis", c("lookbacks", "rag")),
    data,
    target_name,
    model_name,
    geography,
    age_granularity = "none",
    output_path,
    disease,
    y_limit = NA,
    x_limit_upper = NA,
    x_limit_lower = NA, #' or full span: as.Date(min(data$date, na.rm = TRUE)),
    data_source = "",
    plot_historic_fit = TRUE,
    peaks_data = FALSE,
    should_nudge_x = FALSE
    ) {
  # cleaning user inputs:
  plots_include <- lapply( # checks all plot types are recognised
    plots_include,
    \(x) {
      match.arg(
        x,
        c("multiple_cis", "lookbacks", "rag", "base"),
        several.ok = TRUE)
    })

  geography <- vapply( # checks all unique geographies are recognised
    unique(geography),
    \(x) match.arg(x, c("nation", "region", "icb")),
    c("a"), USE.NAMES = FALSE)
  # TODO Maybe add something for trust level or "NHS region" someday?

  if (is.null(x_limit_upper) || is.na(x_limit_upper)) {
    x_limit_upper <- as.Date(max(data$date, na.rm = TRUE))
  } else {
    x_limit_upper <- as.Date(x_limit_upper)
  }

  if (is.null(x_limit_lower) || is.na(x_limit_lower)) {
    x_limit_lower <- data |>
      dplyr::filter(
        prediction_start_date == max(prediction_start_date, na.rm = TRUE)) |>
      dplyr::pull(date) |>
      min() |>
      as.Date()
  } else if (is.numeric(x_limit_lower)) {
    x_limit_lower <- as.Date(x_limit_upper - lubridate::days(x_limit_lower))
  } else {
    x_limit_lower <- as.Date(x_limit_lower)
  }

  disease <- user_check$disease_checker(disease)

  if (is.null(data_source) || data_source == "") { # fill in sources if blank:
    data_source <- tidyr::replace_na(
      data_sources[[disease]],
      "Unspecified source")
  }

  if (is.null(names(y_limit))) { # will create a named list if not supplied
    if (length(y_limit) != length(geography)) {
      y_limit <- y_limit[seq_len(length(geography))]
      # This will pad y_limit with NA if geography is longer,
      #  and crop it down if y_limit is shorter
    }
    # # Alternative version where it pads with the final value:
    # nolint start: commented_code_linter.
    # if (length(y_limit) < length(geography)) {
    # # pad y_limit with final value if there aren't enough given:
    #   y_limit = c(y_limit, rep(dplyr::last(y_limit),
    #                            length(geography) - length(y_limit)))
    # } else if (length(y_limit) > length(geography)) {
    #   y_limit = y_limit[1:length(geography)]
    # }
    # nolint end
    y_limit <- rlang::set_names(as.list(y_limit), geography) # naming the list
  } else if (any(!geography %in% names(y_limit))) {
    y_limit <- c( # Fill in missing geographies from named input with default NA
      y_limit,
      rlang::set_names(
        as.list(rep(NA, length(geography) - length(y_limit))),
        base::setdiff(geography, names(y_limit)))) |>
      as.list()
  }

  # Turn peaks into a named list of df's, split by geography; if not already:
  if ("list" %in% class(peaks_data)) {
    if (length(peaks_data) != length(geography)) {
      peaks_data <- peaks_data[seq_len(length(geography))]
    } # pads with NA or crops to fit geography
  } else {
    peaks_data <- lapply(seq_along(geography), \(x) peaks_data)
  }
  if (is.null(names(peaks_data))) {
    peaks_data <- rlang::set_names(peaks_data, geography) # name list
  }

  if (any(!geography %in% names(peaks_data))) {
    peaks_data <- c( # Fill in missing geographies with NA
      peaks_data,
      rlang::set_names(
        as.list(rep(NA, length(geography) - length(peaks_data))),
        base::setdiff(geography, names(peaks_data)))) |>
      as.list()
  }

  peaks_data <- purrr::imap( # add peak data per geography (because RSV)
    peaks_data,
    \(df, geo) {
      if (is.data.frame(df)) {
        df # leave supplied peaks data in place
      } else if (isTRUE(df)) {
        peaks_handler( # this got complicated, so it's a function
          disease,
          target_name,
          geo, # only usable on one geography at a time; RSV gets complicated
          age_granularity) # age groups only used for RSV
      } else {
        NULL # otherwise drop it from plot
      }
    })

  # Generate plot results:
  results <- projection_plot_builder(
    data = data,
    target_name = target_name,
    model_name = model_name[1],
    geography = geography,
    age_granularity = age_granularity,
    plots_include = plots_include,
    data_source = data_source,
    disease = disease,
    peaks_data = peaks_data,
    plot_historic_fit = plot_historic_fit,
    y_limit = y_limit,
    x_limit_lower = x_limit_lower,
    x_limit_upper = x_limit_upper,
    contains_nowcast = grepl(
      "nowcast", model_name[length(model_name)], ignore.case = TRUE),
    should_nudge_x = should_nudge_x
  )

  # Save out plot results:
  projection_plots_writer(results, output_path)

  results
}

#' RAG plotter
#'
#' Create and save RAG probability plots and maps
#' rag_plotter() which is for showing the model's probabilistic
#' outputs, and is a wrapper around the two projection_plots functions:
#' plot_probability(), for national use, & plot_maps(), for regional & ICB use.
#' You can force it to use the map on national, and vice versa, but unlikely to
#' be needed. This function also does some cleaning of user inputs as it goes,
#' loops through given geographies, and saves the results.
#'
#' @param plots_include A list of the kinds of plots you want to produce.
#' Will loop over each item, though multiple parts can be added into one item to
#' be plotted together, for example, this will give two kinds of plots:
#'  list("multiple_cis", c("lookbacks", "rag", "map"))
#' @param data A data frame, the formatted summary of the model outputs.
#' @param target_name A symbol string of the kind of model being run;
#' current options are one of: `admissions`, `arrival_admissions`, or `occupancy`.
#' @param model_name String of the type of model being plotted.
#' @param geography String of the geographic level, or levels, to be plotted.
#' You can give a list like this to loop through: c("nation", "region", "icb").
#' @param output_path String of the path the the timestamped output folder.
#' @param disease String picking the disease being plotted; always in config.
#' @param data_source Optional string of the data's source.
#'  Defaults based on disease read in from list at the end of this script.
#' @param force_plot Optional string containing "map" &/or "probability" if you
#' want to force that kind of plot regardless of geography. Default is NULL.
#' Everything else will warn you that it may not look good.
#' @param chosen_age_group Optional string describing which age group
#' specifically should be used used. Default is "all", used by most models.
#'
#' @examples
#' # RAG probabilities
#' outputs$rag_plotter(
#'   data = all_formatted_summary, # the summarised model results.
#'   target_name = target_name_sym, # usually admissions or occupancy.
#'   model_name = model_name, # either individual or ensemble model name.
#'   geography = c("nation", "region", "icb"), # will plot bar for national,
#'   # and maps for region & ICBs.
#'   output_path = output_path, # full path to dated outputs folder.
#'   disease = config$overall_params$disease, # all config files have now.
#' )
#'
#' @export
rag_plotter <- function( # nolint: cyclocomp_linter
    data,
    target_name,
    model_name,
    geography,
    output_path,
    disease,
    data_source = "",
    force_plot = NULL, # allows for manual mode
    chosen_age_group = "all"
    ) {
  # filter to appropriate age_group
  data <- data |>
    dplyr::filter(age_group == chosen_age_group)

  # cleaning user input:
  disease <- user_check$disease_checker(disease)
  if (is.null(data_source) || data_source == "") { # select default sources:
    data_source <- tidyr::replace_na(
      data_sources[[disease]],
      "Unspecified source")
  }

  for (geo in geography) {
    geo <- match.arg(geo, c("nation", "region", "icb")) # "NHS region",
    if (geo == "nation" ||
      any(grepl("prob|confi|ci|yard", force_plot, ignore.case = TRUE))) {
      if (geo != "nation") {
        warning(
          "Looks like you're maybe trying to plot the probability outcomes ",
          "of multiple areas at once.\n",
          "If you haven't already filtered the data to an individual region, ",
          "there's a good chance this wont work out well."
        )
      }
      plot_rag_prob <- projection_plots$plot_probability(
        data = data,
        target_name = target_name,
        model_name = model_name,
        geography = geo,
        data_source = data_source,
        disease = dplyr::case_when( #  disease name for printing
          disease %in% names(disease_prints) ~ disease_prints[[disease]],
          TRUE ~ stringr::str_to_title(disease))
      )
      # Create output directory
      output_dir <- fs::dir_create(fs::path(
        output_path,
        geo))

      fname <- glue::glue(
        "{disease}_{model_name}_{chosen_age_group}_rag_projection_probability.png"
      )

      ggplot2::ggsave(
        fs::path(output_dir, fname),
        plot_rag_prob,
        width = 10,
        height = 3,
        dpi = 500
      )
    }

    if (grepl("region|icb", geo, ignore.case = TRUE) ||
      any(grepl("map", force_plot, ignore.case = TRUE))) {
      if (geo == "nation") {
        warning(
          "Looks like you're maybe trying to map the probability outcomes ",
          "of the whole country at once.\n",
          "If you haven't got data for multiple countries, ",
          "and patched projection_plots() to accomodate this, ",
          "there's a good chance this won't help you."
        )
      }

      plot_rag_map <- projection_plots$plot_maps(
        data = data,
        target_name = target_name,
        model_name = model_name,
        geography = geo,
        data_source = data_source,
        disease = dplyr::case_when( #  disease name for printing
          disease %in% names(disease_prints) ~ disease_prints[[disease]],
          .default = stringr::str_to_title(disease))
      )

      # Create output directory
      output_dir <- fs::dir_create(fs::path(
        output_path,
        geo))

      fname <- glue::glue(
        "{disease}_{model_name}_{chosen_age_group}_rag_map.png"
      )

      ggplot2::ggsave(
        fs::path(output_dir, fname),
        plot_rag_map,
        width = 10,
        height = 11,
        dpi = 500
      )
    }
  }
}


#' LOS plotter
#'
#' Create and save length of stay plots for hospital occupancy.
#' This currently doesn't loop over geographies as that's not typically
#' performed with the same data in the current running scripts.
#'
#' @param data A data frame, the formatted summary of the model outputs,
#' built from joining the discharge_results into an expanded grid.
#' @param discharge_results Data frame containing the raw model result.
#' @param geography String of the geographic level to be plotted.
#' Looping through multiple ones is difficult due to the expanded grid of data.
#' @param output_path String of the path the the timestamped output folder.
#' @param disease String picking the disease being plotted; always in config.
#' @param x_limit numeric value for the maximum length of stay to show.
#' @param data_source Optional string of the data's source.
#'  Defaults based on disease read in from list at the end of this script.
#'
#' @examples
#' # Discharge Model and Regional length of stay plots:
#' discharge_results <- run_discharge_region(training_data, overall_params)
#' # explore posterior parameters:
#' los_regional_samples <- expand.grid(
#'   t = seq(0, overall_params$max_lag, 0.1),
#'   nhs_region_name = unique(
#'     discharge_results$training_data$nhs_region_name)) |>
#'   dplyr::left_join(discharge_results$parameters, by = "nhs_region_name") |>
#'   # probability density:
#'   dplyr::mutate(p = dlnorm(t, lognormal_mu, lognormal_sigma)) |>
#'   # cumulative probability density:
#'   dplyr::mutate(cd = plnorm(t, lognormal_mu, lognormal_sigma))
#'
#' # Build summary to plot:
#' los_regional_summary <- los_regional_samples |>
#'   dplyr::group_by(t, nhs_region_name) |>
#'   dplyr::summarise(
#'     p_50 = quantile(p, 0.5),
#'     p_5 = quantile(p, 0.05),
#'     p_95 = quantile(p, 0.95),
#'     c_50 = quantile(cd, 0.5),
#'     c_5 = quantile(cd, 0.05),
#'     c_95 = quantile(cd, 0.95)
#'   ) |>
#'   dplyr::ungroup()
#'
#' # Plot length of stay:
#' outputs$los_plotter(
#'   data = los_regional_summary, # summarised discharge_results with quantiles.
#'   discharge_results = discharge_results, # direct model result
#'   geography = "region", # only one geographic level at a time
#'   output_path = output_path, # timestamped output folder path
#'   disease = config$overall_params$disease # now in all config files here.
#' )
#' @export
los_plotter <- function(
    data,
    discharge_results,
    geography,
    output_path,
    disease,
    x_limit = 15,
    data_source = ""
    ) {
  # cleaning user input:
  disease <- user_check$disease_checker(disease)
  if (is.null(data_source) || data_source == "") { # fill in sources if blank:
    data_source <- tidyr::replace_na(
      data_sources[[disease]],
      "Unspecified source")
  }

  fs::dir_create(glue::glue("{output_path}/los"))

  LOS_plot <- data |> # nolint: object_name_linter
    ggplot2::ggplot() +
    projection_plots$theme_ham() +
    ggplot2::geom_line(ggplot2::aes(x = t, y = p_50), color = "#12436D") +
    ggplot2::geom_ribbon(
      ggplot2::aes(x = t, ymin = p_5, ymax = p_95),
      alpha = 0.4, fill = "#12436D") +
    ggplot2::ylab("Probability of discharge") +
    ggplot2::xlab("Days since admission")  +
    ggplot2::coord_cartesian(xlim = c(0, x_limit)) +
    ggplot2::labs(
      caption = glue::glue(
        "Data source: {data_source} from {
          format(min(discharge_results$predictions$date), '%d %B %Y')} to {
          format(max(discharge_results$predictions$date), '%d %B %Y')}",
        "Produced by Infectious Disease Modelling team - AIA - UKHSA",
        .sep = "\n\n"
      )
    )

  if (geography == "nation") {
    LOS_plot <- LOS_plot + # nolint: object_name_linter
      ggplot2::labs(
        title = glue::glue(
          "National {disease_prints[[disease]]} estimated length of stay in hospital")
      )

    ggplot2::ggsave(
      glue::glue("{output_path}/los/{disease}_los_national.png"),
      LOS_plot,
      width = 10,
      height = 10,
      dpi = 500
    )
  } else if (geography == "region") {
    LOS_plot <- LOS_plot + # nolint: object_name_linter
      ggplot2::facet_wrap(~nhs_region_name) +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = "#12436D")) +
      ggplot2::labs(
        title = glue::glue(
          "Regional {disease_prints[[disease]]}",
          " estimated length of stay in hospital")
      )

    ggplot2::ggsave(
      glue::glue("{output_path}/los/{disease}_los_regional.png"),
      LOS_plot,
      width = 10,
      height = 10,
      dpi = 500
    )
  } # TODO? else if (geography == "icb") {}

  LOS_plot

}

#' Create and save nowcast projection plots
#'
#' Operates as a wrapper around the `projection_plots$plot_nowcast()` function,
#' plus some tidying up of the user input, looping through geographies and
#' included plots, as well as saving the result.
#'
#' @param plots_include A list of the kinds of plots you want to produce.
#' Will loop over each item, though multiple parts can be added into one item to
#' be plotted together, for example, this will give two kinds of plots:
#'  list("multiple_cis", c("lookbacks", "rag", "map"))
#' @param data A data frame, the formatted summary of the model outputs.
#' @param latest_data Dataframe of the latest data since time of nowcast
#'  that must include the columns:
#'  - date
#'  - target
#' @param target_name A symbol string of the kind of model being run;
#' current options are one of: positive_tests.
#' @param model_name String of the type of model being plotted.
#' !ATparam geography Possible String of the geographic level, or levels,
#'  to be plotted.
#' You can give a list like this to loop through: c("nation", "region", "icb").
#' !ATparam age_granularity Possible Optional string of the age grouping
#' fidelity to use.
#' Should be the default "none" for all but RSV (also an option for RSV).
#' RSV can also have "fine", "course", anything else will be treated as "full".
#' @param output_path String of the path for the timestamped output folder.
#' @param disease String picking the disease being plotted; always in config.
#' @param data_source String of the data's source.
#' @param peaks_data Optional Boolean or data frame of peaks values.
#' If TRUE it will look up the peaks based on disease, and use stored values.
#' Will deal with changes needed for specific diseases, see peaks_handler below.
#' @param y_limit numeric value for the maximum y-axis value to show on plots.
#' If you're happy to let the script pick this, leave it as NA.
#' @param x_limit_upper Date to provide upper limit on plot's x-axis.
#' If you're happy to let the script pick this, leave it as NA.
#' @param x_limit_lower Date to provide lower limit on plot's x-axis.
#' If you're happy to let the script pick this, leave it as NA.
#'
#' @examples
#' # Norovirus nowcast plots
#' for (model_name in model_names) {
#'   outputs$nowcast_projections_plotter(
#'     plots_include = list("lookbacks", "daily", "weekly"),
#'     data = all_formatted_summary,
#'     latest_data = latest_data,
#'     target_name = overall_params$target_name,
#'     model_name = model_name,
#'     output_path = output_path,
#'     disease = overall_params$disease,
#'     y_limit = NA,
#'     x_limit_upper = NA,
#'     x_limit_lower = NA,
#'     data_source = overall_params$data_source)
#' }
#'
#' @export
nowcast_projections_plotter <- function(
    plots_include = list("lookbacks", "daily", "weekly"),
    data,
    latest_data,
    target_name,
    model_name,
    # geography = "nation", # nolint: commented_code_linter # we may use these
    # age_granularity = "none", # nolint: commented_code_linter
    output_path,
    disease,
    y_limit = NA,
    x_limit_upper = NA,
    x_limit_lower = NA,
    data_source = "",
    peaks_data = NULL
    ) {
  # TODO add in functionality for geography and age_granularity if needed
  # TODO consider how to move some of the content in
  #  projection_plots$plot_nowcast() into this function

  for (plot_type in plots_include) {
    plot_type <- match.arg(
      plot_type,
      c("lookbacks", "daily", "weekly"),
      several.ok = TRUE)

    plot_base <- projection_plots$plot_nowcast(
      data = data,
      latest_data = latest_data,
      target_name = target_name,
      model_name = model_name,
      # geography = geography, # nolint: commented_code_linter
      # age_granularity = age_granularity, # nolint: commented_code_linter
      plot_type = plot_type,
      data_source = data_source,
      disease = disease,
      output_path = output_path,
      peaks_data = peaks_data,
      y_limit = y_limit,
      x_limit_lower = as.Date(x_limit_lower),
      x_limit_upper = as.Date(x_limit_upper)
    )

  }
}


#### Internal Functions ####

#' Generate projection ggplots
#'
#' Wraps up the plotting function with putting them into a list, ready to save.
#'
#' @param plots_include A list of the kinds of plots you want to produce.
#' Will loop over each item, though multiple parts can be added into one item to
#' be plotted together, for example, this will give two kinds of plots:
#'  list("multiple_cis", c("lookbacks", "rag", "map"))
#' @param data A data frame, the formatted summary of the model outputs.
#' @param target_name A symbol string of the kind of model being run;
#' current options are one of: admissions, arrival_admissions, or occupancy.
#' @param model_name String of the type of model being plotted.
#' @param geography String of the geographic level, or levels, to be plotted.
#' You can give a list like this to loop through: c("nation", "region", "icb").
#' @param age_granularity Optional string of the age grouping fidelity to use.
#' Should be the default "none" for all but RSV (also an option for RSV).
#' RSV can also have "fine", "course", anything else will be treated as "full".
#' @param disease String picking the disease being plotted; always in config.
#' @param y_limit numeric value for the maximum y-axis value to show on plots.
#' If you're happy to let the script pick this, leave it as NA.
#' If you have multiple geographies, with different maximum values, you can
#' supply a named list like this: c("nation" = NA, "region" = 150).
#' If names are not supplied they will be filled in in the geography order.
#' If more geographies than y_limit values are given, the remainder will be NA.
#' @param x_limit_upper Date to provide upper limit on plot's x-axis.
#' @param x_limit_lower Date to provide lower limit on plot's x-axis.
#'  Can also be a number of days before x_limit_upper.
#' @param data_source Optional string of the data's source.
#'  Defaults based on disease read in from list at the end of this script.
#' @param plot_historic_fit Optional Boolean for whether to plot the model fit
#'  and ribbon for the historical fit; the forecast is always shown. Default = T
#' @param peaks_data Optional Boolean or data frame of peaks values.
#' If TRUE it will look up the peaks based on disease, and use stored values.
#' Will deal with changes needed for specific diseases, see peaks_handler below.
#' @param contains_nowcast binary option for whether data contains a nowcast,
#' as these are plotted differently. defaults to `FALSE`.
#' @param should_nudge_x should x be nudged? defaults to `FALSE`.
#' Nudging refers to whether to align the peak text left or right. Left is True
#'
#' @return A dataframe containing a list of the ggplots
#' ~at~export
#'
projection_plot_builder <- function(
    plots_include,
    data,
    target_name,
    model_name,
    geography,
    age_granularity,
    disease,
    y_limit,
    x_limit_lower,
    x_limit_upper,
    data_source,
    plot_historic_fit,
    peaks_data,
    contains_nowcast = FALSE,
    should_nudge_x
    ) {
  disease_print <- ifelse( # reformatting disease name for printing
    disease %in% names(disease_prints),
    disease_prints[[disease]],
    stringr::str_to_title(disease))

  # tibble to return plots in
  results <- tidyr::expand_grid(
    geography = geography,
    plots_include = plots_include) |>
    dplyr::mutate(
      location_covered = ifelse(
        geography == "icb",
        dplyr::first(data$nhser23nm), # data comes pre-filtered to single ICB
        "England"), # The rest cover all England; TODO: handle devolved nations?
      disease_name = disease, # adding these in here for posterity & file name.
      modelled_name = model_name,
      age_group_granularity = age_granularity,
      plot = purrr::map2(
        geography, plots_include,
        \(geo, plot_type) {
          projection_plots$plot_projection(
            data = data,
            target_name = target_name,
            model_name = model_name,
            geography = geo,
            age_granularity = age_granularity,
            plot_elements = plot_type,
            data_source = data_source,
            disease = disease_print,
            peaks_data = peaks_data[[geo]], # handles RSV's geographic oddities
            plot_historic_fit = plot_historic_fit,
            y_limit = y_limit[[geo]], # handles different limits per geography
            x_limit_lower = x_limit_lower,
            x_limit_upper = x_limit_upper,
            contains_nowcast = contains_nowcast,
            should_nudge_x = should_nudge_x)
        }))

  results
}


#' Save out plots
#'
#' @param results A dataframe containing ggplots to save, & file name elements.
#' @param output_path String of the path the the timestamped output folder.
#'
#' @return Nothing; run for its side effect
#' ~at~export

projection_plots_writer <- function(results, output_path) {

  for (plot_i in seq_len(nrow(results))) {
    # Create output directory
    output_dir <- fs::dir_create(fs::path(
      output_path[1],
      results$geography[[plot_i]]))

    # Save outputs
    ggplot2::ggsave(
      filename = paste0(
        output_dir, "/",
        results$disease_name[[plot_i]], "_",
        ifelse( # catch for the collection of ICB level projections, by region
          results$geography[[plot_i]] == "icb",
          paste0(results$location_covered[[plot_i]], "_"), ""),
        results$modelled_name[[plot_i]], "_",
        paste0(results$plots_include[[plot_i]], collapse = "_"),
        ifelse(
          results$age_group_granularity[[plot_i]] == "none",
          "", paste0("_age_", results$age_group_granularity[[plot_i]])),
        "_projection",
        ifelse( # allow for passing in of extra text for file names.
          length(output_path) > 1,
          paste0(
            "_", paste(
              output_path[[seq(from = 2, to = length(output_path), by = 1)]],
              collapse = "_")),
          ""),
        ".png"),
      plot = results$plot[[plot_i]],
      width = 11,
      height = 10,
      dpi = 500
    )
  }
}


#' Peaks Handler
#'
#' Bundles all the horrible, disease specific peaks handling into one place.
#' Do not export, internal use only.
#'
#' @param disease String picking the disease being plotted; always in configs.
#' @param target_name A (symbol) string of the kind of model being run;
#' current options are one of: `admissions`, `arrival_admissions`, or `occupancy`.
#' @param geography String of the geographic level to use; doesn't loop.
#' Only really needed for RSV.
#' @param age_granularity Optional string of the age grouping fidelity to use.
#' Should be the default "none" for all but RSV (also an option for RSV).
#' RSV can also have "fine", "course", or "full" (which shouldn't do anything).
#'
#' ~Atexport~
peaks_handler <- function(
    disease,
    target_name,
    geography = "nation", # only use one; call function within geographies loop
    age_granularity = "none" # only needed for RSV
    ) {
  if (disease == "rsv") {
    return(rsv_peaks_handler( # RSV bundled away in its own function
      target_name = target_name,
      training_data = NULL,
      trust_age_population = NULL,
      geography = geography,
      age_granularity = age_granularity))
  } else if (disease == "norovirus" &&
    grepl("occupancy", target_name, ignore.case = TRUE)) {
    noro_peaks_data <- peaks$get_peaks(
      data = NULL, disease = disease, metric = "peaks_lookup")

    noro_peaks_data <- peaks$peak_rate_calculator(
      pop_data = noro_peaks_data |> # TODO: if trough is available,
        dplyr::filter(variable == "total_beds") |> # make this the average
        dplyr::rename("total_beds" = value) |> # of max & min total_beds.
        dplyr::select(-variable, -name),
      pop_base_data = NULL, # ready made above
      peaks_data = noro_peaks_data,
      numerator_col = "occupancy",
      denominator_col = "total_beds",
      rate_label = "occupancy rate (%)",
      region_col_name = "nhs_region_name",
      disease = disease) |>
      dplyr::filter(variable != "total_beds") # no longer needed
    return(noro_peaks_data)
  } else if (disease %in% c("covid", "influenza") &&
    grepl("occupancy.*rate", target_name, ignore.case = TRUE)) {
    if (!exists("pipeline_path")) {
      config <- config::get(file = here::here(
        disease, "models",
        paste0(disease, "_config.yaml")))
      pipeline_path <- s3$find_latest_file(
        uri = config$files$pipeline_path,
        pattern = config$files$pipeline_pattern)
    }
    rate_peaks_data <- peaks$peak_rate_calculator(
      pop_data = NULL, # calculated internally from base below
      pop_base_data = aws.s3::s3read_using(
        vroom::vroom,
        object = pipeline_path) |>
        dplyr::mutate(total_beds = rowSums(
          dplyr::across(
            dplyr::starts_with("total_")),
          na.rm = TRUE),
        occupancy = rowSums(
          dplyr::across(dplyr::starts_with("occupancy_")), na.rm = TRUE)),
      peaks_data = NULL, # pulled in from s3 internally
      numerator_col = "occupancy",
      denominator_col = "total_beds",
      rate_label = "bed_occupancy_rate_(%)", # needs to match target_name later
      region_col_name = "nhs_region_name",
      disease = disease)
    return(rate_peaks_data)
  } else {
    return(peaks$get_peaks(NULL, disease, "")) # general peaks lookup
  }
}

#
#

#' RSV Peak Handling
#'
#' Targets wasn't compatible with the original version of this function,
#' so I've pulled it out and exported it for use.
#'
#' @param target_name A symbol specifying the kind of model being run;
#'  current options are one of: `admissions`, `arrival_admissions`, or `occupancy`;
#'  `estimate_admissions` should also work, but is more sensitive to changes.
#' @param training_data Dataframe of the data put into the model,
#'  used to extract the available populations (i.e. that of reporting trusts)
#'  by age group from the appropriate winter.
#' @param trust_age_population Dataframe giving all trust populations,
#'  by age group, derived from `source("./rsv/models/src/population.R")`
#' @param geography String of the geographic level to use; single value only.
#'  Current options are "nation" or "region"; "ICB" might be added one day.
#' @param age_granularity Optional string of the age grouping fidelity to use.
#'  Options are the default "none", "fine", "course", or "full".
#'
#' @return Dataframe of peak values for the target RSV
#' @export
#'

rsv_peaks_handler <- function(
    target_name,
    training_data = NULL,
    trust_age_population = NULL,
    geography = "nation", # only use one; call function within geographies loop
    age_granularity = "none"
    ) {
  rsv_peaks_data <- peaks$get_peaks(
    data = NULL,
    disease = "rsv",
    metric = "peaks_lookup")
  # it helps to give it the start of the file name in case someone puts other
  # files containing the string 'rsv' in that folder.
  if ("age_group" %in% colnames(rsv_peaks_data)) {
    if (age_granularity == "none" || age_granularity == FALSE ||
      is.null(age_granularity)) { # remove duplicate lines from age groups:
      rsv_peaks_data <- dplyr::filter(rsv_peaks_data, age_group == "All ages")
    } else { # remove all-age groups from plots with age breakdowns:
      rsv_peaks_data <- dplyr::filter(rsv_peaks_data, age_group != "All ages")
    }
  }

  if (grepl("occupancy", target_name, ignore.case = TRUE)) {
    # calculating the rate before generating peaks only works at trust level
    rsv_peaks_data <- peaks$peak_rate_calculator(
      pop_data = rsv_peaks_data |> # here 'population' means number of beds
        tidyr::pivot_wider(names_from = variable, values_from = value) |>
        dplyr::summarise( # max/mean future-proofs this; summarise drops rest
          total_paed_beds = max(c(total_paed_beds, 0), na.rm = TRUE),
          .by = c(geo_type, geo_area, age_group)),
      peaks_data = rsv_peaks_data,
      numerator_col = "rsv_paediatric_occupancy",
      denominator_col = "total_paed_beds",
      rate_label = rlang::as_string(target_name), # in case it's not already
      disease = "rsv")
  } else if (
    grepl("estimate", target_name, ignore.case = TRUE) &&
      grepl("admission", target_name, ignore.case = TRUE)) {
    if ("pop_contribution" %in% unique(rsv_peaks_data$variable)) {
      # should now be the default path; keeping manual version in else {case}
      rsv_peaks_data <- rsv_peaks_data |>
        dplyr::left_join(
          rsv_peaks_data |> # now calculated and stored in make_rsv_peaks.R
            dplyr::filter(variable == "pop_contribution") |>
            dplyr::rename(pop_contribution = value) |>
            dplyr::select(geo_type, geo_area, age_group, pop_contribution),
          by = c("geo_type", "geo_area", "age_group")) |>
        dplyr::mutate(
          variable = ifelse( # correcting admissions of just the relevant areas
            variable == "admissions" &
              grepl(geography, geo_type, ignore.case = TRUE),
            "estimated admissions",
            variable),
          value = ifelse(
            variable == "estimated admissions",
            value * pop_contribution,
            value)) |>
        dplyr::select(-pop_contribution)
    } else {
      warning("RSV peaks baseline data unavailable.")
      rsv_peaks_data <- FALSE
    }
  }
  return(rsv_peaks_data)
}

#' Rounding Output Values
#'
#' Round probabilities to 2 decimal places and covert to a character.
#'
#' Probabilities above 0.95 are reported as "p > 0.95".
#' Probabilities below 0.05 are reported as "p < 0.05"
#' Otherwise, the probabilities are rounded to two decimal places
#' and converted to a character
#'
#' @param input_data data frame with columns `p_increase`, `p_decrease` & `p_stable`
#' @return data frame with additional columns with names `p_*_rounded`
#' @export
round_probabilities <- function(input_data) {

  input_data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("p_"),

        \(p) {
          dplyr::case_when(
            p > 0.95 ~ "> 0.95",
            p < 0.05 ~ "< 0.05",
            # convert to character to ensure consistent data type
            # regional team asked for 2 dp, will use sprintf to round so trailing zeros are preserved
            # we return this & the original probability so ties can be resolved at their end
            p >= 0.05 & p <= 0.95 ~ as.character(sprintf(p, fmt = "%#.2f")
            ),
            .default = NA_character_
          )
        },

        .names = "{.col}_rounded"

      )
    )

}

##### Age Group Helpers #####

#' Age group aligner
#'
#' Helper function to align common smaller RSV age groups into larger ones.
#' Cannot go the other way round.
#'
#' @param age_group Column of age groups to be
#' @param age_granularity String option of age grouping to compress down to;
#' either "coarse" or the default "fine".
#'
#' @return Age group column where selected narrow age bands have been replaced
#'  with wider age bands
#'
#' @examples
#' pop |>
#'   dplyr::mutate(age_group = align_age_groups(age_group, "coarse")) |>
#'   dplyr::summarise(
#'     population = sum(population, na.rm = TRUE), # to combine values
#'     .by = c(age_group)) # likely other group variable here too
#'
align_age_groups <- function(
    age_group,
    age_granularity = c("fine", "coarse")
    ) {
  age_granularity <- rlang::arg_match(age_granularity)
  # TODO: this could check the available age groups are compatible too;
  # i.e. only go up in size.
  # e.g. if asked for fine but it's already coarse, it should fail.

  dplyr::case_when(
    age_granularity == "fine" &
      age_group %in% c("[0,1)", "[1,2)") ~ "[0,2)",
    age_granularity == "fine" &
      age_group %in% c("[75,85)", "[85,120)") ~ "[75,120)",
    age_granularity == "coarse" &
      age_group %in% c("[0,1)", "[1,2)", "[2,5)") ~ "[0,5)",
    age_granularity == "coarse" &
      age_group %in% c("[65,75)", "[75,85)", "[85,120)") ~ "[65,120)",
    .default = age_group)
}

##### Internal Lookups #####
#' Common data sources and alternate print names by disease.
#' Usage: `data_sources[[disease]]` & `disease_prints[[disease]]`
#' No need to export.
data_sources <- list(
  "covid" = "NHSE UEC COVID-19 data",
  "influenza" = "NHSE UEC Influenza data",
  "norovirus" = "NHSE UEC SitRep - Sentinel",
  "rsv" = "UKHSA SGSS")
disease_prints <- list(
  "covid" = "COVID-19",
  "influenza" = "Influenza",
  "norovirus" = "Norovirus cases",
  "rsv" = "RSV")
