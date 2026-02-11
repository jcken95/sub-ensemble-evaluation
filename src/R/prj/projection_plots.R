#' @name projection_plots
#' @section Version: 0.0.6
#'
#' @title Projection plots
#'
#' @description Functions for plots in winter pressure forecasting
#'
".__module__."


box::use(
  box / deps_,
  box / redshift,
  box / help_,
  box / s3,
  prj / peaks
)

.on_load <- function(ns) {
  deps_$need(
    "cowplot",
    "cli",
    "dplyr",
    "ggforce",
    "ggnewscale",
    "ggplot2",
    "ggtext",
    "glue",
    "lubridate",
    "purrr",
    "rlang",
    "scales",
    "sf",
    "stringr",
    "stats",
    "tibble",
    "tidyr"
  )
}


#' Create a basic plot for projections that can be used for any disease forecast
#'
#' Includes on plot - title, data source, colors by fit/projection, CI ribbon, real data points by geography
#' @param data Dataframe that need to be in the formatted model output and must include the columns:
#'  - prediction_start_date
#'  - model
#'  - location_level
#'  - date
#'  - target_value
#'  - pi_5
#'  - pi_95
#'  - pi_50
#'
#'  Other columns may be required when adding other CIs
#'
#' @param target_name Metric that is being forecasted eg admissions
#' @param geography  Location level to be plotted
#' @param age_granularity Age breakdown to be plotted. Either "none" or anything
#'    the other valid options as defined by the forecast schema.
#' @param model_name Name of model to be plotted
#' @param plot_elements Different parts of the projection plot that can be plotted
#' @param data_source Name of data source that contains the target that is used in the model
#' @param disease Disease being forecasted
#' @param peaks_data Optional Boolean or data frame of peaks values.
#' If TRUE it will look up the peaks based on disease, and use stored values.
#' @param plot_historic_fit Optional Boolean for whether to plot the model fit
#'  and ribbon for the historical fit; the forecast is always shown. Default = T
#' @param y_limit numeric value for the maximum y-axis value to show on plots.
#' Setting to `NA` will give an automatically chosen value.
#' If multiple geographies are plotted, and different maximum values are required, you can
#' supply a vector: c("nation" = NA, "region" = 150).
#' If names are not supplied they will be filled in in the geography order.
#' If more geographies than y_limit values are given, the remainder will be NA.
#' @param x_limit_upper Date to provide upper limit on plot's x-axis.
#' @param x_limit_lower Date to provide lower limit on plot's x-axis.
#'
#' The data_source and disease can be defined in disease specific scripts  and are used for caption/title
#'
#' @examples
#'
#' # Creating the most basic plot on national geography
#'
#' plot_projection(
#'   data = all_formatted,
#'   target_name = "admissions",
#'   geography = "nation",
#'   ages = "all",
#'   plot_elements = "base",
#'   days_plotted = 60,
#'   data_source = "NHSE UEC",
#'   disease = "COVID-19"
#' )
#'
#' # Creating a plot with lookbacks and RAG scale on a regional scale
#'
#' plot_projection(
#'   data = all_formatted,
#'   target_name = "admissions",
#'   geography = "region",
#'   ages = "all",
#'   plot_elements = c("lookbacks", "rag"),
#'   days_plotted = 60,
#'   data_source = "NHSE UEC",
#'   disease = "COVID-19"
#' )
#'
#' # Creating a plot with lookbacks and RAG scale on a national scale with age breakdown
#'
#' plot_projection(
#'   data = all_formatted,
#'   target_name = "admissions",
#'   geography = "national",
#'   ages = "granular",
#'   plot_elements = c("lookbacks", "rag"),
#'   data_source = "NHSE UEC",
#'   disease = "COVID-19"
#' )
#'
#' @export
plot_projection <- function(
  data,
  target_name,
  geography = "nation",
  age_granularity = "none",
  model_name,
  plot_elements,
  data_source,
  disease,
  peaks_data = peaks$get_peaks(data, disease, target_name),
  plot_historic_fit = TRUE,
  y_limit = NA,
  x_limit_upper = NA,
  x_limit_lower = NA,
  contains_nowcast = FALSE,
  should_nudge_x = FALSE
) {
  projection_colours <- c(
    "Model fit" = select_ukhsa_colour("turquoise"),
    "Forecast" = select_ukhsa_colour("dark pink"),
    "Nowcast" = select_ukhsa_colour("light purple")
  )

  fit_identifier <- dplyr::if_else(contains_nowcast, "Nowcast", "Model fit")

  if (is.null(x_limit_lower) || is.na(x_limit_lower)) {
    x_limit_lower <- data |> # set a default for a required value
      dplyr::filter(
        prediction_start_date == max(prediction_start_date, na.rm = TRUE)
      ) |>
      dplyr::pull(date) |>
      min() |>
      as.Date()
  }
  # Format all the data
  # need to order the age factors for nicer looking facet
  data <- data |>
    dplyr::mutate(
      age_group = stats::reorder(
        age_group,
        as.integer(stringr::str_extract(
          age_group,
          "\\d+"
        ))
      ),
      # Shortens names for ICB
      location = location |>
        stringr::str_remove(stringr::fixed("NHS ")) |>
        stringr::str_remove(stringr::fixed(" Integrated Care Board"))
    )

  real_data <- data |>
    dplyr::filter(
      location_level == geography & age_group_granularity == age_granularity,
      date > x_limit_lower
    ) |>
    dplyr::distinct(date, location, age_group, target_value)

  current <- data |>
    dplyr::filter(
      prediction_start_date == max(prediction_start_date, na.rm = TRUE) &
        model == model_name &
        location_level == geography &
        age_group_granularity == age_granularity
    ) |>
    dplyr::mutate(is_projection = ifelse(date >= prediction_start_date, "Forecast", fit_identifier))

  current_projection_text <- paste0(
    "Current forecast \nstarting ",
    max(current$prediction_start_date),
    ":"
  )

  if (grepl("region", geography, ignore.case = TRUE)) {
    real_data <- peaks$peak_cleaning(
      real_data,
      region_col = "location",
      maintain_colnames = TRUE
    )
    current <- peaks$peak_cleaning(
      current,
      region_col = "location",
      maintain_colnames = TRUE
    )
  }
  if (!plot_historic_fit) {
    current <- dplyr::filter(current, is_projection == "Forecast")
  }

  # Format the data for RAG plot
  if ("rag" %in% plot_elements) {
    grouping_columns <- c(
      "location",
      if (age_granularity != "none") "age_group"
    )

    selecting_columns <- c(
      "date",
      "prediction_start_date",
      "location",
      "p_increase",
      "p_decrease",
      "p_stable",
      if (age_granularity != "none") "age_group"
    )

    joining_columns <- c(
      "location",
      "prediction_start_date",
      if (age_granularity != "none") "age_group"
    )

    current <- current |>
      dplyr::group_by(!!!rlang::syms(grouping_columns)) |>
      dplyr::filter(date == max(date)) |>
      dplyr::select(selecting_columns) |>
      tidyr::pivot_longer(
        cols = c("p_increase", "p_decrease", "p_stable"),
        names_to = "is_projection",
        values_to = "probability"
      ) |>
      # order probabilities to resolve a tie is p_i == p_j for some i != j \in (increase, decrease, stable)
      # we will give a preference via the ordering: increase > decrease > stable in the colouring
      # of the forecast bands - this corresponds highlighting a change over stability, and choosing increase,
      # the worst possible outcome, over decrease (best)
      dplyr::mutate(
        ordered_probability = ordered(
          is_projection, # levels specified from low to high:
          levels = c("p_stable", "p_decrease", "p_increase")
        )
      ) |>
      dplyr::group_by(
        date,
        prediction_start_date,
        !!!rlang::syms(grouping_columns)
      ) |>
      dplyr::filter(
        probability == max(probability)
      ) |>
      # separate filter() calls as the second is a tie-break, not an "and"
      # if no tie present, ordered_probability will have one unique value,
      # making this line redundant in those cases
      dplyr::filter(
        ordered_probability == max(ordered_probability)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-probability, -date) |>
      dplyr::right_join(
        dplyr::select(current, -is_projection),
        by = joining_columns
      ) |>
      dplyr::mutate(
        is_projection = ifelse(
          date < prediction_start_date,
          fit_identifier,
          is_projection
        )
      ) |>
      dplyr::mutate(dplyr::across(
        "is_projection",
        \(x) stringr::str_replace(x, "p_", "Forecast - ")
      ))

    projection_colours <- c(
      "Forecast - decrease" = select_ukhsa_colour("dark blue"),
      "Forecast - stable" = select_ukhsa_colour("orange"),
      "Forecast - increase" = select_ukhsa_colour("dark pink"),
      "Nowcast" = select_ukhsa_colour("light purple"),
      "Model fit" = select_ukhsa_colour("turquoise")
    )
  }

  # Base plot
  plot <- ggplot2::ggplot(current, ggplot2::aes(x = date)) +
    theme_pancasts() +
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 1, label.position = "right")
    ) +
    ggplot2::scale_x_date(
      labels = scales::label_date_short(),
      expand = ggplot2::expansion(mult = 0.02)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma(accuracy = NULL),
      expand = ggplot2::expansion(c(0.05, 0.1))
    ) +
    ggplot2::labs(
      x = glue::glue(
        "{prefix} Date",
        prefix = unique(data$target_name) |>
          stringr::str_extract("[aA]dmission|[cC]ase|[oO]ccupancy") |>
          stringr::str_to_title()
      ),
      y = stringr::str_to_title(
        glue::glue("Daily {gsub('_', ' ', target_name)}")
      ),
      title = glue::glue(
        "{disease} {gsub('_', ' ', target_name)} {max(current$forecast_horizon)}-day forecast"
      ),
      caption = glue::glue(
        "Data source: {data_source} from {format(min(real_data$date, na.rm = TRUE), '%d %B %Y')}",
        " to {format(max(real_data$date[!is.na(real_data$target_value)], na.rm = TRUE), '%d %B %Y')}",
        "\n\nProduced by Infectious Disease Modelling team - AIA - UKHSA",
      )
    )

  # Add layers to plot
  if (age_granularity != "none") {
    age_groups <- current |>
      dplyr::filter(age_group_granularity == age_granularity) |>
      dplyr::distinct(age_group) |>
      dplyr::pull()

    # get the lower age in every bracket by finding the number between "[" and "," and sort these numerically
    ordered_ages <- sort(as.numeric(stringr::str_match(age_groups, paste0("\\[", "\\s*(.*?)\\s*", ","))[, 2]))

    # get the maximum age of the highest age bracket by finding the numbers between "," and ")" and taking the max
    max_age <- as.character(max(as.numeric(stringr::str_match(age_groups, paste0(",", "\\s*(.*?)\\s*", "\\)"))[, 2])))

    # now the ages are ordered, reconstruct the ranges as [lo, hi)
    # prepend "["
    age_groups_ordered <- paste0("[", as.character(ordered_ages))
    age_groups_ordered <- paste0(
      age_groups_ordered,
      # add comma
      rep(",", length(age_groups_ordered)),
      # append the next element from the ordered list, or the max age for the final group
      c(as.character(ordered_ages)[-1], max_age),
      # add the closing bracket
      rep(")", length(age_groups_ordered))
    )
  } else {
    age_groups_ordered <- "none"
  }

  plot <- plot |>
    projections_age(disease, age_granularity, age_groups_ordered, current, target_name) |>
    projections_geography(disease, geography, current, target_name) |>
    projections_lookbacks(
      plot_elements,
      current,
      data,
      model_name,
      geography,
      age_granularity
    ) |>
    projections_fit(
      real_data,
      model_name,
      projection_colours,
      current_projection_text
    ) |>
    projections_cis(plot_elements, real_data) |>
    projections_peaks(
      disease,
      geography,
      peaks_data,
      current,
      target_name,
      max(data$date),
      x_limit_upper,
      x_limit_lower,
      should_nudge_x = should_nudge_x
    )

  # set theme
  plot <- plot +
    ggplot2::coord_cartesian(
      xlim = c(x_limit_lower, x_limit_upper),
      ylim = c(0, y_limit)
    ) +
    theme_pancasts()

  plot <- facet_national(plot, geography, disease, age_granularity)

  plot
}

#' Create RAG probability column plot for forecasting projections.
#'
#' Includes on plot - title, data source, colors by projection.
#' @param data Dataframe that need to be in the formatted
#' model output and must include the columns:
#'  - prediciton_start_date
#'  - model
#'  - location_level
#'  - date
#'  - target_value
#'  - pi_5
#'  - pi_95
#'  - pi_50
#'  - p_increase
#'  - p_decrease
#'  - p_stable
#'
#'  Other columns may be required when adding other CIs
#'
#' @param target_name Metric that is being forecasted eg admissions
#' @param geography  Location level to be plotted
#' @param model_name Name of model to be plotted
#' @param data_source Name of data source that contains the target that is used in the model
#' @param disease Disease being forecasted
#'
#' The data_source and disease can be defined in disease specific scripts
#' and are used for caption/title
#'
#' @examples
#'
#' # Creating the plot on national geography
#'
#' plot_projection(
#'   data = all_formatted,
#'   target_name = "admissions",
#'   geography = "nation",
#'   data_source = "NHSE UEC",
#'   disease = "COVID-19"
#' )
#'
#' @export
plot_probability <- function(data, target_name, geography = "nation", model_name, data_source, disease) {
  plot_data <- data |>
    dplyr::filter(
      prediction_start_date == max(prediction_start_date, na.rm = TRUE) &
        model == model_name &
        location_level == geography
    ) |>
    dplyr::group_by(location) |>
    dplyr::filter(date == max(date, na.rm = TRUE)) |>
    dplyr::select(
      date,
      prediction_start_date,
      location,
      p_increase,
      p_decrease,
      p_stable
    ) |>
    tidyr::pivot_longer(
      cols = c("p_increase", "p_decrease", "p_stable"),
      names_to = "is_projection",
      values_to = "probability"
    ) |>
    dplyr::mutate(dplyr::across("is_projection", stringr::str_replace, "p_", "")) |>
    dplyr::mutate(is_projection = factor(is_projection, levels = c("increase", "stable", "decrease")))

  probability_colors <- c(
    "decrease" = select_ukhsa_colour("dark blue"),
    "stable" = select_ukhsa_colour("orange"),
    "increase" = select_ukhsa_colour("dark pink")
  )

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date)) +
    theme_pancasts() +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, label.position = "right")) +
    ggplot2::scale_x_date(labels = NULL) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::theme(legend.position = "bottom", legend.box = "horizontal") +
    ggplot2::geom_col(ggplot2::aes(y = probability, x = date, fill = is_projection), position = "stack") +
    ggplot2::scale_fill_manual(values = probability_colors) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(0.5, 3.5, 0.5, 0.5, "cm"),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      y = "Probability",
      x = NULL,
      fill = "Forecast direction",
      title = "Probability of trend direction for the 14-day forecast",
      caption = glue::glue(
        "Produced by Infectious Disease Modelling team - AIA - UKHSA",
        .sep = "\n\n"
      )
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::coord_flip()

  plot
}

#' Create RAG maps (Region and ICB level) for forecasts
#'
#' Includes on plot - title, data source, colors by projection.
#' @param data Dataframe that need to be in the formatted model output and must include the columns:
#'  - prediciton_start_date
#'  - model
#'  - location_level
#'  - location
#'  - date
#'  - target_value
#'  - pi_5
#'  - pi_95
#'  - pi_50
#'  - p_increase
#'  - p_decrease
#'  - p_stable
#'
#'  Other columns may be required when adding other CIs
#'
#' @param target_name Metric that is being forecasted eg admissions
#' @param geography  Location level to be plotted
#' @param model_name Name of model to be plotted
#' @param data_source Name of data source that contains the target that is used in the model
#' @param disease Disease being forecasted
#'
#' The data_source and disease can be defined in disease specific scripts and are used for caption/title
#'
#' @examples
#'
#' # Creating the map on regional geography
#'
#' plot_projection(
#'   data = all_formatted,
#'   target_name = "admissions",
#'   geography = "region",
#'   data_source = "NHSE UEC",
#'   disease = "COVID-19"
#' )
#'
#' @export
plot_maps <- function(
  data,
  target_name,
  geography = c("region", "icb"),
  model_name,
  days_plotted = 60,
  data_source,
  disease
) {
  geography <- rlang::arg_match(geography)

  real_data <- data |>
    dplyr::filter(
      location_level == geography,
      date > as.Date(max(date)) - days_plotted
    ) |>
    dplyr::distinct(date, location, target_value)

  probability_colors <- c(
    "increase" = select_ukhsa_colour("dark pink"),
    "stable" = select_ukhsa_colour("orange"),
    "decrease" = select_ukhsa_colour("dark blue")
  )

  probability_alphas <- c("0-25%" = 0.25, "26-50%" = 0.5, "51-75%" = 0.75, "75%+" = 1)
  probability_cuts <- c(0, unname(probability_alphas)) # ensures they match

  # If you change these names you'll need to change the values below to match

  rs <- redshift$data_model(c("REDACTED", "REDACTED"))

  region_data_model <- rs$REDACTED |>
    dplyr::select(geometry, nhser22nm)

  region_lookup <- dplyr::collect(region_data_model) |>
    dplyr::rename(nhs_region = nhser22nm) |>
    dplyr::select(nhs_region, geometry)

  if (geography == "region") {
    map_data <- data |>
      dplyr::filter(
        prediction_start_date == max(prediction_start_date, na.rm = TRUE) &
          model == model_name &
          location_level == geography
      ) |>
      dplyr::mutate(is_projection = ifelse(date >= prediction_start_date, "Forecast", "Model fit")) |>
      dplyr::filter(is_projection == "Forecast") |>
      dplyr::filter(date == max(date)) |>
      dplyr::select(date, prediction_start_date, location, p_increase, p_decrease, p_stable) |>
      tidyr::pivot_longer(
        cols = c("p_increase", "p_decrease", "p_stable"),
        names_to = "is_projection",
        values_to = "probability"
      ) |>
      dplyr::group_by(date, prediction_start_date, location) |>
      dplyr::filter(probability == max(probability)) |>
      dplyr::mutate(dplyr::across("is_projection", stringr::str_replace, "p_", "")) |>
      dplyr::ungroup() |>
      dplyr::mutate(dplyr::across("location", stringr::str_replace, " commissioning region", "")) |>
      dplyr::left_join(region_lookup, by = c("location" = "nhs_region")) |>
      dplyr::mutate(
        ranges = cut(
          probability,
          probability_cuts,
          labels = names(probability_alphas)
        )
      ) |>
      sf::st_as_sf(crs = sf::st_crs("epsg:4326"))

    map <-
      ggplot2::ggplot(map_data) +
      ggplot2::geom_sf(
        ggplot2::aes(
          geometry = geometry,
          fill = is_projection,
          alpha = ranges
        ),
        lwd = 0.3
      )
  }

  if (geography == "icb") {
    icb_data_model <- rs$REDACTED |>
      dplyr::select(geometry, icb23nm, icb23cd)

    # Abbreviate ICB names - remove constant prefix and suffix
    icb_lookup <- dplyr::collect(icb_data_model) |>
      dplyr::mutate(
        icb_name = icb23nm |>
          stringr::str_remove(stringr::fixed("NHS ")) |>
          stringr::str_remove(stringr::fixed(" Integrated Care Board"))
      ) |>
      dplyr::select(icb_name, geometry, icb23cd)

    data <- data |>
      dplyr::mutate(
        location = location |>
          stringr::str_remove(stringr::fixed("NHS ")) |>
          stringr::str_remove(stringr::fixed(" Integrated Care Board"))
      )

    map_data <- data |>
      dplyr::filter(
        prediction_start_date == max(prediction_start_date, na.rm = TRUE) &
          model == model_name &
          location_level == geography
      ) |>
      dplyr::mutate(is_projection = ifelse(date >= prediction_start_date, "Forecast", "Model fit")) |>
      dplyr::filter(is_projection == "Forecast") |>
      dplyr::filter(date == max(date)) |>
      dplyr::select(date, prediction_start_date, location, p_increase, p_decrease, p_stable) |>
      tidyr::pivot_longer(
        cols = c("p_increase", "p_decrease", "p_stable"),
        names_to = "is_projection",
        values_to = "probability"
      ) |>
      dplyr::group_by(date, prediction_start_date, location) |>
      dplyr::filter(probability == max(probability)) |>
      dplyr::mutate(dplyr::across("is_projection", stringr::str_replace, "p_", "")) |>
      dplyr::ungroup() |>
      dplyr::left_join(icb_lookup, by = c("location" = "icb_name")) |>
      dplyr::mutate(
        ranges = cut(
          probability,
          probability_cuts,
          labels = names(probability_alphas)
        )
      ) |>
      sf::st_as_sf(crs = sf::st_crs("epsg:4326"))

    map <-
      ggplot2::ggplot(map_data) +
      ggplot2::geom_sf(ggplot2::aes(geometry = geometry, fill = is_projection, alpha = ranges), lwd = 0.3) +
      ggplot2::scale_alpha_discrete(name = "Probability") +
      ggplot2::geom_sf(
        data = region_lookup,
        ggplot2::aes(geometry = geometry),
        lwd = 0.8,
        colour = "black",
        alpha = 0
      )
  }

  if (grepl("ensemble", model_name, ignore.case = TRUE)) {
    model_name <- "Ensemble" # removing long name on shared outputs
  }

  map <- map +
    ggplot2::scale_alpha_manual(
      name = "Probability",
      values = probability_alphas,
    ) +
    ggplot2::scale_fill_manual(
      name = "Direction of trend",
      values = probability_colors
    ) +
    ggplot2::guides(
      alpha = ggplot2::guide_legend(override.aes = list(fill = "gray44"))
    ) +
    theme_pancasts() +
    theme_rag_map() +
    ggplot2::labs(
      title = glue::glue("Forecasted trend for {disease} {target_name} in 14-days"),
      subtitle = glue::glue(
        "Forecast up to {format(max(data$date), '%d %b %Y')}",
        "\n\n{stringr::str_to_title(gsub('_', ' ', model_name))} model"
      ),
      caption = glue::glue(
        "Data source: {data_source} from ",
        "{format(min(real_data$date, na.rm = TRUE), '%d %B %Y')} to ",
        "{format(max(real_data$date[!is.na(real_data$target_value)], na.rm = TRUE), '%d %B %Y')}",
        "\n\nProduced by Infectious Disease Modelling team - AIA - UKHSA",
      )
    )

  map <- cowplot::ggdraw() +
    cowplot::draw_plot(map) +
    cowplot::draw_plot(
      probablity_direction_legend(),
      x = 0.1,
      y = 0.65,
      width = 0.25,
      height = 0.25
    ) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "white"))

  map
}


#' Construct a bivariate legend for the rag map
#' @return `gg`
probablity_direction_legend <- function() {
  direction_colour <- tibble::tibble(
    direction = c("Decrease", "Stable", "Increase"),
    colour = c("dark blue", "orange", "dark pink") |>
      purrr::map(select_ukhsa_colour) |>
      purrr::as_vector()
  )

  legend_data <- tidyr::expand_grid(
    colour = direction_colour$colour,
    probability = c("26-50%", "51-75%", "75%+")
  ) |>
    dplyr::mutate(
      probability = ordered(probability, levels = c("26-50%", "51-75%", "75%+")),
      alpha = dplyr::case_when(
        probability == "26-50%" ~ 0.5,
        probability == "51-75%" ~ 0.75,
        probability == "75%+" ~ 1,
        .default = NA_real_
      )
    ) |>
    dplyr::left_join(
      direction_colour,
      dplyr::join_by(colour)
    ) |>
    dplyr::mutate(
      direction = ordered(direction, levels = c("Decrease", "Stable", "Increase")),
    )

  palette <- direction_colour$colour
  names(palette) <- direction_colour$direction

  legend_data |>
    ggplot2::ggplot(ggplot2::aes(x = direction, y = probability, fill = direction, alpha = alpha)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::labs(x = "\nTrend direction", y = "Probability\n") +
    theme_pancasts() +
    ggplot2::theme(
      rect = ggplot2::element_blank(),
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.95, hjust = 1)
    )
}


#' Create a basic plot for projections that can be used for any disease forecast
#'
#' Includes on plot - title, data source, colors by fit/projection, CI ribbon, real data points by geography
#' @param data Dataframe that need to be in the formatted model output and
#'  must include the columns:
#'  - date
#'  - prediction_date
#'  - model
#'  - target_value
#'  - pi_5
#'  - pi_95
#'  - pi_50
#'  - t_aggregation
#'  Other columns may be required when adding other CIs
#' @param latest_data Dataframe of the latest data since time of nowcast that
#'  must include the columns:
#'  - date
#'  - target
#' @param target_name Metric that is being nowcasted e.g. "positive_tests"
#' @param model_name Name of model to be plotted
#' @param plot_type Type of nowcast plot:
#'  - "lookbacks": plot all lookbacks on a single plot
#'  - "daily": plot separate daily nowcast for each lookback
#'  - "weekly": plot separate weekly nowcast for each lookback
#' @param data_source Name of data source that contains the target that is used
#'  in the model
#' @param disease Disease being nowcasted
#' @param output_path Local output folder path to save plots into
#' @param peaks_data Optional Boolean or data frame of peaks values.
#' @param y_limit numeric value for the maximum y-axis value to show on plots.
#' If you're happy to let the script pick this, leave it as NA.
#' @param x_limit_upper Date to provide upper limit on plot's x-axis.
#' If you're happy to let the script pick this, leave it as NA.
#' @param x_limit_lower Date to provide lower limit on plot's x-axis.
#' If you're happy to let the script pick this, leave it as NA.
#'
#' @examples
#'
#' # Creating a plot with all lookbacks and
#' # daily and weekly plots for each lookback
#'
#' plot_nowcast(
#'   data = all_formatted_summary,
#'   latest_data = latest_data,
#'   target_name = "positive_tests",
#'   model_name = "gam",
#'   plot_type = c("lookbacks", "daily", "weekly"),
#'   data_source = "UKHSA SGSS",
#'   disease = "norovirus",
#'   output_path = output_path,
#'   peaks_data = NA,
#'   y_limit = NA,
#'   x_limit_upper = NA,
#'   x_limit_lower = NA
#' )
#'
#' @export
plot_nowcast <- function(
  data,
  latest_data,
  target_name,
  model_name,
  plot_type,
  data_source,
  disease,
  output_path = output_path,
  peaks_data = NULL,
  y_limit = NA,
  x_limit_upper = NA,
  x_limit_lower = NA
) {
  # TODO add in functionality for geography and age_granularity when needed

  plot_colors <- c(
    "Data at time of nowcast" = "#5aae61",
    "Data since nowcast" = "#762a83",
    "Nowcast" = "black"
  )

  if ("prediction_start_date" %in% names(data)) {
    # catch old column name
    data <- dplyr::rename(data, prediction_date = prediction_start_date)
  }

  if (plot_type %in% c("daily", "weekly")) {
    for (nowcast_date in unique(data$prediction_date)) {
      data_filtered <- data |>
        dplyr::filter(prediction_date == as.Date(nowcast_date))

      min_date <- min(
        data_filtered |>
          dplyr::filter(t_aggregation == "daily") |>
          dplyr::pull(date),
        na.rm = TRUE
      )

      max_date <- max(
        data_filtered |>
          dplyr::filter(t_aggregation == "daily") |>
          dplyr::pull(date),
        na.rm = TRUE
      )

      if (is.null(x_limit_lower) || is.na(x_limit_lower)) {
        x_limit_lower_nowcast <- min_date |>
          as.Date()
      } else {
        x_limit_lower_nowcast <- x_limit_lower
      }

      latest_data_filtered <- latest_data |>
        dplyr::filter(date <= max_date, date >= min_date)

      data_filtered <- data_filtered |>
        dplyr::filter(t_aggregation == plot_type)

      if (plot_type == "daily") {
        plt <- ggplot2::ggplot(data_filtered, ggplot2::aes(x = date)) +
          ggplot2::geom_col(
            data = latest_data_filtered,
            ggplot2::aes(
              x = date,
              y = target_value,
              fill = "Data since nowcast"
            ),
            alpha = 0.6,
            position = "identity"
          ) +
          ggplot2::geom_col(
            ggplot2::aes(y = target_value, fill = "Data at time of nowcast"),
            alpha = 0.6,
            position = "identity"
          ) +
          ggplot2::geom_ribbon(
            ggplot2::aes(ymin = pi_95, ymax = pi_5, fill = "Nowcast"),
            alpha = 0.2
          ) +
          ggplot2::geom_line(
            ggplot2::aes(y = pi_50),
            color = "black",
            linewidth = 0.7,
            linetype = "dashed"
          ) +
          ggplot2::labs(
            x = "Specimen date",
            y = glue::glue("Daily {disease} {gsub('_', ' ', target_name)}"),
            fill = paste0(
              "Nowcast up to ",
              max(data_filtered$prediction_date),
              ":"
            ),
            title = glue::glue(
              "Nowcast for {disease} {gsub('_', ' ', target_name)} up to {
              max(data_filtered$prediction_date)}"
            ),
            caption = cat(
              "Data source: ",
              data_source,
              "from",
              format(min(latest_data_filtered$date, na.rm = TRUE), "%d %B %Y"),
              "to",
              format(
                max(
                  latest_data_filtered$date[
                    !is.na(
                      latest_data_filtered$target_value
                    )
                  ],
                  na.rm = TRUE
                ),
                "%d %B %Y"
              ),
              "\n\n",
              "Produced by Infectious Disease Modelling team - AIA - UKHSA"
            )
          ) +
          ggplot2::scale_fill_manual(values = plot_colors) +
          ggplot2::scale_x_date(
            label = scales::label_date_short(),
            breaks = "1 week"
          ) +
          theme_pancasts() +
          ggplot2::theme(
            panel.grid.major = ggplot2::element_line(
              colour = "black",
              linewidth = 0.05,
              linetype = "dashed"
            ),
            panel.border = ggplot2::element_rect(
              colour = "black",
              fill = NA,
              linewidth = 1
            ),
            text = ggplot2::element_text(size = 12),
            legend.position = "bottom"
          )
      }

      if (plot_type == "weekly") {
        # Currently not used - consider depricating if stakeholders do not ask for it
        # in 2024/25

        weekly_latest_data <- latest_data_filtered |>
          dplyr::mutate(
            date = as.Date(date),
            max_date_weekday = lubridate::wday(max(date)),
            week_starting = date -
              lubridate::wday(
                date + 7 - max_date_weekday
              ) +
              1
          ) |>
          dplyr::group_by(week_starting) |>
          dplyr::summarise(target_value = sum(target_value)) |>
          dplyr::rename(date = week_starting)

        data_filtered <- data_filtered |>
          dplyr::mutate(week_end = date + 7)

        plt <- ggplot2::ggplot(data_filtered, ggplot2::aes(x = date)) +
          ggplot2::geom_col(
            data = weekly_latest_data,
            ggplot2::aes(
              x = date,
              y = target_value,
              fill = "Data since nowcast"
            ),
            alpha = 0.6,
            just = 0,
            width = 6.8,
            position = "identity"
          ) +
          ggplot2::geom_col(
            ggplot2::aes(y = target_value, fill = "Data at time of nowcast"),
            alpha = 0.6,
            just = 0,
            width = 6.8,
            position = "identity"
          ) +
          ggplot2::geom_point(
            ggplot2::aes(x = date + 3.5, y = pi_50, color = "Nowcast")
          ) +
          ggplot2::geom_errorbar(ggplot2::aes(
            x = date + 3.5,
            ymin = pi_5,
            ymax = pi_95,
            color = "Nowcast"
          )) +
          ggplot2::labs(
            x = "Specimen date",
            y = glue::glue("Weekly {disease} {gsub('_', ' ', target_name)}"),
            fill = paste0(
              "Nowcast up to ",
              max(data_filtered$prediction_date),
              ":"
            ),
            title = glue::glue(
              "Nowcast for {disease} {gsub('_', ' ', target_name)} up to {
              max(data_filtered$prediction_date)}"
            ),
            caption = cat(
              "Data source:",
              data_source,
              "from",
              format(min(latest_data_filtered$date, na.rm = TRUE), "%d %B %Y"),
              "to",
              format(
                max(
                  latest_data_filtered$date[
                    !is.na(
                      latest_data_filtered$target_value
                    )
                  ],
                  na.rm = TRUE
                ),
                "%d %B %Y"
              ),
              "\n\n",
              "Produced by Infectious Disease Modelling team - AIA - UKHSA"
            ),
            color = NULL
          ) +
          ggplot2::scale_fill_manual(values = plot_colors) +
          ggplot2::scale_color_manual(values = plot_colors) +
          ggplot2::scale_x_date(
            label = scales::label_date_short(),
            breaks = seq(from = min(weekly_latest_data$date), to = max(weekly_latest_data$date) + 7, by = "week")
          ) +
          theme_pancasts() +
          ggplot2::theme(
            panel.grid.major = ggplot2::element_line(colour = "black", linewidth = 0.05, linetype = "dashed"),
            panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
            text = ggplot2::element_text(size = 12),
            legend.position = "bottom"
          )
      }

      # set axis limits
      plt <- plt +
        ggplot2::coord_cartesian(
          xlim = c(x_limit_lower_nowcast, x_limit_upper),
          ylim = c(0, y_limit)
        )

      ggplot2::ggsave(
        filename = paste0(
          output_path,
          "/weekly_nowcasts/",
          model_name,
          "_",
          plot_type,
          "_",
          as.Date(nowcast_date),
          ".png"
        ),
        plt,
        width = 9,
        height = 7,
        dpi = 500
      )
    }
  }

  if (plot_type %in% c("lookbacks")) {
    # plot for daily nowcast only
    data <- data |>
      dplyr::filter(t_aggregation == "daily")

    n_lookbacks <- length(unique(data$prediction_date)) - 1

    lookback_colors <- scales::seq_gradient_pal(
      "#1b7837",
      "#a6dba0",
      "Lab"
    )(seq(0, 1, length.out = n_lookbacks)) |>
      rev()

    current_nowcast_text <- paste0(
      "Latest nowcast \nup to ",
      max(data$prediction_date),
      ":"
    )

    plt <- ggplot2::ggplot(
      data |>
        dplyr::filter(prediction_date == max(prediction_date)),
      ggplot2::aes(x = date)
    ) +
      ggplot2::geom_ribbon(
        data = data |>
          dplyr::filter(prediction_date != max(prediction_date)),
        ggplot2::aes(
          ymin = pi_95,
          ymax = pi_5,
          x = date,
          fill = as.factor(prediction_date)
        ),
        alpha = 0.4
      ) +
      ggplot2::scale_fill_manual(values = lookback_colors) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          order = 2,
          nrow = 2,
          label.position = "bottom"
        )
      ) +
      ggplot2::labs(fill = "Past nowcasts") +
      ggnewscale::new_scale_fill() +
      ggplot2::geom_point(
        data = latest_data |>
          dplyr::filter(date <= max(data$date)),
        ggplot2::aes(
          x = date,
          y = target_value,
          fill = "Latest data",
          col = "Latest data"
        ),
        alpha = 1,
        size = 1
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = pi_95, ymax = pi_5, fill = "Latest nowcast"),
        alpha = 0.4
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = pi_50),
        color = "black",
        linewidth = 0.5,
        linetype = "dashed"
      ) +
      ggplot2::labs(
        x = "Specimen date",
        y = "Norovirus postive tests",
        fill = NULL
      ) +
      ggplot2::scale_fill_manual(
        values = c("Latest nowcast" = "#762a83", "Latest data" = "black")
      ) +
      ggplot2::scale_color_manual(
        values = c("Latest nowcast" = "#762a83", "Latest data" = "black")
      ) +
      # nolint start: commented_code_linter
      # ggplot2::scale_x_date(
      #  label = scales::label_date_short(), breaks = "1 week",
      #  limits = c(as.Date(lower_x), as.Date(upper_x))) + # nolint end
      theme_pancasts() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(
          colour = "black",
          linewidth = 0.05,
          linetype = "dashed"
        ),
        panel.border = ggplot2::element_rect(
          colour = "black",
          fill = NA,
          linewidth = 1
        ),
        text = ggplot2::element_text(size = 12),
        legend.position = "bottom"
      ) +
      ggplot2::labs(
        fill = current_nowcast_text
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(nrow = 2, label.position = "right"),
        color = "none"
      )

    if (is.null(x_limit_lower) || is.na(x_limit_lower)) {
      x_limit_lower <- data$date |>
        min() |>
        as.Date()
    }

    # set axis limits
    plt <- plt +
      ggplot2::coord_cartesian(
        xlim = c(x_limit_lower, x_limit_upper),
        ylim = c(0, y_limit)
      )

    ggplot2::ggsave(
      filename = paste0(output_path, "/norovirus_", model_name, "_lookbacks_", max(data$prediction_date), ".png"),
      plt,
      width = 9,
      height = 7,
      dpi = 500
    )
  }
}

# ggplot2 themes ----

#' Plotting theme for Health Analysis Modelling team
#'
#' `r lifecycle::badge("superseded")`
#'  in favour of `theme_pancasts()`
#'
#' @param base_size,base_family See `help(ggplot2::theme_bw)`.
#'
#' @export
theme_ham <- function(base_size = 14, base_family = "sans") {
  ggplot2::theme_bw(base_size, base_family) +
    ggplot2::theme(
      panel.spacing = ggplot2::unit(1.5, "lines"),
      panel.border = ggplot2::element_rect(
        color = "grey50",
        fill = NA,
        linewidth = 1,
        linetype = 1
      ),
      plot.background = ggplot2::element_rect(
        fill = "white",
        colour = "white"
      ),
      panel.background = ggplot2::element_rect(
        fill = "white",
        colour = "white"
      ),
      panel.grid = ggplot2::element_line(colour = "#D9D9D9"),
      strip.text = ggplot2::element_text(colour = "white", face = "bold"),
      axis.title = ggplot2::element_text(colour = "grey50", face = "bold"),
      strip.background = ggplot2::element_rect(
        colour = "grey50",
        fill = "grey50"
      ),
      legend.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(linetype = 3, linewidth = 0.2),
      plot.caption = ggplot2::element_text(
        size = 12,
        lineheight = 0.5,
        color = "grey50",
        hjust = 0
      ),
      plot.caption.position = "plot",
      plot.title = ggtext::element_textbox(
        width = ggplot2::unit(1, "npc"),
        size = 16,
        padding = ggplot2::margin(t = 10)
      ),
      plot.title.position = "plot"
    )
}

#' Pancasts plotting theme
#' For usage in Winter 24/25 ... and beyond
#'
#' @param base_size base font size, given in pts
#'
#' @examples
#'
#' mtcars |>
#'   ggplot2::ggplot(
#'     ggplot2::aes(x = wt, y = mpg)
#'   ) +
#'   theme_pancasts()
#'
#' @export
theme_pancasts <- function(base_size = 14) {
  title_text_colour <- "black"
  base_text_colour <- select_ukhsa_colour("teal")
  axis_text_colour <- select_ukhsa_colour("grey")

  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      # text options
      text = ggplot2::element_text(family = "arial"),

      plot.title = ggplot2::element_text(
        size = 18,
        face = "bold",
        colour = title_text_colour
      ),

      plot.subtitle = ggplot2::element_text(
        size = 12,
        lineheight = 0.5,
        colour = axis_text_colour,
        hjust = 0
      ),

      axis.title = ggplot2::element_text(
        size = 12,
        colour = axis_text_colour,
        face = "bold"
      ),

      axis.text = ggplot2::element_text(
        size = 12,
        colour = axis_text_colour
      ),

      plot.caption = ggplot2::element_text(hjust = 0),
      # faceting options
      strip.background = ggplot2::element_rect(fill = base_text_colour),
      strip.text = ggplot2::element_text(
        colour = "white",
        face = "bold"
      ),

      # legend options
      legend.position = "bottom",
      legend.box = "horizontal"
    )
}

#' Additional theme parameters for RAG maps
#' For usage in Winter 24/25 ... and beyond
#'
#' @return `gg`
theme_rag_map <- function() {
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    legend.position = "none"
  )
}

#' print tibble of UKHSA text colours
#' @param plot_colours display a plot of the colours? defaults to `FALSE`
#' @example ukhsa_colours()
#' @export
ukhsa_colours <- function(plot_colours = FALSE) {
  colour_scheme <- tibble::tribble(
    ~name          , ~hex      , ~type  ,

    # plotting
    "dark blue"    , "#12436D" , "plot" ,
    "turquoise"    , "#28A197" , "plot" ,
    "dark pink"    , "#801650" , "plot" ,
    "orange"       , "#F46A25" , "plot" ,
    "dark grey"    , "#3D3D3D" , "plot" ,
    "light purple" , "#A285D1" , "plot" ,

    # text
    "teal"         , "#007C91" , "text" ,
    "midnight"     , "#003B5C" , "text" ,
    "plum"         , "#582C83" , "text" ,
    "moonlight"    , "#1D57A5" , "text" ,
    "wine"         , "#8A1B61" , "text" ,
    "cherry"       , "#E40046" , "text" ,

    # not an official UKHSA colour, but grey is in pack style guide
    "grey"         , "#808080" , "text" ,
  )

  if (plot_colours) {
    colours <- colour_scheme$hex
    names(colours) <- colour_scheme$name

    plt <- colour_scheme |>
      ggplot2::ggplot(ggplot2::aes(
        x = name,
        y = 1,
        fill = name
      )) +
      ggplot2::labs(x = "", y = "") +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_manual(
        values = colours
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none")

    print(plt)
  }

  colour_scheme
}

#' obtain the hex code of a UKHSA text colour by name
#' @param colour string. Name of UKHSA colour
#' @example select_ukhsa_colour("teal")
#' @export
select_ukhsa_colour <- function(colour) {
  rlang::arg_match(colour, values = ukhsa_colours()$name)

  ukhsa_colours() |>
    dplyr::filter(name == colour) |>
    dplyr::pull(hex)
}

# internal helpers ----

#' Add geography to projections plot
#'
#' @param gg a ggplot
#' @param chosen_disease disease chosen to model specified as a string
#' @param chosen_geography the chosen geography, specified as a lower case string
#' @param current_prediction data frame of current prediction; must have columns `nhser23nm` and `forecase_horizon`
#' @param target_name metric that is being forecasted eg admissions
projections_geography <- function(
  gg,
  chosen_disease,
  chosen_geography,
  current_prediction,
  target_name
) {
  if (!chosen_geography %in% c("region", "icb")) {
    return(gg)
  } else if (chosen_geography == "region") {
    gg <- gg +
      ggplot2::facet_wrap(ggplot2::vars(location), ncol = 3, scales = "fixed") +
      ggplot2::labs(
        title = glue::glue(
          "Regional {chosen_disease} {gsub('_', ' ', target_name)} ",
          "{max(current_prediction$forecast_horizon)}-day forecast"
        )
      ) +
      ggplot2::theme(
        legend.position = "inside",
        legend.position.inside = c(0.65, 0.125),
        legend.box = "vertical",
        legend.direction = "horizontal",
        strip.background = ggplot2::element_rect(fill = "#12436D")
      )
  } else {
    # ICB specifics
    gg <- gg +
      ggplot2::facet_wrap(
        ~location,
        ncol = 3,
        scales = "fixed",
        labeller = ggplot2::label_wrap_gen(30)
      ) +
      ggplot2::labs(
        title = glue::glue(
          "{max(current_prediction$nhser23nm)} ICB {chosen_disease} {gsub('_', ' ', target_name)} ",
          "{max(current_prediction$forecast_horizon)}-day forecast"
        )
      ) +
      ggplot2::theme(
        legend.box = "horizontal",
        legend.direction = "horizontal",
        strip.background = ggplot2::element_rect(fill = "#12436D"),
        strip.text.x = ggplot2::element_text(size = 8)
      )
  }
  gg
}

#' Add peaks to projections plot
#'
#' @param gg a ggplot
#' @param chosen_disease disease chosen to model specified as a string
#' @param chosen_geography location level to be plotted
#' @param peaks_data optional logical or data frame of peaks values.
#' @param current_prediction data frame of current prediction; must have columns `nhser23nm` and `forecase_horizon`
#' @param target_name metric that is being forecasted eg admissions
#' @param x_limit_upper date to provide upper limit x-axis
#' @param x_limit_lower date to provide lower limit x-axis
#' @param should_nudge_x should x be nudged? defaults to `FALSE`
projections_peaks <- function(
  gg,
  chosen_disease,
  chosen_geography,
  peaks_data,
  current_prediction,
  target_name,
  end_date,
  x_limit_upper = NA,
  x_limit_lower = NA,
  should_nudge_x = FALSE
) {
  if (is.null(peaks_data)) {
    return(gg)
  } else {
    nudge_x <- dplyr::case_when(
      isFALSE(should_nudge_x) ~ 0,
      is.na(x_limit_upper) | is.na(x_limit_lower) ~ 15,
      .default = -as.numeric(x_limit_upper - x_limit_lower)
    )

    peaks_data <- peaks$filter_peaks(
      peaks_data,
      area = dplyr::case_when(
        chosen_geography == "region" ~ "region",
        chosen_geography == "NHS region" ~ "NHS region",
        # TODO: If we ever start using Non-NHS English regions we'll have to
        #       rewrite all the geography stuff in the script to specify which.
        #' geography == "trust" ~ 'trust code'
        # TODO: please check what the trust level would be for this;
        #       doesn't seem to have been written here yet
        #       Need ICB capability?
        .default = "nation"
      ),
      metric = target_name, # should be easy enough
      timespan = NULL # TODO: May need to get more specific; 'winter 2022/23'
    ) |>
      dplyr::rename(location = geo_area) # makes facet_wrap ready

    gg <- gg +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = value, group = interaction(name, location)),
        data = peaks_data,
        linetype = "dashed",
        colour = "grey50",
        size = 0.3
      ) +
      ggplot2::geom_text(
        ggplot2::aes(x = date, y = value, label = name),
        data = dplyr::mutate(peaks_data, "date" = end_date),
        vjust = -0.2,
        hjust = ifelse(should_nudge_x, 0, 1),
        colour = "grey50",
        size = 3,
        nudge_x = nudge_x
      )
  }
  gg
}

#' Add age to projections plot
#'
#' @param gg a ggplot
#' @param chosen_disease disease chosen to model specified as a string
#' @param age_granularity the age granularity, specified as a lower case string
#' @param current_prediction data frame of current prediction; must have columns `nhser23nm` and `forecase_horizon`
#' @param target_name metric that is being forecasted eg `"admissions"`
projections_age <- function(gg, chosen_disease, age_granularity, age_groups_ordered, current_prediction, target_name) {
  if (age_granularity == "none") {
    return(gg)
  } else if (age_granularity != "none") {
    gg <- gg +
      ggplot2::facet_wrap(ggplot2::vars(factor(age_group, age_groups_ordered)), nrow = 2, scales = "fixed") +
      ggplot2::labs(
        title = glue::glue(
          "Age stratified {chosen_disease} {gsub('_', ' ', target_name)} ",
          "{max(current_prediction$forecast_horizon)}-day forecast"
        )
      ) +
      ggplot2::theme(
        legend.box = "vertical",
        legend.direction = "horizontal",
        # legend.position = c(1, 0.5), # nolint: commented_code_linter
        strip.background = ggplot2::element_rect(fill = "#12436D")
      )
  }
  gg
}


#' Add lookbacks to projections plot
#'
#' @param gg a ggplot
#' @param plot_elements different parts of the projection plot that can be plotted
#' @param current_prediction data frame of current prediction; must have columns `nhser23nm` and `forecase_horizon`
#' @param target_name metric that is being forecasted eg admissions
#' @param data dataframe that need to be in the formatted model output and must include the columns:
#'  - prediction_start_date
#'  - model
#'  - location_level
#'  - date
#'  - target_value
#'  - pi_5
#'  - pi_95
#'  - pi_50
#'  @param model_name name of model to be plotted
#'  @param geography location level to be plotted
#'  @param age_granularity age breakdown to be plotted. Either "none" or anything
#'    the other valid options as defined by the forecast schema.
projections_lookbacks <- function(
  gg,
  plot_elements,
  current_prediction,
  data,
  model_name,
  geography,
  age_granularity
) {
  if (!"lookbacks" %in% plot_elements) {
    return(gg)
  }
  lookback_data <- generate_lookback_data(data, model_name, geography, age_granularity)

  max_lookback <- max(lookback_data$lookback) / 7
  lookback_data$lookback <- factor(lookback_data$lookback, levels = seq(max_lookback, 1) * 7)

  lookback_colors <- scales::seq_gradient_pal(
    # choice from RColorBrewer::brewer.pal(9, "Greens")
    # second darkest to mid darkest
    "#006D2C",
    "#41AB5D",
    "Lab"
  )(seq(0, 1, length.out = max_lookback)) |>
    rev()

  gg <- gg +
    ggplot2::geom_ribbon(
      data = lookback_data,
      ggplot2::aes(ymin = pi_5, ymax = pi_95, fill = as.factor(lookback)),
      alpha = 0.4
    ) +
    ggplot2::scale_fill_manual(values = lookback_colors) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 2, nrow = 1, label.position = "bottom")
    ) +
    ggplot2::labs(fill = "Past forecasts (days prior)\n90% prediction interval:")
  gg
}

# Add model fit, projection and real data to projections plot
#' @param gg a ggplot
#' @param real_data tibble containing time series for observed metric of interest.
#'  Must have columns `date` and `target_value`
#' @param model_name string indicating model name
#' @param projection_colours named vector of colours
#' @param projection_text String of projection label

projections_fit <- function(
  gg,
  real_data,
  model_name,
  projection_colours,
  projection_text
) {
  if (grepl("ensemble", model_name)) {
    model_name <- "ensemble"
  }
  # model fit, projection and real data added to the plot
  gg <- gg +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_ribbon(ggplot2::aes(
      ymin = pi_5,
      ymax = pi_95,
      fill = is_projection,
      alpha = "90% interval"
    )) +
    ggplot2::geom_line(
      ggplot2::aes(y = pi_50, color = is_projection),
      linewidth = 0.75,
      lineend = "round"
    ) +
    ggplot2::geom_point(
      data = real_data,
      ggplot2::aes(x = date, y = target_value),
      size = 0.75,
      alpha = 0.4
    ) +
    ggplot2::scale_alpha_manual(
      breaks = c("95% interval"),
      values = c(0.4),
      guide = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = ggplot2::alpha(projection_colours, 0.4)
    ) +
    ggplot2::scale_color_manual(values = projection_colours) +
    ggplot2::labs(
      color = projection_text,
      fill = projection_text,
      subtitle = glue::glue(
        "{stringr::str_to_title(gsub('_', ' ', model_name))} model"
      )
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = 3, label.position = "right"),
      color = ggplot2::guide_legend(nrow = 3, label.position = "right"),
      alpha = ggplot2::guide_legend(nrow = 2, label.position = "right")
    )

  gg
}

#' Add multiple CIs to projections plot
#' @param gg a ggplot
#' @param plot_elements different parts of the projection plot that can be plotted
#' @param real_data tibble containing time series for observed metric of interest. Must have columns `date` and
#' `target_value`
projections_cis <- function(gg, plot_elements, real_data) {
  if (!"multiple_cis" %in% plot_elements) {
    return(gg)
  }
  gg <- gg +
    ggplot2::geom_point(data = real_data, ggplot2::aes(x = date, y = target_value), size = 0.75, alpha = 0.75) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = pi_25, ymax = pi_75, fill = is_projection, alpha = "50% interval")) +
    ggplot2::geom_point(data = real_data, ggplot2::aes(x = date, y = target_value), size = 0.75, alpha = 0.75) +
    ggplot2::scale_alpha_manual(breaks = c("90% interval", "50% interval"), values = c(0.4, 0.5)) +
    ggplot2::labs(alpha = "Prediction interval:")

  gg
}


#' Helper to generate data for lookbacks
#' @param data dataframe that need to be in the formatted model output and must include the columns:
#'  - prediction_start_date
#'  - model
#'  - location_level
#'  - date
#'  - target_value
#'  - pi_5
#'  - pi_95
#'  - pi_50
#'  @param model_name name of model to be plotted
#'  @param geography location level to be plotted
#'  @param age_granularity age breakdown to be plotted. Either "none" or anything
#'    the other valid options as defined by the forecast schema.
generate_lookback_data <- function(data, model_name, geography, age_granularity) {
  if (dplyr::n_distinct(stats::na.omit(data$prediction_start_date)) == 1) {
    cli::cli_abort("You are trying to plot lookbacks with only one lookback in the data")
  }

  lookback_data <- data |>
    dplyr::filter(
      model == model_name & location_level == geography & age_group_granularity == age_granularity
    ) |>
    dplyr::mutate(
      lookback = as.integer(max(prediction_start_date, na.rm = TRUE) - prediction_start_date),
      is_projection = ifelse(date >= prediction_start_date, TRUE, FALSE)
    ) |>
    dplyr::filter(
      is_projection,
      lookback > 0
    )

  if (grepl("region", geography, ignore.case = TRUE)) {
    lookback_data <- peaks$peak_cleaning(
      lookback_data, # standardising region names
      region_col = "location",
      maintain_colnames = TRUE
    )
  }

  lookback_data
}


#' Turn a list of regional tibble summaries into a single tibble
#'  @param plot_list a list of tibbles. Each list must have columns `geography`, `plot`, `region`
#'  @param chosen_geography geographic breakdown of the lists
#'  @param chosen_disease disease chosen to model, specified as a string
#'
#'  @export
tabulate_plot_list <- function(plot_list, chosen_geography, chosen_disease) {
  plot_list |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      "{{chosen_geography}}" := purrr::map(plot, \(p) unique(p$data$location)) # nolint: object_name_linter.
    ) |>
    tidyr::unnest({{ chosen_geography }}) |>
    ## match plot location to the row index so that the correct facet (by row number) can be grabbed later
    ## rowwise ensures logicals of length 1 in the which()
    ## more robust solution than adding a row number
    dplyr::rowwise() |>
    dplyr::mutate(
      ## facets are ordered by the levels of the (factor version) of the variable
      plot_id = which({{ chosen_geography }} == levels(as.factor(plot$data$location)))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      plot = purrr::map2(
        plot,
        plot_id,
        \(p, row_id) {
          p |>
            defacet(location, row_id) +
            ggplot2::labs(title = glue::glue("{chosen_disease} 14-day admissions forecast"), subtitle = NULL)
        }
      )
    ) |>
    dplyr::select(-dplyr::any_of(c("plot_id", "name", "plots_include")))
}


# Faceting helpers ----

#' Recover a single plot from a faceted ggplot
#'
#' @param faceted_gg a faceted `gg` object
#' @param facet_variable variable the faceted ggplot is faceted by, specified as a symbol
#' @param plot_index integter. Which plot do you want to recover?
#' @return `gg`
#'
#' @examples
#'
#' plt <- mtcars |>
#'   ggplot2::ggplot(ggplot2::aes(x = mpg, y = cyl)) +
#'   ggplot2::geom_point() +
#'   ggplot2::facet_wrap(ggplot2::vars(carb))
#' defacet(plt, carb, 1)
defacet <- function(faceted_gg, facet_variable, plot_index) {
  faceted_gg +
    ggforce::facet_wrap_paginate(
      ggplot2::vars({{ facet_variable }}),
      nrow = 1,
      ncol = 1,
      page = plot_index
    ) +
    ggplot2::theme(legend.position = "bottom")
}


#' Add an "England" facet banner to a plot
facet_national <- function(gg, geography, disease, age_granularity = "none") {
  if (
    geography != "nation" ||
      (tolower(disease) == "rsv" && age_granularity != "none")
  ) {
    return(gg)
  }

  build <- ggplot2::ggplot_build(gg)

  n_panel <- length(build$layout$layout$PANEL)

  if (n_panel > 1) {
    cli::cli_abort(
      c(
        "Number of panels is greater than one.",
        "x" = "National geography plot can only have a single panel."
      )
    )
  }

  gg + ggplot2::facet_wrap(ggplot2::vars("England"), strip.position = "top")
}
