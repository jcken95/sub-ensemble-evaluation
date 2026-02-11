#' @name narratives
#' @section Version: 0.1.2
#'
#' @title Narratives
#'
#' @description Functions for creating narratives.
#'
".__module__."

box::use(
  box / deps_,
  box / help_
)

.on_load <- function(ns) {
  deps_$need(
    "dplyr",
    "fs",
    "glue",
    "purrr",
    "stringr",
    "tidyr"
  )
}

#### External Functions ####

#' Narrative text output
#'
#' Create a .txt file that has current epidemic numbers and forecasting numbers
#' at the desired geographic level.
#'
#' @param data Dataframe that need to be in the formatted model output
#'  and must include the columns:
#'  - prediciton_start_date
#'  - model
#'  - location_level
#'  - age_group
#'  - date
#'  - target_value
#'  - pi_5
#'  - pi_90
#'  - pi_50
#'  Other columns may be required when adding other PIs
#' @param target_name String of metric that is being forecasted,
#' typically "admissions" or "occupancy"
#' @param geography  Location level to be summarised
#' @param age_granularity Age level to be summarised
#' @param model_name Name of model to be summarised
#' @param disease Disease being forecasted
#' @param output_path Character giving the filename prefix,
#' and optionally the parent folder location on the local system.
#' @param as_tibble Returns output of the function as a tibble.
#'  Defaults to `FALSE`
#'
#' @returns string of text output.
#' Side effect: a .txt file containing the current and forecast values
#' (and a 90% PI) of the target metric for the disease, model and
#' geographic level in question
#'
#' @examples
#' # Creating a narrative on current and forecast COVID-19 epidemic numbers at a
#' # national level
#'
#' narrative_txt_output(
#'   data = all_formatted,
#'   target_name = target_name,
#'   model_name = model_name,
#'   geography = "nation",
#'   disease = "COVID-19",
#'   output_path = output_path
#' )
#'
#' @export
narrative_txt_output <- function(
    data,
    target_name,
    model_name,
    geography,
    age_granularity = "none",
    disease,
    output_path,
    rounding_level = 0,
    is_percent = FALSE,
    includes_nowcast = FALSE,
    as_tibble = FALSE) {
  current <- data |>
    dplyr::filter(
      !is.na(target_value),
      prediction_start_date == max(prediction_start_date, na.rm = TRUE),
      model == model_name,
      location_level == geography,
      age_group_granularity == age_granularity
    ) |>
    dplyr::mutate(
      target_7av = round(
        zoo::rollmean(target_value, k = 7, align = "right", fill = NA),
        digits = rounding_level
      )
    ) |>
    dplyr::group_by(location, age_group) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::contains("pi_"),
        \(x) zoo::rollmean(x, k = 7, align = "right", fill = NA)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::contains("pi_"),
        \(x) round(x, rounding_level)
      ),
      .by = date
    ) |>
    dplyr::filter(date == prediction_start_date - 1) |>
    dplyr::select(date, location, target_7av, pi_50, pi_95, pi_5)


  projection <- data |>
    dplyr::filter(
      prediction_start_date == max(prediction_start_date, na.rm = TRUE),
      model == model_name,
      location_level == geography,
      age_group_granularity == age_granularity
    ) |>
    dplyr::group_by(location, age_group) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::contains("pi_"),
        \(x) zoo::rollmean(x, k = 7, align = "right", fill = NA)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::contains("pi_"),
        \(x) round(x, rounding_level)
      ),
      .by = date
    ) |>
    dplyr::select(date, location, age_group, pi_50, pi_5, pi_95) |>
    dplyr::filter(date == max(date))

  latest_date <- unique(current$date)
  latest_av <- current[, c("target_7av", "location", "pi_50", "pi_5", "pi_95")]

  target_name_tidy <- gsub("_", " ", target_name)

  if (isFALSE(includes_nowcast)) {
    rolling_average <- glue::glue(
      'As of {format(latest_date, "%d %B %Y")}, ',
      "the 7-day rolling average for {disease} {target_name_tidy} ",
      'is {unique(latest_av$target_7av)}{ifelse(is_percent,"%","")}.\n',
      "The forecasted 7-day rolling average up to the ",
      '{format(projection$date, "%d %B %Y")} is ',
      '{projection$pi_50}{ifelse(is_percent,"%","")} ',
      '(90% PI: {projection$pi_5}{ifelse(is_percent,"%","")} - ',
      '{projection$pi_95}{ifelse(is_percent,"%","")}).'
    )
  } else {
    rolling_average <- glue::glue(
      'As of {format(latest_date, "%d %B %Y")}, ',
      "the estimated 7-day rolling average for {disease} {target_name_tidy} ",
      'is {current$pi_50}{ifelse(is_percent,"%","")} ',
      '(90% PI: {current$pi_5}{ifelse(is_percent,"%","")} - ',
      '{current$pi_95}{ifelse(is_percent,"%","")}).\n',
      "The forecasted 7-day rolling average up to the ",
      '{format(projection$date, "%d %B %Y")} is ',
      '{projection$pi_50}{ifelse(is_percent,"%","")} ',
      '(90% PI: {projection$pi_5}{ifelse(is_percent,"%","")} - ',
      '{projection$pi_95}{ifelse(is_percent,"%","")}).'
    )
  }

  output_narrative <- glue::glue(
    "Summary: {geography} level ({model_name})\n{rolling_average}"
  )

  # Covert occupancy rate to number of beds:
  if (is_percent && geography == "nation" && grepl("occupancy", output_path)) {
    skip <- FALSE # set default for skipping beds projection
    if (!"population" %in% names(data)) {
      if ("total_beds" %in% names(data)) { # suitable replacement for hospitals
        data <- dplyr::rename(data, "total_beds" = "population")
      } else {
        skip <- TRUE # no usable column available
      }
    }

    if (!skip) {
      beds_base <- data |>
        dplyr::filter(
          date == latest_date,
          location_level == "nation",
          !is.na(population)
        ) |>
        dplyr::arrange(prediction_start_date) |>
        utils::tail(1) |> # pull latest prediction (in case total beds changed)
        dplyr::pull(population)
      skip <- is.na(beds_base) && beds_base < 1 # last check; is data available?
    }

    if (!skip) {
      projected_beds <- projection |>
        dplyr::mutate(
          dplyr::across(
            dplyr::starts_with("pi_"),
            ~ round(. / 100 * beds_base)
          )
        )

      bed_summary <- glue::glue(
        "This is approximately {round(latest_av$target_7av/100*beds_base)} ",
        "currently occupied beds, ",
        "with an equivalent forecast of {projected_beds$pi_50} beds ",
        "(90% PI: {projected_beds$pi_5} - {projected_beds$pi_95})."
      )

      output_narrative <- glue::glue("{output_narrative}\n{bed_summary}")
    } else { # skipping beds projection
      error_message <- noquote(paste(
        "No valid beds/population data found to base estimate on;\n",
        "Please supply data with a full 'population' or 'total_beds' column."
      ))

      output_narrative <- glue::glue("{output_narrative}\n\n{error_message}")
    }
  }

  save_narrative_text_file(
    narrative_text = output_narrative,
    output_path = fs::path(output_path, "narrative", geography),
    file_name = glue::glue("{model_name}_forecast_narrative.txt")
  )

  if (as_tibble) {
    locations <- data |>
      dplyr::filter(location_level == geography) |>
      dplyr::distinct(location) |>
      dplyr::pull(location)

    output_narrative <- tibble::tibble(
      location = locations,
      narrative = output_narrative
    )
  }

  output_narrative
}


#' Create narrative tables
#'
#' Create a dataframe giving required outputs for assessment.
#'
#' @param data Dataframe that need to be in the formatted model output and must include the columns:
#'  - prediction_start_date
#'  - model
#'  - location_level
#'  - age_group
#'  - date
#'  - target_value
#'  - pi_5
#'  - pi_90
#'  - pi_50
#' @param target_name String of metric that is being forecasted,
#' typically "admissions" or "occupancy"
#' @param location_levels  Location levels to summarised
#' @param age_granularity Age level to be summarised
#' @param model_name Name of model to be summarised (usually the ensemble)
#' @param disease Disease being forecasted
#' @param location_levels String(s) of areas to include;
#'  e.g. "nation" or "region"
#'  @param age_granularity String of age grouping level to use, default: "none"
#'  @param output_path Optional string of location to save out a .csv of table
#'
#'
#' @returns Dataframe of relevant locations and groups for making assessments,
#' giving probabilities, rolling averages and targets
#'
#' @examples
#' # Creating a table of summarised forecasts and probabilities for influenza
#' # bed occupancy
#'
#' formatted_output_table <- narratives$create_narrative_tables(
#'   data = occupancy_formatted_summary,
#'   target_name = overall_params$target_name,
#'   model_name = "ensemble_conversion",
#'   disease = overall_params$disease,
#'   location_levels = c("nation", "region"),
#'   age_granularity = "none",
#'   output_path = output_path # timestamped folder to save results into
#' )
#'
#' @export
create_narrative_tables <- function(
    data,
    target_name,
    model_name,
    disease,
    location_levels = c("nation", "region"),
    age_granularity = "none",
    output_path = NULL) {
  output_table <- data |>
    dplyr::filter(model == model_name) |>
    dplyr::mutate(h = 1 + date - prediction_start_date) |> # calculate a horizon
    dplyr::filter( # select required spatio-age groups
      location_level %in% location_levels,
      age_group_granularity %in% age_granularity
    ) |>
    # only care about the most recent forecast:
    dplyr::slice_max(order_by = prediction_start_date) |>
    dplyr::arrange(date) |>
    dplyr::mutate( # calculate averages for ease of comparison
      dplyr::across(
        c("target_value", dplyr::starts_with("pi_")),
        \(x) zoo::rollmean(x, k = 7, na.pad = TRUE, align = "right"),
        .names = "{.col}_avg7"
      ), # generate names based on the column name
      .by = c(
        "location_level", "location",
        "age_group", "age_group_granularity"
      )
    ) |>
    # keep only the day before forecast, first week, and final week:
    dplyr::filter(h %in% c(0, 7, 14)) |>
    dplyr::mutate(week = as.integer(h) / 7) |>
    dplyr::select(
      date, week, location_level, location,
      age_group_granularity, age_group, target_value_avg7,
      dplyr::starts_with("p_"), dplyr::ends_with("_avg7")
    ) |>
    dplyr::mutate( # cleaning for ease of interpretation
      dplyr::across(dplyr::starts_with("p"), \(x) round(x, 3)),
      target_value_avg7 = round(target_value_avg7, 1),
      age_group = forcats::fct_reorder( # factoring ages in numerical order
        age_group,
        age_group |>
          stringr::str_extract("\\d+") |> # "none" or "All Ages" => ""
          as.integer() |> # "" => NA
          tidyr::replace_na(9999)
      )
    ) |> # puts NA (i.e. non-numeric groups) last.
    dplyr::arrange(
      location_level, location, age_group_granularity, age_group, date
    ) |>
    dplyr::mutate( # add in identifiers:
      disease = disease,
      metric = target_name,
      # remove the p_ we do not care about:
      p_increase = dplyr::if_else(week == 0, NA_real_, p_increase),
      p_stable = dplyr::if_else(week == 0, NA_real_, p_stable),
      p_decrease = dplyr::if_else(week == 0, NA_real_, p_decrease)
    )

  if (!is.null(output_path)) {
    output_path <- fs::dir_create(fs::path(output_path, "narrative"))

    readr::write_csv(
      output_table,
      fs::path(
        output_path,
        paste0(
          "output_table_", disease, "_", target_name,
          ifelse(
            disease == "rsv" | age_granularity != "none",
            paste0("_", age_granularity),
            ""
          ), ".csv"
        )
      )
    )
  }

  output_table
}


#' map narrative text output
#'
#' Create a .txt file that has lists the number of ICB areas in each forecast
#' classification and the classification with the majority of ICB areas.
#'
#' @param data Dataframe that need to be in the formatted model output
#' and must include the columns:
#'  - prediciton_start_date
#'  - model
#'  - location_level
#'  - age_group
#'  - date
#'  - target_value
#'  - pi_5
#'  - pi_90
#'  - pi_50
#'  Other columns may be required when adding other PIs
#' @param age_granularity Age level to be plotted
#' @param model_name Name of model to be plotted
#' @param disease Disease being forecasted
#' @param output_path Character giving the filename prefix,
#' and optionally the parent folder location on the local system.
#'
#' @returns string of text output. Side effect:  a .txt file that lists the
#'  number of ICB areas in each forecast classification and the classification
#'  with the majority of ICB areas.
#'
#' @examples
#' # Creating a narrative on forecasted COVID-19 forecast at an icb level
#'
#' icb_map_narrative_txt_output(
#'   data = all_formatted,
#'   model_name = model_name,
#'   disease = "COVID-19",
#'   output_path = output_path
#' )
#'
#' @export
icb_map_narrative_txt_output <- function(
    data,
    model_name,
    age_granularity = "none",
    disease,
    output_path) {
  # Filter data according to model specifications
  current <- data |>
    dplyr::filter(
      !is.na(target_value),
      !is.na(prediction_start_date),
      prediction_start_date == max(prediction_start_date),
      model == model_name,
      location_level == "icb",
      age_group_granularity == age_granularity,
      date == prediction_start_date - 1
    ) |>
    dplyr::select(date, location)

  # Count number of ICB areas for each projection classification:
  # increase, decrease, and stable.
  projection <- data |>
    dplyr::filter(
      prediction_start_date == max(prediction_start_date, na.rm = TRUE),
      model == model_name,
      location_level == "icb"
    ) |>
    dplyr::mutate(is_projection = ifelse(
      date >= prediction_start_date, "Projection", "Model fit"
    )) |>
    dplyr::filter(
      is_projection == "Projection",
      date == max(date)
    ) |>
    dplyr::select(
      date, prediction_start_date, location,
      p_increase, p_decrease, p_stable
    ) |>
    tidyr::pivot_longer(
      cols = c("p_increase", "p_decrease", "p_stable"),
      names_to = "projection", names_prefix = "p_",
      values_to = "probability"
    ) |>
    dplyr::group_by(date, prediction_start_date, location) |>
    dplyr::filter(probability == max(probability)) |>
    dplyr::mutate(
      projection = factor( # for count() to keep 0s for absent projections.
        projection,
        levels = c("increase", "decrease", "stable")
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::count(projection, name = "location_count", .drop = FALSE)

  latest_date <- unique(current$date)

  overall_trend <- projection |>
    dplyr::filter(location_count == max(location_count, na.rm = TRUE)) |>
    dplyr::pull(projection) |>
    unique()


  overall_trend <- if (length(overall_trend) == 1) {
    overall_trend
  } else { # Catch the possibility of a split trend.
    paste(# Add note and replace final ',' with ', and' (handles 3-way tie):
      "split evenly between",
      sub(",([^,]*)$", ", and\\1", paste(overall_trend, collapse = ", ")))
  }


  narrative <- glue::glue(
    "As of {format(latest_date, '%d %B %Y')}, there are\n   - ",
    projection[projection$projection == "increase", "location_count"][[1]],
    " ICB areas forecast to increase over the next 14 days\n   - ",
    projection[projection$projection == "stable", "location_count"][[1]],
    " ICB areas forecast to be stable over the next 14 days\n   - ",
    projection[projection$projection == "decrease", "location_count"][[1]],
    " ICB areas forecast to decrease over the next 14 days\n",
    "\nOverall, the most likely trend forecast for ICB areas ",
    "over the next 14 days is {overall_trend}."
  )


  save_narrative_text_file(
    narrative_text = narrative,
    output_path = fs::path(output_path, "narrative", "icb"),
    file_name = glue::glue("{model_name}_map_narrative.txt")
  )


  narrative
}



#' Length of stay narrative text output
#'
#' Create a .txt file that gives length of stay summary statistics of the los
#' at the desired geographic level.
#'
#' @param data Dataframe of samples
#' @param geography  Location level: "nation" or "region"
#' @param disease Disease being modelled
#' @param output_path Character giving the filename prefix,
#' and optionally the parent folder location on the local system.
#'
#' @returns a .txt file giving length of stay median and PI at the desired
#' geographic level.
#'
#' @examples
#' # Creating a narrative for national length of stay
#'
#' los_narrative_txt_output(
#'   data = los_national_samples,
#'   geography = "nation",
#'   disease = "COVID-19",
#'   output_path = output_path
#' )
#'
#' @export
los_narrative_txt_output <- function(
    data,
    geography,
    disease,
    output_path) {
  geography <- match.arg(arg = geography, choices = c("nation", "region"))
  # N.B. This function does not currently handle ICB level.

  median <- data |>
    dplyr::group_by(nhs_region_name, .draw) |>
    dplyr::filter(cd > 0.5) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::group_by(dplyr::across( # catch for regional version
      if (geography == "region") "nhs_region_name"
    )) |>
    dplyr::summarise(
      pi_50 = round(stats::quantile(t, 0.50), digits = 1),
      pi_05 = round(stats::quantile(t, 0.05), digits = 1),
      pi_95 = round(stats::quantile(t, 0.95), digits = 1)
    ) |>
    dplyr::ungroup() # only needed for region

  if (geography == "region") {
    median <- median |>
      dplyr::mutate(
        regional_narative = paste0(
          nhs_region_name, ": ", pi_50,
          " days (90% PI: ", pi_05, " - ", pi_95, ")"
        )
      ) |>
      dplyr::arrange(nhs_region_name)

    narrative <- glue::glue(
      "Regionally, the estimated median length of stay in hospital for ",
      "{disease} is (90% PI for median):\n",
      paste(median$regional_narative, collapse = "\n")
    )
  } else { # For national:
    narrative <- glue::glue(
      "Nationally, the estimated median length of stay in hospital for ",
      "{disease} is {median$pi_50} days (90% PI for median: ",
      "{median$pi_05} - {median$pi_95})."
    )
  }

  save_narrative_text_file(
    narrative_text = narrative,
    output_path = fs::path(output_path, "narrative", "los"),
    file_name = glue::glue("los_{geography}al_narrative.txt")
  )

  narrative
}



#### Internal Functions ####
# TODO: a function to calculate likeliness and confidence scores too? #1465

#' Narrative text file saver
#'
#' @param narrative_text String of text to write into a file.
#' @param output_path String of pathway to folder to save to;
#'  will build if that folder doesn't already exist.
#' @param file_name String of the desired file name;
#'  preferably with .txt extension, but this will add/replace if necessary.
#'
#' @keywords internal

save_narrative_text_file <- function(
    narrative_text,
    output_path,
    file_name) {
  # Check output path contains a `narrative` folder:
  if (!grepl("narrative", output_path, ignore.case = TRUE)) {
    output_path <- fs::path(output_path, "narrative")
  }
  # check file name has the right file format:
  if (!stringr::str_ends(file_name, ".txt")) {
    if (grepl(".", file_name)) { # detect and drop any incorrect extensions
      file_name <- stringr::str_extract(file_name, ".+?(?=\\.)")
    }
    file_name <- paste0(file_name, ".txt")
  }

  output_path <- fs::dir_create(output_path) # builds a folder to save to.

  writeLines(
    text = narrative_text,
    con = fs::path(output_path, file_name)
  )
}
