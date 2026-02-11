# Extract a ymd date from a string
#' @param str a string
#' @return date in ymd format (as a string)
str_extract_date <- function(str) {
  stringr::str_extract(str, "\\d{4}-\\d{2}-\\d{2}")
}

#' Round a date to the cloest corresponding Wednesday (forecasting day).
#' Approach is to round to the previous Sunday to identify the forecasting week,
#' then push foreward to Wednesday

#' @param upload_date forecast upload date specified as string in ymd format
#'
round_to_wednesday <- function(upload_date) {
  last_sunday <- upload_date |>
    lubridate::ymd() |>
    lubridate::floor_date(unit = "week")
  # bump forward to wednesday
  last_sunday + lubridate::days(3)
}


#' Function to extract names of trusts that were excluded in real-time.
#' @param path Path to where the yaml file storing trust exclusions are stored.
#' Note: last updated 26/02/2024
load_trust_exclusion <- function(
  path = here::here("evaluation/post-season-evaluation/trust_exclusion.yaml")
) {
  trusts <- yaml::read_yaml(path)

  trusts
}


#' Scoring-ready forecasts
#' @param forecast_with_truth interval based forecasts (quantiles) with true, historic, value attached
#' @param disease disease of interest. See `user_check$disease_checker()` for allowed values
#' @param metric disease metric. Must be one of "admissions" or "occupancy"
#' @param chosen_location_level location level for forecast breakdown, defaults to "national"
prepare_forecasts <- function(
  forecast_with_truth,
  disease,
  metric = "admissions",
  chosen_location_level = "national"
) {
  box::use(prj / user_check[disease_checker])

  disease <- disease_checker(disease)

  metric <- rlang::arg_match(metric, values = c("admissions", "occupancy"))

  chosen_metric <- rlang::sym(glue::glue("{disease}_{metric}"))

  forecast_with_truth |>
    dplyr::filter(
      location_level == "nation",
      stringr::str_detect(model, "ensemble"),
      date > prediction_start_date
    ) |>
    dplyr::select(
      upload_date,
      prediction_start_date,
      date,
      !!chosen_metric,
      dplyr::starts_with("pi_")
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("pi_"),
      values_to = "predicted",
      names_to = "quantile_level"
    ) |>
    dplyr::mutate(
      quantile_level = stringr::str_remove(quantile_level, "pi_"),
      quantile_level = as.numeric(quantile_level) / 100
    ) |>
    tidyr::drop_na() |>
    dplyr::rename(observed = !!chosen_metric)
}

#' Aggregate the number of admissions by a chosen geography
#'
#' @param admissions tibble or data frame with admissions
#' @param chosen_geography chosen geography specified as a string or symbol
sum_admissions_by_geography <- function(admissions, chosen_geography) {
  chosen_geography <- rlang::arg_match(chosen_geography, c("nation", "nhs_region_name", "icb_name"))
  geography_sym <- rlang::sym(chosen_geography)
  # string manipulation outside of below mutate to make SQL-friendly
  location_level_string <- chosen_geography |>
    stringr::str_extract(("icb|region")) |>
    dplyr::coalesce("nation")

  admissions |>
    dplyr::summarise(
      covid_admissions = sum(covid_admissions, na.rm = TRUE),
      influenza_admissions = sum(influenza_admissions, na.rm = TRUE),
      rsv_admissions = sum(rsv_admissions, na.rm = TRUE),
      # only need to use `by` for sub-national geographies
      .by = c(date, age_group, if (chosen_geography != "nation") chosen_geography)
    ) |>
    dplyr::mutate(
      location_level = location_level_string,
      "nation" = "England"
    ) |>
    dplyr::mutate(location = !!rlang::sym(geography_sym), .keep = "unused")
}


#' Aggregate occupancy by a chosen geography
#'
#' Assumes data pre-seperated by disease
#'
#' @param admissions tibble or data frame with admissions
#' @param chosen_geography chosen geography specified as a string or symbol
sum_occupancy_by_geography <- function(admissions, chosen_geography) {
  chosen_geography <- rlang::arg_match(chosen_geography, c("nation", "nhs_region_name", "icb_name"))
  geography_sym <- rlang::sym(chosen_geography)
  # string manipulation outside of below mutate to make SQL-friendly
  location_level_string <- chosen_geography |>
    stringr::str_extract(("icb|region")) |>
    dplyr::coalesce("nation")

  admissions |>
    dplyr::summarise(
      occupancy = sum(occupancy, na.rm = TRUE),
      total_beds = sum(total_beds, na.rm = TRUE),
      # only need to use `by` for sub-national geographies
      .by = c(date, disease, if (chosen_geography != "nation") chosen_geography)
    ) |>
    dplyr::mutate(
      location_level = location_level_string,
      "nation" = "England"
    ) |>
    dplyr::mutate(location = !!rlang::sym(chosen_geography), .keep = "unused")
}


#' Small helper to seperate disease and metric out from a single stringt
#' @param input_data tibble or data frame with `name` column
seperate_disease_metric <- function(input_data) {
  dplyr::mutate(
    input_data,
    disease = stringr::str_extract(name, "(covid|influenza|rsv|norovirus)"),
    metric = stringr::str_extract(name, "(admissions|occupancy|cases)"),
    .keep = "unused"
  )
}

#' Load the MRF structures for RSV
load_rsv_mrf <- function() {
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

#' Load age and trust populations for RSV
load_rsv_trust_age_population <- function() {
  box::use(box / redshift)
  rs <- redshift$data_model()
  rs$REDACTED |>
    dplyr::collect()
}

#' Load all data for evaluation
#'
#' @param summary (redshift) table of model summaries
#' @param observed observed covid, flu and RSV data. Norovirus is loaded in seperately
#' @param lookups data used to look up NHS trust information, etc.
#' @return tibble of forecast summaries with observed (true) data

load_evaluation_summary <- function(summary, observed, lookups) {
  excluded_trusts <- load_trust_exclusion()

  noro_data_path <- "PATH REDACTED"

  box::use(box / redshift)

  rs <- redshift$data_model("REDACTED")

  norovirus_observed <- rs$REDACTED |>
    dplyr::filter(date > "2024-01-01") |>
    dplyr::rename(observed_target = target) |>
    dplyr::mutate(
      # manually add in metadata
      age_group = "all",
      location_level = "nation",
      nation = "England",
      location = "England",
      disease = "norovirus",
      metric = "cases",
      .keep = "unused"
    ) |>
    # Note: we are not applying trust exclusion because cases do not necessarily
    # have associated trusts.
    dplyr::summarise(
      observed_target = sum(observed_target, na.rm = TRUE),
      .by = c(date, age_group, location_level, nation, location, disease, metric)
    ) |>
    dplyr::collect()

  # Calculate admissions ----

  ## rsv ----

  mrf_structures <- load_rsv_mrf()
  trust_age_population <- load_rsv_trust_age_population() |>
    dplyr::mutate(
      age_group = dplyr::case_when(
        age == 0 ~ "[0, 1)",
        age == "1" ~ "[1, 2)",
        age < 5 ~ "[2, 5)",
        age < 18 ~ "[5, 18)",
        age < 65 ~ "[18, 65)",
        age < 75 ~ "[65, 75)",
        age < 85 ~ "[75, 85)",
        age < 120 ~ "[85, 120)",
        .default = NA_character_
      ),

      .keep = "unused"
    ) |>
    dplyr::summarise(population = sum(population, na.rm = TRUE), .by = c(trust_code, age_group))

  rsv_admissions <- observed |>
    dplyr::select(
      date,
      trust_code,
      # we don't want all RSV columns, only those corresponding to admissions
      dplyr::starts_with("rsv_n")
    ) |>
    # apply rsv filter exclusions immediately
    dplyr::filter(!(trust_code %in% excluded_trusts$rsv$filter)) |>
    dplyr::rename_with(\(.) sub("^rsv_n", "n", .)) |>
    dplyr::arrange(date, trust_code) |>
    dplyr::collect() |>
    dplyr::left_join(
      lookups$nhs_trusts |>
        dplyr::select(code, name, nhser24nm) |>
        dplyr::rename(
          "trust_code" = code,
          "trust_name" = name,
          "nhs_region_name" = nhser24nm
        ) |>
        dplyr::collect(),
      by = c("trust_code")
    ) |>
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
    dplyr::left_join(
      trust_age_population,
      by = c("trust_code", "age_group")
    ) |>
    dplyr::mutate(rsv_admissions = dplyr::coalesce(count, 0), .keep = "unused") |>
    # adding in the trust exclusion late only once the data are formatted correctly
    dplyr::mutate(rsv_admissions = dplyr::if_else(trust_code %in% excluded_trusts$rsv$zero, 0, rsv_admissions)) |>
    dplyr::mutate(
      # aggregate to reported age groups for the 24/25 season
      age_group = dplyr::case_when(
        age_group %in% c("[0,1)", "[1,2)") ~ "[0,2)",
        age_group %in% c("[75,85)", "[85,120)") ~ "[75,120)",
        .default = age_group
      )
    )

  rsv_admissions <- rsv_admissions |>
    dplyr::summarise(
      rsv_admissions = sum(rsv_admissions, na.rm = TRUE),
      population = sum(population),
      age_group = "all",
      .by = c(date, trust_code, nhs_region_name)
    ) |>
    dplyr::bind_rows(rsv_admissions)

  ## covid and flu ----

  flu_admissions <- observed |>
    dplyr::select(
      date,
      influenza_admissions,
      trust_code,
      icb_name,
      nhs_region_name,
      population
    ) |>
    # apply flu exclusions
    dplyr::filter(!(trust_code %in% excluded_trusts$influenza$filter)) |>
    dplyr::mutate(
      influenza_admissions = dplyr::if_else(trust_code %in% excluded_trusts$influenza$zero, 0, influenza_admissions)
    ) |>
    dplyr::mutate(age_group = "all") |>
    dplyr::collect()

  covid_admissions <- observed |>
    dplyr::select(
      date,
      covid_admissions,
      trust_code,
      icb_name,
      nhs_region_name,
      population
    ) |>
    # apply covid exclusions
    # currently no exclusions for covid
    dplyr::filter(!(trust_code %in% excluded_trusts$covid$filter)) |>
    dplyr::mutate(covid_admissions = dplyr::if_else(trust_code %in% excluded_trusts$covid$zero, 0, covid_admissions)) |>
    dplyr::collect() |>
    dplyr::mutate(age_group = "all")

  admissions <- dplyr::bind_rows(
    flu_admissions,
    covid_admissions
  ) |>
    dplyr::bind_rows(rsv_admissions)

  admissions_by_geography <- purrr::map(
    c("icb_name", "nhs_region_name", "nation"),
    \(geo) sum_admissions_by_geography(admissions, geo)
  ) |>
    dplyr::bind_rows() |>
    tidyr::pivot_longer(
      cols = c("covid_admissions", "influenza_admissions", "rsv_admissions"),
      values_to = "observed_target"
    ) |>
    seperate_disease_metric()

  # Calculate occupancy ----

  occupancy <- observed |>
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
      covid_occupancy,
      influenza_occupancy
    ) |>
    # Ensure consistent row order (otherwise random row order from database
    # may lead to unnecessary rerunning of downstream targets!)
    dplyr::arrange(date, trust_code) |>
    dplyr::collect() |>
    tidyr::pivot_longer(
      cols = c("covid_occupancy", "influenza_occupancy"),
      names_to = "name",
      values_to = "occupancy"
    ) |>
    seperate_disease_metric() |>
    # apply flu exclusions
    dplyr::filter(!(disease == "influenza" & trust_code %in% excluded_trusts$influenza$filter)) |>
    dplyr::mutate(
      occupancy = dplyr::if_else(
        disease == "influenza" &
          trust_code %in% excluded_trusts$influenza$zero,
        0,
        occupancy
      )
    ) |>
    dplyr::select(!trust_code)

  occupancy_by_geography <- purrr::map(
    c("icb_name", "nhs_region_name", "nation"),
    \(geo) sum_occupancy_by_geography(occupancy, geo)
  ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      occupancy_rate = 100 * occupancy / total_beds,
      metric = "occupancy_rate",
      age_group = "all"
    ) |>
    dplyr::select(
      date,
      location_level,
      location,
      "observed_target" = occupancy_rate,
      disease,
      metric,
      age_group
    )

  # Combine all metrics ----

  observed_by_geography <- dplyr::bind_rows(
    admissions_by_geography,
    occupancy_by_geography,
    norovirus_observed
  ) |>
    dplyr::select(-nation)

  observed_by_geography
}

#' score quantile forecasts
#' @param scoring_ready data.frame (or similar) with predicted and observed values.
#' #' See [scoringutils::as_forecast_quantile()] for further details
#' @param forecast_unit Name of the columns in data (after any renaming of columns) that denote the unit of a single
#' forecast. See [scoringutils::get_forecast_unit()] for details.
#' @return object of type `scores`
score_quantiles <- function(scoring_ready, forecast_unit) {
  divide <- function(x, y) {
    x / y
  }

  quantile_forecast <- scoringutils::as_forecast_quantile(scoring_ready, forecast_unit = forecast_unit)
  # "population" must be in the `forecast_unit` for this to work!
  quantile_forecast |>
    scoringutils::transform_forecasts(fun = divide, label = "per_capita", y = quantile_forecast$population) |>
    # occupancy rate is already a normalised metric, so will remove
    dplyr::filter(!(metric == "occupancy_rate" & scale == "per_capita")) |>
    scoringutils::transform_forecasts(fun = \(x) log(1 + x), label = "log_1p") |>
    scoringutils::score()
}

#' plot a scoring metric for many models over time
#' @param scored_forecasts scored forecasts; object of type `scores`, for example
#' @param score name of score to plot, specified as a symbol
#' @param transform option function to transform the score; defaults to `identity` (i.e. no transformation)
#' @return `gg`
#' @examples
#' forecasts_scored |>
#'   dplyr::filter(location_level == "nation", metric == "admissions") |>
#'   scoringutils::summarise_scores(by = c("model", "prediction_start_date", "disease", "metric")) |>
#'   plot_score_over_time(wis, log) +
#'   ggplot2::facet_grid(~disease)
plot_score_over_time <- function(scored_forecasts, score, transform = identity) {
  scored_forecasts <- fill_holiday_break(scored_forecasts)

  reported_forecasts <- scored_forecasts |>
    dplyr::filter(is_reported_model) |>
    dplyr::mutate(model = "reported_model")

  scored_forecasts <- dplyr::filter(scored_forecasts, !grepl("ensemble", model))

  models <- c("reported_model", unique(scored_forecasts$model))
  model_colours <- c("black", RColorBrewer::brewer.pal(length(models) - 1, "Paired"))
  names(model_colours) <- models

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = reported_forecasts,
      ggplot2::aes(x = prediction_start_date, y = transform({{ score }}), colour = "reported_model"),
      linewidth = 2
    ) +
    ggplot2::geom_line(
      data = scored_forecasts,
      ggplot2::aes(
        x = prediction_start_date,
        y = transform({{ score }}),
        groups = model,
        colour = model
      )
    ) +
    ggplot2::scale_colour_manual(values = model_colours) +
    ggplot2::scale_linetype()
}

#' Small helper to store dates of holiday break
#' @param date_name name of new column specified as a string
#' @return tibble
holiday_break <- function(date_name = "prediction_start_date") {
  tibble::tibble(
    "{date_name}" := seq(from = as.Date("2024-12-19"), to = as.Date("2025-01-05"), by = "day") # nolint: object_name_linter.
  )
}

#' Take scored forecasts and fill with NAs during the holiday break
#' @param scored_forecasts forecasts scored with `scoringutils::score()`
#' @return scored forecasts with empty entries for the holiday period
fill_holiday_break <- function(scored_forecasts) {
  # need to obtain the forecast unit from the data, and fill gaps based on this unit
  # this will be all of the columns, apart form the prediction start date and the scoring metrics

  implied_unit <- colnames(scored_forecasts)
  implied_unit <- implied_unit[
    !implied_unit %in% c("prediction_start_date", scoringutils::get_metrics(scored_forecasts))
  ]

  holiday_period <- holiday_break() |>
    tidyr::expand_grid(dplyr::distinct(scored_forecasts, !!!rlang::syms(implied_unit))) |>
    tidyr::drop_na()

  empty_forecasts <- scored_forecasts |>
    dplyr::select(prediction_start_date, dplyr::all_of(implied_unit)) |>
    dplyr::distinct() |>
    dplyr::bind_rows(holiday_period) |>
    dplyr::arrange(prediction_start_date) |>
    dplyr::filter(disease != "norovirus")

  dplyr::right_join(scored_forecasts, empty_forecasts, by = c("prediction_start_date", implied_unit))
}

#' Order regions
#' small helper
#' @param input_data data frame with `location` column
order_regions <- function(input_data) {
  nhs_regions <- c(
    "London",
    "South West",
    "Midlands",
    "South East",
    "East of England",
    "North West",
    "North East and Yorkshire"
  )

  # Pop england at front so it's on the left in facet plots; order otherwise arbitrary
  dplyr::mutate(input_data, location = ordered(location, c("England", nhs_regions)))
}

#' Classify trend direction
#'
#' @param value observed value of a metric
#' @param lagged_value a previous value of a metric to compare to
#' @param threshold a scalar value indicating at which point the relative difference between a value and it's
#' lagged value can be considered to be not-stable. Defaults to 0.2 (20%), as used in winter 24/25.
#'
#' @return string describing trend categorisation
classify_trend <- function(value, lagged_value, threshold = 0.2) {
  threshold <- abs(threshold)

  relative_metric_change <- (value / lagged_value) - 1

  trend <- dplyr::case_when(
    value == lagged_value ~ "stable", # useful when lagged is zero
    relative_metric_change > threshold ~ "increase",
    relative_metric_change < -threshold ~ "decrease",
    abs(relative_metric_change) <= threshold ~ "stable",
    .default = NA_character_
  )

  trend
}

#' Find most likely trned
#' Take trend probabilities and output the most likely trend
#' @param input_data dataframe with columns `p_{increase, decrease, stable}`
#' @return data frame with additional `trend` and `trend_probability` columns
most_likely_trend <- function(input_data) {
  input_data |>
    dplyr::mutate(
      trend_probability = max(c(p_increase, p_decrease, p_stable)),
      trend = dplyr::case_when(
        max(trend_probability) == p_stable ~ "stable",
        max(trend_probability) == p_increase ~ "increase",
        max(trend_probability) == p_decrease ~ "decrease",
        .default = NA
      ),
      trend = ordered(trend, levels = c("decrease", "stable", "increase"))
    )
}

#' Identify if models were used in ensembles.
#' small helper to tidy main script.
#' @param summary_data data in quantile format.
identify_ensemble_inclusion <- function(summary_data) {
  summary_data |>
    # note: the ensemble is only defined where there are multi-models,
    # rather than a "single" model ensemble, such as early RSV and noro.
    dplyr::mutate(
      # lets exclude the "conversion" ensemble from the definition
      # ensemble_conversion is the name of the occupancy model
      "is_ensemble" = model %LIKE% "%ensemble%" & !(model %LIKE% "%conversion%"),
      "ensemble_name" = dplyr::if_else(is_ensemble, model, NA)
    ) |>
    dplyr::mutate(
      # as there's only one non-NA value, the max can apply across models
      ensemble_name = max(ensemble_name, na.rm = TRUE),
      .by = c(prediction_start_date, location, age_group, target_name, disease, metric, model_date)
    ) |>
    dplyr::mutate(
      "model_in_ensemble" = dplyr::case_when(
        is_ensemble ~ "ensemble",
        is.na(ensemble_name) ~ NA,
        ensemble_name %LIKE% paste0("%", model, "%") ~ "included",
        .default = "excluded"
      )
    ) |>
    dplyr::select(-is_ensemble, -ensemble_name)
}

#' Estimate quantile based ensembles and include them.
#' @param scoring_ready_data Data in `scoringutils` ready format.
#' @param forecasting_unit set of columns defining the unit of a forecast
#' We exploit the long data format to produce ensembles as summary statistics of
#' those models that were used in the the original ensemble.
add_quantile_ensembles <- function(scoring_ready_data, forecasting_unit) {
  ensemble_unit <- c(
    forecasting_unit[!forecasting_unit == "model"],
    "quantile_level",
    "observed"
  )

  # mean average of each quantile for included models
  ensemble_mean <- scoring_ready_data |>
    dplyr::filter(model_in_ensemble == "included") |>
    dplyr::summarise(
      predicted = mean(predicted),
      p_increase = mean(p_increase),
      p_stable = mean(p_stable),
      p_decrease = mean(p_decrease),
      .by = dplyr::all_of(ensemble_unit)
    ) |>
    # note: need to normalise probabilities after averaging
    # as probabilities may not sum to 1
    dplyr::mutate(
      normalise_factor = (1 / (p_increase + p_stable + p_decrease)),
      p_increase = p_increase * normalise_factor,
      p_stable = p_stable * normalise_factor,
      p_decrease = p_decrease * normalise_factor,
      .by = dplyr::all_of(ensemble_unit)
    ) |>
    dplyr::select(-normalise_factor) |>
    dplyr::mutate(model = "ensemble_mean")

  # median average of all models included
  ensemble_median <- scoring_ready_data |>
    dplyr::filter(model_in_ensemble == "included") |>
    dplyr::summarise(
      predicted = median(predicted),
      p_increase = median(p_increase),
      p_stable = median(p_stable),
      p_decrease = median(p_decrease),
      .by = dplyr::all_of(ensemble_unit)
    ) |>
    # note: need to normalise probabilities after averaging
    # as probabilities may not sum to 1
    dplyr::mutate(
      normalise_factor = (1 / (p_increase + p_stable + p_decrease)),
      p_increase = p_increase * normalise_factor,
      p_stable = p_stable * normalise_factor,
      p_decrease = p_decrease * normalise_factor,
      .by = dplyr::all_of(ensemble_unit)
    ) |>
    dplyr::select(-normalise_factor) |>
    dplyr::mutate(model = "ensemble_median")

  dplyr::bind_rows(
    scoring_ready_data,
    ensemble_mean,
    ensemble_median
  )
}

#' Generate set of Leave One Model Out (LOMO) ensembles using a quantile averaging method
#' @param scoring_ready_data Data in `scoringutils` ready format.
#' @param forecasting_unit vector of column names defining the unit of a forecast
#' LOMO ensemble sets are generated from string matching and joining
#' of the set of models used each week.
#' This generates the LOMO ensembles and adds them onto the scoring ready data.
#' This only works for times where an ensemble of 3 or model models were used.
generate_lomo_models <- function(scoring_ready_data, forecasting_unit) {
  # for our LOMO analysis we will stick to

  # create a unit set of models per forecast date, metric and disease
  unique_ensemble_models <- scoring_ready_data |>
    dplyr::select(dplyr::all_of(forecasting_unit), model_in_ensemble) |>
    dplyr::filter(
      prediction_start_date == date,
      location_level == "nation",
      age_group_granularity == "none",
      model_in_ensemble %in% c("included")
    ) |>
    dplyr::select("prediction_start_date", "disease", "metric", "model") |>
    dplyr::distinct() |>
    dplyr::mutate(model = stringr::str_trim(model))

  # concatenate the unique set of models into single rows for intersection checks
  unique_ensemble_models_short <- unique_ensemble_models |>
    dplyr::summarise(
      ensemble_components = paste(unique(model), collapse = " "),
      n_models = dplyr::n_distinct(model),
      .by = c("prediction_start_date", "disease", "metric")
    )

  # we are going to remove one model (leave one model out) of each ensemble set
  lomo_data <- unique_ensemble_models |>
    dplyr::rename(model_excluded = model) |>
    dplyr::left_join(unique_ensemble_models_short, by = c("prediction_start_date", "disease", "metric")) |>
    # doing string matching as nested vectors got unnecessarily complex and slow.
    dplyr::mutate(
      lomo_ensemble_components = ensemble_components |>
        stringr::str_remove(model_excluded) |>
        stringr::str_squish()
    ) |>
    # DECISION: only doing this analysis for when we have an ensemble of 3 or more
    dplyr::filter(n_models >= 3)

  # we need to define what we are going to group by for the summary stat ensemble
  ensemble_unit <- c(
    forecasting_unit[!forecasting_unit == "model"],
    "quantile_level",
    "observed",
    "model_excluded"
  )

  # with our set of "lomo ensemble models" defined, bring in the forecasted data to
  # then calculate the new ensembles from. Initially just a mean ensemble.
  lomo_ensemble <- lomo_data |>
    # with the unique set of models per lomo ensemble we now need to bring in each actual
    # prediction per model. To do this we will have multiple predictions per LOMO data
    # (hence: many-to-many). Once we have all combinations, we can filter down to only
    # the LOMO models.
    dplyr::left_join(
      scoring_ready_data,
      by = c("prediction_start_date", "disease", "metric"),
      relationship = "many-to-many"
    ) |>
    # keep only models that are specified in the LOMO ensemble name.
    dplyr::filter(stringr::str_detect(lomo_ensemble_components, model)) |>
    # create the ensemble on the subset of LOMO models
    dplyr::summarise(
      predicted = mean(predicted),
      p_increase = mean(p_increase),
      p_stable = mean(p_stable),
      p_decrease = mean(p_decrease),
      .by = dplyr::any_of(ensemble_unit)
    ) |>
    # clear name as LOMO
    dplyr::mutate(model = paste0("ensemble_lomo_", model_excluded), .keep = "unused") |>
    # bind on the original data so we have everything together
    dplyr::bind_rows(
      scoring_ready |>
        dplyr::filter(model == "ensemble_mean")
    ) |>
    # we only want to keep the ensemble_mean where we actually have existing
    # LOMO models (can't score nothing against the baseline)
    dplyr::filter(!all(model == "ensemble_mean"), .by = c("disease", "prediction_start_date", "location_level"))

  lomo_ensemble
}

#' Plot a Pareto front
#'
#' Assumes we want to plot two utilities called `.mean` and `.sd`
#'
#' @param .data data frame of all `.mean` and `.sd` values
#' @param .front the pareto fron of `.mean` and `.sd` values
#' @param .disease name of disease for front to be plotted (for example, `covid-19` or `influenza`)
#' @param .scale which scale is the front on? For example, `natural` or `log`
plot_front <- function(.data, .front, .disease, .scale) {
  plot_title <- glue::glue("disease={.disease} | scale={.scale}")

  front <- .front |>
    dplyr::filter(disease == .disease, scale == .scale)

  .data |>
    dplyr::filter(disease == .disease, scale == .scale) |>
    tidyr::unnest(summary) |>
    dplyr::mutate(is_front = dplyr::if_else(model %in% front$model, "PO", "NPO")) |>
    ggplot2::ggplot(ggplot2::aes(x = log(.mean), y = log(.sd))) +
    ggplot2::geom_point() +
    ggplot2::geom_line(data = front, colour = "red") +
    ggrepel::geom_text_repel(ggplot2::aes(label = model, colour = is_front), force = 10) +
    ggplot2::scale_colour_manual(values = c("PO" = "red", "NPO" = "black")) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::theme(legend.position = "none")
}


#' Plot a Pareto front for distinct scores
#'
#' Assumes we want to plot two positive utilities called `rps` and `wis`
#'
#' @param .data data frame of all `rps` and `wis`
#' @param .front the pareto fronr of `rps` and `wis` values
#' @param .disease name of disease for front to be plotted (for example, `covid-19` or `influenza`)
plot_score_front <- function(.data, .front, .disease) {
  plot_title <- glue::glue("disease = {.disease}")

  front <- .front |>
    dplyr::filter(disease == .disease)

  .data |>
    dplyr::filter(disease == .disease) |>
    tidyr::unnest(summary) |>
    dplyr::mutate(is_front = dplyr::if_else(model %in% front$model, "PO", "NPO")) |>
    ggplot2::ggplot(ggplot2::aes(x = wis_per_100k, y = rps)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(data = front, colour = "red") +
    ggrepel::geom_text_repel(ggplot2::aes(label = model, colour = is_front), force = 10) +
    ggplot2::scale_colour_manual(values = c("PO" = "red", "NPO" = "black")) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::theme(legend.position = "none")
}

#' Plot a Pareto front for distinct scores - publication quality version
#'
#' Assumes we want to plot two positive utilities called `rps` and `wis`
#'
#' @param .data data frame of all `rps` and `wis`
#' @param .front the pareto fronr of `rps` and `wis` values
#' @param .disease name of disease for front to be plotted (for example, `covid-19` or `influenza`)
plot_score_front_publication <- function(.data, .front, .disease) {
  front <- .front |>
    dplyr::filter(disease == .disease)

  .data |>
    dplyr::filter(disease == .disease) |>
    tidyr::unnest(summary) |>
    dplyr::mutate(is_front = dplyr::if_else(model %in% front$model, "PO", "NPO")) |>
    ggplot2::ggplot(ggplot2::aes(x = wis_per_100k, y = rps)) +
    ggplot2::geom_point(ggplot2::aes(colour = is_front, fill = is_front)) +
    ggplot2::geom_line(data = front, colour = projection_plots$select_ukhsa_colour("teal")) +
    ggrepel::geom_text_repel(ggplot2::aes(label = model, colour = is_front), force = 10) +
    ggplot2::scale_colour_manual(values = c("PO" = projection_plots$select_ukhsa_colour("teal"), "NPO" = "black")) +
    ggplot2::scale_fill_manual(values = c("PO" = projection_plots$select_ukhsa_colour("teal"), "NPO" = "black")) +
    projection_plots$theme_pancasts() +
    ggplot2::theme(legend.position = "none")
}


# Function to plot predictions for UKHSA report.
plot_report_forecasts <- function(x, title_str) {
  x |>
    ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        x = date,
        ymin = pi_5,
        ymax = pi_95,
        group = prediction_start_date,
        fill = as.factor(prediction_start_date),
        alpha = "90%"
      ),
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        x = date,
        ymin = pi_25,
        ymax = pi_75,
        group = prediction_start_date,
        fill = as.factor(prediction_start_date),
        alpha = "50%"
      ),
    ) +
    ggplot2::geom_line(
      ggplot2::aes(x = date, y = pi_50, group = prediction_start_date),
      linetype = 1,
      alpha = 0.6
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = date, y = observed_target),
      size = 0.1
    ) +
    ggplot2::scale_alpha_manual("Prediction intervals", values = c("90%" = 0.3, "50%" = 0.6)) +
    ggplot2::facet_wrap(disease ~ metric, scale = "free_y", ncol = 3) +
    ggplot2::scale_x_date(breaks = scales::date_breaks(width = "2 weeks"), labels = scales::label_date_short(), ) +
    guides(fill = "none") +
    labs(
      y = "target value",

      subtitle = title_str
    ) +
    coord_cartesian(ylim = c(0, NA))
}

#'  generate samples of a model fit
#'  a reimplementation of `intervals$generate_samples()` to the fitted model
generate_samples_fitted <- function(.data, .model, .n_pi_samples = 500, method = c("gaussian", "mh"), ...) {
  method <- match.arg(method)

  results <- gratia::fitted_samples(
    seed = 1,
    model = .model,
    data = .data,
    n = .n_pi_samples,
    method = method,
    ...
  ) |>
    dplyr::rename(
      .sample = .draw,
      .value = .fitted
    )

  formatted_results <- .data |>
    # generate the row number in the raw data
    dplyr::mutate(row = seq_len(dplyr::n())) |>
    dplyr::left_join(results, by = c("row" = ".row")) |>
    dplyr::select(-row)

  return(formatted_results)
}


model_code_to_name <- tibble::tribble(
  ~name                , ~full_name         ,
  "ets"                , "ETS"              ,
  "gam_cr"             , "Cubic Regression" ,
  "gr_mean"            , "Mean GR"          ,
  "gam_gp"             , "Gaussian process" ,
  "gam_rw"             , "Random Walk"      ,
  "gr_gp"              , "GP growth rate"   ,
  "historic_gr_median" , "Median GR"
)
