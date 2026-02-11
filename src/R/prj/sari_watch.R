#' @name sari_watch
#' @section Version: 0.0.1
#'
#' @title
#' Processing SARI-Watch sentinel surveillance data
#'
#' @description
#' Functions to help handling the SARI-Watch official statistics data for COVID-19
#' and seasonal respiratory data.
#'
#' @seealso
#' * [sari_watch$load_sari()]
#'
".__module__."

box::use(box / deps_, box / help_, box / ops[...], box / s3)

.on_load <- function(ns) {
  deps_$need(
    "dplyr",
    "stats",
    "readODS",
    "janitor",
    "rlang"
  )

  deps_$min_version("dplyr", "1.1.0")
}


#' Load in, clean & transform SARI-Watch data
#'
#' Helper function to handle data munging for a data set that should be updated yearly.
#'
#' @param disease Character value corresponding to the disease/pathogen of interest. Uses a lookup
#' table to find the corresponding data set. Currently only "influenza" supported.
#'
#' @returns Dataframe containing past seasons SARI-Watch data as well as summary statistics.
#'
#' @examples
#' sari_data <- sari_watch$load_sari(disease = "influenza") |>
#'   dplyr::filter(season == "2019")
#'
#' @export
load_sari <- function(disease = "influenza", season_exclude = "2024") {
  # currently only supporting influenza data.
  sheets <- list(
    "influenza" = "Figure_29", # SARI-Watch influenza admission rates
    "rsv" = "Figure_35" # SARI-Watch RSV admission rates
  )

  # obtained from:
  # https://www.gov.uk/government/statistics/national-flu-and-covid-19-surveillance-reports-2024-to-2025-season # nolint
  path <- "PATH REDACTED"

  # load in the relevant sheet from S3
  clean_data <- s3$read_using(s3_uri = path, fn = \(x) {
    readODS::read_ods(x, sheet = sheets[[disease]], skip = 3)
  }) |>
    janitor::clean_names() |>
    dplyr::select(-c("week_number")) |>
    # date munging
    dplyr::mutate(
      date = lubridate::dmy(date),
      # probably a nicer way to order..
      epiweek = factor(lubridate::epiweek(date), levels = c(40:52, 1:20), ordered = TRUE)
    ) |>
    dplyr::mutate(
      season = stringr::str_extract(season, pattern = "[0-9]{4}") |>
        factor(ordered = TRUE)
    ) |>
    # keep only main winter season, could be changed
    dplyr::filter(epiweek >= 40 | epiweek <= 20) |>
    # transform outcome
    dplyr::arrange(season, epiweek) |>
    # add in a smoothed rate to reduce noise
    dplyr::mutate(rate_smooth = zoo::rollmean(rate, k = 3, na.pad = TRUE, align = "center"), .by = c("season")) |>
    # smoothed rate will be missing at start and finish, fill
    tidyr::fill(rate_smooth, .direction = "downup") |>
    dplyr::mutate(
      rate_log = log(rate),
      rate_smooth_log = log(rate_smooth),
      growth_rate = 100 * (rate_log - dplyr::lag(rate_log)),
      growth_rate_smooth = 100 * (rate_smooth_log - dplyr::lag(rate_smooth_log)),
      .by = "season"
    ) |>
    # make our relative data more sensibly structured
    dplyr::mutate(
      growth_rate = dplyr::case_when(
        # NA when in the first time point
        is.na(growth_rate) ~ 0,
        # t_x = 2, t_x-1 = 0, 2-log(0) = Inf
        # in this case t_x > t_x-1, therefore the GR should be positive but we can fix it
        is.infinite(growth_rate) & growth_rate > 0 ~ 50,
        # t_x = 0, t_x-1 = 2, log(0) - 2 = -Inf
        # in this case t_x < t_x-1, so we will fix as a negative growth rate
        is.infinite(growth_rate) & growth_rate < 0 ~ -50,
        # NaN when t_1 & t_2 are 0, log(0) - log(0) = NaN
        is.nan(growth_rate) ~ 0,
        # assume double-quadrupling in a week is not reasonable
        growth_rate > 150 ~ 100,
        growth_rate < -150 ~ -100,
        .default = growth_rate
      )
    ) |>
    # make our relative data more sensibly structured
    # (doing the same for our smoothed data)
    dplyr::mutate(
      growth_rate_smooth = dplyr::case_when(
        # NA when in the first time point
        is.na(growth_rate_smooth) ~ 0,
        # t_x = 2, t_x-1 = 0, 2-log(0) = Inf
        # in this case t_x > t_x-1, therefore the GR should be positive but we can fix it
        is.infinite(growth_rate_smooth) & growth_rate_smooth > 0 ~ 50,
        # t_x = 0, t_x-1 = 2, log(0) - 2 = -Inf
        # in this case t_x < t_x-1, so we will fix as a negative growth rate
        is.infinite(growth_rate_smooth) & growth_rate_smooth < 0 ~ -50,
        # NaN when t_1 & t_2 are 0, log(0) - log(0) = NaN
        is.nan(growth_rate_smooth) ~ 0,
        # assume double-quadrupling in a week is not reasonable
        growth_rate_smooth > 150 ~ 100,
        growth_rate_smooth < -150 ~ -100,

        .default = growth_rate_smooth
      )
    ) |>
    dplyr::select(
      c("epiweek", "season", "rate", "rate_smooth", "growth_rate", "growth_rate_smooth")
    ) |>
    # we do not want the most reason seasons data
    dplyr::filter(season != season_exclude)

  # generate summary stats of the different seasons.
  # Defining a new column pair for each statistic then pivotting.
  summary_results <- clean_data |>
    dplyr::summarise(
      # median
      rate_median = stats::median(rate),
      growth_rate_median = stats::median(growth_rate),
      # median_smooth
      rate_median_smooth = stats::median(rate_smooth),
      growth_rate_median_smooth = stats::median(growth_rate_smooth),
      # mean
      rate_mean = mean(rate),
      growth_rate_mean = mean(growth_rate),
      # mean_smooth
      rate_mean_smooth = mean(rate_smooth),
      growth_rate_mean_smooth = mean(growth_rate_smooth),
      # lower
      rate_lower = min(rate),
      growth_rate_lower = min(growth_rate),
      # lower_smooth
      rate_lower_smooth = min(rate_smooth),
      growth_rate_lower_smooth = min(growth_rate_smooth),
      # upper
      rate_upper = max(rate),
      growth_rate_upper = max(growth_rate),
      # upper_smooth
      rate_upper_smooth = max(rate_smooth),
      growth_rate_upper_smooth = max(growth_rate_smooth),
      # no change
      rate_no_change = 0,
      growth_rate_no_change = 0,
      .by = c("epiweek")
    ) |>
    # the munging is a bit ugly but works
    tidyr::pivot_longer(cols = c(dplyr::starts_with("rate_"), dplyr::starts_with("growth_rate_"))) |>
    dplyr::mutate(
      metric = dplyr::if_else(
        stringr::str_detect(name, "growth"),
        "growth_rate",
        "rate"
      ),
      season = stringr::str_remove(name, "(rate_)|(growth_rate_)")
    ) |>
    dplyr::select(-name) |>
    tidyr::pivot_wider(values_from = value, names_from = metric)

  combined_data <- dplyr::bind_rows(
    clean_data,
    summary_results
  ) |>
    dplyr::select(c("epiweek", "season", "rate", "growth_rate"))

  return(combined_data)
}
