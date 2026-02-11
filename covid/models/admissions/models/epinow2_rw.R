#' @name epinow2_rw
#' @section Version: 0.0.1
#'
#' @title
#' COVID-19 admissions EpiNow2 Random Walk model
#'
#' @description
#' EpiNow2 Random Walk model for COVID-19 admissions forecasting.
#' Model is fit at the ICB level, trend is estimated independently.
#' Model is on a natural count scale, with a population susceptibility effect.
#'
#' @details
#' # Development
#' Model first developed spring 2025 with the aim of including a semi-mechanistic
#' approach to forecasting in a generative Bayesian manner. The EpiNow2 package
#' is a mature and well maintained package used for this purpose.
#'
#' # Key assumptions
#' - There are observations of admissions, and latent infections that can be calculated.
#' - Using estimates of disease parameters, infections can be inferred from observed admissions.
#' These include a range of time delays and scaling factors.
#' - Using the latent infections the time varying reproduction can be estimated (Rt)
#' - Rt varies according to some trend approach, in this case a RW
#' - the most recent estimate of Rt is projected forward, and from that infections are
#' converted to future admissions.
#'
#' The process we have implimented is (this is not strictly true as things are forward
#' generated not backward):
#' - We take a Rt varying over time as a weekly RW
#' - From this Rt we get daily infections each day.
#' - From the daily infections we add a time delay of infection to hospitalisation, which is
#' a combination of the incubation period and symptom onset to hospitalisation delay.
#' - The infections are then scaled down to admissions by the Infection Hospitalisation Risk (IHR).
#' - Admissions are observed as a poisson distribution with day-of-week effects.
#' - Future admissions are estimated by holding Rt constant,
#' then calculating future infections then admissions.
#'
#' There are a range of options for how we encode these epi parameters.
#' They can be incorporated as a fixed value, a distribution, and a distribution with parameter uncertainty.
#'
#' # Epi Parameter sources
#'
#' - Incubation period:
#' <https://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-023-03070-8>
#' BA.5 (3.81 days, 95% CI: 2.01–5.61 days)
#' - Onset to hospitalisation:
#' Using subject matter expertise and a wide sd
#' - Generation time:
#' <https://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-023-03070-8>
#' 2.96 days (95% CI: 2.54–3.38 days)
#' - Infection hospitalisation risk:
#' <https://www.gov.uk/government/statistics/winter-coronavirus-covid-19-infection-study-estimates-of-epidemiological-characteristics-england-and-scotland-2023-to-2024/winter-coronavirus-covid-19-infection-study-estimates-of-infection-hospitalisation-and-fatality-risk-30-may-2024#:~:text=Infection%20hospitalisation%20risk,-In%20England%2C%20between&text=This%20corresponds%20to%20a%201,those%20aged%2075%20and%20over> # nolint
#' ~0.05 as scaled to all admissions. Currently no associated uncertainty.
#'
#' The population is obtained from our standard trust catchment method, with
#' a scaling factor applied
#' \eqn{\text{susceptible\_population} = \text{susceptible\_scale} \times \text{population}}
#'
#' This scaling factor is challenging to parameterise. We have used the assumption
#' of on average 2 SARS-CoV-2 infections per year, corresponding to a scale factor of 0.5.
#'
#' # Other documentation
#'
#' * Package docs: <https://epiforecasts.io/EpiNow2/dev/articles/EpiNow2.html>
#' * Supporting paper: <https://wellcomeopenresearch.org/articles/5-112>
#'
#' @seealso
#' * [epinow2_rw$run_epinow2_rw()]
#'


box::use(
  box / deps_,
  box / help_
)


.on_load <- function(ns) {
  deps_$need(
    "dplyr",
    "glue",
    "lubridate",
    "EpiNow2",
    "stats",
    "tidyr"
  )
}



#' Run EpiNow2 random walk model
#'
#' @param .data Data used to train the model.
#' @param forecast_horizon Size of forecasting window, i.e. number of days of
#'   forecast values to obtain, starting with `prediction_date`.
#' @param n_pi_samples Number of replications to be simulated.
#' @param prediction_date Date of first forecast value to produce.
#' @param output_variables A vector of column names to keep in the output data
#'   frame.
#' @param hyperparams A named list of model hyperparameters.
#'
#' @returns A list with two elements: `sample_predictions` is a data frame
#'   containing model prediction samples, and `model` is a vector of
#'   the model object from EpiNow2.
#'
#' @export
run_epinow2_rw <- function(
    .data,
    forecast_horizon = 14,
    n_pi_samples = 500,
    prediction_date,
    output_variables,
    hyperparams) {
  box::use(
    prj / intervals
  )

  prediction_start_date <- as.Date(prediction_date) # maintain informative name

  train_data <- .data |>
    # naming convention for package columns
    dplyr::rename(
      confirm = target,
      # package requires use of "region", though we have an ICB
      region = icb_name) |>
    # create appropriate temporal split
    dplyr::filter(
      date < prediction_start_date,
      date >= prediction_start_date - hyperparams$training_length
    )

  # use `cases` for simplified data to align with package naming convention
  cases <- train_data |>
    # model fails with unused columns
    dplyr::select(
      -c("nhs_region_name", "population")
    )


  # an average of the population by location
  imputed_population <- 60e6 / 42


  generation_time <- EpiNow2::LogNormal(mean = hyperparams$generation_time__mean,
    sd = hyperparams$generation_time__sd,
    max = 14)
  incubation_period <- EpiNow2::LogNormal(mean = hyperparams$incubation_period__mean,
    sd = hyperparams$incubation_period__sd,
    max = 14)
  onset_to_hospitalisation <- EpiNow2::LogNormal(mean = hyperparams$onset_to_hospitalisation__mean,
    sd = hyperparams$onset_to_hospitalisation__sd,
    max = 14)

  inf_to_hosp <- incubation_period + onset_to_hospitalisation
  inf_hosp_risk <- EpiNow2::Fixed(hyperparams$ihr)

  rt_prior <- EpiNow2::LogNormal(mean = hyperparams$initial_rt__mean,
    sd = hyperparams$initial_rt__sd)

  # produce a standard Rt option for all locations.
  rt_raw <- EpiNow2::rt_opts(
    prior = rt_prior,
    # take the period to be a week due to daily data
    rw = 7,
    # uses out of sample approach which may change in next version.
    pop = imputed_population,
    future = "latest"
  )


  # create a unique and complete population lookup table
  population_table <- train_data |>
    dplyr::select(region, population) |>
    # fill in any missing.
    # apply susceptible factor to reduce immune population
    dplyr::mutate(susceptible_population = dplyr::coalesce(
      round(population * hyperparams$susceptible_scale),
      round(imputed_population * hyperparams$susceptible_scale)),
    .keep = "unused") |>
    dplyr::distinct() |>
    dplyr::summarise(susceptible_population = mean(susceptible_population),
      .by = "region")

  population_list <- stats::setNames(as.list(population_table$susceptible_population),
    population_table$region)

  # this generates a separate set of rt_opts per location, which we can
  # then update with the correct population by location.
  rt <- EpiNow2::opts_list(
    rt_raw,
    cases
  )

  # fix the population value for each region
  for (location in names(population_list)) {
    rt[[location]]["pop"] <- population_list[[location]]
  }


  model <- EpiNow2::regional_epinow(
    data = cases,
    generation_time = EpiNow2::gt_opts(generation_time),
    delays = EpiNow2::delay_opts(inf_to_hosp),
    rt = rt,
    gp = NULL,
    obs = EpiNow2::obs_opts(
      family = "negbin",
      week_effect = TRUE,
      scale = inf_hosp_risk,
    ),
    forecast = EpiNow2::forecast_opts(
      horizon = forecast_horizon
    ),
    stan = EpiNow2::stan_opts(
      method = "sampling",
      control = list(
        adapt_delta = 0.99,
        max_treedepth = 12),
      warmup = hyperparams$n_warmup,
      samples = n_pi_samples,
      cores = 2,
      chains = 2,
      seed = 5678,
      backend = "cmdstanr"
    ),
    # turn logs into print only to avoid writing output in parallel.
    logs = EpiNow2::setup_logging(
      threshold = "INFO",
      file = NULL,
      mirror_to_console = TRUE,
      name = "EpiNow2"
    ),
    verbose = FALSE
  )

  # create a spine of location and sample to join onto
  # as some locations are excluded due to low count we lose a key.
  spine <- tidyr::expand_grid(
    icb_name = unique(train_data$region),
    date = seq(min(train_data$date),
      max(train_data$date) + forecast_horizon,
      by = "day"),
    .sample = seq(1, n_pi_samples)
  ) |>
    dplyr::left_join(train_data, by = c(
      "icb_name" = "region", "date"
    ))


  output_data_samples <-
    # get at the model data. We care about reported cases,
    # but there are many other things estimated
    purrr::pluck(model, "regional") |>
    purrr::imap(.f = \(x, y) {
      # for each ICB extract the samples
      x$estimates$samples |>
        # process the data to reduce size
        dplyr::mutate(icb_name = y) |>
        dplyr::filter(variable == "reported_cases") |>
        dplyr::select(
          "icb_name", "date", "sample", "value"
        )
    }
    ) |>
    dplyr::bind_rows() |>
    dplyr::as_tibble() |>
    dplyr::full_join(
      spine,
      by = c("icb_name", "date", "sample" = ".sample")
    ) |>
    dplyr::group_by(icb_name) |>
    dplyr::arrange(date) |>
    tidyr::fill(population, nhs_region_name, .direction = "down") |>
    # doing this impute for the ICBs dropped for having too
    # many zeros.
    dplyr::mutate(value = dplyr::coalesce(value, 0)) |>
    dplyr::ungroup() |>
    dplyr::rename(
      .sample = sample,
      .value = value,
      target = confirm
    ) |>
    dplyr::mutate(
      model = "epinow2_rw",
      prediction_start_date = prediction_start_date
    ) |>
    dplyr::select(dplyr::any_of(output_variables)) |>
    # perform thinning of the model fit only (not future predictions)
    # keep some recent data for trend assessment
    dplyr::filter(date >= prediction_start_date - (forecast_horizon + 1) | .sample %% hyperparams$fit_thinning == 0)



  return(
    list(
      sample_predictions = output_data_samples,
      model = model
    )
  )
}
