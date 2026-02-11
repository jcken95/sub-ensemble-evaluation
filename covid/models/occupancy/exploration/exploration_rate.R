# Script to run COVID occupancy stan model (version 1)

#### DESCRIPTION ####
#
# Admissions to occupancy conversion model
#
#

# ENVIRONMENT SET UP
wd <- system("echo $(git rev-parse --show-toplevel)", intern = TRUE)
setwd(wd)
# install all required packages for the modelling
source(paste0(wd, "/covid/models/src/depends.R"))


# SET GLOBAL SEED for reproducibility
set.seed(8675309)


#' options(dplyr.summarise.inform = FALSE)
#' Sys.setenv("R_BOX_PATH" = fs::path(
#'   rprojroot::find_root(rprojroot::is_git_root), "src", "R"))

box::use(
  box / redshift,
  box / s3
)

library(bayesplot)


rs <- redshift$data_model()

total_adult_beds <- rs$REDACTED |>
  dplyr::filter(
    metric_name == "Total adult G&A beds open" |
      metric_name == "Total paediatric G&A beds open"
  ) |>
  dplyr::rename(total_beds = metric_value) |>
  dplyr::group_by(date, trust_code) |>
  dplyr::summarise(total_beds = sum(total_beds, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::select(date, trust_code, total_beds) |>
  dplyr::collect()


covid_metrics <- rs$REDACTED |>
  dplyr::select(
    date,
    trust_code,
    trust_name,
    nhs_region,
    admissions,
    occupancy
  ) |>
  dplyr::collect() |>
  dplyr::left_join(total_adult_beds, by = c("trust_code", "date")) |>
  dplyr::filter(!is.na(admissions) & !is.na(occupancy) & !is.na(total_beds))


national_covid <- covid_metrics |>
  # dplyr::filter(nhs_region == "Midlands"| nhs_region == "London") |>
  dplyr::group_by(date, nhs_region) |>
  dplyr::summarise(
    admissions = sum(admissions),
    occupancy = sum(occupancy),
    total_beds = sum(total_beds)
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(nhs_region) |>
  dplyr::arrange(date) |>
  dplyr::mutate(t = as.integer(date - min(date))) |>
  dplyr::ungroup() |>
  dplyr::filter(date >= "2022-12-01") |>
  dplyr::mutate(admissions_rate = admissions / total_beds, occupancy_rate = occupancy / total_beds) |>
  dplyr::mutate(r = as.numeric(as.factor(nhs_region)))

region_lookup <- national_covid |>
  dplyr::select(r, nhs_region) |>
  dplyr::distinct()


national_covid |>
  ggplot2::ggplot() +
  geom_line(aes(x = date, y = admissions), col = "green") +
  geom_line(aes(x = date, y = occupancy)) +
  facet_wrap(~nhs_region)


con <- file("covid/models/occupancy2/discharge_rate.stan")
cat(paste(readLines(con), collapse = "\n"))
close(con)

input_data <- list(
  N = nrow(national_covid),
  `T` = max(national_covid$t), # can we get a less reserved name?
  R = length(unique(national_covid$r)),
  J = 28,
  day = national_covid$t,
  region = national_covid$r,
  x = national_covid$admissions_rate,
  y = national_covid$occupancy_rate
)


## ----robust-fit---------------------------------------------------------------
mod <- cmdstanr::cmdstan_model("covid/models/occupancy2/discharge_rate.stan")

# we fix the degrees of freedom of the t-distribution
# (low df = wider in the tails)
mod <- mod$sample(
  data = input_data,
  iter_warmup = 500,
  iter_sampling = 500,
  chains = 2,
  parallel_chains = 2
)

mod$summary()
# bayesplot::mcmc_trace(mod$draws()) # nolint: commented_code_linter

draws <- mod$draws() |>
  posterior::as_draws_df()

preds <- draws |>
  dplyr::select(.draw, dplyr::starts_with("y_hat")) |>
  tidyr::pivot_longer(cols = dplyr::starts_with("y_hat")) |>
  dplyr::mutate(r = stringr::str_extract(name, "\\d+")) |>
  dplyr::mutate(
    t = stringr::str_remove(
      stringr::str_extract(name, ",\\d+"),
      ","
    )
  ) |>
  dplyr::mutate(t = as.integer(t), r = as.numeric(r)) |>
  dplyr::group_by(t, r) |>
  dplyr::summarise(
    pi_50 = quantile(value, p = 0.5, na.rm = TRUE),
    pi_95 = quantile(value, p = 0.95, na.rm = TRUE),
    pi_5 = quantile(value, p = 0.05, na.rm = TRUE)
  ) |>
  dplyr::ungroup()

output <- national_covid |>
  dplyr::left_join(preds, by = c("t", "r"))


output |>
  ggplot() +
  geom_line(aes(x = date, y = occupancy_rate), col = "maroon") +
  geom_line(aes(x = date, y = admissions_rate), col = "orange") +
  geom_line(aes(x = date, y = pi_50), col = "darkgreen") +
  geom_ribbon(
    aes(x = date, ymin = pi_5, ymax = pi_95),
    fill = "darkgreen",
    alpha = 0.3
  ) +
  facet_wrap(~nhs_region)


params <- draws |>
  dplyr::select(.draw, lognormal_mu, lognormal_sigma)

los <- data.frame(t = seq(0, input_data$J, 0.1)) |>
  dplyr::cross_join(params) |>
  dplyr::mutate(p = dlnorm(t, lognormal_mu, lognormal_sigma)) |>
  dplyr::mutate(pd = plnorm(t, lognormal_mu, lognormal_sigma)) |>
  dplyr::group_by(t) |>
  dplyr::summarise(
    p_50 = quantile(p, 0.5),
    p_5 = quantile(p, 0.05),
    p_95 = quantile(p, 0.95),
    c_50 = quantile(pd, 0.5),
    c_5 = quantile(pd, 0.05),
    c_95 = quantile(pd, 0.95)
  ) |>
  dplyr::ungroup()


los |>
  ggplot() +
  geom_line(aes(x = t, y = p_50)) +
  geom_ribbon(
    aes(x = t, ymin = p_5, ymax = p_95),
    alpha = 0.4,
    fill = "steelblue"
  ) +
  ylab("p discharge") +
  xlab("days since admission")


los |>
  ggplot() +
  geom_line(aes(x = t, y = c_50)) +
  geom_ribbon(
    aes(x = t, ymin = c_5, ymax = c_95),
    alpha = 0.4,
    fill = "darkgreen"
  ) +
  ylim(0, 1) +
  ylab("cumulative p discharge") +
  xlab("days since admission")
