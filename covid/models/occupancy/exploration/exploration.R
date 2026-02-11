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


covid_metrics <- rs$REDACTED |>
  dplyr::select(
    date,
    trust_code,
    trust_name,
    nhs_region,
    admissions,
    occupancy
  ) |>
  dplyr::filter(!is.na(admissions) & !is.na(occupancy)) |>
  dplyr::collect()


national_covid <- covid_metrics |>
  dplyr::group_by(date) |>
  dplyr::summarise(admissions = sum(admissions), occupancy = sum(occupancy)) |>
  dplyr::arrange(date) |>
  dplyr::filter(date >= "2022-12-01") |>
  dplyr::mutate(t = seq_len(dplyr::n()))


national_covid |>
  ggplot2::ggplot() +
  geom_line(aes(x = date, y = admissions), col = "green") +
  geom_line(aes(x = date, y = occupancy))


con <- file("covid/models/occupancy2/discharge.stan")
cat(paste(readLines(con), collapse = "\n"))
close(con)

input_data <- list(
  N = nrow(national_covid),
  J = 28,
  x = national_covid$admissions,
  y = national_covid$occupancy
)


## ----robust-fit---------------------------------------------------------------
mod <- cmdstanr::cmdstan_model("covid/models/occupancy2/discharge.stan")

# we fix the degrees of freedom of the t-distribution
#  (low df = wider in the tails)
mod <- mod$sample(data = input_data, iter_warmup = 500, iter_sampling = 500, chains = 2, parallel_chains = 2)

mod$summary()
#' bayesplot::mcmc_trace(mod$draws())

draws <- mod$draws() |>
  posterior::as_draws_df()

preds <- draws |>
  dplyr::select(.draw, dplyr::starts_with("y_hat")) |>
  tidyr::pivot_longer(cols = dplyr::starts_with("y_hat")) |>
  dplyr::mutate(t = as.integer(stringr::str_extract(name, "\\d+"))) |>
  dplyr::group_by(t) |>
  dplyr::summarise(
    pi_50 = quantile(value, p = 0.5),
    pi_95 = quantile(value, p = 0.95),
    pi_5 = quantile(value, p = 0.05)
  ) |>
  dplyr::ungroup()

output <- national_covid |>
  dplyr::left_join(preds, by = "t")


output |>
  ggplot() +
  geom_line(aes(x = date, y = occupancy), col = "maroon") +
  geom_line(aes(x = date, y = admissions), col = "orange") +
  geom_line(aes(x = date, y = pi_50), col = "darkgreen") +
  geom_ribbon(
    aes(x = date, ymin = pi_5, ymax = pi_95),
    fill = "darkgreen",
    alpha = 0.3
  )


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
