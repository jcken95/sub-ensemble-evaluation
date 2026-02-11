# code to bring in, clean and do basic analysis of WCIS delta data for scoring.

box::use(box / s3, box / redshift, prj / projection_plots)

library(ggplot2)
library(feasts)

ggplot2::theme_set(projection_plots$theme_pancasts())

rs <- redshift$data_model("REDACTED")

valid_trusts <- rs$REDACTED |>
  dplyr::filter(isTRUE(is_active), type == "acute") |>
  dplyr::collect()

# obtained from: https://www.england.nhs.uk/statistics/statistical-work-areas/uec-sitrep/urgent-and-emergency-care-daily-situation-reports-2023-24/ # nolint: line_length_linter
# defintions found: https://www.england.nhs.uk/long-read/process-and-definitions-for-the-daily-situation-report-web-form/ # nolint: line_length_linter
uec_2324_path <- "PATH REDACTED"
# due to the annoying structure we need to extract dates separately, then join them on.
dates <- s3$read_using(s3_uri = uec_2324_path, fn = \(x) {
  readxl::read_xlsx(x, sheet = "Total G&A Core Esc beds", skip = 12, n_max = 1)
}) |>
  tidyr::pivot_longer(cols = dplyr::everything(), values_to = "date") |>
  tidyr::drop_na() |>
  dplyr::arrange(date) |>
  dplyr::mutate(date_order = dplyr::row_number(), date = as.Date(date)) |>
  dplyr::select(-name)

clean_data <- s3$read_using(s3_uri = uec_2324_path, fn = \(x) {
  readxl::read_xlsx(x, sheet = "Total G&A Core Esc beds", skip = 14)
}) |>
  dplyr::select(-c(`...2`)) |>
  janitor::clean_names() |>
  dplyr::filter(!is.na(code)) |>
  # there is a painful time based ordering to the dates as columns.
  # Lets first elongate to work with values not columns.
  tidyr::pivot_longer(cols = dplyr::starts_with("total"), names_to = "metric", values_to = "value") |>
  # the ordering of the columns suffixes can be used as orderings to join the dates onto.
  # we need to extract these numbers ([0-9]+ regex). They do not start at 1 (so need -4)
  # they are over each metric so if we are grouping (we need / 2 and ceiling)
  dplyr::mutate(
    column_number = as.integer(stringr::str_extract(metric, "[0-9]+")),
    column_number = ceiling(0.5 * (column_number - 4)),
    metric = stringr::str_remove(metric, "_[0-9]+")
  ) |>
  # bring in the actual dates on the basis of column order
  dplyr::left_join(dates, by = c("column_number" = "date_order")) |>
  dplyr::select(-column_number) |>
  dplyr::filter(code %in% valid_trusts$code)

clean_data

combined_beds <- clean_data |>
  dplyr::summarise(value = sum(value), .by = -c("metric", "value")) |>
  dplyr::mutate(metric = "total_beds")

processed_data <- clean_data |>
  dplyr::bind_rows(combined_beds) |>
  dplyr::arrange(date) |>
  dplyr::mutate(daily_diff = value - dplyr::lag(value), .by = c("nhs_england_region", "code", "name", "metric")) |>
  dplyr::left_join(
    rs$REDACTED |>
      dplyr::collect(),
    by = c("code" = "trust_code")
  ) |>
  # remove first day as no differenced data
  dplyr::filter(date != min(date))

# explore the overall trends
processed_data |>
  ggplot() +
  geom_line(aes(x = date, y = value, group = name), alpha = 0.25) +
  facet_wrap(~metric, scale = "free_y")

# explore the daily diffs
processed_data |>
  ggplot() +
  geom_line(aes(x = date, y = daily_diff, group = name), alpha = 0.1) +
  facet_wrap(~metric, scale = "free_y")

# seems like 14% of trusts only report zero escalation beds... suspicious
processed_data |>
  dplyr::summarise(all_zero = sum(all(value == 0)), .by = c("metric", "name")) |>
  dplyr::summarise(prop_zero_trusts = sum(all_zero) / dplyr::n(), .by = "metric")

# which trusts were all zero?
processed_data |>
  dplyr::summarise(all_zero = all(value == 0), .by = c("metric", "name")) |>
  dplyr::filter(all_zero)
# looks like a mix between specialist acute trusts (childrens, cancer, orthopedic)
# and some larger trusts such as MFT, UCL, Sheffield Teaching.
# In the first case these trusts are small and have limited impact on pathogen reporting.
# In the second case these are likely outliers. E.g. MFT MUST has more than zero escalation beds.

processed_data <- processed_data |>
  dplyr::mutate(all_zero = all(value == 0), .by = c("metric", "name", "code")) |>
  dplyr::mutate(value_pc = 1e5 * value / population, daily_diff_pc = 1e5 * daily_diff / population) |>
  # we are only removing the escalation beds, does this make sense or should
  # we remove those trusts from all metrics?
  dplyr::filter(!all_zero) |>
  tsibble::as_tsibble(key = c("code", "metric", "nhs_england_region"), index = "date")

processed_data |>
  ggplot() +
  geom_line(aes(x = date, y = value, group = name), alpha = 0.1) +
  facet_wrap(~metric, scale = "free_y")

processed_data |>
  ggplot() +
  geom_line(aes(x = date, y = value_pc, group = name), alpha = 0.1) +
  facet_wrap(~metric, scale = "free_y")

processed_data |>
  ggplot() +
  geom_line(aes(x = date, y = daily_diff, group = name), alpha = 0.1) +
  facet_wrap(~metric, scale = "free_y")

processed_data |>
  ggplot() +
  geom_line(aes(x = date, y = daily_diff_pc, group = name), alpha = 0.1) +
  facet_wrap(~metric, scale = "free_y")

processed_data |>
  ggplot() +
  tidybayes::stat_histinterval(aes(x = daily_diff_pc), .width = c(0.5, 0.66, 0.9, 0.95)) +
  coord_cartesian(xlim = c(-10, 10)) +
  labs(x = "daily change in beds per 100k population") +
  facet_wrap(~metric, scale = "free_y")

# check trust population distribution
rs$REDACTED |>
  dplyr::collect() |>
  ggplot() +
  tidybayes::stat_histinterval(aes(x = population), breaks = 15)

processed_data |>
  tibble::as_tibble() |>
  dplyr::mutate(daily_diff_pc_abs = abs(daily_diff_pc)) |>
  dplyr::summarise(
    .q95 = quantile(daily_diff_pc_abs, 0.95),
    .q90 = quantile(daily_diff_pc_abs, 0.9),
    .q75 = quantile(daily_diff_pc_abs, 0.75),
    .q50 = quantile(daily_diff_pc_abs, 0.5),
    .q25 = quantile(daily_diff_pc_abs, 0.25),
    .q5 = quantile(daily_diff_pc_abs, 0.05),
    .by = c("metric")
  ) |>
  # apply to an average trust size
  dplyr::mutate(dplyr::across(dplyr::starts_with(".q"), \(x) 3e5 * x / 1e5))

# explore DoW effect
processed_data |>
  tibble::as_tibble() |>
  dplyr::mutate(dow = lubridate::wday(date, label = TRUE)) |>
  dplyr::summarise(
    .q95 = quantile(daily_diff_pc, 0.95),
    .q90 = quantile(daily_diff_pc, 0.9),
    .q75 = quantile(daily_diff_pc, 0.75),
    .q50 = quantile(daily_diff_pc, 0.5),
    .q25 = quantile(daily_diff_pc, 0.25),
    .q10 = quantile(daily_diff_pc, 0.1),
    .q5 = quantile(daily_diff_pc, 0.05),
    .by = c("metric", "dow")
  ) |>
  # apply to an average trust size
  dplyr::mutate(dplyr::across(dplyr::starts_with(".q"), \(x) 3e5 * x / 1e5)) |>
  ggplot() +
  geom_linerange(aes(x = dow, ymin = .q10, ymax = .q90), alpha = 0.7) +
  geom_point(aes(x = dow, y = .q90, color = "90%")) +
  geom_point(aes(x = dow, y = .q50, color = "50%")) +
  geom_point(aes(x = dow, y = .q10, color = "10%")) +
  labs(y = "Daily difference in beds for average Trust") +
  facet_wrap(~metric)

processed_data |>
  ggplot() +
  stat_ecdf(aes(x = abs(daily_diff_pc), group = metric, color = metric)) +
  coord_cartesian(xlim = c(0, 20)) +
  scale_x_continuous(breaks = seq(0, 20)) +
  labs(
    title = "Cumulative distribution of the absolute value of the daily difference per capita",
    subtitle = "Shows more variation in total beds as expected compared to component metrics"
  )

# explore autocorrelation

# autocorrelation of value
# time series is highly autocorrelated with some evidence of DoW effects
feasts::ACF(processed_data, y = value_pc) |>
  tibble::as_tibble() |>
  dplyr::summarise(
    .q50 = quantile(acf, 0.5, na.rm = TRUE),
    .q75 = quantile(acf, 0.75, na.rm = TRUE),
    .q25 = quantile(acf, 0.25, na.rm = TRUE),
    .by = c("metric", "lag")
  ) |>
  ggplot() +
  geom_line(aes(x = lag, y = .q50)) +
  geom_ribbon(aes(x = lag, ymax = .q75, ymin = .q25), alpha = 0.4) +
  labs(y = "ACF") +
  facet_wrap(~metric)


# autocorrelation of difference
# differences have a clear periodicity in their autocorrelation, indicating
# we should explore taking into account DoW effects when exploring differences.
# HOWEVER, the correlation isn't massive? For escalation beds the correlation at 7 days is
# 0 - 0.2, which while noticable isn't explaining all the variation.
feasts::ACF(processed_data, y = daily_diff_pc) |>
  tibble::as_tibble() |>
  dplyr::summarise(
    .q50 = quantile(acf, 0.5, na.rm = TRUE),
    .q75 = quantile(acf, 0.75, na.rm = TRUE),
    .q25 = quantile(acf, 0.25, na.rm = TRUE),
    .by = c("metric", "lag")
  ) |>
  ggplot() +
  geom_line(aes(x = lag, y = .q50)) +
  geom_ribbon(aes(x = lag, ymax = .q75, ymin = .q25), alpha = 0.4) +
  labs(y = "ACF") +
  facet_wrap(~metric)

# Explore rolling averages to remove DOW effects

processed_data <- processed_data |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    value_pc_mean = zoo::rollmean(x = value_pc, k = 7, na.pad = TRUE, align = "center"),
    daily_diff_pc_mean = zoo::rollmean(x = daily_diff_pc, k = 7, na.pad = TRUE),
    .by = c("metric", "code")
  )

processed_data |>
  ggplot() +
  geom_line(aes(x = date, y = value_pc_mean, group = name), alpha = 0.1) +
  facet_wrap(~metric, scale = "free_y")

processed_data |>
  ggplot() +
  geom_line(aes(x = date, y = daily_diff_pc_mean, group = name), alpha = 0.1) +
  facet_wrap(~metric, scale = "free_y")

processed_data |>
  tibble::as_tibble() |>
  dplyr::mutate(daily_diff_pc_abs = abs(daily_diff_pc_mean)) |>
  dplyr::filter(!is.na(daily_diff_pc_abs)) |>
  dplyr::summarise(
    .q95 = quantile(daily_diff_pc_abs, 0.95),
    .q90 = quantile(daily_diff_pc_abs, 0.9),
    .q75 = quantile(daily_diff_pc_abs, 0.75),
    .q50 = quantile(daily_diff_pc_abs, 0.5),
    .q25 = quantile(daily_diff_pc_abs, 0.25),
    .q5 = quantile(daily_diff_pc_abs, 0.05),
    .by = c("metric")
  ) |>
  # apply to an average trust size
  dplyr::mutate(dplyr::across(dplyr::starts_with(".q"), \(x) 3e5 * x / 1e5))


# final analysis
final_data <- processed_data |>
  tibble::as_tibble() |>
  dplyr::mutate(daily_diff_pc_abs = daily_diff_pc, dow = lubridate::wday(date, label = TRUE)) |>
  dplyr::summarise(
    .q95 = quantile(daily_diff_pc, 0.95),
    .q90 = quantile(daily_diff_pc, 0.9),
    .q75 = quantile(daily_diff_pc, 0.75),
    .q50 = quantile(daily_diff_pc, 0.5),
    .q25 = quantile(daily_diff_pc, 0.25),
    .q10 = quantile(daily_diff_pc, 0.1),
    .q5 = quantile(daily_diff_pc, 0.05),
    .by = c("metric", "dow")
  ) |>
  dplyr::filter(metric == "total_escalation_beds_open") |>
  dplyr::mutate(increase = .q90, decrease = .q10) |>
  tidyr::pivot_longer(cols = c(increase, decrease), names_to = "change", values_to = "delta") |>
  dplyr::select(-dplyr::starts_with(".q"), -metric) |>
  dplyr::mutate(delta = abs(delta))
final_data


# increases and decreases are not equal across the different days of the week.
# Some days there is much more capacity to net increase bed numbers, and some days are more
# likely to have net decreases in bed numbers.
# If we look at the variation of both increases and decreases together we have central skewing,
# and if we look at absolute values alone we lose directionality.
final_data |>
  ggplot() +
  geom_point(
    aes(x = forcats::fct_rev(dow), y = delta, group = change, color = change),
    position = position_dodge(width = 0.5)
  ) +
  geom_linerange(
    aes(x = dow, ymin = 0, ymax = delta, group = change, color = change),
    position = position_dodge(width = 0.5)
  ) +
  labs(y = "daily change in beds per 100k population", x = "day of week", title = "Escalation bed changes") +
  scale_color_manual(name = "change direction", values = c("increase" = "maroon", "decrease" = "steelblue3")) +
  scale_y_continuous(breaks = seq(0, 5, 0.5)) +
  ggplot2::coord_flip()


# lets make some toy data to explore what our proposed delta cone would look like
# for a utility bound.
toy_data <- data.frame(
  date = seq(as.Date("2024-01-01"), by = "day", length.out = 100)
) |>
  dplyr::mutate(dow = lubridate::wday(date, label = TRUE)) |>
  dplyr::left_join(
    final_data |>
      tidyr::pivot_wider(names_from = "change", values_from = "delta"),
    by = "dow"
  ) |>
  dplyr::mutate(cumulative_increase = cumsum(increase), cumulative_decrease = cumsum(-decrease))

toy_data |>
  ggplot() +
  geom_ribbon(aes(x = date, ymax = cumulative_increase, ymin = cumulative_decrease), alpha = 0.4) +
  labs(y = "upper bound in utility")
