box::use(
  testthat[...],
  prj / intervals[...]
)


test_that("dummy tests to be replaced, see #1530", {
  # Construct example data - a nice "up'n'down" curve over a 100-day period
  samples <- tibble::tibble(
    "date" = seq.Date(from = as.Date("2024-01-01"), by = "day", length.out = 100),
    "perfect_curve" = (\(x) -(x^2) + 101 * x)(1:100),
    # Make the real values a bit messier
    "target" = pmax(perfect_curve + cumsum(runif(100, -100, 100)), 1),
    ".value" = perfect_curve + cumsum(runif(100, -100, 100)),
    "population" = 10000,
    ".sample" = 0,
    "model" = "test"
  ) |>
    dplyr::select(!perfect_curve)

  # Optional: have a look at what that looks like
  plot(samples$target, type = "l")
  lines(samples$.value, type = "l", col = "red")

  out <- discretise_trends(samples, upper_rate = 0.1, lower_rate = -0.1, forecast_horizon = 7)

  # We should have new columns corresponding to probabilities
  expect_named(out, c(colnames(samples), "p_increase", "p_stable", "p_decrease"))

  # Intuitively, should show increase, then stable in the middle, then decrease

  # TODO actually test this
  # TODO aren't we supposed to have values *between* 0 and 1, if they are probabilities??
  # we seem to have a lot of {0, 1} values but not anything in between...
  expect_true(TRUE)
})
