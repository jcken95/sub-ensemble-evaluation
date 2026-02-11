# load data ------------------------------------------------------------------------------------------------------------

box::use(
  box / deps_,
  box / redshift,
  box / s3,
  prj / projection_plots,
  prj / intervals
)

# Life is too short to preappend ggplot each time
library(ggplot2)


deps_$need("scoringutils>=2.1.0")

ggplot2::theme_set(projection_plots$theme_pancasts())

source("evaluation/helpers.R")
