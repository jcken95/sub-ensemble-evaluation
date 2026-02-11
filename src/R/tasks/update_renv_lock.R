#' Snapshot dependencies for tasks
#'
#' This is NOT currently a full {renv}-controlled project, but we are using the
#' snapshotting functionality to keep track of which packages we are using, and
#' the versions of those packages.
#'
#' Source this script to update the `src/R/tasks/renv.lock` file with the names
#' and versions of the packages in *your* current libraries (see [.libPaths()]).
#'
#' The lockfile is used to ensure the R environment which we use to run tasks on
#' an automated basis, is consistent with the R environment where we develop our
#' pipelines.
#'
#' Note how `renv.lock` is used here:
#' https://github.com/REDACTED

# Specify locations for which we need to snapshot dependencies
# NB. we _could_ simplify things somewhat by using renv::snapshot() to write
# a project-wide `renv.lock` - BUT since we will use this snapshot to restore an
# R environment every time we want to run our automated tasks, it makes sense to
# keep it as lightweight as possible
paths <- c(
  # The tasks directory itself
  "src/R/tasks",

  # Production pipelines
  yaml::read_yaml("_targets.yaml") |>
    purrr::map_chr("script") |>
    fs::path_dir(),

  # Locations of box modules used in pipelines
  "src/R/box",
  "src/R/prj"
)


# Calculate dependencies
deps <- renv::dependencies(paths)$Package


# Write dependencies to src/R/tasks/renv.lock
withr::with_options(
  list("renv.snapshot.filter" = \(.) deps),
  renv::snapshot("src/R/tasks", type = "custom")
)
