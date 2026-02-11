#' [WARNING]
#' This script is not intended to be run again, but is retained for record keeping purposes.

# download, process and re-save summary outputs
# will remove the ensemble from summary outputs because
# 1. it is the same as the gam_dow model; ensemble is trivial
# 2. do not want to confuse this retrospective ensemble within-season modelling outputs

# If you need to rerun this analysis
# 1. Re-run the corresponding targets pipeline(s)
# 2. Update the summary files to the new s3 URI(s)
# 3. Set `run_script <- TRUE`
# 4. Allow files to be overwritten in any relevant
run_script <- FALSE

if (run_script) {
  s3_root <- "PATH REDACTED"

  summary_files <- c(
    "covid-19/PATH REDACTED",
    "influenza/PATH REDACTED",
    "rsv/PATH REDACTED"
  )

  diseases <- c("covid-19", "influenza", "rsv")

  new_directories <- purrr::walk(
    diseases,
    \(disease) {
      s3fs::s3_dir_create(
        glue::glue(
          "PATH REDACTED"
        )
      )
    }
  )

  output_files <- purrr::walk(
    summary_files,
    \(fpath) {
      s3_path <- glue::glue("{s3_root}/{fpath}")

      summary <- aws.s3::s3read_using(vroom::vroom, object = s3_path)

      summary <- summary |>
        dplyr::filter(model == "gam_dow")

      s3_location <- stringr::str_replace(s3_path, "admissions/", "admissions/processed/")

      aws.s3::s3write_using(
        summary,
        FUN = readr::write_csv,
        object = s3_location
      )

      s3_location
    }
  )

  # manually copy files to glue bucket ----

  glue_root <- "PATH REDACTED"

  ## copy summary files ----

  ## Can't use `commit_output()` here due to the non-standard file paths
  ## Therefore, manual copy with {s3fs}

  output_files
  purrr::walk(
    diseases,
    \(disease) {
      source_file <- glue::glue("{s3_root}/{output_files[stringr::str_detect(output_files, disease)]}") |>
        stringr::str_replace("admissions/", "admissions/processed/")

      file_tail <- source_file |>
        stringr::str_split("/") |>
        unlist() |>
        utils::tail(1)

      # choose model date as day _after_ last seasonal run

      glue_summary_path <- glue::glue(
        "PATH REDACTED"
      )

      s3fs::s3_file_copy(
        source_file,
        glue_summary_path
      )
    }
  )

  ## copy samples files

  purrr::walk(
    diseases,
    \(disease) {
      source_file <- glue::glue("{s3_root}/{output_files[stringr::str_detect(output_files, disease)]}")

      file_tail <- source_file |>
        stringr::str_split("/") |>
        unlist() |>
        utils::tail(1) |>
        stringr::str_replace("summary", "samples")

      # choose model date as day _after_ last seasonal run

      glue_summary_path <- glue::glue(
        "PATH REDACTED",
        "PATH REDACTED"
      )

      s3fs::s3_file_copy(
        source_file,
        glue_summary_path
      )
    }
  )
}
