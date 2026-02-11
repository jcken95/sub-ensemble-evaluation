args <- commandArgs(trailingOnly = TRUE)
project <- args[1]

Sys.setenv(TAR_PROJECT = project)

# Prepend extra options to start of target script
# TODO put this in the script itself?
script <- targets::tar_config_get("script")
content <- c(
  "REDACTED",
  readLines(script)
)
writeLines(content, script)

# Run pipeline
targets::tar_make()
