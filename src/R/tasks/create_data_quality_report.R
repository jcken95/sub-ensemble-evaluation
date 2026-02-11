timestamp <- format(Sys.time(), "%Y-%m-%d_%H:%M:%S%Z")

quarto::quarto_render("pipelines/quality/quality_report.qmd")

s3fs::s3_file_upload(
  "pipelines/quality/quality_report.html",
  "PATH REDACTED"
)
