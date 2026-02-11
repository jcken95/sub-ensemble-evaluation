server <- function(input, output, session) {
  box::use(box / s3)

  s3_base <- "PATH REDACTED"

  local_dir <- withr::local_tempdir()
  local_manifest <- list()

  shiny::observe({
    available_metrics <- s3fs::s3_dir_ls(s3fs::s3_path(s3_base, input$disease), type = "directory") |>
      # Only keep subdirs containing a "local_outputs/" subdir
      purrr::keep(\(.) length(s3fs::s3_dir_ls(., type = "directory", glob = "local_outputs")) > 0) |>
      s3fs::s3_path_file()

    shiny::updateSelectInput(inputId = "metric", choices = available_metrics)
  })

  shiny::observe({
    available_times <- s3fs::s3_dir_ls(s3fs::s3_path(s3_base, input$disease, input$metric, "local_outputs")) |>
      s3fs::s3_path_file() |>
      setdiff("manifests")

    shiny::updateSelectInput(inputId = "time", choices = rev(sort(available_times)))
  })

  s3_location <- shiny::reactive({
    shiny::validate(
      shiny::need(
        all(purrr::map_lgl(list(input$disease, input$metric, input$time), shiny::isTruthy)),
        "Ensure all inputs are selected"
      )
    )

    s3fs::s3_path(s3_base, input$disease, input$metric, "local_outputs", input$time)
  })

  s3_manifest_path <- shiny::reactive(
    s3fs::s3_path(
      s3fs::s3_path_dir(s3_location()),
      "manifests",
      s3fs::s3_path_file(s3_location()) |>
        s3fs::s3_path_ext_set("manifest.json")
    )
  )

  s3_lockfile_path <- shiny::reactive(
    s3fs::s3_path_ext_set(
      s3_manifest_path(),
      "json.lock"
    )
  )

  # Tidy up lockfile if we quit the app having forgotten to check in, or if we crash for any reason
  shiny::observe({
    session$onSessionEnded(function() {
      suspendInterrupts({
        # Doesn't matter if it doesn't exist
        try(s3fs::s3_file_delete(shiny::isolate(s3_lockfile_path())), silent = TRUE)
      })
    })
  })

  shiny::observe({
    # Change buttons
    for (id in c("disease", "metric", "time")) {
      shinyjs::disable(id)
    }
    shinyjs::hide("checkout")

    # Check for existing lockfile
    if (s3fs::s3_file_exists(s3_lockfile_path())) {
      info <- s3$read_using(s3_lockfile_path(), jsonlite::read_json)
      shiny::showNotification(
        glue::glue("This is already checked out! (By: {info$user})"),
        type = "error"
      )

      shinyjs::show("checkout")
      for (id in c("disease", "metric", "time")) {
        shinyjs::enable(id)
      }
      return(NULL)
    }

    # Put S3 lockfile
    s3$write_using(
      Sys.info()[c("user", "nodename")] |> as.list(),
      s3_lockfile_path(),
      \(x, to) jsonlite::write_json(x, to, auto_unbox = TRUE),
      verbose = FALSE
    )

    # Download content
    if (fs::dir_exists(local_dir)) {
      fs::dir_delete(local_dir)
    }
    fs::dir_create(local_dir)
    s3$read_using(
      s3_location(),
      \(.) utils::unzip(., exdir = local_dir, unzip = "unzip"),
      local_path = withr::local_tempfile(fileext = ".zip")
    )

    # Fill local manifest
    fs::dir_ls(local_dir, recurse = TRUE, type = "file") |>
      fs::path_rel(local_dir) |>
      purrr::walk(
        \(.) {
          local_manifest[[.]] <<- list(
            "md5" = shiny::reactiveVal(unname(tools::md5sum(fs::path(local_dir, .)))),
            "approved" = shiny::reactiveVal(FALSE),
            "notes" = shiny::reactiveVal("")
          )
        }
      )

    # Update with values from S3 manifest
    if (s3fs::s3_file_exists(s3_manifest_path())) {
      existing_manifest <- s3$read_using(s3_manifest_path(), jsonlite::read_json)
      purrr::iwalk(
        local_manifest,
        \(val, nm) {
          if (is.null(local_manifest[[nm]])) {
            return(NULL)
          }

          # Always copy notes
          val$notes(existing_manifest[[nm]]$notes)

          # Only copy approval status if file is identical
          if (val$md5() == existing_manifest[[nm]]$md5) {
            val$approved(existing_manifest[[nm]]$approved)
          }

          # Radio buttons don't actually get updated until after next "flush"
          # So delay JS actions until then!
          session$onFlushed(function() {
            shinyjs::toggleClass(
              selector = glue::glue(".sidebar .radio:has(input[value='{nm}']) span"),
              class = "bg-success",
              condition = shiny::isolate(val$approved())
            )

            shinyjs::toggleClass(
              selector = glue::glue(".sidebar .radio:has(input[value='{nm}']) span"),
              class = "bg-warning",
              condition = (!(shiny::isolate(val$approved())) &&
                (shiny::isolate(val$notes()) != ""))
            )
          })
        }
      )
    }

    shiny::updateRadioButtons(inputId = "file", choices = names(local_manifest))

    shinyjs::show("file")
    shinyjs::show("checkin")
  }) |>
    shiny::bindEvent(input$checkout)

  shiny::observe(
    if (shiny::isTruthy(input$file)) {
      path <- fs::path(local_dir, input$file)

      output$content_inner <- switch(
        fs::path_ext(input$file),
        "png" = shiny::renderImage(
          list(src = path, style = "max-width: 100%; max-height: 100%; min-height: 500px;"),
          deleteFile = FALSE
        ),
        "txt" = shiny::renderPrint(cat(readLines(path, warn = FALSE), sep = "\n")),
        "csv" = shiny::renderTable(utils::read.csv(path))
      )

      # Make sure content_inner has rendered first - so delay this until
      # next flush
      session$onFlushed(function() {
        output$content <- shiny::renderUI({
          switch(
            fs::path_ext(input$file),
            "png" = shiny::imageOutput("content_inner", width = "100%", height = "100%"),
            "txt" = shiny::verbatimTextOutput("content_inner"),
            "csv" = shiny::tableOutput("content_inner")
          )
        })
      })

      shiny::updateTextAreaInput(inputId = "notes", value = local_manifest[[input$file]]$notes())
      shinyjs::show("notes")
      shinyjs::toggle("approve", condition = !(local_manifest[[input$file]]$approved()))
      shinyjs::toggle("unapprove", condition = local_manifest[[input$file]]$approved())
    } else {
      output$content <- NULL
      shinyjs::hide("notes")
      shinyjs::hide("approve")
      shinyjs::hide("unapprove")
    }
  )

  shiny::observe(
    if (shiny::isTruthy(input$file)) {
      shinyjs::toggle("save_notes", condition = (input$notes != local_manifest[[input$file]]$notes()))
    }
  )

  shiny::observe(
    if (shiny::isTruthy(input$file)) {
      local_manifest[[input$file]]$notes(input$notes)

      shinyjs::toggleClass(
        selector = glue::glue(".sidebar .radio:has(input[value='{input$file}']) span"),
        class = "bg-warning",
        condition = (!(local_manifest[[input$file]]$approved()) && (input$notes != ""))
      )
    }
  ) |>
    shiny::bindEvent(input$save_notes)

  shiny::observe({
    local_manifest[[input$file]]$approved(TRUE)

    shinyjs::hide("approve")
    shinyjs::show("unapprove")

    shinyjs::removeClass(
      selector = glue::glue(".sidebar .radio:has(input[value='{input$file}']) span"),
      class = "bg-warning"
    )

    shinyjs::addClass(
      selector = glue::glue(".sidebar .radio:has(input[value='{input$file}']) span"),
      class = "bg-success"
    )
  }) |>
    shiny::bindEvent(input$approve)

  shiny::observe({
    local_manifest[[input$file]]$approved(FALSE)

    shinyjs::removeClass(
      selector = glue::glue(".sidebar .radio:has(input[value='{input$file}']) span"),
      class = "bg-success"
    )

    shinyjs::toggleClass(
      selector = glue::glue(".sidebar .radio:has(input[value='{input$file}']) span"),
      class = "bg-warning",
      condition = (input$notes != "")
    )

    shinyjs::hide("unapprove")
    shinyjs::show("approve")
  }) |>
    shiny::bindEvent(input$unapprove)

  shiny::observe({
    # Inverted steps to input$checkout :)
    shinyjs::hide("file")

    # Put manifest to S3
    s3$write_using(
      purrr::modify_depth(local_manifest, 2, \(.) .()), # evaluate all reactiveVals
      s3_manifest_path(),
      \(x, to) jsonlite::write_json(x, to, auto_unbox = TRUE),
      overwrite = TRUE,
      verbose = FALSE
    )

    # Delete S3 lockfile
    s3fs::s3_file_delete(s3_lockfile_path())

    # Re-enable UI
    shinyjs::hide("checkin")
    shinyjs::show("checkout")
    for (id in c("disease", "metric", "time")) {
      shinyjs::enable(id)
    }

    shiny::updateRadioButtons(inputId = "file", choices = "", selected = "")

    # Empty local manifest
    local_manifest <<- list()
  }) |>
    shiny::bindEvent(input$checkin, ignoreNULL = TRUE, ignoreInit = TRUE)
}
