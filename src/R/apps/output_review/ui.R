
ui <- bslib::page_fillable(
  theme = bslib::bs_theme(bootswatch = "sandstone"),

  shinyjs::useShinyjs(),
  shiny::tags$head(
    shiny::tags$style(paste(
      ".radio {white-space: nowrap;}",
      "h2 {font-size: 1.6rem;}",
      "#menu .card-body {overflow: visible !important;}"
    ))
  ),

  shiny::titlePanel("Pancasting Output Review Tool"),

  bslib::card(
    id = "menu",
    height = "120px",
    fill = FALSE,
    bslib::layout_columns(
      shiny::selectInput("disease", "Disease", choices = c("covid-19", "influenza", "norovirus", "rsv")),
      shiny::selectInput("metric", "Metric", choices = NULL),
      shiny::selectInput("time", "Model fitted at", choices = NULL),
      shiny::div(
        shiny::actionButton("checkout", "Check out", class = "btn-primary"),
        shinyjs::hidden(shiny::actionButton("checkin", "Check in", class = "btn-danger"))
      )
    )
  ),

  bslib::layout_sidebar(

    sidebar = bslib::sidebar(
      width = "50%",
      shinyjs::hidden(shiny::radioButtons("file", label = NULL, choices = "", width = "100%"))
    ),

    bslib::card(
      full_screen = TRUE,
      height = "75%",
      shiny::uiOutput("content", height = "100%")
    ),

    bslib::card(
      height = "25%",
      bslib::layout_columns(
        col_widths = c(2, 10),
        shiny::div(
          shinyjs::hidden(shiny::actionButton("approve", "Approve", class = "btn-success")),
          shinyjs::hidden(shiny::actionButton("unapprove", "Un-approve", class = "btn-warning"))
        ),
        shiny::div(
          shinyjs::hidden(shiny::textAreaInput("notes", "Notes", width = "100%")),
          shinyjs::hidden(shiny::actionButton("save_notes", "Save", class = "btn-secondary"))
        )
      )
    )
  )
)
