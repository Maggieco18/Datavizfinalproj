library(shiny)
options(shiny.maxRequestSize = 200 * 1024^2)
options(shiny.launch.browser = TRUE)
has_shinycssloaders <- requireNamespace("shinycssloaders", quietly = TRUE)
library(jsonlite)
library(stringr)
library(dplyr)
library(tibble)
library(purrr)
library(readr)

source("R/config.R")
source("R/llm.R")
source("R/pipeline.R")
source("R/scoring.R")
source("R/ui_helpers.R")
source("R/region_data.R")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  div(
    class = "app-shell",
    div(
      class = "app-header",
      div(
        class = "title-block",
        h1("Emergency Preparedness Consulting Analyzer"),
        p("Upload or paste a preparedness plan to generate a structured scorecard, gap analysis, and prioritized recommendations.")
      ),
      div(
        class = "badge-block",
        span(class = "badge", "Healthcare Preparedness"),
        span(class = "badge", "Consulting Output"),
        span(class = "badge", "GenAI-Assisted")
      )
    ),
    div(
      class = "app-body",
      div(
        class = "panel panel-input",
        h2("Input Plan"),
        div(
          class = "panel-section",
          h3("Plan Source"),
          fileInput("plan_file", "Upload PDF or text file", accept = c(".pdf", ".txt")),
          textAreaInput("plan_text", "Or paste plan text", rows = 10, placeholder = "Paste preparedness plan text here...")
        ),
        div(
          class = "panel-section",
          h3("Facility Context"),
          div(
            class = "section-grid",
            selectInput(
              "facility_type",
              "Facility Type",
              choices = c(
                "Hospital",
                "Clinic",
                "Health Post",
                "Long-Term Care",
                "School (K-12)",
                "Higher Education",
                "Department of Public Safety",
                "Public Health Department",
                "Emergency Management Agency",
                "Other"
              )
            ),
            selectInput(
              "emergency_focus",
              "Emergency Focus",
              choices = c("General", "Infectious Disease", "Natural Disaster", "Mass Casualty", "Other")
            )
          )
        ),
        div(
          class = "panel-section",
          h3("Region Settings"),
          div(
            class = "section-grid",
            selectInput(
              "region_state",
              "Region (State/Territory)",
              choices = c(setNames(state.abb, state.name), "DC" = "DC"),
              selected = "CA"
            ),
            selectizeInput(
              "region_county",
              "County (optional)",
              choices = c("Not applicable"),
              options = list(create = TRUE, placeholder = "Start typing a county name")
            )
          )
        ),
        div(
          class = "panel-section",
          h3("Analysis Window"),
          sliderInput("history_years", "History Window (years)", min = 3, max = 20, value = 5),
          textInput(
            "incident_types",
            "Incident Types (optional)",
            placeholder = "Example: Hurricane, Flood, Wildfire"
          ),
          checkboxGroupInput(
            "fema_types",
            "FEMA Declaration Types",
            choices = c(
              "Major Disaster (DR)" = "DR",
              "Emergency (EM)" = "EM",
              "Fire Management Assistance (FM)" = "FM"
            ),
            selected = c("DR", "EM")
          )
        ),
        div(
          class = "cta-row",
          actionButton("run_analysis", "Run Analysis", class = "primary-btn"),
          div(class = "hint", "Tip: For the best results, include headers, roles, and contact procedures in the plan text.")
        )
      ),
      div(
        class = "panel panel-output",
        h2("Consulting Output"),
        div(
          class = "tabs-shell",
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Scorecard",
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(tableOutput("scorecard"))
              } else {
                tableOutput("scorecard")
              }
            ),
            tabPanel(
              "Gap Analysis",
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(uiOutput("gap_list"))
              } else {
                uiOutput("gap_list")
              }
            ),
            tabPanel(
              "Action Plan",
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(uiOutput("action_plan"))
              } else {
                uiOutput("action_plan")
              }
            ),
          tabPanel(
            "Executive Summary",
            if (has_shinycssloaders) {
              shinycssloaders::withSpinner(uiOutput("exec_summary"))
            } else {
              uiOutput("exec_summary")
            }
          ),
          tabPanel(
            "Regional Context",
              h3("FEMA Incident Patterns"),
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(tableOutput("fema_table"))
              } else {
                tableOutput("fema_table")
              },
              uiOutput("fema_note"),
              h3("CDC Notifiable Conditions"),
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(tableOutput("cdc_table"))
              } else {
                tableOutput("cdc_table")
              },
              uiOutput("cdc_note"),
              h3("NWS Forecast Snapshot"),
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(tableOutput("nws_forecast"))
              } else {
                tableOutput("nws_forecast")
              },
              uiOutput("nws_forecast_note"),
              h3("NWS Storm Events (History Window)"),
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(tableOutput("nws_table"))
              } else {
                tableOutput("nws_table")
              },
              uiOutput("nws_note"),
              h3("Plan Coverage vs Local Patterns"),
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(tableOutput("coverage_table"))
              } else {
                tableOutput("coverage_table")
              },
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(uiOutput("regional_gap"))
              } else {
                uiOutput("regional_gap")
              }
            )
          )
        )
      )
    ),
    div(
      class = "app-footer",
      span("Frameworks: FEMA, CDC, WHO (adaptable)."),
      span("LLM: configurable via environment variables.")
    )
  )
)

server <- function(input, output, session) {
  analysis_result <- reactive({
    plan_text <- load_plan_text(input$plan_file, input$plan_text)
    validate_plan_text(plan_text)
    incident_types <- parse_incident_types(input$incident_types)

    run_full_analysis(
      plan_text = plan_text,
      facility_type = input$facility_type,
      emergency_focus = input$emergency_focus,
      region_state = input$region_state,
      region_county = input$region_county,
      history_years = input$history_years,
      fema_types = input$fema_types,
      incident_types = incident_types
    )
  }) %>%
    bindCache(
      input$plan_text,
      if (is.null(input$plan_file)) "" else input$plan_file$datapath,
      input$facility_type,
      input$emergency_focus,
      input$region_state,
      input$region_county,
      input$history_years,
      input$fema_types,
      input$incident_types
    ) %>%
    bindEvent(input$run_analysis, ignoreInit = TRUE)

  observeEvent(input$region_state, {
    county_choices <- get_county_choices(input$region_state)
    county_choices <- c("Not applicable", county_choices)
    updateSelectizeInput(
      session,
      "region_county",
      choices = county_choices,
      server = TRUE
    )
  })

  output$scorecard <- renderTable({
    analysis_result()$scorecard
  })

  output$gap_list <- renderUI({
    format_gap_list(analysis_result()$gaps)
  })

  output$action_plan <- renderUI({
    format_action_plan(analysis_result()$action_plan)
  })

  output$exec_summary <- renderUI({
    summary <- analysis_result()$executive_summary
    tags$p(summary)
  })

  output$fema_table <- renderTable({
    analysis_result()$regional$fema$table
  })

  output$fema_note <- renderUI({
    note <- analysis_result()$regional$fema$note
    if (!is.null(note) && nzchar(note)) tags$p(class = "hint", note)
  })

  output$cdc_table <- renderTable({
    analysis_result()$regional$cdc$table
  })

  output$cdc_note <- renderUI({
    note <- analysis_result()$regional$cdc$note
    if (!is.null(note) && nzchar(note)) tags$p(class = "hint", note)
  })

  output$coverage_table <- renderTable({
    analysis_result()$regional$coverage
  })

  output$nws_forecast <- renderTable({
    analysis_result()$regional$weather$table
  })

  output$nws_forecast_note <- renderUI({
    note <- analysis_result()$regional$weather$note
    if (!is.null(note) && nzchar(note)) tags$p(class = "hint", note)
  })

  output$nws_table <- renderTable({
    analysis_result()$regional$nws$table
  })

  output$nws_note <- renderUI({
    note <- analysis_result()$regional$nws$note
    if (!is.null(note) && nzchar(note)) tags$p(class = "hint", note)
  })

  output$regional_gap <- renderUI({
    format_regional_gap(analysis_result()$regional$gap)
  })
}

shinyApp(ui, server)
