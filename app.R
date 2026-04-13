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

preload_outbreaks_dataset()

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(HTML("
      window.addEventListener('error', function(event) {
        var box = document.getElementById('js-error-banner');
        if (!box) return;
        box.style.display = 'block';
        box.textContent = 'JS Error: ' + (event.message || event.error || event);
      });
      window.addEventListener('unhandledrejection', function(event) {
        var box = document.getElementById('js-error-banner');
        if (!box) return;
        box.style.display = 'block';
        box.textContent = 'JS Promise Rejection: ' + (event.reason && event.reason.message ? event.reason.message : String(event.reason || event));
      });
    "))
  ),
  div(
    class = "app-shell",
    tags$div(id = "js-error-banner", class = "js-error-banner", style = "display:none;"),
    div(
      class = "app-header",
      div(
        class = "title-block",
        h1("Emergency Preparedness Plan Analyzer"),
        p("Upload or paste a preparedness plan to generate a structured scorecard, gap analysis, and prioritized recommendations.")
      ),
      NULL
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
          h3("Framework Selection"),
          selectInput(
            "analysis_framework",
            "Framework",
            choices = c("CDC PHEP", "FEMA", "WHO ERF"),
            selected = "CDC PHEP"
          )
        ),
        div(
          class = "panel-section",
          h3("Region Settings"),
          div(
            class = "section-grid",
            selectizeInput(
              "region_country",
              "Country",
              choices = c("United States", "Canada", "Mexico", "United Kingdom", "Australia", "India", "Other"),
              options = list(create = TRUE, placeholder = "Start typing a country"),
              selected = "United States"
            ),
            conditionalPanel(
              condition = "input.region_country === 'Other'",
              textInput(
                "region_country_other",
                "Country (Other)",
                placeholder = "Type your country name"
              )
            ),
            selectInput(
              "region_state",
              "Region (State/Territory)",
              choices = c("NA" = "NA", setNames(state.abb, state.name), "DC" = "DC"),
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
              div(
                class = "scorecard-meta",
                div(
                  class = "scorecard-scale",
                  strong("Score scale: "),
                  "80–100 = Low Risk, 50–79 = Moderate Risk, 0–49 = High Risk"
                ),
                uiOutput("framework_note"),
                uiOutput("llm_status"),
                uiOutput("framework_picker"),
                div(
                  class = "scorecard-links",
                  span("Domain details are listed below.")
                )
              ),
              div(
                class = "scorecard-table",
                if (has_shinycssloaders) {
                  shinycssloaders::withSpinner(tableOutput("scorecard"))
                } else {
                  tableOutput("scorecard")
                }
              ),
              uiOutput("domain_defs_button"),
              uiOutput("domain_defs")
            ),
            tabPanel(
              "Insights",
              h3("Executive Summary"),
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(uiOutput("exec_summary"))
              } else {
                uiOutput("exec_summary")
              },
              h3("Key Gaps"),
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(uiOutput("gap_list"))
              } else {
                uiOutput("gap_list")
              }
            ),
            tabPanel(
              "Regional Context",
              div(
                class = "regional-grid",
                div(
                  class = "regional-column",
                  if (has_shinycssloaders) {
                    shinycssloaders::withSpinner(tableOutput("cdc_table"))
                  } else {
                    tableOutput("cdc_table")
                  },
                  NULL,
                  h4("Identified Hazards"),
                  if (has_shinycssloaders) {
                    shinycssloaders::withSpinner(tableOutput("hazard_identified_table"))
                  } else {
                    tableOutput("hazard_identified_table")
                  },
                  NULL
                ),
                div(
                  class = "regional-column",
                  h3("Recommendations"),
                  if (has_shinycssloaders) {
                    shinycssloaders::withSpinner(uiOutput("regional_recommendations"))
                  } else {
                    uiOutput("regional_recommendations")
                  }
                )
              )
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
  show_domain_defs <- reactiveVal(FALSE)

  observeEvent(input$toggle_domain_defs, {
    show_domain_defs(!show_domain_defs())
  })

  pretty_cdc_table <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    df |>
      dplyr::rename(
        Condition = condition,
        Cases = cases,
        `Year Range` = year_range
      )
  }
  pretty_outbreaks_table <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    df |>
      dplyr::rename(
        Disease = Disease,
        Events = events,
        `Year Range` = year_range
      )
  }

  country_value <- reactive({
    if (!is.null(input$region_country) &&
        input$region_country == "Other" &&
        !is.null(input$region_country_other) &&
        nzchar(input$region_country_other)) {
      return(input$region_country_other)
    }
    input$region_country
  })

  analysis_result <- reactive({
    plan_text <- load_plan_text(input$plan_file, input$plan_text)
    validate_plan_text(plan_text)
    incident_types <- parse_incident_types(input$incident_types)

    run_full_analysis(
      plan_text = plan_text,
      facility_type = input$facility_type,
      emergency_focus = input$emergency_focus,
      framework_choice = input$analysis_framework,
      region_country = country_value(),
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
      input$analysis_framework,
      input$region_country,
      input$region_country_other,
      input$region_state,
      input$region_county,
      input$history_years,
      input$fema_types,
      input$incident_types
    ) %>%
    bindEvent(input$run_analysis, ignoreInit = TRUE)

  observeEvent(list(input$region_state, input$region_country, input$region_country_other), {
    if (!is.null(country_value()) && tolower(country_value()) == "united states") {
      county_choices <- get_county_choices(input$region_state)
      county_choices <- c("Not applicable", county_choices)
      updateSelectizeInput(
        session,
        "region_county",
        choices = county_choices,
        server = TRUE
      )
    } else {
      updateSelectizeInput(
        session,
        "region_county",
        choices = c("Not applicable"),
        server = TRUE
      )
    }
  })

  observeEvent(list(input$region_country, input$region_country_other), {
    if (!is.null(country_value()) && tolower(country_value()) != "united states") {
      updateSelectInput(session, "region_state", selected = "NA")
    }
  })

  output$scorecard <- renderTable({
    analysis_result()$scorecard
  })

  output$framework_note <- renderUI({
    country <- country_value() %||% "United States"
    framework_choice <- input$analysis_framework %||% "CDC PHEP"
    llm_note <- if (!is.null(app_config$llm_api_key) &&
      nzchar(app_config$llm_api_key) &&
      !is.null(app_config$llm_api_base) &&
      nzchar(app_config$llm_api_base)) {
      paste0(" Scoring uses an LLM rubric-based review of plan strength (model: ", app_config$llm_model, ").")
    } else {
      " Scoring uses rule-based signals from the plan text."
    }
    applicability <- if (framework_choice == "CDC PHEP" &&
      !(input$facility_type %in% c("Public Health Department", "Emergency Management Agency"))) {
      " Note: CDC PHEP is designed for public health agencies; applicability may be limited for other facility types."
    } else {
      ""
    }
    tags$p(
      class = "hint",
      paste0("Scorecard uses the 4-domain model. Framework mapping uses ", framework_choice, ".", applicability, llm_note)
    )
  })

  output$llm_status <- renderUI({
    status <- analysis_result()$llm
    if (is.null(status)) return(NULL)
    if (isTRUE(status$used)) {
      tags$p(class = "hint", paste("LLM status: successful rubric-based review (model:", app_config$llm_model, ")."))
    } else if (!is.null(status$error) && nzchar(status$error)) {
      tags$p(class = "hint", paste("LLM status: fallback to rule-based (model:", app_config$llm_model, "). Error:", status$error))
    } else {
      tags$p(class = "hint", paste("LLM status: fallback to rule-based (model:", app_config$llm_model, "). No LLM response."))
    }
  })

  output$framework_picker <- renderUI({
    framework_choice <- input$analysis_framework %||% "CDC PHEP"
    tagList(
      tags$p(class = "hint", paste0("Selected framework: ", framework_choice)),
      NULL
    )
  })

  output$domain_defs_button <- renderUI({
    label <- if (show_domain_defs()) "Hide Domain Definitions" else "Show Domain Definitions"
    div(class = "scorecard-defs-btn", actionButton("toggle_domain_defs", label))
  })

  output$domain_defs <- renderUI({
    if (!show_domain_defs()) return(NULL)
    legend_items <- list(
      list(
        label = "Command",
        detail = "Incident command structure, leadership roles, decision authority, and succession."
      ),
      list(
        label = "Communication",
        detail = "Internal/external communication systems, public information, and redundancy."
      ),
      list(
        label = "Operations",
        detail = "Response actions, coordination mechanisms, and operational execution."
      ),
      list(
        label = "Logistics",
        detail = "Resource management, personnel, facilities, supplies, and support services."
      )
    )

    tags$div(
      class = "scorecard-details",
      tags$div(
        class = "framework-card",
        h3("Domain Definitions"),
        tags$ul(
          lapply(legend_items, function(item) {
            tags$li(tags$strong(item$label), ": ", item$detail)
          })
        )
      )
    )
  })

  output$gap_list <- renderUI({
    format_gap_list(analysis_result()$gaps)
  })

  output$exec_summary <- renderUI({
    format_exec_summary_narrative(
      facility_type = input$facility_type,
      emergency_focus = input$emergency_focus,
      final_score = analysis_result()$final_score,
      scorecard = analysis_result()$scorecard,
      gaps = analysis_result()$gaps,
      actions = analysis_result()$action_plan
    )
  })

  output$cdc_table <- renderTable({
    pretty_cdc_table(analysis_result()$regional$cdc$table)
  }, rownames = FALSE)

  output$cdc_note <- renderUI({
    note <- analysis_result()$regional$cdc$note
    source_note <- if (tolower(app_config$cdc_source %||% "wonder") == "wonder") {
      "CDC data source: CDC WONDER NNDSS Annual Summary (D130) filtered by selected state and years."
    } else {
      "CDC data source: NNDSS (data.cdc.gov) filtered by selected state and years."
    }
    if (!is.null(note) && nzchar(note)) {
      tagList(
        tags$p(class = "hint", note),
        tags$p(class = "hint", source_note)
      )
    } else {
      tags$p(class = "hint", source_note)
    }
  })

  output$hazard_identified_table <- renderTable({
    df <- analysis_result()$regional$hazard_identified
    if (is.null(df) || nrow(df) == 0) return(df)
    df |>
      dplyr::rename(
        Hazard = hazard_label,
        Source = source,
        Events = frequency,
        `Year Range` = year_range
      )
  }, rownames = FALSE)

  output$regional_recommendations <- renderUI({
    format_hazard_recommendations(analysis_result()$regional$hazard_recommendations)
  })

}

shinyApp(ui, server)
