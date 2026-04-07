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
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(tableOutput("scorecard"))
              } else {
                tableOutput("scorecard")
              },
              uiOutput("framework_details")
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
              },
              h3("Action Plan"),
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(uiOutput("action_plan"))
              } else {
                uiOutput("action_plan")
              }
            ),
            tabPanel(
              "Regional Context",
              h3("Raw Source Data"),
              h4("FEMA Incident Patterns"),
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(tableOutput("fema_table"))
              } else {
                tableOutput("fema_table")
              },
              uiOutput("fema_note"),
              h4("CDC Notifiable Conditions"),
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(tableOutput("cdc_table"))
              } else {
                tableOutput("cdc_table")
              },
              uiOutput("cdc_note"),
              h3("Hazard Summary"),
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(tableOutput("hazard_priority_table"))
              } else {
                tableOutput("hazard_priority_table")
              },
              uiOutput("hazard_priority_note"),
              h3("Identified Hazards"),
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(tableOutput("hazard_identified_table"))
              } else {
                tableOutput("hazard_identified_table")
              },
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(uiOutput("hazard_analysis"))
              } else {
                uiOutput("hazard_analysis")
              },
              h3("Domain Coverage"),
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(tableOutput("hazard_domain_table"))
              } else {
                tableOutput("hazard_domain_table")
              },
              uiOutput("domain_toggle_button"),
              uiOutput("domain_legend"),
              if (has_shinycssloaders) {
                shinycssloaders::withSpinner(uiOutput("regional_gap"))
              } else {
                uiOutput("regional_gap")
              },
              tags$details(
                tags$summary("Global Outbreak Data (Disease Outbreak News)"),
                tableOutput("outbreaks_table"),
                uiOutput("outbreaks_note")
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

  observeEvent(input$toggle_domains, {
    show_domain_defs(!show_domain_defs())
  })

  pretty_fema_table <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    df |>
      dplyr::rename(
        `Incident Type` = incidentType,
        Events = events,
        `Year Range` = year_range
      )
  }
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
  pretty_coverage_table <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    df |>
      dplyr::rename(
        Source = source,
        Event = event,
        Events = count,
        Coverage = coverage,
        Domains = domains
      )
  }
  pretty_domains <- function(domain_text) {
    if (is.null(domain_text) || !nzchar(domain_text)) return(domain_text)
    parts <- trimws(strsplit(domain_text, ",", fixed = TRUE)[[1]])
    label_map <- c(
      communication = "Communication & Coordination",
      workforce = "Workforce Capacity",
      supply_chain = "Supply Chain & Logistics",
      surge_planning = "Surge Planning",
      risk_assessment = "Risk & Hazard Identification",
      continuity = "Continuity of Operations",
      governance = "Governance & Documentation"
    )
    pretty <- vapply(
      parts,
      function(p) {
        key <- tolower(p)
        label_map[[key]] %||% stringr::str_to_title(gsub("_", " ", key))
      },
      character(1)
    )
    paste(pretty, collapse = ", ")
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
    llm_note <- if (!is.null(app_config$llm_api_key) &&
      nzchar(app_config$llm_api_key) &&
      !is.null(app_config$llm_api_base) &&
      nzchar(app_config$llm_api_base)) {
      paste0(" Scoring uses an LLM rubric-based review of plan strength (model: ", app_config$llm_model, ").")
    } else {
      " Scoring uses rule-based signals from the plan text."
    }
    if (tolower(country) != "united states") {
      tags$p(
        class = "hint",
        paste0("Scorecard uses the 4-domain model. Framework mapping shown below uses WHO ERF for non-U.S. plans.", llm_note)
      )
    } else {
      applicability <- if (input$facility_type %in% c("Public Health Department", "Emergency Management Agency")) {
        ""
      } else {
        " Note: CDC PHEP is designed for public health agencies; applicability may be limited for other facility types."
      }
      tags$p(
        class = "hint",
        paste0("Scorecard uses the 4-domain model. Framework mapping shown below uses CDC PHEP or FEMA core capabilities.", applicability, llm_note)
      )
    }
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
    non_us <- !is.null(country_value()) && tolower(country_value()) != "united states"
    if (non_us) {
      tagList(
        tags$p(class = "hint", "Framework mapping is fixed to WHO ERF for non-U.S. plans."),
        tags$input(type = "hidden", id = "framework_view", value = "WHO ERF"),
        selectInput(
          "domain_view",
          "Domain",
          choices = c("Command", "Communication", "Operations", "Logistics"),
          selected = "Command"
        )
      )
    } else {
      tagList(
        selectInput(
          "framework_view",
          "Framework mapping",
          choices = c("CDC PHEP", "FEMA"),
          selected = "CDC PHEP"
        ),
        selectInput(
          "domain_view",
          "Domain",
          choices = c("Command", "Communication", "Operations", "Logistics"),
          selected = "Command"
        )
      )
    }
  })

  output$framework_details <- renderUI({
    framework_key <- if (!is.null(input$framework_view) && input$framework_view == "WHO ERF") {
      "who_erf"
    } else if (!is.null(input$framework_view) && input$framework_view == "FEMA") {
      "fema"
    } else {
      "cdc_phep"
    }
    domain_key <- input$domain_view %||% "Command"
    mapping <- framework_domain_map[[framework_key]] %||% list()
    categories <- mapping[[domain_key]] %||% character(0)

    tags$div(
      class = "scorecard-details",
      tags$div(
        class = "framework-card",
        h3(paste(domain_key, "mapping")),
        if (length(categories) > 0) {
          tags$ul(lapply(categories, tags$li))
        } else {
          tags$p("No mapped categories available.")
        }
      )
    )
  })

  output$gap_list <- renderUI({
    format_gap_list(analysis_result()$gaps)
  })

  output$action_plan <- renderUI({
    format_action_plan(analysis_result()$action_plan)
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

  output$fema_table <- renderTable({
    pretty_fema_table(analysis_result()$regional$fema$table)
  }, rownames = FALSE)

  output$fema_note <- renderUI({
    note <- analysis_result()$regional$fema$note
    if (!is.null(note) && nzchar(note)) tags$p(class = "hint", note)
  })

  output$cdc_table <- renderTable({
    pretty_cdc_table(analysis_result()$regional$cdc$table)
  }, rownames = FALSE)

  output$cdc_note <- renderUI({
    note <- analysis_result()$regional$cdc$note
    if (!is.null(note) && nzchar(note)) tags$p(class = "hint", note)
  })

  output$coverage_table <- renderTable({
    df <- pretty_coverage_table(analysis_result()$regional$coverage)
    if (!is.null(df) && nrow(df) > 0) {
      df$Domains <- vapply(df$Domains, pretty_domains, character(1))
    }
    df
  }, rownames = FALSE)

  output$hazard_priority_table <- renderTable({
    df <- analysis_result()$regional$hazard_priority
    if (is.null(df) || nrow(df) == 0) return(df)
    df |>
      dplyr::rename(
        Hazard = hazard_label,
        Source = source,
        Frequency = frequency,
        `Recency (years)` = recency_years,
        Severity = severity,
        Priority = priority,
        `Priority Score` = priority_score
      )
  }, rownames = FALSE)

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

  output$hazard_priority_note <- renderUI({
    res <- analysis_result()$regional
    hazard_df <- res$hazard_priority
    if (!is.null(hazard_df) && nrow(hazard_df) > 0) return(NULL)
    fema_n <- nrow(res$fema$table %||% tibble::tibble())
    cdc_n <- nrow(res$cdc$table %||% tibble::tibble())
    outbreak_n <- nrow(res$outbreaks$table %||% tibble::tibble())
    tags$p(
      class = "hint",
      paste0(
        "No hazards found. FEMA rows: ", fema_n,
        "; CDC rows: ", cdc_n,
        "; Outbreak rows: ", outbreak_n,
        ". Check filters or year window."
      )
    )
  })

  output$hazard_domain_table <- renderTable({
    df <- analysis_result()$regional$hazard_domain_coverage
    if (is.null(df) || nrow(df) == 0) return(df)
    df |>
      dplyr::rename(
        Hazard = hazard,
        Priority = priority,
        Source = source,
        Domain = domain,
        Coverage = status,
        Rationale = rationale
      )
  }, rownames = FALSE)

  output$outbreaks_table <- renderTable({
    pretty_outbreaks_table(analysis_result()$regional$outbreaks$table)
  }, rownames = FALSE)

  output$outbreaks_note <- renderUI({
    note <- analysis_result()$regional$outbreaks$note
    if (!is.null(note) && nzchar(note)) tags$p(class = "hint", note)
  })

  output$regional_gap <- renderUI({
    format_regional_gap(analysis_result()$regional$gap)
  })

  output$hazard_analysis <- renderUI({
    format_hazard_analysis(analysis_result()$regional$hazard_eval)
  })

  output$domain_toggle_button <- renderUI({
    label <- if (show_domain_defs()) "Hide Domain Definitions" else "Show Domain Definitions"
    actionButton("toggle_domains", label)
  })

  output$domain_legend <- renderUI({
    if (!show_domain_defs()) return(NULL)
    legend_items <- list(
      list(
        label = "Communication & Coordination",
        detail = "Incident communication protocols, leadership roles, external coordination, contact lists."
      ),
      list(
        label = "Workforce Capacity",
        detail = "Surge staffing, role reassignments, just-in-time training, staff wellbeing support."
      ),
      list(
        label = "Supply Chain & Logistics",
        detail = "Critical supply inventory, vendor diversification, medication management, distribution plans."
      ),
      list(
        label = "Surge Planning",
        detail = "Bed capacity expansion, triage flow, alternate care sites, discharge acceleration."
      ),
      list(
        label = "Risk & Hazard Identification",
        detail = "Hazard identification, scenario assumptions, vulnerability analysis, risk updates."
      ),
      list(
        label = "Continuity of Operations",
        detail = "Essential services continuity, IT resilience, utilities contingencies, recovery planning."
      ),
      list(
        label = "Governance & Documentation",
        detail = "Plan ownership, approval workflow, drills, after-action reviews, update cadence."
      )
    )

    tagList(
      tags$p(class = "hint", "Domain definitions:"),
      tags$ul(
        lapply(legend_items, function(item) {
          tags$li(tags$strong(item$label), ": ", item$detail)
        })
      )
    )
  })
}

shinyApp(ui, server)
