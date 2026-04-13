format_gap_list <- function(gaps) {
  if (length(gaps) == 0) {
    return(tags$p("No gaps identified."))
  }

  if (is.character(gaps)) {
    detail_map <- c(
      "No Incident Command System defined" = "No ICS/incident command structure was detected in the plan text (e.g., command roles, chain of command, activation authority).",
      "Insufficient training and drills" = "Training and exercise references appear minimal (fewer than two mentions), suggesting limited preparedness cadence.",
      "No backup power system identified" = "No backup power or generator capability was identified in the plan text."
    )
    return(tagList(
      lapply(gaps, function(gap) {
        div(
          class = "gap-card",
          h3(gap),
          p(detail_map[[gap]] %||% "Flagged because the plan text did not contain the expected indicators for this requirement.")
        )
      })
    ))
  }

  tagList(
    lapply(gaps, function(gap) {
      div(
        class = "gap-card",
        h3(gap$domain),
        p(strong("Status: "), gap$status),
        p(strong("Evidence: "), gap$evidence),
        p(strong("Key Components: "), paste(gap$components, collapse = ", "))
      )
    })
  )
}

join_items <- function(items) {
  items <- items[nzchar(items)]
  n <- length(items)
  if (n == 0) return("")
  if (n == 1) return(items[1])
  if (n == 2) return(paste(items[1], "and", items[2]))
  paste0(paste(items[1:(n - 1)], collapse = ", "), ", and ", items[n])
}

format_exec_summary_narrative <- function(
  facility_type,
  emergency_focus,
  final_score,
  scorecard,
  gaps,
  actions
) {
  top_domain <- "N/A"
  lowest_domain <- "N/A"
  if (!is.null(scorecard) && nrow(scorecard) > 0 && "score" %in% names(scorecard)) {
    top_domain <- scorecard |>
      arrange(desc(score)) |>
      slice_head(n = 1) |>
      pull(domain)
    lowest_domain <- scorecard |>
      arrange(score) |>
      slice_head(n = 1) |>
      pull(domain)
  }

  score_text <- if (!is.null(final_score)) {
    paste0("Preparedness score: ", round(final_score, 1))
  } else {
    "Preparedness score: unavailable"
  }

  readiness_text <- if (!is.null(final_score) && final_score >= 80) {
    "Readiness is strong."
  } else if (!is.null(final_score) && final_score >= 50) {
    "Readiness is moderate with clear improvement needs."
  } else if (!is.null(final_score)) {
    "Readiness is low and needs significant improvement."
  } else {
    "Readiness could not be assessed."
  }

  domain_summary <- paste0(
    if (!is.na(top_domain) && top_domain != "N/A") {
      paste0("Strongest: ", top_domain, ". ")
    } else {
      ""
    },
    if (!is.na(lowest_domain) && lowest_domain != "N/A") {
      paste0("Needs improvement: ", lowest_domain, ".")
    } else {
      ""
    }
  )

  key_components <- character(0)
  if (length(gaps) == 0) {
    key_components <- character(0)
  } else if (is.character(gaps)) {
    key_components <- gaps
  } else {
    component_lists <- lapply(gaps, function(gap) gap$components %||% character(0))
    key_components <- unique(unlist(component_lists))
    if (length(key_components) == 0) {
      key_components <- vapply(gaps, function(gap) gap$domain %||% "Domain", character(1))
    }
  }
  key_components <- head(key_components[nzchar(key_components)], 5)

  tagList(
    tags$p(paste(
      "For the", facility_type, "(", tolower(emergency_focus), "focus).",
      score_text, readiness_text, domain_summary
    )),
    if (length(key_components) > 0) {
      tagList(
        tags$p(strong("Key components to strengthen:")),
        tags$ul(lapply(key_components, tags$li))
      )
    }
  )
}

format_action_plan <- function(actions) {
  if (length(actions) == 0) {
    return(tags$p("No recommendations available."))
  }

  if (is.character(actions) || !is.list(actions)) {
    if (!is.character(actions)) actions <- as.character(actions)
    short <- head(actions[nzchar(actions)], 5)
    return(tags$ul(lapply(short, tags$li)))
  }

  items <- lapply(actions, function(action) {
    if (!is.list(action)) return(as.character(action))
    rec <- action$recommendation %||% ""
    if (!nzchar(rec)) return("")
    prefix <- action$priority %||% ""
    if (nzchar(prefix)) {
      paste0(prefix, ": ", rec)
    } else {
      rec
    }
  })
  items <- head(items[nzchar(items)], 5)
  if (length(items) == 0) {
    return(tags$p("No recommendations available."))
  }
  tags$ul(lapply(items, tags$li))
}

format_regional_gap <- function(regional_gap) {
  if (is.null(regional_gap) || length(regional_gap) == 0 || !is.list(regional_gap)) {
    return(tags$p("Regional gap interpretation unavailable."))
  }

  summary <- regional_gap$summary %||% "Regional gap interpretation unavailable."
  insights <- regional_gap$gap_insights %||% list()
  focus <- regional_gap$recommended_focus %||% list()

  tagList(
    h3("Gap Interpretation"),
    p(summary),
    if (length(insights) > 0) {
      tagList(
        h4("Key Gaps"),
        tagList(lapply(insights, function(item) {
          div(
            class = "gap-card",
            p(strong(item$pattern %||% "Pattern")),
            p(item$impact %||% "")
          )
        }))
      )
    },
    if (length(focus) > 0) {
      tagList(
        h4("Recommended Focus Areas"),
        tags$ul(lapply(focus, tags$li))
      )
    }
  )
}

format_hazard_analysis <- function(hazard_eval) {
  if (is.null(hazard_eval) || length(hazard_eval) == 0 || !is.list(hazard_eval)) {
    return(tags$p("Hazard prioritization unavailable for the selected region."))
  }

  label_map <- c(
    communication = "Communication & Coordination",
    workforce = "Workforce Capacity",
    supply_chain = "Supply Chain & Logistics",
    surge_planning = "Surge Planning",
    risk_assessment = "Risk & Hazard Identification",
    continuity = "Continuity of Operations",
    governance = "Governance & Documentation"
  )

  tagList(
    lapply(hazard_eval, function(h) {
      if (!is.list(h)) return(NULL)
      observed <- h$observed %||% list()
      expected_domains <- vapply(
        h$expected_domains %||% list(),
        function(d) label_map[[d]] %||% stringr::str_to_title(gsub("_", " ", d)),
        character(1)
      )
      tagList(
        div(
          class = "hazard-card",
          h3(paste("Hazard:", h$hazard %||% "Unknown")),
          p(strong("Priority: "), h$priority %||% "Unknown"),
          p(strong("Source: "), h$source %||% "Unknown"),
          p(strong("Expected Domains:")),
          tags$ul(lapply(expected_domains, tags$li)),
          p(strong("Observed Coverage:")),
          tags$ul(
            lapply(observed, function(item) {
              if (!is.list(item)) return(tags$li("Coverage unavailable."))
              tags$li(
                strong((item$label %||% item$domain_id %||% "Domain")),
                ": ",
                (item$status %||% "unknown"),
                " (",
                (item$rationale %||% "no rationale"),
                if (nzchar(item$evidence %||% "")) paste0("; ", item$evidence) else "",
                ")"
              )
            })
          ),
          if (!is.null(h$recommendations) && length(h$recommendations) > 0) {
            tagList(
              p(strong("Recommendations:")),
              tags$ul(lapply(h$recommendations, tags$li))
            )
          }
        )
      )
    })
  )
}

format_hazard_recommendations <- function(recs) {
  if (is.null(recs) || length(recs) == 0 || !is.list(recs)) {
    return(tags$p("No hazard-based recommendations available."))
  }

  items <- recs$recommendations %||% character(0)
  items <- head(items[nzchar(items)], 6)
  if (length(items) == 0) {
    return(tags$p("No hazard-based recommendations available."))
  }
  tags$ul(lapply(items, tags$li))
}
