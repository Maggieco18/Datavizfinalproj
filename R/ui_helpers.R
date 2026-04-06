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
    paste0("the overall preparedness score is ", round(final_score, 1))
  } else {
    "the overall preparedness score is unavailable"
  }

  base_sentence <- paste0(
    "For the ", facility_type, " with a ", tolower(emergency_focus),
    " emergency focus, ", score_text, ", indicating that ",
    if (!is.null(final_score) && final_score >= 80) {
      "current preparedness is strong."
    } else if (!is.null(final_score) && final_score >= 50) {
      "there are moderate improvement needs."
    } else if (!is.null(final_score)) {
      "substantial improvement is needed."
    } else {
      "an overall assessment could not be completed."
    }
  )

  domain_sentence <- paste0(
    if (!is.na(top_domain) && top_domain != "N/A") {
      paste0(" The strongest area is ", top_domain, ".")
    } else {
      ""
    },
    if (!is.na(lowest_domain) && lowest_domain != "N/A") {
      paste0(" The area needing the most improvement is ", lowest_domain, ".")
    } else {
      ""
    }
  )

  gap_sentence <- NULL
  if (length(gaps) == 0) {
    gap_sentence <- "No major gaps were identified based on the current rule checks."
  } else if (is.character(gaps)) {
    detail_map <- c(
      "No Incident Command System defined" = "This suggests leadership roles and activation authority are unclear, which can slow decision-making during escalation.",
      "Insufficient training and drills" = "This raises the risk of role confusion and inconsistent execution during a real event.",
      "No backup power system identified" = "This creates vulnerability for critical operations, IT systems, and life-safety equipment during outages."
    )
    sentences <- vapply(
      gaps,
      function(gap) {
        detail <- detail_map[[gap]]
        if (is.null(detail)) {
          detail <- "This indicates a weakness that could hinder coordinated response."
        }
        paste0(gap, ". ", detail)
      },
      character(1)
    )
    gap_sentence <- paste0("Key gaps include: ", paste(sentences, collapse = " "))
  } else {
    sentences <- vapply(
      gaps,
      function(gap) {
        domain <- gap$domain %||% "Domain"
        status <- gap$status %||% "unknown status"
        evidence <- gap$evidence %||% "limited evidence"
        components <- gap$components %||% character(0)
        component_text <- if (length(components) > 0) {
          paste0(" Components to strengthen: ", paste(components, collapse = ", "), ".")
        } else {
          ""
        }
        paste0(domain, " is currently ", status, ". Evidence includes ", evidence, ".", component_text)
      },
      character(1)
    )
    gap_sentence <- paste0("Key gaps include: ", paste(sentences, collapse = " "))
  }

  action_sentence <- NULL
  if (length(actions) == 0) {
    action_sentence <- "No immediate corrective actions were triggered by the rule set."
  } else if (is.character(actions)) {
    action_sentence <- paste0("Immediate priorities are to ", join_items(actions), ".")
  } else {
    if (!is.list(actions)) {
      action_sentence <- paste0("Immediate priorities are to ", join_items(as.character(actions)), ".")
      return(tags$p(paste(base_sentence, domain_sentence, gap_sentence, action_sentence)))
    }
    action_sentence <- paste0(
      "Immediate priorities are to ",
      join_items(vapply(actions, function(a) if (is.list(a)) a$recommendation %||% "" else "", character(1))),
      "."
    )
  }

  tags$p(paste(base_sentence, domain_sentence, gap_sentence, action_sentence))
}

format_action_plan <- function(actions) {
  if (length(actions) == 0) {
    return(tags$p("No recommendations available."))
  }

  if (is.character(actions) || !is.list(actions)) {
    if (!is.character(actions)) actions <- as.character(actions)
    return(tagList(
      lapply(actions, function(action) {
        div(
          class = "action-card",
          h3("Recommended Action"),
          p(action)
        )
      })
    ))
  }

  tagList(
    lapply(actions, function(action) {
      if (!is.list(action)) {
        return(div(
          class = "action-card",
          h3("Recommended Action"),
          p(as.character(action))
        ))
      }
      div(
        class = "action-card",
        h3(action$domain %||% "Recommended Action"),
        p(strong("Priority: "), action$priority %||% "Unknown"),
        p(strong("Timeframe: "), action$timeframe %||% "Unspecified"),
        p(action$recommendation %||% "")
      )
    })
  )
}

format_regional_gap <- function(regional_gap) {
  if (is.null(regional_gap) || length(regional_gap) == 0) {
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
            p(strong("Coverage: "), item$coverage %||% "unknown"),
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
  if (is.null(hazard_eval) || length(hazard_eval) == 0) {
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
