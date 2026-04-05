format_gap_list <- function(gaps) {
  if (length(gaps) == 0) {
    return(tags$p("No gaps identified."))
  }

  if (is.character(gaps)) {
    return(tagList(
      lapply(gaps, function(gap) {
        div(
          class = "gap-card",
          h3(gap),
          p("Flagged by deterministic rule checks.")
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

format_action_plan <- function(actions) {
  if (length(actions) == 0) {
    return(tags$p("No recommendations available."))
  }

  if (is.character(actions)) {
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
      div(
        class = "action-card",
        h3(action$domain),
        p(strong("Priority: "), action$priority),
        p(strong("Timeframe: "), action$timeframe),
        p(action$recommendation)
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
