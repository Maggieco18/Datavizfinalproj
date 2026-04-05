library(httr)

call_llm_json <- function(system_prompt, user_prompt, schema_hint = NULL) {
  if (app_config$llm_api_key == "" || app_config$llm_api_base == "") {
    return(mock_llm_response(schema_hint))
  }

  request_body <- list(
    model = app_config$llm_model,
    temperature = app_config$llm_temperature,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    )
  )

  response <- POST(
    url = paste0(app_config$llm_api_base, "/v1/chat/completions"),
    add_headers(Authorization = paste("Bearer", app_config$llm_api_key)),
    encode = "json",
    body = request_body,
    timeout(app_config$llm_timeout)
  )

  if (http_error(response)) {
    stop("LLM request failed: ", content(response, as = "text"))
  }

  parsed <- content(response, as = "parsed", type = "application/json")
  message_content <- parsed$choices[[1]]$message$content
  safe_parse_json(message_content, schema_hint = schema_hint)
}

safe_parse_json <- function(text, schema_hint = NULL) {
  cleaned <- str_trim(text)
  cleaned <- str_replace(cleaned, "^```json", "")
  cleaned <- str_replace(cleaned, "^```", "")
  cleaned <- str_replace(cleaned, "```$", "")

  tryCatch(
    fromJSON(cleaned, simplifyVector = TRUE),
    error = function(e) {
      mock_llm_response(schema_hint)
    }
  )
}

mock_llm_response <- function(schema_hint = NULL) {
  if (identical(schema_hint, "extraction")) {
    return(list(
      domains = list(
        list(
          id = "communication",
          status = "partial",
          evidence = "Roles are mentioned but escalation and external coordination are not explicit."
        ),
        list(
          id = "workforce",
          status = "missing",
          evidence = "No surge staffing or cross-training protocols described."
        ),
        list(
          id = "supply_chain",
          status = "partial",
          evidence = "Inventory listed without vendor diversification or distribution plan."
        ),
        list(
          id = "surge_planning",
          status = "partial",
          evidence = "Triage process outlined, but no alternate care sites or discharge plan."
        ),
        list(
          id = "risk_assessment",
          status = "missing",
          evidence = "No documented hazard identification or vulnerability assessment found."
        ),
        list(
          id = "continuity",
          status = "missing",
          evidence = "No continuity of operations or IT resilience plan specified."
        ),
        list(
          id = "governance",
          status = "partial",
          evidence = "Plan ownership noted, but drills and review cadence absent."
        )
      )
    ))
  }

  if (identical(schema_hint, "recommendations")) {
    return(list(
      executive_summary = paste(
        "The preparedness plan contains initial operational procedures but lacks",
        "surge staffing, continuity planning, and governance cadence. Addressing",
        "these gaps will improve readiness and resilience."
      ),
      recommendations = list(
        list(
          domain_id = "workforce",
          priority = "High",
          timeframe = "Immediate",
          recommendation = "Develop a surge staffing protocol with role reassignments and just-in-time training."
        ),
        list(
          domain_id = "continuity",
          priority = "High",
          timeframe = "Immediate",
          recommendation = "Create continuity procedures for essential services, IT, and utility disruptions."
        ),
        list(
          domain_id = "governance",
          priority = "Medium",
          timeframe = "Short-Term",
          recommendation = "Institute quarterly drills and an after-action review workflow tied to plan updates."
        ),
        list(
          domain_id = "risk_assessment",
          priority = "Medium",
          timeframe = "Short-Term",
          recommendation = "Document a hazard identification and vulnerability assessment to guide objectives and resource planning."
        )
      )
    ))
  }

  if (identical(schema_hint, "regional_gap")) {
    return(list(
      summary = paste(
        "Local incident patterns show recurring natural hazard declarations and",
        "ongoing infectious disease activity. The plan partially addresses these",
        "drivers but is weakest in continuity, surge planning, and workforce depth."
      ),
      gap_insights = list(
        list(
          pattern = "High-frequency storm and flooding declarations",
          coverage = "partial",
          impact = "Limited continuity and logistics detail could slow service restoration."
        ),
        list(
          pattern = "Notifiable disease activity across multiple conditions",
          coverage = "missing",
          impact = "Insufficient surge staffing and supply chain planning for outbreaks."
        )
      ),
      recommended_focus = c(
        "Expand continuity of operations procedures for utility and facility disruptions.",
        "Formalize surge staffing and just-in-time training for infectious disease events.",
        "Strengthen risk assessment updates tied to regional hazard trends."
      )
    ))
  }

  list()
}

polish_executive_summary <- function(base_summary, facility_type, emergency_focus, domain_scores, final_score, gaps, actions) {
  if (app_config$llm_api_key == "" || app_config$llm_api_base == "") {
    return(base_summary)
  }

  score_tbl <- tryCatch(
    toJSON(domain_scores, auto_unbox = TRUE, pretty = TRUE),
    error = function(e) "[]"
  )

  prompt <- paste(
    "You are polishing an executive summary for a consulting deliverable.",
    "Keep it concise, factual, and aligned to the deterministic findings.",
    "Do not introduce new gaps or recommendations.",
    "Return JSON only in the schema: {\"executive_summary\":\"...\"}.",
    "\nContext:",
    paste("Facility type:", facility_type),
    paste("Emergency focus:", emergency_focus),
    paste("Final score:", round(final_score, 1)),
    "\nDomain scores JSON:",
    score_tbl,
    "\nGaps:",
    if (length(gaps) == 0) "None" else paste(gaps, collapse = "; "),
    "\nActions:",
    if (length(actions) == 0) "None" else paste(actions, collapse = "; "),
    "\nBase summary:",
    base_summary
  )

  response <- call_llm_json(
    system_prompt = "You polish consulting executive summaries without changing facts.",
    user_prompt = prompt,
    schema_hint = "executive_summary"
  )

  if (!is.null(response$executive_summary)) response$executive_summary else base_summary
}
