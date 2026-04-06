library(httr)

call_llm_json <- function(system_prompt, user_prompt, schema_hint = NULL) {
  if (app_config$llm_api_key == "" || app_config$llm_api_base == "") {
    return(mock_llm_response(schema_hint))
  }

  provider <- tolower(app_config$llm_provider %||% "openai_compatible")
  if (provider %in% c("gemini", "google", "google_gemini", "generativelanguage")) {
    return(call_gemini_json(system_prompt, user_prompt, schema_hint = schema_hint))
  }

  model_name <- app_config$llm_model
  if (is.null(model_name) ||
      length(model_name) == 0 ||
      is.na(model_name) ||
      !nzchar(model_name) ||
      tolower(model_name) %in% c("none", "null", "nil", "na")) {
    model_name <- "gpt-5-mini"
  }

  base_url <- sub("/v1/?$", "", app_config$llm_api_base)
  request_body <- list(
    model = model_name,
    temperature = app_config$llm_temperature,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    )
  )

  response <- POST(
    url = paste0(base_url, "/v1/chat/completions"),
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

call_gemini_json <- function(system_prompt, user_prompt, schema_hint = NULL) {
  base_url <- sub("/+$", "", app_config$llm_api_base)

  model_name <- app_config$llm_model
  if (is.null(model_name) ||
      length(model_name) == 0 ||
      is.na(model_name) ||
      !nzchar(model_name) ||
      tolower(model_name) %in% c("none", "null", "nil", "na")) {
    model_name <- "gemini-1.5-flash"
  }
  model_name <- sub("^models/", "", model_name)

  merged_prompt <- paste(
    system_prompt,
    "",
    user_prompt,
    "",
    "Return JSON only.",
    sep = "\n"
  )

  request_body <- list(
    contents = list(
      list(
        role = "user",
        parts = list(list(text = merged_prompt))
      )
    ),
    generationConfig = list(
      temperature = app_config$llm_temperature
    )
  )

  response <- POST(
    url = paste0(base_url, "/v1beta/models/", model_name, ":generateContent"),
    query = list(key = app_config$llm_api_key),
    encode = "json",
    body = request_body,
    timeout(app_config$llm_timeout)
  )

  if (http_error(response)) {
    stop("LLM request failed: ", content(response, as = "text"))
  }

  parsed <- content(response, as = "parsed", type = "application/json")
  if (!is.list(parsed)) {
    raw_text <- content(response, as = "text")
    parsed <- tryCatch(
      jsonlite::fromJSON(raw_text, simplifyVector = FALSE),
      error = function(e) NULL
    )
  }
  if (is.null(parsed) || !is.list(parsed)) {
    return(mock_llm_response(schema_hint))
  }

  candidates <- parsed$candidates %||% list()
  if (length(candidates) == 0 || is.null(candidates[[1]]$content$parts)) {
    return(mock_llm_response(schema_hint))
  }

  parts <- candidates[[1]]$content$parts
  if (is.null(parts) || length(parts) == 0) {
    return(mock_llm_response(schema_hint))
  }
  message_content <- paste(vapply(parts, function(part) part$text %||% "", ""), collapse = "")
  safe_parse_json(message_content, schema_hint = schema_hint)
}

assess_plan_domains <- function(plan_text, facility_type, emergency_focus) {
  prompt <- paste(
    "You are an emergency preparedness reviewer.",
    "Evaluate the plan using four domains: Command, Communication, Operations, Logistics.",
    "Score each domain on a 1–5 scale (1=Absent, 2=Weak, 3=Partial, 4=Strong, 5=Very Strong).",
    "Provide short evidence snippets and a brief rationale for each domain.",
    "List the top gaps and recommended actions.",
    "Return JSON only in the schema:",
    "{\"domains\":[{\"id\":\"command\",\"label\":\"Command\",\"score\":1-5,\"evidence\":\"...\",\"rationale\":\"...\",\"gaps\":[\"...\"],\"strengths\":[\"...\"]}],\"overall_summary\":\"...\",\"recommended_actions\":[{\"domain_id\":\"command\",\"priority\":\"High|Medium|Low\",\"timeframe\":\"Immediate|Short-Term|Long-Term\",\"recommendation\":\"...\"}]}",
    "\nFacility type:", facility_type,
    "\nEmergency focus:", emergency_focus,
    "\nPlan text:\n", plan_text
  )

  call_llm_json(
    system_prompt = "You review emergency preparedness plans and produce structured, evidence-based scoring.",
    user_prompt = prompt,
    schema_hint = "domain_scoring"
  )
}

safe_parse_json <- function(text, schema_hint = NULL) {
  cleaned <- trimws(text)
  cleaned <- sub("^```json", "", cleaned)
  cleaned <- sub("^```", "", cleaned)
  cleaned <- sub("```$", "", cleaned)

  parsed <- tryCatch(
    fromJSON(cleaned, simplifyVector = TRUE),
    error = function(e) {
      NULL
    }
  )

  if (is.null(parsed) || !is.list(parsed)) {
    return(mock_llm_response(schema_hint))
  }

  parsed
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

  if (identical(schema_hint, "domain_scoring")) {
    return(list(
      domains = list(
        list(
          id = "command",
          label = "Command",
          score = 3,
          evidence = "Incident command roles are referenced, but activation criteria are unclear.",
          rationale = "Roles are mentioned without clear authority or escalation paths.",
          gaps = c("Define activation triggers and chain of command."),
          strengths = c("Basic incident command roles are mentioned.")
        ),
        list(
          id = "communication",
          label = "Communication",
          score = 2,
          evidence = "Limited references to communication protocols or public messaging.",
          rationale = "Communication appears ad hoc with no clear cadence or channels.",
          gaps = c("Add internal/external communication protocols and contact redundancy."),
          strengths = c("Some coordination language appears.")
        ),
        list(
          id = "operations",
          label = "Operations",
          score = 3,
          evidence = "Operational response steps are outlined at a high level.",
          rationale = "Procedures exist but lack detailed workflows.",
          gaps = c("Expand operational procedures and resource triggers."),
          strengths = c("Response actions are mentioned.")
        ),
        list(
          id = "logistics",
          label = "Logistics",
          score = 2,
          evidence = "Supplies and continuity resources are minimally described.",
          rationale = "Logistics planning is limited and lacks redundancy details.",
          gaps = c("Document supply chain redundancy and backup power."),
          strengths = c("Basic resource needs are listed.")
        )
      ),
      overall_summary = "The plan includes core elements but lacks depth in communication and logistics.",
      recommended_actions = list(
        list(
          domain_id = "communication",
          priority = "High",
          timeframe = "Immediate",
          recommendation = "Define communication protocols, roles, and contact redundancy."
        ),
        list(
          domain_id = "logistics",
          priority = "High",
          timeframe = "Short-Term",
          recommendation = "Add backup power and supply chain continuity procedures."
        )
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
