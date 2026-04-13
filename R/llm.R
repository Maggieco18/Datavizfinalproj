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
    fallback <- mock_llm_response(schema_hint)
    attr(fallback, "llm_fallback_notice") <- "LLM unavailable, using fallback."
    return(fallback)
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
    fallback <- mock_llm_response(schema_hint)
    attr(fallback, "llm_fallback_notice") <- "LLM unavailable, using fallback."
    return(fallback)
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
    fallback <- mock_llm_response(schema_hint)
    attr(fallback, "llm_fallback_notice") <- "LLM unavailable, using fallback."
    return(fallback)
  }

  candidates <- parsed$candidates %||% list()
  if (length(candidates) == 0 || is.null(candidates[[1]]$content$parts)) {
    fallback <- mock_llm_response(schema_hint)
    attr(fallback, "llm_fallback_notice") <- "LLM unavailable, using fallback."
    return(fallback)
  }

  parts <- candidates[[1]]$content$parts
  if (is.null(parts) || length(parts) == 0) {
    fallback <- mock_llm_response(schema_hint)
    attr(fallback, "llm_fallback_notice") <- "LLM unavailable, using fallback."
    return(fallback)
  }
  message_content <- paste(vapply(parts, function(part) part$text %||% "", ""), collapse = "")
  safe_parse_json(message_content, schema_hint = schema_hint)
}

build_domain_definition_text <- function(domain_ids) {
  if (is.null(preparedness_framework$domains)) return("")
  entries <- lapply(preparedness_framework$domains, function(d) {
    list(
      id = d$id %||% "",
      label = d$label %||% "",
      components = d$components %||% character(0)
    )
  })
  entries <- entries[vapply(entries, function(e) e$id %in% domain_ids, logical(1))]
  if (length(entries) == 0) return("")

  paste(
    vapply(entries, function(e) {
      paste0(
        "- ", e$label, " (", e$id, "): ",
        paste(e$components, collapse = "; ")
      )
    }, character(1)),
    collapse = "\n"
  )
}

assess_hazard_coverage <- function(plan_text, hazard_label, hazard_type, expected_domains) {
  # LLM-based semantic classification of plan coverage for hazard-specific domains.
  domain_definitions <- build_domain_definition_text(expected_domains)

  prompt <- paste(
    "You are evaluating an emergency preparedness plan for hazard-specific coverage.",
    "Hazard:", hazard_label,
    "Hazard type:", hazard_type,
    "Expected domains:", paste(expected_domains, collapse = ", "),
    "Domain definitions:",
    domain_definitions,
    "Classify each expected domain as Present, Partial, or Missing based on the plan text.",
    "Present = clearly addressed with specific actions.",
    "Partial = mentioned but vague or not hazard-specific.",
    "Missing = not addressed.",
    "For each domain, provide a short rationale and one targeted recommendation if status is Partial or Missing.",
    "Return JSON only in this schema:",
    "{\"hazard\":\"...\",\"observed\":[{\"domain_id\":\"communication\",\"label\":\"Communication & Coordination\",\"status\":\"present|partial|missing\",\"rationale\":\"...\",\"evidence\":\"...\",\"recommendation\":\"...\"}]}",
    "\nPlan text:\n",
    plan_text
  )

  call_llm_json(
    system_prompt = "You are a preparedness analyst producing structured hazard-by-domain coverage.",
    user_prompt = prompt,
    schema_hint = "hazard_coverage"
  )
}

assess_plan_domains <- function(plan_text, facility_type, emergency_focus, framework_choice = "CDC PHEP") {
  framework_label <- framework_choice %||% "CDC PHEP"
  framework_text <- if (framework_label == "WHO ERF") {
    "Align scoring to WHO Emergency Response Framework (ERF) and incident management best practices."
  } else if (framework_label == "FEMA") {
    "Align scoring to FEMA NIMS and core capability expectations."
  } else {
    "Align scoring to CDC PHEP capability expectations."
  }

  prompt <- paste(
    "You are an expert emergency preparedness reviewer trained in FEMA NIMS, CDC PHEP, and WHO Emergency Response Framework (ERF) principles.",
    
    "Evaluate the plan using four domains: Command, Communication, Operations, Logistics.",
    
    "Use the following domain definitions:",
    "Command: Incident Command System (ICS) structure, leadership roles, authority, decision-making hierarchy, and succession.",
    "Communication: Internal and external communication systems, public information strategy, redundancy, and information-sharing (e.g., JIS/JIC alignment).",
    "Operations: Response actions, coordination mechanisms, Emergency Support Function (ESF) alignment, and operational execution.",
    "Logistics: Resource management, personnel, facilities, supply chains, and support services.",
    
    "Score each domain on a 1–5 scale:",
    "1 = Absent (no evidence of capability)",
    "2 = Weak (minimal, vague, or poorly defined elements)",
    "3 = Partial (some elements present but incomplete or inconsistent)",
    "4 = Strong (well-defined and mostly complete)",
    "5 = Very Strong (comprehensive, clearly defined, and aligned with best practices)",
    
    framework_text,
    "If the plan is not designed for the selected framework, evaluate its alignment gaps explicitly.",
    "Evaluate appropriately for the plan type (e.g., strategic framework vs operational plan). Do not penalize lack of tactical detail if the document is intended to be high-level.",
    
    "For each domain, provide detailed and traceable analysis:",
    
    "- Provide a score (integer 1–5 only).",
    
    "- Provide evidence:",
    "  * Include 2–4 specific references from the document",
    "  * For each reference include:",
    "    - section name or heading if available",
    "    - description of where it appears (e.g., 'Concept of Operations section', 'Logistics Section')",
    "    - direct quote or close paraphrase",
    "  * If no evidence exists, explicitly state 'No evidence found'",
    
    "- Provide a detailed rationale:",
    "  * Minimum 4–6 sentences",
    "  * Explicitly compare what is present in the plan vs what is expected under FEMA NIMS, CDC PHEP, and WHO ERF",
    "  * Explain why the assigned score is appropriate",
    "  * Clearly explain why the score is not higher",
    "  * Identify whether limitations are due to missing detail, missing structure, or lack of alignment with best practices",
    
    "- List key strengths:",
    "  * Include specific, concrete elements from the plan",
    "  * Avoid vague or generic statements",
    
    "- List specific gaps:",
    "  * Identify missing or underdeveloped components",
    "  * Focus on coordination, life safety, and operational effectiveness",
    
    "When identifying gaps, prioritize issues that impact coordination, life safety, or operational effectiveness.",
    
    "Prioritize citing operationally relevant sections (e.g., Concept of Operations, roles/responsibilities, ESFs, logistics, communications). Avoid over-weighting introductory or descriptive sections.",
    
    "List the top gaps across all domains and recommended actions.",
    
    "For recommended actions:",
    "- High priority: critical gaps that could significantly impair emergency response",
    "- Medium priority: important improvements that strengthen capability",
    "- Low priority: enhancements or optimizations",
    "- Timeframe definitions:",
    "  Immediate = urgent or quickly implementable",
    "  Short-Term = requires planning but achievable in near future",
    "  Long-Term = requires significant investment or structural change",
    
    "Ensure all outputs are grounded in the provided text. Do not infer content that is not present.",
    
    "Return JSON only in the schema:",
    "{\"domains\":[{\"id\":\"command\",\"label\":\"Command\",\"score\":1,\"evidence\":\"...\",\"rationale\":\"...\",\"gaps\":[\"...\"],\"strengths\":[\"...\"]}],\"overall_summary\":\"...\",\"recommended_actions\":[{\"domain_id\":\"command\",\"priority\":\"High|Medium|Low\",\"timeframe\":\"Immediate|Short-Term|Long-Term\",\"recommendation\":\"...\"}]}",
    
    "\nFacility type:", facility_type,
    "\nEmergency focus:", emergency_focus,
    "\nPlan text:\n", plan_text
  )
  
  call_llm_json(
    system_prompt = "You review emergency preparedness plans and produce structured, evidence-based, and highly detailed scoring aligned with FEMA, CDC, and WHO frameworks. You provide traceable references and thorough justifications.",
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
    fallback <- mock_llm_response(schema_hint)
    attr(fallback, "llm_fallback_notice") <- "LLM unavailable, using fallback."
    return(fallback)
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

  if (identical(schema_hint, "regional_recommendations")) {
    return(list(
      recommendations = c(
        "Clarify hazard-specific triggers and activation thresholds.",
        "Define surge staffing and cross-training for outbreak response.",
        "Add supply chain contingencies for high-frequency events.",
        "Document continuity of operations for essential services.",
        "Update risk assessment based on recent regional incidents."
      )
    ))
  }

  if (identical(schema_hint, "hazard_coverage")) {
    return(list(
      hazard = "Flood",
      observed = list(
        list(
          domain_id = "communication",
          label = "Communication & Coordination",
          status = "missing",
          rationale = "The plan does not describe flood-specific alerting or evacuation messaging.",
          evidence = "",
          recommendation = "Add flood-specific communication protocols and public messaging triggers."
        ),
        list(
          domain_id = "supply_chain",
          label = "Supply Chain & Logistics",
          status = "partial",
          rationale = "Supplies are referenced but disruption contingencies are not described.",
          evidence = "Mentions inventory without alternative sourcing.",
          recommendation = "Define backup supply routes and vendor contingencies for flood disruptions."
        )
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
