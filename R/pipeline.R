load_plan_text <- function(plan_file, plan_text) {
  if (!is.null(plan_text) && nzchar(plan_text)) {
    return(plan_text)
  }

  if (is.null(plan_file)) {
    return("")
  }

  file_path <- plan_file$datapath
  file_ext <- tools::file_ext(plan_file$name)

  if (tolower(file_ext) == "pdf" && requireNamespace("pdftools", quietly = TRUE)) {
    return(paste(pdftools::pdf_text(file_path), collapse = "\n"))
  }

  readr::read_file(file_path)
}

validate_plan_text <- function(plan_text) {
  if (!nzchar(plan_text)) {
    stop("Please upload a plan or paste text before running the analysis.")
  }
}

preprocess_text <- function(plan_text) {
  plan_text |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

extract_features <- function(plan_text) {
  list(
    has_ics = str_detect(plan_text, regex("incident command", ignore_case = TRUE)),
    has_eop = str_detect(plan_text, regex("emergency operations plan", ignore_case = TRUE)),
    has_evacuation = str_detect(plan_text, regex("evacuation", ignore_case = TRUE)),
    has_backup_power = str_detect(plan_text, regex("generator|backup power", ignore_case = TRUE)),
    communication_mentions = str_count(plan_text, regex("communication", ignore_case = TRUE)),
    training_mentions = str_count(plan_text, regex("training|drill|exercise", ignore_case = TRUE))
  )
}

status_from_count <- function(count, present = 2, partial = 1) {
  if (is.na(count) || count < partial) return("missing")
  if (count >= present) return("present")
  "partial"
}

build_domain_status <- function(plan_text) {
  indicators <- tibble::tibble(
    id = c(
      "communication",
      "workforce",
      "supply_chain",
      "surge_planning",
      "risk_assessment",
      "continuity",
      "governance"
    ),
    hits = c(
      str_count(plan_text, regex("communication|coordination|incident command|ics", ignore_case = TRUE)),
      str_count(plan_text, regex("staff|staffing|workforce|surge|cross[- ]?training", ignore_case = TRUE)),
      str_count(plan_text, regex("supply|inventory|vendor|logistics|ppe", ignore_case = TRUE)),
      str_count(plan_text, regex("surge|triage|alternate care|bed capacity", ignore_case = TRUE)),
      str_count(plan_text, regex("risk assessment|hazard|vulnerability|hva", ignore_case = TRUE)),
      str_count(plan_text, regex("continuity|coop|business continuity|it resilience|backup power", ignore_case = TRUE)),
      str_count(plan_text, regex("governance|approval|review|after[- ]action|aar", ignore_case = TRUE))
    )
  )

  indicators |>
    mutate(
      status = map_chr(hits, status_from_count),
      evidence = paste0("Keyword hits: ", hits)
    ) |>
    transmute(
      id,
      status,
      evidence
    ) |>
    purrr::pmap(function(...) list(...))
}

build_executive_summary_base <- function(facility_type, emergency_focus, domain_scores, final_score, gaps, actions) {
  top_domain <- domain_scores |>
    arrange(desc(score)) |>
    slice_head(n = 1) |>
    pull(domain)

  lowest_domain <- domain_scores |>
    arrange(score) |>
    slice_head(n = 1) |>
    pull(domain)

  gap_line <- if (length(gaps) == 0) {
    "No critical gaps were detected based on the submitted plan text."
  } else {
    paste0("Key gaps flagged: ", paste(gaps, collapse = "; "), ".")
  }

  action_line <- if (length(actions) == 0) {
    "No immediate corrective actions were triggered by the rule set."
  } else {
    paste0("Immediate actions recommended: ", paste(actions, collapse = "; "), ".")
  }

  paste(
    "Facility type:", facility_type, "| Emergency focus:", emergency_focus, ".",
    "Overall preparedness score:", round(final_score, 1), ".",
    "Strongest area:", top_domain, ".",
    "Needs the most improvement:", lowest_domain, ".",
    gap_line,
    action_line
  )
}

build_regional_gap_summary <- function(coverage_table) {
  if (is.null(coverage_table) || nrow(coverage_table) == 0) {
    return(list(
      summary = "Regional coverage comparison unavailable due to limited incident data.",
      gap_insights = list(),
      recommended_focus = list()
    ))
  }

  focused <- coverage_table |>
    arrange(coverage, desc(count)) |>
    slice_head(n = 4)

  gap_insights <- purrr::pmap(
    focused,
    function(source, event, count, coverage, domains, ...) {
      list(
        pattern = paste(source, "-", event),
        coverage = coverage,
        impact = paste0("Coverage rated as ", coverage, " for domains: ", domains, ".")
      )
    }
  )

  focus <- focused |>
    mutate(focus_item = paste0("Improve coverage for ", event, " (", coverage, ").")) |>
    pull(focus_item)

  list(
    summary = "Regional incident history highlights areas where plan coverage is weakest.",
    gap_insights = gap_insights,
    recommended_focus = focus
  )
}

run_full_analysis <- function(
  plan_text,
  facility_type,
  emergency_focus,
  region_state,
  region_county,
  history_years,
  fema_types = c("DR", "EM"),
  incident_types = character(0)
) {
  cleaned_text <- preprocess_text(plan_text)
  features <- extract_features(cleaned_text)

  domain_scores <- score_domains(features)
  weights <- get_weights(emergency_focus)
  final_score <- compute_final_score(domain_scores, weights)
  scorecard <- build_scorecard(domain_scores)

  gaps <- detect_gaps(features)
  actions <- generate_actions(gaps)

  executive_summary_base <- build_executive_summary_base(
    facility_type,
    emergency_focus,
    domain_scores,
    final_score,
    gaps,
    actions
  )

  executive_summary <- polish_executive_summary(
    executive_summary_base,
    facility_type,
    emergency_focus,
    domain_scores,
    final_score,
    gaps,
    actions
  )

  domain_status <- build_domain_status(cleaned_text)
  regional_analysis <- build_regional_analysis(
    region_state,
    region_county,
    history_years,
    list(domains = domain_status),
    fema_types,
    incident_types
  )
  regional_gap <- build_regional_gap_summary(regional_analysis$coverage)

  list(
    features = features,
    scorecard = scorecard,
    final_score = final_score,
    gaps = gaps,
    action_plan = actions,
    executive_summary = executive_summary,
    regional = list(
      fema = regional_analysis$fema,
      cdc = regional_analysis$cdc,
      nws = regional_analysis$nws,
      weather = regional_analysis$weather,
      coverage = regional_analysis$coverage,
      gap = regional_gap
    )
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
