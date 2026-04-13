get_env_nonempty <- function(name, fallback = "") {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(fallback)
  lower <- tolower(value)
  if (lower %in% c("none", "null", "nil", "na")) return(fallback)
  value
}

llm_provider_default <- get_env_nonempty("LLM_PROVIDER", "openai_compatible")
llm_provider_lower <- tolower(llm_provider_default)
llm_api_base_default <- if (llm_provider_lower %in% c("gemini", "google", "google_gemini", "generativelanguage")) {
  "https://generativelanguage.googleapis.com"
} else {
  get_env_nonempty("LITELLM_API_BASE", "https://litellm.oit.duke.edu")
}
llm_model_default <- if (llm_provider_lower %in% c("gemini", "google", "google_gemini", "generativelanguage")) {
  "gemini-1.5-flash"
} else {
  get_env_nonempty("LITELLM_MODEL", "gpt-5-mini")
}

app_config <- list(
  llm_provider = llm_provider_default,
  llm_api_base = get_env_nonempty("LLM_API_BASE", llm_api_base_default),
  llm_api_key = get_env_nonempty("LLM_API_KEY", get_env_nonempty("LITELLM_API_KEY", "")),
  llm_model = get_env_nonempty("LLM_MODEL", llm_model_default),
  llm_temperature = as.numeric(get_env_nonempty("LLM_TEMPERATURE", "0.2")),
  llm_timeout = as.numeric(get_env_nonempty("LLM_TIMEOUT", "60")),
  cdc_app_token = get_env_nonempty("CDC_APP_TOKEN", ""),
  cdc_source = get_env_nonempty("CDC_SOURCE", "wonder"),
  cdc_wonder_request = get_env_nonempty("CDC_WONDER_REQUEST", "data/cdc_wonder_request.xml"),
  cdc_wonder_response = get_env_nonempty("CDC_WONDER_RESPONSE", "data/cdc_wonder_response.csv")
)

fema_guidance <- list(
  cpg101 = list(
    name = "Comprehensive Preparedness Guide (CPG) 101",
    version = "Version 3.0 (September 2021)",
    planning_steps = c(
      "Form a Collaborative Planning Team",
      "Understand the Situation",
      "Determine Goals and Objectives",
      "Plan Development",
      "Plan Preparation, Review, and Approval",
      "Plan Implementation and Maintenance"
    )
  ),
  mission_areas = c("Prevention", "Protection", "Mitigation", "Response", "Recovery"),
  core_capabilities = c(
    "Planning",
    "Public Information and Warning",
    "Operational Coordination",
    "Logistics and Supply Chain Management",
    "Infrastructure Systems",
    "Situational Assessment",
    "Environmental Response/Health and Safety",
    "Mass Care Services",
    "Supply Chain Integrity and Security",
    "Risk and Disaster Resilience Assessment",
    "Community Resilience",
    "Threats and Hazards Identification"
  )
)

preparedness_framework <- list(
  domains = list(
    list(
      id = "communication",
      label = "Communication & Coordination",
      components = c(
        "Emergency communication protocol",
        "Leadership roles and escalation paths",
        "External agency coordination",
        "Contact lists and redundancy"
      ),
      fema_alignment = c("Public Information and Warning", "Operational Coordination")
    ),
    list(
      id = "workforce",
      label = "Workforce Capacity",
      components = c(
        "Staffing surge plan",
        "Role reassignments",
        "Just-in-time training",
        "Staff wellbeing support"
      ),
      fema_alignment = c("Planning", "Operational Coordination")
    ),
    list(
      id = "supply_chain",
      label = "Supply Chain & Logistics",
      components = c(
        "Critical supply inventory",
        "Vendor diversification",
        "Medication management",
        "Distribution plan"
      ),
      fema_alignment = c("Logistics and Supply Chain Management", "Supply Chain Integrity and Security")
    ),
    list(
      id = "surge_planning",
      label = "Surge Planning",
      components = c(
        "Bed capacity expansion",
        "Triage and patient flow",
        "Alternate care sites",
        "Discharge acceleration"
      ),
      fema_alignment = c("Mass Care Services", "Situational Assessment")
    ),
    list(
      id = "risk_assessment",
      label = "Risk & Hazard Identification",
      components = c(
        "Hazard identification and risk assessment",
        "Scenario-based assumptions",
        "Vulnerability analysis",
        "Updates tied to changing risks"
      ),
      fema_alignment = c("Threats and Hazards Identification", "Risk and Disaster Resilience Assessment")
    ),
    list(
      id = "continuity",
      label = "Continuity of Operations",
      components = c(
        "Essential services continuity",
        "Data and IT resilience",
        "Power and utilities contingency",
        "Recovery planning"
      ),
      fema_alignment = c("Infrastructure Systems", "Community Resilience")
    ),
    list(
      id = "governance",
      label = "Governance & Documentation",
      components = c(
        "Collaborative planning team",
        "Goals and objectives aligned to risks",
        "Plan development, review, and approval",
        "Exercises, after-action review, and updates"
      ),
      fema_alignment = c("Planning")
    )
  )
)

cdc_phep_capabilities <- list(
  list(
    id = "community_preparedness",
    label = "Community Preparedness",
    keywords = c("community preparedness", "stakeholder engagement", "partnerships", "public outreach")
  ),
  list(
    id = "community_recovery",
    label = "Community Recovery",
    keywords = c("recovery", "continuity", "restoration", "reconstitution")
  ),
  list(
    id = "emergency_operations_coordination",
    label = "Emergency Operations Coordination",
    keywords = c("incident command", "ics", "eoc", "coordination", "situational awareness")
  ),
  list(
    id = "emergency_public_information",
    label = "Emergency Public Information and Warning",
    keywords = c("public information", "risk communication", "media", "warning", "alert")
  ),
  list(
    id = "fatality_management",
    label = "Fatality Management",
    keywords = c("fatality", "mortuary", "decedent", "remains")
  ),
  list(
    id = "information_sharing",
    label = "Information Sharing",
    keywords = c("information sharing", "data sharing", "situational report", "sitrep")
  ),
  list(
    id = "mass_care",
    label = "Mass Care",
    keywords = c("shelter", "mass care", "congregate care", "feeding")
  ),
  list(
    id = "medical_countermeasure_dispensing",
    label = "Medical Countermeasure Dispensing",
    keywords = c("countermeasure", "dispensing", "pod", "medication distribution")
  ),
  list(
    id = "medical_materials_management",
    label = "Medical Materiel Management and Distribution",
    keywords = c("medical supply", "inventory", "logistics", "distribution")
  ),
  list(
    id = "medical_surge",
    label = "Medical Surge",
    keywords = c("surge capacity", "bed capacity", "staffing surge", "alternate care")
  ),
  list(
    id = "non_pharmaceutical_interventions",
    label = "Non-Pharmaceutical Interventions",
    keywords = c("isolation", "quarantine", "social distancing", "masking")
  ),
  list(
    id = "public_health_law",
    label = "Public Health Law",
    keywords = c("legal authority", "emergency powers", "orders", "waiver")
  ),
  list(
    id = "responder_safety",
    label = "Responder Safety and Health",
    keywords = c("responder safety", "ppe", "occupational health", "safety protocols")
  ),
  list(
    id = "volunteer_management",
    label = "Volunteer Management",
    keywords = c("volunteer", "mrc", "credentialing", "just-in-time training")
  ),
  list(
    id = "surveillance_epidemiology",
    label = "Surveillance and Epidemiology Investigation",
    keywords = c("surveillance", "case investigation", "contact tracing", "epidemiology")
  )
)

who_erf_domains <- list(
  list(
    id = "risk_assessment",
    label = "Risk Assessment",
    keywords = c("risk assessment", "hazard analysis", "vulnerability", "threat")
  ),
  list(
    id = "incident_management",
    label = "Incident Management",
    keywords = c("incident management", "ics", "eoc", "coordination", "command")
  ),
  list(
    id = "operations",
    label = "Operations",
    keywords = c("operations", "response", "field teams", "implementation")
  ),
  list(
    id = "logistics",
    label = "Logistics",
    keywords = c("logistics", "supply chain", "procurement", "distribution")
  ),
  list(
    id = "communications",
    label = "Communications",
    keywords = c("risk communication", "media", "public information", "alerts")
  ),
  list(
    id = "planning_monitoring",
    label = "Planning and Monitoring",
    keywords = c("planning", "monitoring", "evaluation", "after-action")
  ),
  list(
    id = "health_services",
    label = "Health Services",
    keywords = c("clinical care", "treatment", "medical services", "case management")
  )
)

framework_domain_map <- list(
  cdc_phep = list(
    Command = c(
      "Emergency Operations Coordination",
      "Information Sharing",
      "Public Health Law"
    ),
    Communication = c(
      "Emergency Public Information and Warning",
      "Community Preparedness",
      "Information Sharing"
    ),
    Operations = c(
      "Medical Surge",
      "Medical Countermeasure Dispensing",
      "Non-Pharmaceutical Interventions",
      "Surveillance and Epidemiology Investigation"
    ),
    Logistics = c(
      "Medical Materiel Management and Distribution",
      "Mass Care",
      "Fatality Management",
      "Community Recovery"
    )
  ),
  fema = list(
    Command = c(
      "Planning",
      "Operational Coordination",
      "Public Information and Warning"
    ),
    Communication = c(
      "Public Information and Warning",
      "Operational Coordination"
    ),
    Operations = c(
      "Mass Care Services",
      "Situational Assessment",
      "Environmental Response/Health and Safety"
    ),
    Logistics = c(
      "Logistics and Supply Chain Management",
      "Infrastructure Systems",
      "Supply Chain Integrity and Security"
    )
  ),
  who_erf = list(
    Command = c(
      "Incident Management",
      "Planning and Monitoring",
      "Risk Assessment"
    ),
    Communication = c(
      "Communications"
    ),
    Operations = c(
      "Operations",
      "Health Services"
    ),
    Logistics = c(
      "Logistics"
    )
  )
)
