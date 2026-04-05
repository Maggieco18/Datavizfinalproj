app_config <- list(
  llm_provider = Sys.getenv("LLM_PROVIDER", unset = "openai_compatible"),
  llm_api_base = Sys.getenv("LLM_API_BASE", unset = ""),
  llm_api_key = Sys.getenv("LLM_API_KEY", unset = ""),
  llm_model = Sys.getenv("LLM_MODEL", unset = "gpt-4.1-mini"),
  llm_temperature = as.numeric(Sys.getenv("LLM_TEMPERATURE", unset = "0.2")),
  llm_timeout = as.numeric(Sys.getenv("LLM_TIMEOUT", unset = "60")),
  cdc_app_token = Sys.getenv("CDC_APP_TOKEN", unset = "")
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
