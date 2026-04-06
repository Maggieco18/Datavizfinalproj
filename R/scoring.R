normalize_domains <- function(domains) {
  if (is.data.frame(domains)) {
    return(purrr::pmap(domains, function(...) list(...)))
  }
  if (is.null(domains)) return(list())
  domains
}

score_domains <- function(features) {
  tibble::tibble(
    domain = c("Command", "Communication", "Operations", "Logistics"),
    score = c(
      ifelse(features$has_ics, 100, 20),
      min(features$communication_mentions * 10, 100),
      ifelse(features$has_evacuation, 80, 30),
      ifelse(features$has_backup_power, 90, 25)
    )
  )
}

llm_domains_to_scorecard <- function(llm_domains) {
  if (is.null(llm_domains) || length(llm_domains) == 0) {
    return(tibble::tibble(domain = character(0), score = numeric(0)))
  }

  tibble::tibble(
    domain = vapply(
      llm_domains,
      function(d) {
        if (!is.null(d$label) && nzchar(d$label)) return(d$label)
        d$id
      },
      character(1)
    ),
    score = vapply(
      llm_domains,
      function(d) {
        raw <- d$score
      if (is.null(raw) || is.na(raw)) return(20)
      raw <- max(1, min(5, as.numeric(raw)))
      raw * 20
    },
      numeric(1)
    )
  )
}

llm_domains_to_gaps <- function(llm_domains) {
  if (is.null(llm_domains) || length(llm_domains) == 0) {
    return(list())
  }

  lapply(llm_domains, function(d) {
    score <- if (is.null(d$score)) 0 else as.numeric(d$score)
    status <- if (score >= 4) "strong" else if (score >= 3) "partial" else "missing"
    evidence <- d$rationale
    if (is.null(evidence) || !nzchar(evidence)) evidence <- if (!is.null(d$evidence)) d$evidence else ""
    components <- if (!is.null(d$gaps)) d$gaps else character(0)
    list(
      domain = if (!is.null(d$label) && nzchar(d$label)) d$label else d$id,
      status = status,
      evidence = evidence,
      components = components
    )
  })
}

llm_actions_to_plan <- function(actions) {
  if (is.null(actions) || length(actions) == 0) {
    return(character(0))
  }

  if (is.character(actions)) return(actions)

  out <- vapply(
    actions,
    function(a) {
      rec <- if (!is.null(a$recommendation)) a$recommendation else ""
      if (!nzchar(rec)) return("")
      rec
    },
    character(1)
  )

  out[nzchar(out)]
}

score_framework_from_keywords <- function(cleaned_text, framework) {
  if (is.null(framework) || length(framework) == 0) {
    return(tibble::tibble(domain = character(0), score = numeric(0)))
  }

  tibble::tibble(
    domain = vapply(
      framework,
      function(item) {
        if (!is.null(item$label) && nzchar(item$label)) return(item$label)
        item$id
      },
      character(1)
    ),
    score = vapply(
      framework,
      function(item) {
        keywords <- if (!is.null(item$keywords)) item$keywords else character(0)
        if (length(keywords) == 0) return(20)
        pattern <- paste0("\\b(", paste(keywords, collapse = "|"), ")\\b")
        count <- stringr::str_count(cleaned_text, stringr::regex(pattern, ignore_case = TRUE))
        if (count == 0) return(20)
        if (count == 1) return(50)
        if (count == 2) return(70)
        if (count == 3) return(85)
        95
      },
      numeric(1)
    )
  )
}

compute_framework_score <- function(domain_scores) {
  if (nrow(domain_scores) == 0) return(NA_real_)
  mean(domain_scores$score)
}

get_weights <- function(focus) {
  switch(focus,
    "Mass Casualty" = c(Command = 0.25, Communication = 0.20, Operations = 0.35, Logistics = 0.20),
    "Natural Disaster" = c(Command = 0.20, Communication = 0.20, Operations = 0.25, Logistics = 0.35),
    "Infectious Disease" = c(Command = 0.20, Communication = 0.25, Operations = 0.30, Logistics = 0.25),
    c(Command = 0.25, Communication = 0.25, Operations = 0.25, Logistics = 0.25)
  )
}

compute_final_score <- function(domain_scores, weights) {
  sum(domain_scores$score * weights[domain_scores$domain])
}

build_scorecard <- function(domain_scores) {
  domain_scores |>
    mutate(
      risk = case_when(
        score >= 80 ~ "Low Risk",
        score >= 50 ~ "Moderate Risk",
        TRUE ~ "High Risk"
      )
    )
}

detect_gaps <- function(features) {
  gaps <- c()

  if (!features$has_ics) {
    gaps <- c(gaps, "No Incident Command System defined")
  }

  if (features$training_mentions < 2) {
    gaps <- c(gaps, "Insufficient training and drills")
  }

  if (!features$has_backup_power) {
    gaps <- c(gaps, "No backup power system identified")
  }

  gaps
}

generate_actions <- function(gaps) {
  action_map <- list(
    "No Incident Command System defined" = "Implement ICS with defined leadership roles",
    "Insufficient training and drills" = "Conduct quarterly simulation exercises",
    "No backup power system identified" = "Install redundant generator systems"
  )

  unlist(action_map[gaps])
}
