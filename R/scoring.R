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
