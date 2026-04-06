library(httr)

fetch_json <- function(url, query = list(), headers = list(), timeout_seconds = 30) {
  header_vec <- character(0)
  if (!is.null(headers) && length(headers) > 0) {
    header_vec <- unlist(headers, use.names = TRUE)
  }

  response <- httr::GET(
    url = url,
    query = query,
    add_headers(.headers = header_vec),
    timeout(timeout_seconds)
  )

  if (httr::http_error(response)) {
    resp_url <- response$url %||% url
    stop("Request failed: HTTP ", httr::status_code(response), " for ", resp_url)
  }

  content_type <- httr::headers(response)[["content-type"]] %||% ""
  if (!str_detect(tolower(content_type), "application/json")) {
    resp_url <- response$url %||% url
    stop("Request failed: non-JSON response received from ", resp_url, ".")
  }

  httr::content(response, as = "parsed", type = "application/json")
}

get_county_choices <- function(state_code) {
  if (!requireNamespace("maps", quietly = TRUE)) {
    return(character(0))
  }

  state_name <- tolower(state.name[match(state_code, state.abb)] %||% state_code)
  county_fips <- maps::county.fips

  choices <- county_fips |>
    mutate(
      state = sub(",.*", "", polyname),
      county = sub(".*,", "", polyname)
    ) |>
    filter(state == state_name) |>
    distinct(county) |>
    arrange(county) |>
    pull(county)

  str_to_title(choices)
}

normalize_county_name <- function(county) {
  county |>
    tolower() |>
    str_replace_all("\\s+county$", "") |>
    str_replace_all("\\s+parish$", "") |>
    str_replace_all("\\s+borough$", "") |>
    str_replace_all("\\s+census area$", "") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

parse_incident_types <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(character(0))
  }

  text |>
    str_split(",") |>
    unlist(use.names = FALSE) |>
    str_trim() |>
    discard(~ .x == "")
}

get_region_centroid <- function(state_code, county = NULL) {
  if (!requireNamespace("maps", quietly = TRUE)) {
    state_idx <- match(state_code, state.abb)
    if (is.na(state_idx)) {
      return(NULL)
    }
    return(list(lat = state.center$y[state_idx], lon = state.center$x[state_idx]))
  }

  state_name <- tolower(state.name[match(state_code, state.abb)] %||% state_code)
  county_norm <- normalize_county_name(county %||% "")

  region <- if (nzchar(county_norm)) {
    paste0(state_name, ",", county_norm)
  } else {
    state_name
  }

  map_level <- if (nzchar(county_norm)) "county" else "state"
  map_data <- tryCatch(
    maps::map(map_level, regions = region, plot = FALSE, fill = TRUE),
    error = function(e) NULL
  )

  if (is.null(map_data)) {
    state_idx <- match(state_code, state.abb)
    if (is.na(state_idx)) {
      return(NULL)
    }
    return(list(lat = state.center$y[state_idx], lon = state.center$x[state_idx]))
  }

  lon <- map_data$x
  lat <- map_data$y
  lon <- lon[!is.na(lon)]
  lat <- lat[!is.na(lat)]

  if (length(lon) == 0 || length(lat) == 0) {
    state_idx <- match(state_code, state.abb)
    if (is.na(state_idx)) {
      return(NULL)
    }
    return(list(lat = state.center$y[state_idx], lon = state.center$x[state_idx]))
  }

  list(lat = mean(lat), lon = mean(lon))
}

fetch_nws_forecast <- function(lat, lon) {
  user_agent <- "datavizfinalproject (shiny app)"
  point_url <- sprintf("https://api.weather.gov/points/%.4f,%.4f", lat, lon)
  point <- tryCatch(
    fetch_json(point_url, headers = list("User-Agent" = user_agent)),
    error = function(e) NULL
  )

  forecast_url <- point$properties$forecast %||% NULL
  if (is.null(forecast_url)) {
    return(list(table = tibble::tibble(), note = "NWS forecast unavailable for the selected region."))
  }

  forecast <- tryCatch(
    fetch_json(forecast_url, headers = list("User-Agent" = user_agent)),
    error = function(e) NULL
  )

  periods <- forecast$properties$periods %||% list()
  if (length(periods) == 0) {
    return(list(table = tibble::tibble(), note = "NWS forecast unavailable for the selected region."))
  }

  tbl <- tibble::as_tibble(periods) |>
    transmute(
      period = .data[["name"]] %||% "",
      forecast = .data[["shortForecast"]] %||% "",
      temperature = paste0(.data[["temperature"]], " ", .data[["temperatureUnit"]]),
      wind = paste(.data[["windSpeed"]], .data[["windDirection"]])
    ) |>
    slice_head(n = 6)

  list(table = tbl, note = NULL)
}

fetch_nws_forecast_for_region <- function(state_code, county = NULL) {
  centroid <- get_region_centroid(state_code, county)
  if (is.null(centroid)) {
    return(list(table = tibble::tibble(), note = "NWS forecast unavailable for the selected region."))
  }

  fetch_nws_forecast(centroid$lat, centroid$lon)
}

fetch_fema_disasters <- function(
  state_code,
  years_back = 10,
  county = NULL,
  disaster_types = c("DR", "EM"),
  incident_types = character(0)
) {
  start_date <- as.Date(Sys.Date()) - years_back * 365
  base_urls <- c(
    "https://www.fema.gov/api/open/v1/FemaWebDisasterDeclarations"
  )

  filter_parts <- c(
    "stateCode eq '", state_code, "' and declarationDate ge '",
    format(start_date, "%Y-%m-%d"), "'"
  )

  if (!is.null(disaster_types) && length(disaster_types) > 0) {
    declaration_types <- vapply(
      disaster_types,
      function(x) {
        if (x == "DR") return("Major Disaster")
        if (x == "EM") return("Emergency")
        if (x == "FM") return("Fire Management")
        x
      },
      character(1)
    )
    type_filter <- paste0("declarationType in ('", paste(declaration_types, collapse = "','"), "')")
    filter_parts <- c(filter_parts, " and ", type_filter)
  }

  if (!is.null(incident_types) && length(incident_types) > 0) {
    incident_filter <- paste0("incidentType in ('", paste(incident_types, collapse = "','"), "')")
    filter_parts <- c(filter_parts, " and ", incident_filter)
  }

  filter <- paste0(filter_parts, collapse = "")

  query <- list(
    "$filter" = filter,
    "$select" = paste(
      c(
        "disasterNumber",
        "disasterName",
        "declarationType",
        "incidentType",
        "declarationDate",
        "stateCode",
        "stateName",
        "incidentBeginDate",
        "incidentEndDate"
      ),
      collapse = ","
    ),
    "$orderby" = "declarationDate desc",
    "$top" = "1000"
  )

  records <- list()
  page_count <- 0
  max_pages <- 10
  error_note <- NULL
  last_error <- NULL
  headers <- list(
    "User-Agent" = "datavizfinalproject (shiny app)",
    "Accept" = "application/json"
  )

  fetch_with_base <- function(base_url) {
    records <- list()
    next_url <- base_url
    page_count <- 0
    query_local <- query
    had_response <- FALSE
    last_error_local <- NULL

    repeat {
      page_count <- page_count + 1
      response <- tryCatch(
        fetch_json(next_url, query = query_local, headers = headers),
        error = function(e) {
          last_error_local <<- conditionMessage(e)
          NULL
        }
      )
      if (is.null(response)) break

      had_response <- TRUE
      batch <- response$FemaWebDisasterDeclarations %||% list()
      records <- c(records, batch)

      next_url <- response$metadata$`next`
      query_local <- list()

      if (is.null(next_url) || page_count >= max_pages) break
    }

    list(records = records, had_response = had_response, last_error = last_error_local)
  }

  records <- list()
  had_response_any <- FALSE
  for (base_url in base_urls) {
    result <- fetch_with_base(base_url)
    records <- result$records
    had_response_any <- result$had_response
    last_error <- result$last_error
    if (had_response_any) break
  }

  if (!had_response_any) {
    error_note <- paste(
      "FEMA data temporarily unavailable.",
      if (!is.null(last_error)) paste0("(", last_error, ")") else NULL
    )
  }

  if (length(records) == 0) {
    out <- tibble::tibble()
    attr(out, "note") <- error_note
    return(out)
  }

  df <- dplyr::bind_rows(records)

  if (!is.null(county) && nzchar(county) && tolower(county) != "not applicable") {
    if ("declaredCountyArea" %in% names(df)) {
      county_norm <- normalize_county_name(county)
      df <- df |>
        filter(!is.na(declaredCountyArea)) |>
        mutate(
          declaredCountyNorm = declaredCountyArea |>
            tolower() |>
            str_replace_all("\\s+county$", "") |>
            str_replace_all("\\s+parish$", "") |>
            str_replace_all("\\s+borough$", "") |>
            str_replace_all("\\s+census area$", "") |>
            str_replace_all("\\s+", " ") |>
            str_trim()
        ) |>
        filter(str_detect(declaredCountyNorm, fixed(county_norm)))
    } else if ("designatedArea" %in% names(df)) {
      county_norm <- normalize_county_name(county)
      df <- df |>
        filter(!is.na(designatedArea)) |>
        mutate(
          designatedAreaNorm = designatedArea |>
            tolower() |>
            str_replace_all("\\s+county$", "") |>
            str_replace_all("\\s+parish$", "") |>
            str_replace_all("\\s+borough$", "") |>
            str_replace_all("\\s+census area$", "") |>
            str_replace_all("\\s+", " ") |>
            str_trim()
        ) |>
        filter(str_detect(designatedAreaNorm, fixed(county_norm)))
    } else {
      attr(df, "note") <- "FEMA disaster declarations on fema.gov are filtered at the state level; county filtering is not available for this dataset."
    }
  }

  attr(df, "note") <- error_note
  df
}

summarize_fema_incidents <- function(fema_df, top_n = 8) {
  note <- attr(fema_df, "note", exact = TRUE)
  if (nrow(fema_df) == 0) {
    return(list(table = tibble::tibble(), note = note %||% "No FEMA disaster declarations found for the selected region/time range."))
  }

  fema_df |>
    mutate(
      declarationDate = as.Date(declarationDate),
      year = as.integer(format(declarationDate, "%Y"))
    ) |>
    filter(!is.na(incidentType)) |>
    group_by(incidentType) |>
    summarise(
      events = n(),
      year_range = paste0(min(year, na.rm = TRUE), "–", max(year, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    arrange(desc(events)) |>
    slice_head(n = top_n) |>
    list(table = _, note = note)
}

fetch_cdc_metadata <- function() {
  fetch_json("https://data.cdc.gov/api/views/x9gk-5huc.json")
}

fetch_storm_events_index <- function(timeout_seconds = 20) {
  index_url <- "https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/"
  response <- tryCatch(
    httr::GET(index_url, timeout(timeout_seconds)),
    error = function(e) NULL
  )

  if (is.null(response) || httr::http_error(response)) {
    return(character(0))
  }

  text <- httr::content(response, as = "text", encoding = "UTF-8")
  strsplit(text, "\n", fixed = TRUE)[[1]]
}

resolve_storm_events_files <- function(years) {
  lines <- fetch_storm_events_index()
  if (length(lines) == 0) {
    return(tibble::tibble())
  }
  matches <- str_match(
    lines,
    "StormEvents_details-ftp_v1\\.0_d(\\d{4})_c(\\d{8})\\.csv\\.gz"
  )

  files <- tibble::tibble(
    file = matches[, 1],
    year = suppressWarnings(as.integer(matches[, 2])),
    cdate = suppressWarnings(as.integer(matches[, 3]))
  ) |>
    filter(!is.na(year)) |>
    filter(year %in% years) |>
    group_by(year) |>
    slice_max(order_by = cdate, n = 1, with_ties = FALSE) |>
    ungroup()

  if (nrow(files) == 0) {
    return(tibble::tibble())
  }

  files |>
    mutate(url = paste0("https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/", file))
}

parse_damage_value <- function(value) {
  raw <- toupper(str_trim(value %||% ""))
  if (raw == "" || is.na(raw)) return(0)

  multiplier <- 1
  if (str_ends(raw, "K")) {
    multiplier <- 1e3
    raw <- str_remove(raw, "K$")
  } else if (str_ends(raw, "M")) {
    multiplier <- 1e6
    raw <- str_remove(raw, "M$")
  } else if (str_ends(raw, "B")) {
    multiplier <- 1e9
    raw <- str_remove(raw, "B$")
  }

  suppressWarnings(as.numeric(raw) * multiplier)
}

download_storm_events_csv <- function(url, timeout_seconds = 30) {
  tmp <- tempfile(fileext = ".csv.gz")
  response <- tryCatch(
    httr::GET(url, write_disk(tmp, overwrite = TRUE), timeout(timeout_seconds)),
    error = function(e) NULL
  )

  if (is.null(response) || httr::http_error(response)) {
    return(NULL)
  }

  tmp
}

fetch_nws_storm_events <- function(state_code, years_back = 5, county = NULL) {
  capped_years <- min(max(years_back, 1), 5)
  note <- if (years_back > capped_years) {
    paste0("Storm events history capped at ", capped_years, " years for performance.")
  } else {
    NULL
  }

  current_year <- as.integer(format(Sys.Date(), "%Y"))
  years <- seq(current_year - capped_years + 1, current_year)
  files <- resolve_storm_events_files(years)

  if (nrow(files) == 0) {
    return(list(data = tibble::tibble(), note = note %||% "Storm events data unavailable for the selected range."))
  }

  state_name <- toupper(state.name[match(state_code, state.abb)] %||% state_code)
  county_lower <- tolower(county %||% "")

  cols_needed <- c(
    "STATE", "CZ_NAME", "CZ_TYPE", "EVENT_TYPE", "BEGIN_DATE_TIME",
    "END_DATE_TIME", "DAMAGE_PROPERTY", "DAMAGE_CROPS",
    "DEATHS_DIRECT", "DEATHS_INDIRECT", "INJURIES_DIRECT", "INJURIES_INDIRECT"
  )

  all_rows <- list()
  for (i in seq_len(nrow(files))) {
    file_url <- files$url[i]
    local_file <- download_storm_events_csv(file_url)
    if (is.null(local_file)) next

    df <- tryCatch(
      suppressWarnings(readr::read_csv(
        local_file,
        show_col_types = FALSE,
        col_select = any_of(cols_needed)
      )),
      error = function(e) tibble::tibble()
    )

    if (nrow(df) == 0) next

    df <- df |>
      mutate(
        STATE = toupper(.data[["STATE"]]),
        CZ_NAME = .data[["CZ_NAME"]],
        CZ_TYPE = .data[["CZ_TYPE"]]
      ) |>
      filter(STATE == state_name)

    if (nzchar(county_lower)) {
      df <- df |>
        filter(toupper(CZ_TYPE) == "C") |>
        filter(str_detect(tolower(CZ_NAME), fixed(county_lower)))
    }

    if (nrow(df) > 0) all_rows <- c(all_rows, list(df))
  }

  if (length(all_rows) == 0) {
    return(list(data = tibble::tibble(), note = note %||% "Storm events data unavailable for the selected range."))
  }

  list(data = bind_rows(all_rows), note = note)
}

summarize_nws_storm_events <- function(nws_result, top_n = 8) {
  df <- nws_result$data
  if (nrow(df) == 0) {
    return(list(table = tibble::tibble(), note = nws_result$note %||% "Storm events data unavailable for the selected range."))
  }

  summary_tbl <- df |>
    mutate(
      event_type = .data[["EVENT_TYPE"]],
      begin_time = suppressWarnings(as.POSIXct(.data[["BEGIN_DATE_TIME"]], tz = "UTC")),
      damage_property = map_dbl(.data[["DAMAGE_PROPERTY"]], parse_damage_value),
      damage_crops = map_dbl(.data[["DAMAGE_CROPS"]], parse_damage_value),
      deaths = suppressWarnings(as.numeric(.data[["DEATHS_DIRECT"]])) +
        suppressWarnings(as.numeric(.data[["DEATHS_INDIRECT"]])),
      injuries = suppressWarnings(as.numeric(.data[["INJURIES_DIRECT"]])) +
        suppressWarnings(as.numeric(.data[["INJURIES_INDIRECT"]]))
    ) |>
    mutate(
      total_damage = coalesce(damage_property, 0) + coalesce(damage_crops, 0),
      deaths = coalesce(deaths, 0),
      injuries = coalesce(injuries, 0)
    ) |>
    filter(!is.na(event_type)) |>
    group_by(event_type) |>
    summarise(
      events = n(),
      total_damage = sum(total_damage, na.rm = TRUE),
      total_deaths = sum(deaths, na.rm = TRUE),
      total_injuries = sum(injuries, na.rm = TRUE),
      year_range = {
        years <- suppressWarnings(as.integer(format(begin_time, "%Y")))
        if (all(is.na(years))) {
          NA_character_
        } else {
          paste0(min(years, na.rm = TRUE), "–", max(years, na.rm = TRUE))
        }
      },
      .groups = "drop"
    ) |>
    arrange(desc(total_damage), desc(events)) |>
    slice_head(n = top_n)

  list(table = summary_tbl, note = nws_result$note)
}

pick_col <- function(fields, patterns) {
  lower_fields <- tolower(fields)
  for (pattern in patterns) {
    idx <- which(str_detect(lower_fields, pattern))
    if (length(idx) > 0) return(fields[idx[1]])
  }
  NULL
}

fetch_cdc_nndss <- function(state_code, years_back = 5) {
  metadata <- tryCatch(fetch_cdc_metadata(), error = function(e) NULL)
  fields <- metadata$columns$fieldName %||% character(0)

  state_col <- pick_col(fields, c("reporting_area", "reportingarea", "state", "jurisdiction"))
  year_col <- pick_col(fields, c("year", "mmwr_year"))
  week_col <- pick_col(fields, c("mmwr_week", "week"))
  condition_col <- pick_col(fields, c("condition", "disease"))
  cases_col <- pick_col(fields, c("cumulative_current_year", "current_year", "cum_current_year", "current_week", "cases_current_week", "weekly_cases"))

  if (is.null(state_col) || is.null(condition_col)) {
    state_col <- "reporting_area"
    year_col <- year_col %||% "mmwr_year"
    week_col <- week_col %||% "mmwr_week"
    condition_col <- "condition"
    cases_col <- cases_col %||% "cumulative_current_year"
  }

  state_name <- state.name[match(state_code, state.abb)] %||% state_code
  start_year <- as.integer(format(Sys.Date(), "%Y")) - years_back

  where_clauses <- c(
    paste0(state_col, " in ('", state_code, "','", state_name, "')")
  )
  if (!is.null(year_col)) {
    where_clauses <- c(where_clauses, paste0(year_col, " >= ", start_year))
  }

  query <- list(
    "$where" = paste(where_clauses, collapse = " AND "),
    "$limit" = "50000"
  )

  headers <- list()
  if (nzchar(app_config$cdc_app_token %||% "")) {
    headers[["X-App-Token"]] <- app_config$cdc_app_token
  }

  base_url <- "https://data.cdc.gov/resource/x9gk-5huc.json"
  records <- list()
  offset <- 0

  repeat {
    query[["$offset"]] <- offset
    batch <- tryCatch(
      fetch_json(base_url, query = query, headers = headers),
      error = function(e) list()
    )

    if (length(batch) == 0) break

    records <- c(records, batch)
    if (length(batch) < 50000) break

    offset <- offset + 50000
  }

  df <- if (length(records) == 0) tibble::tibble() else tibble::as_tibble(records)

  if (nrow(df) == 0) {
    return(list(data = tibble::tibble(), cols = list(), note = "CDC data temporarily unavailable."))
  }

  list(
    data = df,
    cols = list(
      state = state_col,
      year = year_col,
      week = week_col,
      condition = condition_col,
      cases = cases_col
    ),
    note = NULL
  )
}

summarize_cdc_conditions <- function(cdc_result, top_n = 8) {
  df <- cdc_result$data
  cols <- cdc_result$cols

  if (nrow(df) == 0 || is.null(cols$condition)) {
    return(list(table = tibble::tibble(), note = cdc_result$note %||% "No CDC records found for the selected region/time range."))
  }

  condition_col <- cols$condition
  year_col <- cols$year
  week_col <- cols$week
  cases_col <- cols$cases

  df <- df |>
    mutate(
      condition = .data[[condition_col]]
    )

  if (!is.null(year_col)) {
    df <- df |>
      mutate(year = as.integer(.data[[year_col]]))
    latest_year <- max(df$year, na.rm = TRUE)
    df <- df |> filter(year == latest_year)
  }

  if (!is.null(cases_col)) {
    df <- df |>
      mutate(cases = suppressWarnings(as.numeric(.data[[cases_col]])))
  } else {
    df$cases <- NA_real_
  }

  cases_is_cumulative <- !is.null(cases_col) && str_detect(tolower(cases_col), "cumulative|current_year|cum_")

  summary_tbl <- df |>
    filter(!is.na(condition)) |>
    group_by(condition) |>
    summarise(
      cases = if (all(is.na(cases))) {
        NA_real_
      } else if (cases_is_cumulative) {
        max(cases, na.rm = TRUE)
      } else {
        sum(cases, na.rm = TRUE)
      },
      weeks_reported = if (!is.null(week_col)) n_distinct(.data[[week_col]]) else n(),
      .groups = "drop"
    ) |>
    arrange(desc(cases), desc(weeks_reported)) |>
    slice_head(n = top_n)

  list(table = summary_tbl, note = NULL)
}

parse_year_range_end <- function(year_range) {
  if (is.null(year_range) || !nzchar(year_range)) return(NA_integer_)
  parts <- str_split(as.character(year_range), "–|-", simplify = TRUE)
  if (ncol(parts) == 0) return(NA_integer_)
  end_year <- suppressWarnings(as.integer(parts[, ncol(parts)]))
  end_year
}

normalize_hazard_type <- function(raw_label, source) {
  label <- raw_label %||% ""
  key <- tolower(label)

  if (source == "CDC") {
    return(list(type = "infectious_disease", label = label))
  }

  if (str_detect(key, "hurricane|typhoon|tropical")) {
    return(list(type = "hurricane", label = "Hurricane / Tropical Storm"))
  }
  if (str_detect(key, "flood")) {
    return(list(type = "flood", label = "Flood"))
  }
  if (str_detect(key, "fire|wildfire")) {
    return(list(type = "wildfire", label = "Wildfire"))
  }
  if (str_detect(key, "tornado")) {
    return(list(type = "tornado", label = "Tornado"))
  }
  if (str_detect(key, "winter|snow|ice|blizzard")) {
    return(list(type = "winter_storm", label = "Winter Storm"))
  }
  if (str_detect(key, "drought")) {
    return(list(type = "drought", label = "Drought"))
  }
  if (str_detect(key, "earthquake|tsunami|volcano")) {
    return(list(type = "earthquake", label = "Earthquake / Seismic"))
  }
  if (str_detect(key, "severe storm|storm|wind|hail|lightning")) {
    return(list(type = "severe_storm", label = "Severe Storm"))
  }
  if (str_detect(key, "heat|excessive heat")) {
    return(list(type = "heat", label = "Extreme Heat"))
  }
  if (str_detect(key, "chemical|hazmat")) {
    return(list(type = "chemical", label = "Chemical / Hazmat"))
  }

  list(type = "other", label = stringr::str_to_title(label))
}

hazard_domain_map <- list(
  hurricane = c("communication", "supply_chain", "surge_planning", "continuity", "risk_assessment"),
  flood = c("communication", "supply_chain", "continuity", "risk_assessment"),
  wildfire = c("communication", "workforce", "supply_chain", "continuity", "risk_assessment"),
  tornado = c("communication", "surge_planning", "continuity", "risk_assessment"),
  winter_storm = c("communication", "workforce", "supply_chain", "continuity", "risk_assessment"),
  severe_storm = c("communication", "supply_chain", "continuity", "risk_assessment"),
  drought = c("supply_chain", "continuity", "risk_assessment"),
  earthquake = c("communication", "surge_planning", "continuity", "risk_assessment"),
  heat = c("communication", "workforce", "surge_planning", "continuity", "risk_assessment"),
  infectious_disease = c("communication", "workforce", "supply_chain", "surge_planning", "risk_assessment", "continuity"),
  chemical = c("communication", "workforce", "supply_chain", "continuity", "risk_assessment"),
  other = c("communication", "risk_assessment", "continuity")
)

domain_recommendation_map <- list(
  communication = "Define hazard-specific communication protocols, alerts, and coordination pathways.",
  workforce = "Specify surge staffing, role reassignments, and just-in-time training for the hazard.",
  supply_chain = "Document critical supply triggers, vendor contingencies, and distribution plans.",
  surge_planning = "Outline capacity expansion, triage flow, and alternate care/site activation steps.",
  risk_assessment = "Include hazard vulnerability assessments and scenario-based assumptions.",
  continuity = "Add continuity procedures for essential services, IT, utilities, and recovery.",
  governance = "Clarify plan ownership, approval, drills, and after-action update cadence."
)

normalize_hazards <- function(fema_summary, cdc_summary, nws_summary) {
  # Combine FEMA/CDC/NWS summaries into a unified hazard table.
  hazards <- list()
  current_year <- as.integer(format(Sys.Date(), "%Y"))

  if (!is.null(fema_summary$table) && nrow(fema_summary$table) > 0) {
    fema_rows <- fema_summary$table |>
      mutate(
        hazard_info = map(incidentType, ~ normalize_hazard_type(.x, "FEMA")),
        hazard_type = map_chr(hazard_info, "type"),
        hazard_label = map_chr(hazard_info, "label"),
        source = "FEMA",
        frequency = events,
        last_year = parse_year_range_end(year_range),
        recency_years = ifelse(is.na(last_year), NA_real_, current_year - last_year),
        severity = NA_real_
      ) |>
      select(hazard_type, hazard_label, source, frequency, recency_years, severity, last_year)
    hazards <- c(hazards, list(fema_rows))
  }

  if (!is.null(cdc_summary$table) && nrow(cdc_summary$table) > 0) {
    cdc_rows <- cdc_summary$table |>
      mutate(
        hazard_info = map(condition, ~ normalize_hazard_type(.x, "CDC")),
        hazard_type = map_chr(hazard_info, "type"),
        hazard_label = map_chr(hazard_info, "label"),
        source = "CDC",
        frequency = ifelse(!is.na(weeks_reported), weeks_reported, cases),
        last_year = as.integer(format(Sys.Date(), "%Y")),
        recency_years = 0,
        severity = cases
      ) |>
      select(hazard_type, hazard_label, source, frequency, recency_years, severity, last_year)
    hazards <- c(hazards, list(cdc_rows))
  }

  if (!is.null(nws_summary$table) && nrow(nws_summary$table) > 0) {
    nws_rows <- nws_summary$table |>
      mutate(
        hazard_info = map(event_type, ~ normalize_hazard_type(.x, "NWS")),
        hazard_type = map_chr(hazard_info, "type"),
        hazard_label = map_chr(hazard_info, "label"),
        source = "NWS",
        frequency = events,
        last_year = parse_year_range_end(year_range),
        recency_years = ifelse(is.na(last_year), NA_real_, current_year - last_year),
        severity = total_damage + (total_deaths * 1e6) + (total_injuries * 1e4)
      ) |>
      select(hazard_type, hazard_label, source, frequency, recency_years, severity, last_year)
    hazards <- c(hazards, list(nws_rows))
  }

  if (length(hazards) == 0) return(tibble::tibble())
  bind_rows(hazards)
}

scale_to_unit <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) return(rep(0, length(x)))
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(1, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

score_hazards <- function(hazard_table, weight_frequency = 0.45, weight_recency = 0.35, weight_severity = 0.2) {
  # Priority scoring blends frequency, recency, and severity (normalized 0-1).
  if (is.null(hazard_table) || nrow(hazard_table) == 0) return(hazard_table)

  freq_score <- scale_to_unit(hazard_table$frequency)
  recency_score <- ifelse(
    is.na(hazard_table$recency_years),
    0,
    1 - scale_to_unit(hazard_table$recency_years)
  )
  severity_score <- scale_to_unit(hazard_table$severity)

  if (all(is.na(hazard_table$severity)) || all(severity_score == 0)) {
    weight_severity <- 0
  }
  total_weight <- weight_frequency + weight_recency + weight_severity
  weight_frequency <- weight_frequency / total_weight
  weight_recency <- weight_recency / total_weight
  weight_severity <- weight_severity / total_weight

  hazard_table |>
    mutate(
      priority_score = (freq_score * weight_frequency) +
        (recency_score * weight_recency) +
        (severity_score * weight_severity),
      priority = case_when(
        priority_score >= 0.66 ~ "High",
        priority_score >= 0.33 ~ "Moderate",
        TRUE ~ "Low"
      )
    )
}

evaluate_hazard_coverage <- function(plan_text, hazard_table, extraction_domains = NULL, max_n = 5) {
  # Evaluate plan coverage for the highest-priority hazards using the LLM.
  if (is.null(hazard_table) || nrow(hazard_table) == 0) return(list())

  fallback_status <- list()
  if (!is.null(extraction_domains)) {
    norm_domains <- normalize_domains(extraction_domains)
    fallback_status <- setNames(
      tolower(map_chr(norm_domains, "status")),
      map_chr(norm_domains, "id")
    )
  }

  top_hazards <- hazard_table |>
    arrange(desc(priority_score)) |>
    slice_head(n = max_n)

  results <- lapply(seq_len(nrow(top_hazards)), function(i) {
    row <- top_hazards[i, ]
    expected_domains <- hazard_domain_map[[row$hazard_type]] %||% hazard_domain_map$other
    expected_domains <- unique(c(expected_domains, "governance"))

    coverage <- tryCatch(
      assess_hazard_coverage(
        plan_text = plan_text,
        hazard_label = row$hazard_label,
        hazard_type = row$hazard_type,
        expected_domains = expected_domains
      ),
      error = function(e) NULL
    )

    observed <- coverage$observed
    if (is.null(observed) || !is.list(observed)) {
      observed <- lapply(expected_domains, function(d) {
        status <- fallback_status[[d]] %||% "missing"
        list(
          domain_id = d,
          label = stringr::str_to_title(gsub("_", " ", d)),
          status = status,
          rationale = if (status == "present") {
            "Plan text indicates this domain is addressed."
          } else if (status == "partial") {
            "Plan text mentions this domain but lacks hazard-specific detail."
          } else {
            "Plan text does not address this domain for the hazard."
          },
          evidence = "",
          recommendation = domain_recommendation_map[[d]] %||% "Add clear hazard-specific procedures for this domain."
        )
      })
    }

    recommendations <- vapply(
      observed,
      function(item) {
        if (!is.list(item)) return("")
        status <- tolower(item$status %||% "")
        if (status %in% c("missing", "partial")) {
          return(item$recommendation %||% "")
        }
        ""
      },
      character(1)
    )

    list(
      hazard = row$hazard_label,
      priority = row$priority,
      source = row$source,
      expected_domains = expected_domains,
      observed = observed,
      recommendations = recommendations[nzchar(recommendations)]
    )
  })

  results
}

map_incident_to_domains <- function(incident_type) {
  incident <- tolower(incident_type %||% "")

  if (str_detect(incident, "hurricane|typhoon|tropical")) {
    return(c("communication", "supply_chain", "surge_planning", "continuity", "risk_assessment"))
  }
  if (str_detect(incident, "flood")) {
    return(c("communication", "supply_chain", "continuity", "risk_assessment"))
  }
  if (str_detect(incident, "fire|wildfire")) {
    return(c("communication", "workforce", "supply_chain", "continuity", "risk_assessment"))
  }
  if (str_detect(incident, "tornado")) {
    return(c("communication", "surge_planning", "continuity", "risk_assessment"))
  }
  if (str_detect(incident, "severe storm|storm")) {
    return(c("communication", "supply_chain", "continuity", "risk_assessment"))
  }
  if (str_detect(incident, "thunderstorm wind|high wind|strong wind|wind")) {
    return(c("communication", "supply_chain", "continuity", "risk_assessment"))
  }
  if (str_detect(incident, "hail|lightning")) {
    return(c("communication", "continuity", "risk_assessment"))
  }
  if (str_detect(incident, "earthquake|tsunami|volcano")) {
    return(c("communication", "surge_planning", "continuity", "risk_assessment"))
  }
  if (str_detect(incident, "winter|snow|ice")) {
    return(c("communication", "supply_chain", "continuity", "risk_assessment"))
  }
  if (str_detect(incident, "drought")) {
    return(c("supply_chain", "continuity", "risk_assessment"))
  }
  if (str_detect(incident, "biological|pandemic")) {
    return(c("communication", "workforce", "supply_chain", "surge_planning", "risk_assessment", "continuity"))
  }
  if (str_detect(incident, "chemical|hazmat")) {
    return(c("communication", "workforce", "supply_chain", "continuity", "risk_assessment"))
  }

  c("communication", "risk_assessment", "continuity")
}

map_cdc_to_domains <- function(condition) {
  c("communication", "workforce", "supply_chain", "surge_planning", "risk_assessment", "continuity")
}

status_rank <- function(status) {
  status <- tolower(status %||% "missing")
  dplyr::case_when(
    status == "present" ~ 3,
    status == "partial" ~ 2,
    status == "missing" ~ 1,
    TRUE ~ 1
  )
}

worst_status <- function(statuses) {
  if (length(statuses) == 0) return("missing")
  statuses <- ifelse(is.na(statuses), "missing", statuses)
  if (any(tolower(statuses) == "missing")) return("missing")
  if (any(tolower(statuses) == "partial")) return("partial")
  "present"
}

build_coverage_table <- function(fema_summary, cdc_summary, nws_summary, extraction) {
  domain_status <- normalize_domains(extraction$domains)
  status_map <- setNames(
    tolower(map_chr(domain_status, "status")),
    map_chr(domain_status, "id")
  )

  coverage_rows <- list()

  if (nrow(fema_summary$table) > 0) {
    fema_rows <- fema_summary$table |>
      mutate(
        source = "FEMA",
        event = incidentType,
        count = events,
        required_domains = map(event, map_incident_to_domains),
        coverage = map_chr(required_domains, ~ worst_status(status_map[.x]))
      ) |>
      select(source, event, count, coverage, required_domains)

    coverage_rows <- c(coverage_rows, list(fema_rows))
  }

  if (nrow(cdc_summary$table) > 0) {
    cdc_rows <- cdc_summary$table |>
      mutate(
        source = "CDC",
        event = condition,
        count = cases,
        required_domains = map(event, map_cdc_to_domains),
        coverage = map_chr(required_domains, ~ worst_status(status_map[.x]))
      ) |>
      select(source, event, count, coverage, required_domains)

    coverage_rows <- c(coverage_rows, list(cdc_rows))
  }

  if (nrow(nws_summary$table) > 0) {
    nws_rows <- nws_summary$table |>
      mutate(
        source = "NWS Storm Events",
        event = event_type,
        count = events,
        required_domains = map(event, map_incident_to_domains),
        coverage = map_chr(required_domains, ~ worst_status(status_map[.x]))
      ) |>
      select(source, event, count, coverage, required_domains)

    coverage_rows <- c(coverage_rows, list(nws_rows))
  }

  if (length(coverage_rows) == 0) {
    return(tibble::tibble())
  }

  bind_rows(coverage_rows) |>
    mutate(
      domains = map_chr(required_domains, ~ paste(.x, collapse = ", "))
    ) |>
    select(source, event, count, coverage, domains)
}

build_regional_gap_prompt <- function(extraction, fema_summary, cdc_summary, nws_summary, weather_summary, coverage_table) {
  plan_status <- normalize_domains(extraction$domains)
  plan_json <- toJSON(plan_status, auto_unbox = TRUE, pretty = TRUE)

  fema_json <- if (nrow(fema_summary$table) > 0) {
    toJSON(fema_summary$table, auto_unbox = TRUE, pretty = TRUE)
  } else {
    "[]"
  }

  cdc_json <- if (nrow(cdc_summary$table) > 0) {
    toJSON(cdc_summary$table, auto_unbox = TRUE, pretty = TRUE)
  } else {
    "[]"
  }

  coverage_json <- if (nrow(coverage_table) > 0) {
    toJSON(coverage_table, auto_unbox = TRUE, pretty = TRUE)
  } else {
    "[]"
  }

  nws_json <- if (nrow(nws_summary$table) > 0) {
    toJSON(nws_summary$table, auto_unbox = TRUE, pretty = TRUE)
  } else {
    "[]"
  }

  weather_json <- if (nrow(weather_summary$table) > 0) {
    toJSON(weather_summary$table, auto_unbox = TRUE, pretty = TRUE)
  } else {
    "[]"
  }

  paste(
    "You are a healthcare emergency preparedness consultant.",
    "Interpret the gap between plan coverage and local historical incident patterns.",
    "Use FEMA disaster declarations, CDC notifiable disease patterns, and NWS Storm Events damage history to highlight mismatches.",
    "Use the current NWS forecast to add situational context (do not treat forecast as historical evidence).",
    "Return JSON only in the schema:",
    "{\"summary\":\"...\",\"gap_insights\":[{\"pattern\":\"...\",\"coverage\":\"present|partial|missing\",\"impact\":\"...\"}],\"recommended_focus\":[\"...\"]}",
    "\nPlan domain status:",
    plan_json,
    "\nFEMA incident summary:",
    fema_json,
    "\nCDC condition summary:",
    cdc_json,
    "\nNWS Storm Events summary:",
    nws_json,
    "\nNWS Forecast snapshot:",
    weather_json,
    "\nCoverage crosswalk:",
    coverage_json
  )
}

build_regional_analysis <- function(
  region_country,
  region_state,
  region_county,
  history_years,
  extraction,
  plan_text,
  fema_types = c("DR", "EM"),
  incident_types = character(0)
) {
  if (!is.null(region_country) && tolower(region_country) != "united states") {
    note <- "Regional data sources are currently US-only (FEMA, CDC, NWS)."
    empty <- list(table = tibble::tibble(), note = note)
    return(list(
      fema = empty,
      cdc = empty,
      nws = empty,
      weather = empty,
      coverage = tibble::tibble(),
      hazards = tibble::tibble(),
      hazard_eval = list()
    ))
  }

  fema_note <- NULL
  fema_df <- tryCatch(
    fetch_fema_disasters(
      region_state,
      years_back = history_years,
      county = region_county,
      disaster_types = fema_types,
      incident_types = incident_types
    ),
    error = function(e) {
      fema_note <<- "FEMA data temporarily unavailable."
      tibble::tibble()
    }
  )

  if (nrow(fema_df) == 0 &&
      !is.null(region_county) &&
      nzchar(region_county) &&
      tolower(region_county) != "not applicable") {
    fema_df_state <- tryCatch(
      fetch_fema_disasters(
        region_state,
        years_back = history_years,
        county = NULL,
        disaster_types = fema_types,
        incident_types = incident_types
      ),
      error = function(e) tibble::tibble()
    )

    if (nrow(fema_df_state) > 0) {
      fema_df <- fema_df_state
      fema_note <- "No county-level FEMA matches found; showing statewide declarations instead."
    }
  }

  fema_summary <- tryCatch(
    summarize_fema_incidents(fema_df),
    error = function(e) list(table = tibble::tibble(), note = "FEMA data temporarily unavailable.")
  )

  if (!is.null(fema_note)) {
    fema_summary$note <- fema_note
  } else if (nrow(fema_df) == 0 && is.null(fema_summary$note)) {
    fema_summary$note <- paste0(
      "No FEMA declarations found (types: ",
      paste(fema_types, collapse = ", "),
      "; years: ",
      history_years,
      ")."
    )
  }

  cdc_result <- tryCatch(
    fetch_cdc_nndss(region_state, years_back = max(3, history_years %/% 2)),
    error = function(e) list(data = tibble::tibble(), cols = list(), note = "CDC data temporarily unavailable.")
  )

  cdc_summary <- tryCatch(
    summarize_cdc_conditions(cdc_result),
    error = function(e) list(table = tibble::tibble(), note = "CDC data temporarily unavailable.")
  )

  nws_result <- fetch_nws_storm_events(region_state, years_back = history_years, county = region_county)
  nws_summary <- summarize_nws_storm_events(nws_result)

  weather_summary <- fetch_nws_forecast_for_region(region_state, region_county)

  coverage_table <- build_coverage_table(fema_summary, cdc_summary, nws_summary, extraction)
  hazard_table <- normalize_hazards(fema_summary, cdc_summary, nws_summary)
  hazard_table <- score_hazards(hazard_table)
  hazard_eval <- evaluate_hazard_coverage(plan_text, hazard_table, extraction$domains, max_n = 5)

  hazard_priority <- if (nrow(hazard_table) > 0) {
    hazard_table |>
      arrange(desc(priority_score)) |>
      mutate(
        priority_score = round(priority_score, 2)
      ) |>
      select(hazard_label, source, frequency, recency_years, severity, priority, priority_score)
  } else {
    tibble::tibble()
  }

  hazard_domain_coverage <- if (length(hazard_eval) > 0) {
    bind_rows(lapply(hazard_eval, function(h) {
      observed <- h$observed %||% list()
      if (length(observed) == 0) return(tibble::tibble())
      tibble::tibble(
        hazard = h$hazard %||% "Unknown",
        priority = h$priority %||% "Unknown",
        source = h$source %||% "Unknown",
        domain = vapply(observed, function(o) o$label %||% o$domain_id %||% "Domain", character(1)),
        status = vapply(observed, function(o) o$status %||% "unknown", character(1)),
        rationale = vapply(observed, function(o) o$rationale %||% "", character(1))
      )
    }))
  } else {
    tibble::tibble()
  }

  list(
    fema = fema_summary,
    cdc = cdc_summary,
    nws = nws_summary,
    weather = weather_summary,
    coverage = coverage_table,
    hazards = hazard_table,
    hazard_eval = hazard_eval,
    hazard_priority = hazard_priority,
    hazard_domain_coverage = hazard_domain_coverage
  )
}
