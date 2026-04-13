library(httr)

fetch_json <- function(url, query = list(), headers = list(), timeout_seconds = 30) {
  header_vec <- character(0)
  if (!is.null(headers) && length(headers) > 0) {
    header_vec <- unlist(headers, use.names = TRUE)
  }
  if (is.null(names(header_vec)) || !("User-Agent" %in% names(header_vec))) {
    header_vec <- c(header_vec, "User-Agent" = "datavizfinalproject/1.0")
  }
  if (is.null(names(header_vec)) || !("Accept" %in% names(header_vec))) {
    header_vec <- c(header_vec, "Accept" = "application/json")
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

regional_cache <- new.env(parent = emptyenv())

cache_get <- function(cache_key, max_age_minutes = 60) {
  entry <- regional_cache[[cache_key]]
  if (is.null(entry)) return(NULL)
  if (is.null(entry$ts)) return(NULL)
  age <- as.numeric(difftime(Sys.time(), entry$ts, units = "mins"))
  if (!is.na(age) && age <= max_age_minutes) return(entry$value)
  NULL
}

cache_set <- function(cache_key, value) {
  regional_cache[[cache_key]] <- list(value = value, ts = Sys.time())
  value
}

fetch_cdc_metadata <- function() {
  fetch_json("https://data.cdc.gov/api/views/x9gk-5huc.json")
}

read_wonder_request <- function(path) {
  if (!file.exists(path)) {
    return(list(params = list(), note = paste0("CDC WONDER request file not found at ", path)))
  }

  if (requireNamespace("xml2", quietly = TRUE)) {
    xml <- xml2::read_xml(path)
    nodes <- xml2::xml_find_all(xml, ".//parameter")
    names <- xml2::xml_text(xml2::xml_find_first(nodes, ".//name"))
    values <- xml2::xml_text(xml2::xml_find_first(nodes, ".//value"))
    params <- as.list(values)
    names(params) <- names
    return(list(params = params, note = NULL))
  }

  text <- readLines(path, warn = FALSE, encoding = "UTF-8")
  text <- paste(text, collapse = "")
  names <- str_match_all(text, "<name>(.*?)</name>")[[1]][, 2]
  values <- str_match_all(text, "<value>(.*?)</value>")[[1]][, 2]
  if (length(names) == 0 || length(values) == 0) {
    return(list(params = list(), note = "CDC WONDER request file could not be parsed."))
  }
  len <- min(length(names), length(values))
  params <- as.list(values[seq_len(len)])
  names(params) <- names[seq_len(len)]
  list(params = params, note = NULL)
}

normalize_state_token <- function(value) {
  value <- toupper(value %||% "")
  value <- str_replace_all(value, "[^A-Z]", "")
  value
}

read_wonder_response_csv <- function(path) {
  if (!file.exists(path)) {
    return(list(data = tibble::tibble(), note = paste0("CDC WONDER response file not found at ", path)))
  }
  df <- tryCatch(
    suppressWarnings(readr::read_csv(path, show_col_types = FALSE, progress = FALSE)),
    error = function(e) tibble::tibble()
  )
  if (nrow(df) == 0) {
    return(list(data = tibble::tibble(), note = "CDC WONDER response file is empty or unreadable."))
  }
  list(data = df, note = NULL)
}

parse_wonder_csv <- function(text) {
  if (!nzchar(text)) {
    return(tibble::tibble())
  }

  if (str_detect(text, "<html") || str_detect(text, "Request failed") || str_detect(text, "ERROR")) {
    return(tibble::tibble())
  }

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  lines <- lines[!str_detect(lines, "^\\s*$")]
  if (length(lines) == 0) return(tibble::tibble())

  header_idx <- which(str_detect(lines, "Year") & str_detect(lines, "Condition"))
  if (length(header_idx) == 0) {
    header_idx <- which(str_detect(lines, "Year") | str_detect(lines, "Condition") | str_detect(lines, "State"))
  }
  if (length(header_idx) == 0) {
    header_idx <- 1
  } else {
    header_idx <- header_idx[1]
  }

  csv_text <- paste(lines[header_idx:length(lines)], collapse = "\n")
  df <- tryCatch(
    suppressWarnings(readr::read_csv(I(csv_text), show_col_types = FALSE, progress = FALSE)),
    error = function(e) tibble::tibble()
  )

  if (nrow(df) == 0) return(df)

  df <- df |>
    filter(!if_all(everything(), ~ is.na(.x)))

  df
}

fetch_cdc_wonder_nndss <- function(state_code, years_back = 5) {
  request_path <- app_config$cdc_wonder_request %||% ""
  request <- read_wonder_request(request_path)
  if (length(request$params) == 0) {
    return(list(data = tibble::tibble(), cols = list(), note = request$note %||% "CDC WONDER request template missing."))
  }

  params <- request$params
  dataset_code <- params$dataset_code %||% "D130"
  endpoint <- paste0("https://wonder.cdc.gov/controller/datarequest/", dataset_code)

  params[["O_export-format"]] <- "csv"
  params[["O_javascript"]] <- "off"
  params[["O_timeout"]] <- params[["O_timeout"]] %||% "600"
  params[["action-Send"]] <- "Send"
  params[["stage"]] <- "request"

  year_key <- paste0("V_", dataset_code, ".V1")
  state_key <- paste0("V_", dataset_code, ".V2")
  condition_key <- paste0("V_", dataset_code, ".V3")

  if (!year_key %in% names(params)) {
    year_key <- names(params)[str_detect(names(params), "\\.V1$")][1] %||% year_key
  }
  if (!state_key %in% names(params)) {
    state_key <- names(params)[str_detect(names(params), "\\.V2$")][1] %||% state_key
  }
  if (!condition_key %in% names(params)) {
    condition_key <- names(params)[str_detect(names(params), "\\.V3$")][1] %||% condition_key
  }

  params[[year_key]] <- "*All*"
  params[[condition_key]] <- "*All*"

  state_name <- state.name[match(state_code, state.abb)] %||% state_code
  state_candidates <- c(state_code, state_name, "*All*")

  post_wonder <- function(request_params, use_response = FALSE) {
    response <- tryCatch(
      httr::POST(
        endpoint,
        body = request_params,
        encode = "form",
        timeout(60),
        user_agent("datavizfinalproject/1.0"),
        add_headers(
          "Accept" = "text/csv, text/plain, */*",
          "Content-Type" = "application/x-www-form-urlencoded"
        )
      ),
      error = function(e) NULL
    )
    if (is.null(response) || httr::http_error(response)) {
      return(NULL)
    }
    if (use_response) return(response)
    httr::content(response, as = "text", encoding = "UTF-8")
  }

  response_text <- NULL
  response_obj <- NULL
  used_state_value <- NULL
  for (state_value in state_candidates) {
    params[[state_key]] <- state_value
    response_obj <- post_wonder(params, use_response = TRUE)
    response_text <- if (is.null(response_obj)) NULL else httr::content(response_obj, as = "text", encoding = "UTF-8")
    if (!is.null(response_text) && nzchar(response_text)) {
      used_state_value <- state_value
      break
    }
  }

  if (is.null(response_text)) {
    response_path <- app_config$cdc_wonder_response %||% ""
    fallback <- read_wonder_response_csv(response_path)
    if (nrow(fallback$data) > 0) {
      df <- fallback$data
      note <- "CDC WONDER live request failed; using cached response file."
    } else {
      return(list(data = tibble::tibble(), cols = list(), note = "CDC WONDER request failed. Try re-exporting the WONDER API request with O_export-format=csv and ensure no CAPTCHA is required."))
    }
  } else {
    df <- parse_wonder_csv(response_text)
    note <- NULL
  }
  if (!is.null(response_obj) && !is.null(response_text)) {
    content_type <- httr::headers(response_obj)[["content-type"]] %||% ""
    if (!str_detect(tolower(content_type), "csv") && str_detect(tolower(response_text), "<html")) {
      response_path <- app_config$cdc_wonder_response %||% ""
      fallback <- read_wonder_response_csv(response_path)
      if (nrow(fallback$data) > 0) {
        df <- fallback$data
        note <- "CDC WONDER returned HTML; using cached response file."
      } else {
        return(list(data = tibble::tibble(), cols = list(), note = "CDC WONDER returned HTML (likely blocked or agreement required). Please open the WONDER site, accept any agreement, then re-export the API request and response as CSV."))
      }
    }
  }

  if (nrow(df) == 0) {
    response_path <- app_config$cdc_wonder_response %||% ""
    fallback <- read_wonder_response_csv(response_path)
    if (nrow(fallback$data) > 0) {
      df <- fallback$data
      note <- "CDC WONDER returned no data; using cached response file."
    } else {
      return(list(data = tibble::tibble(), cols = list(), note = "CDC WONDER returned no data (or HTML). Re-export the API request and ensure the response is CSV."))
    }
  }

  cols <- names(df)
  state_col <- pick_col(cols, c("state", "states", "jurisdiction", "reporting area"))
  year_col <- pick_col(cols, c("year"))
  condition_col <- pick_col(cols, c("condition", "disease"))
  cases_col <- pick_col(cols, c("case count", "cases", "count", "number"))

  if (!is.null(state_col)) {
    state_code_norm <- normalize_state_token(state_code)
    state_name_norm <- normalize_state_token(state_name)
    target_tokens <- c(state_code_norm, state_name_norm)
    if (state_code_norm %in% c("US", "USA") || state_name_norm %in% c("UNITEDSTATES")) {
      target_tokens <- unique(c(target_tokens, "US", "USA", "UNITEDSTATES"))
    }
    allow_all <- isTRUE(used_state_value == "*All*")
    df <- df |>
      mutate(.state_norm = normalize_state_token(.data[[state_col]])) |>
      filter(if (allow_all) TRUE else .state_norm %in% target_tokens) |>
      select(-.state_norm)
  }

  if (!is.null(year_col)) {
    start_year <- as.integer(format(Sys.Date(), "%Y")) - years_back
    df <- df |>
      mutate(.cdc_year = suppressWarnings(as.integer(.data[[year_col]]))) |>
      filter(is.na(.cdc_year) | .cdc_year >= start_year) |>
      select(-.cdc_year)
  }

  if (!is.null(condition_col)) {
    df <- df |>
      filter(!is.na(.data[[condition_col]])) |>
      filter(!str_detect(tolower(.data[[condition_col]]), "^total$"))
  }

  list(
    data = df,
    cols = list(
      state = state_col,
      year = year_col,
      week = NULL,
      condition = condition_col,
      cases = cases_col
    ),
    note = note
  )
}

pick_col <- function(fields, patterns) {
  lower_fields <- tolower(fields)
  for (pattern in patterns) {
    idx <- which(str_detect(lower_fields, pattern))
    if (length(idx) > 0) return(fields[idx[1]])
  }
  NULL
}

fetch_cdc_nndss_socrata <- function(state_code, years_back = 5) {
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
  error_note <- NULL
  offset <- 0

  repeat {
    query[["$offset"]] <- offset
    batch <- tryCatch(
      fetch_json(base_url, query = query, headers = headers),
      error = function(e) {
        error_note <<- conditionMessage(e)
        list()
      }
    )

    if (length(batch) == 0) break

    records <- c(records, batch)
    if (length(batch) < 50000) break

    offset <- offset + 50000
  }

  df <- if (length(records) == 0) tibble::tibble() else tibble::as_tibble(records)

  if (nrow(df) == 0) {
    note <- "CDC data temporarily unavailable."
    if (!is.null(error_note) && nzchar(error_note)) {
      note <- paste0(note, " ", error_note)
    }
    if (!nzchar(app_config$cdc_app_token %||% "")) {
      note <- paste0(note, " Add CDC_APP_TOKEN to increase access.")
    }
    return(list(data = tibble::tibble(), cols = list(), note = note))
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

fetch_cdc_nndss <- function(state_code, years_back = 5) {
  source <- tolower(app_config$cdc_source %||% "wonder")
  if (source == "wonder") {
    return(fetch_cdc_wonder_nndss(state_code, years_back = years_back))
  }
  fetch_cdc_nndss_socrata(state_code, years_back = years_back)
}

normalize_country_name <- function(name) {
  name <- tolower(name %||% "")
  name <- str_replace_all(name, "[^a-z ]", " ")
  name <- str_replace_all(name, "\\s+", " ")
  str_trim(name)
}

fetch_outbreaks_dataset <- function(cache_hours = 24) {
  url <- "https://raw.githubusercontent.com/jatorresmunguia/disease_outbreak_news/main/Last%20update/outbreaks_14022026.csv"
  cache_path <- file.path(tempdir(), "outbreaks_14022026.csv")
  mem_cached <- cache_get("outbreaks_dataset", max_age_minutes = cache_hours * 60)
  if (!is.null(mem_cached)) return(mem_cached)
  if (file.exists(cache_path)) {
    age_hours <- as.numeric(difftime(Sys.time(), file.info(cache_path)$mtime, units = "hours"))
    if (!is.na(age_hours) && age_hours < cache_hours) {
      df <- readr::read_csv(cache_path, show_col_types = FALSE)
      return(cache_set("outbreaks_dataset", df))
    }
  }
  resp <- tryCatch(httr::GET(url, timeout(30)), error = function(e) NULL)
  if (is.null(resp) || httr::http_error(resp)) {
    return(tibble::tibble())
  }
  raw_text <- httr::content(resp, as = "text", encoding = "UTF-8")
  readr::write_file(raw_text, cache_path)
  df <- readr::read_csv(cache_path, show_col_types = FALSE)
  cache_set("outbreaks_dataset", df)
}

preload_outbreaks_dataset <- function() {
  tryCatch(fetch_outbreaks_dataset(), error = function(e) tibble::tibble())
}

summarize_outbreaks_by_disease <- function(country, years_back = 5, top_n = 8) {
  df <- fetch_outbreaks_dataset()
  if (nrow(df) == 0) {
    return(list(table = tibble::tibble(), note = "Global outbreak data unavailable."))
  }

  pick_df_col <- function(df, candidates) {
    cols <- names(df)
    lower <- tolower(cols)
    for (cand in candidates) {
      idx <- which(lower == cand)
      if (length(idx) > 0) return(cols[idx[1]])
    }
    NULL
  }

  country_col <- pick_df_col(df, c("country"))
  year_col <- pick_df_col(df, c("year"))
  disease_col <- pick_df_col(df, c("disease"))
  if (is.null(country_col) || is.null(year_col) || is.null(disease_col)) {
    return(list(table = tibble::tibble(), note = "Outbreak dataset missing required columns."))
  }

  current_year <- as.integer(format(Sys.Date(), "%Y"))
  start_year <- current_year - years_back + 1
  target <- normalize_country_name(country)
  df <- df |>
    mutate(
      country_norm = normalize_country_name(.data[[country_col]]),
      Year = suppressWarnings(as.integer(.data[[year_col]])),
      Disease = .data[[disease_col]]
    ) |>
    filter(Year >= start_year, Year <= current_year)

  if (target %in% c("united states", "usa", "us", "united states of america")) {
    df <- df |> filter(str_detect(country_norm, "united states"))
  } else if (nzchar(target)) {
    df <- df |> filter(country_norm == target)
  }

  if (nrow(df) == 0) {
    return(list(table = tibble::tibble(), note = "No outbreaks found for the selected country and year range."))
  }

  summary_tbl <- df |>
    filter(!is.na(Disease)) |>
    group_by(Disease) |>
    summarise(
      events = n(),
      year_range = paste0(min(Year, na.rm = TRUE), "–", max(Year, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    arrange(desc(events)) |>
    slice_head(n = top_n)

  list(table = summary_tbl, note = NULL)
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
  if (is.null(year_range)) return(NA_integer_)
  yr <- as.character(year_range)
  parts <- str_split(yr, "–|-")
  vapply(parts, function(p) {
    p <- p[nzchar(p)]
    if (length(p) == 0) return(NA_integer_)
    suppressWarnings(as.integer(p[length(p)]))
  }, integer(1))
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

normalize_hazards <- function(fema_summary, cdc_summary, outbreaks_summary = NULL) {
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

  if (!is.null(outbreaks_summary$table) && nrow(outbreaks_summary$table) > 0) {
    outbreak_rows <- outbreaks_summary$table |>
      mutate(
        hazard_info = map(Disease, ~ normalize_hazard_type(.x, "CDC")),
        hazard_type = map_chr(hazard_info, "type"),
        hazard_label = Disease,
        source = "Disease Outbreaks Data",
        frequency = events,
        last_year = parse_year_range_end(year_range),
        recency_years = ifelse(is.na(last_year), NA_real_, current_year - last_year),
        severity = NA_real_
      ) |>
      select(hazard_type, hazard_label, source, frequency, recency_years, severity, last_year)
    hazards <- c(hazards, list(outbreak_rows))
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

build_regional_gap_prompt <- function(extraction, fema_summary, cdc_summary, outbreaks_summary) {
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

  outbreaks_json <- if (!is.null(outbreaks_summary$table) && nrow(outbreaks_summary$table) > 0) {
    toJSON(outbreaks_summary$table, auto_unbox = TRUE, pretty = TRUE)
  } else {
    "[]"
  }

  paste(
    "You are a healthcare emergency preparedness consultant.",
    "Interpret the gap between plan coverage and regional incident patterns.",
    "Use FEMA disaster declarations, CDC notifiable disease patterns, and global outbreak trends to highlight mismatches.",
    "Return JSON only in the schema:",
    "{\"summary\":\"...\",\"gap_insights\":[{\"pattern\":\"...\",\"coverage\":\"present|partial|missing\",\"impact\":\"...\"}],\"recommended_focus\":[\"...\"]}",
    "\nPlan domain status:",
    plan_json,
    "\nFEMA incident summary:",
    fema_json,
    "\nCDC condition summary:",
    cdc_json,
    "\nGlobal outbreak summary:",
    outbreaks_json
  )
}

build_regional_recommendations_prompt <- function(plan_text, fema_summary, cdc_summary, outbreaks_summary, hazard_identified, framework_choice = "CDC PHEP") {
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

  outbreaks_json <- if (!is.null(outbreaks_summary$table) && nrow(outbreaks_summary$table) > 0) {
    toJSON(outbreaks_summary$table, auto_unbox = TRUE, pretty = TRUE)
  } else {
    "[]"
  }

  hazards_json <- if (!is.null(hazard_identified) && nrow(hazard_identified) > 0) {
    toJSON(hazard_identified, auto_unbox = TRUE, pretty = TRUE)
  } else {
    "[]"
  }

  paste(
    "You are an emergency preparedness analyst.",
    paste0("Use the selected framework: ", framework_choice, "."),
    "Compare the plan text against regional hazards to identify what is missing or weak.",
    "Return concise bullet-style recommendations (short phrases).",
    "Return JSON only in the schema:",
    "{\"recommendations\":[\"...\"]}",
    "\nPlan text:\n",
    plan_text,
    "\nFEMA incident summary:",
    fema_json,
    "\nCDC condition summary:",
    cdc_json,
    "\nGlobal outbreak summary:",
    outbreaks_json,
    "\nIdentified hazards:",
    hazards_json
  )
}

build_regional_analysis <- function(
  region_country,
  region_state,
  region_county,
  history_years,
  extraction,
  plan_text,
  framework_choice,
  fema_types = c("DR", "EM"),
  incident_types = character(0)
) {
  timing_mark <- function(label, start_time) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    message(sprintf("[timing] %s: %.2fs", label, elapsed))
  }

  t0 <- Sys.time()
  cache_key_base <- paste(
    region_country %||% "",
    region_state %||% "",
    region_county %||% "",
    history_years %||% "",
    paste(fema_types %||% "", collapse = ","),
    paste(incident_types %||% "", collapse = ","),
    app_config$cdc_source %||% "wonder",
    sep = "|"
  )

  non_us <- !is.null(region_country) && tolower(region_country) != "united states"

  fema_note <- NULL
  t_fema <- Sys.time()
  cached_fema <- cache_get(paste0("fema|", cache_key_base), max_age_minutes = 180)
  fema_df <- if (non_us) {
    fema_note <- "FEMA data is U.S.-only."
    tibble::tibble()
  } else if (!is.null(cached_fema)) {
    cached_fema
  } else {
    tryCatch(
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
  }
  if (!non_us) {
    cache_set(paste0("fema|", cache_key_base), fema_df)
  }
  timing_mark("FEMA fetch+cache", t_fema)

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

  t_cdc <- Sys.time()
  cached_cdc <- cache_get(paste0("cdc|", cache_key_base), max_age_minutes = 180)
  cdc_result <- if (non_us) {
    list(data = tibble::tibble(), cols = list(), note = "CDC data is U.S.-only.")
  } else if (is.null(region_state) || !nzchar(region_state) || toupper(region_state) == "NA") {
    list(
      data = tibble::tibble(),
      cols = list(),
      note = "CDC data requires a U.S. state/territory selection."
    )
  } else if (!is.null(cached_cdc)) {
    cached_cdc
  } else {
    tryCatch(
      fetch_cdc_nndss(region_state, years_back = max(1, history_years)),
      error = function(e) list(
        data = tibble::tibble(),
        cols = list(),
        note = paste0("CDC data temporarily unavailable. ", conditionMessage(e))
      )
    )
  }
  if (!non_us && nzchar(region_state %||% "") && toupper(region_state) != "NA") {
    cache_set(paste0("cdc|", cache_key_base), cdc_result)
  }
  timing_mark("CDC fetch+cache", t_cdc)

  cdc_summary <- tryCatch(
    summarize_cdc_conditions(cdc_result),
    error = function(e) list(table = tibble::tibble(), note = "CDC data temporarily unavailable.")
  )

  t_outbreaks <- Sys.time()
  outbreaks_cache_key <- paste0("outbreaks|", normalize_country_name(region_country), "|", history_years)
  outbreaks_summary <- cache_get(outbreaks_cache_key, max_age_minutes = 360)
  if (is.null(outbreaks_summary)) {
    outbreaks_summary <- summarize_outbreaks_by_disease(region_country, years_back = history_years)
    cache_set(outbreaks_cache_key, outbreaks_summary)
  }
  timing_mark("Outbreaks summary", t_outbreaks)

  t_hazards <- Sys.time()
  hazard_table <- tryCatch(
    normalize_hazards(fema_summary, cdc_summary, outbreaks_summary),
    error = function(e) tibble::tibble()
  )
  if (nrow(hazard_table) == 0 && nrow(fema_summary$table) > 0) {
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    hazard_table <- fema_summary$table |>
      mutate(
        hazard_type = "fema_incident",
        hazard_label = incidentType,
        source = "FEMA",
        frequency = events,
        last_year = parse_year_range_end(year_range),
        recency_years = ifelse(is.na(last_year), NA_real_, current_year - last_year),
        severity = NA_real_
      ) |>
      select(hazard_type, hazard_label, source, frequency, recency_years, severity, last_year)
  }
  hazard_table <- tryCatch(
    score_hazards(hazard_table),
    error = function(e) hazard_table
  )
  if (nrow(hazard_table) == 0 && nrow(fema_summary$table) > 0) {
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    hazard_table <- fema_summary$table |>
      mutate(
        hazard_type = "fema_incident",
        hazard_label = incidentType,
        source = "FEMA",
        frequency = events,
        last_year = parse_year_range_end(year_range),
        recency_years = ifelse(is.na(last_year), NA_real_, current_year - last_year),
        severity = NA_real_,
        priority_score = 0.5,
        priority = "Moderate"
      ) |>
      select(hazard_type, hazard_label, source, frequency, recency_years, severity, last_year, priority, priority_score)
  }
  timing_mark("Hazard normalize+score", t_hazards)
  timing_mark("Regional analysis total", t0)

  hazard_identified <- if (nrow(hazard_table) > 0) {
    base <- hazard_table |>
      select(hazard_label, source, frequency, last_year) |>
      mutate(
        year_range = ifelse(
          !is.na(last_year),
          paste0(last_year, "–", last_year),
          NA_character_
        )
      )

    if (nrow(fema_summary$table) > 0) {
      fema_years <- fema_summary$table |>
        select(incidentType, year_range)
      base <- base |>
        left_join(fema_years, by = c("hazard_label" = "incidentType"), suffix = c("", "_fema")) |>
        mutate(
          year_range = ifelse(source == "FEMA" & !is.na(year_range_fema), year_range_fema, year_range)
        ) |>
        select(hazard_label, source, frequency, year_range)
    } else {
      base <- base |>
        select(hazard_label, source, frequency, year_range)
    }

    base
  } else {
    tibble::tibble()
  }

  t_llm_recs <- Sys.time()
  plan_key <- paste0("plan|", nchar(plan_text %||% ""), "|", substr(plan_text %||% "", 1, 120))
  rec_key <- paste0("regional_recs|", cache_key_base, "|", plan_key)
  regional_recommendations <- cache_get(rec_key, max_age_minutes = 360)
  if (is.null(regional_recommendations)) {
    prompt <- build_regional_recommendations_prompt(
      plan_text,
      fema_summary,
      cdc_summary,
      outbreaks_summary,
      hazard_identified,
      framework_choice
    )
    regional_recommendations <- tryCatch(
      call_llm_json(
        system_prompt = "You provide concise, plan-specific recommendations for regional hazards.",
        user_prompt = prompt,
        schema_hint = "regional_recommendations"
      ),
      error = function(e) NULL
    )
    if (is.null(regional_recommendations) || !is.list(regional_recommendations)) {
      regional_recommendations <- list(recommendations = character(0))
    }
    cache_set(rec_key, regional_recommendations)
  }
  timing_mark("Regional recommendations LLM", t_llm_recs)

  list(
    fema = fema_summary,
    cdc = cdc_summary,
    outbreaks = outbreaks_summary,
    hazard_identified = hazard_identified,
    hazard_recommendations = regional_recommendations
  )
}
