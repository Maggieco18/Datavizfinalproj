# Emergency Preparedness Consulting Analyzer

This Shiny app ingests a preparedness plan (paste or upload) and produces a structured consulting-style output aligned to FEMA CPG 101 and FEMA core capabilities:

- Preparedness scorecard by domain
- Gap analysis with evidence
- Prioritized action plan
- Executive summary

## Run Locally

```r
install.packages(c("shiny", "shinycssloaders", "jsonlite", "stringr", "dplyr", "tibble", "purrr", "readr", "httr", "maps"))
# Optional for PDF support
install.packages("pdftools")

shiny::runApp()
```

## LLM Configuration

The app supports OpenAI-compatible chat completions endpoints and Google Gemini. Configure via environment variables:

```bash
export LLM_API_BASE="https://your-llm-endpoint"
export LLM_API_KEY="your-key"
export LLM_MODEL="your-model"
export LLM_TEMPERATURE="0.2"
```

If `LLM_API_BASE` or `LLM_API_KEY` is missing, the app falls back to a mock response so you can test the UI.

### Gemini example

```bash
export LLM_PROVIDER="gemini"
export LLM_API_KEY="my api key"
export LLM_MODEL="gemini-1.5-flash"
# Optional override (defaults to https://generativelanguage.googleapis.com)
export LLM_API_BASE="https://generativelanguage.googleapis.com"
```

## Regional Data

The Regional Context tab fetches live data from FEMA OpenFEMA disaster declarations, CDC NNDSS weekly data, the NWS Storm Events Database (via NOAA/NCEI CSV files), and the current NWS forecast. County dropdowns are powered by the `maps` package. If you are rate-limited by CDC, you can supply an app token:

```bash
export CDC_APP_TOKEN="your-socrata-app-token"
```
