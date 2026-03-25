# AI Systematic Review Screening

An R pipeline that automates title/abstract and full-text screening for systematic reviews. It implements a two-stage LLM workflow — abstract screening from a citations CSV, followed by full-text screening from PDFs — with optional structured data extraction. The approach is modelled on the [Otto-SR architecture](https://doi.org/10.1101/2025.06.13.25329541) (Cao et al., 2025), using ScreenPrompt-style chain-of-thought prompting with a deliberate inclusion bias to minimise missed relevant studies.

## Overview

```
Citations CSV ──► Abstract screening ──► [included abstracts]
                                                  │
                              PDFs ──► opendataloader-pdf ──► Markdown ──► Full-text screening ──► [included papers]
                                                                                                          │
                                                                                          Data extraction (optional)
                                                                                                          │
                                                              Human reference (optional) ──► Agreement metrics
                                                                                           (sensitivity, specificity,
                                                                                            κ, Wilson 95 % CIs)
```

The pipeline produces:

- **Abstract screening results** — per-citation include/exclude decision with confidence, rationale, and per-criterion chain-of-thought evaluation
- **Full-text screening results** — per-paper decision with the same structured output
- **Extraction results** (optional) — user-defined variables extracted from each included full-text paper
- **Agreement metrics** (optional) — sensitivity, specificity, precision, F1, Cohen's κ with Wilson score 95 % CIs for each stage

## Prerequisites

- **R** ≥ 4.1 with packages: `tidyverse`, `ellmer`, `readxl`, `janitor`, `cli`, `glue`, `qs`, `here`, `testthat`, `furrr`, `future`
- **Python** ≥ 3.10 with [`opendataloader-pdf`](https://github.com/opendataloader-project/opendataloader-pdf)
- **Java** ≥ 11 (required by opendataloader-pdf)

```bash
pip install opendataloader-pdf
```

Set your API key in `.Renviron`:

```
ANTHROPIC_API_KEY=sk-ant-...
# or OPENAI_API_KEY / GOOGLE_API_KEY for other providers
```

## Project Structure

```
├── R/
│   ├── 01_pdf_conversion_functions.R   # PDF → markdown + citation CSV ingestion
│   ├── 02_screening_functions.R        # Abstract and full-text LLM screening
│   ├── 03_comparison_functions.R       # Agreement metrics with Wilson CIs
│   ├── 04_extraction_functions.R       # Structured data extraction
│   └── run_pipeline.R                  # Full pipeline orchestration
├── tests/testthat/
│   ├── test-pdf_conversion_functions.R
│   ├── test-screening_functions.R
│   ├── test-comparison_functions.R
│   └── test-extraction_functions.R
├── data/
│   ├── raw/
│   │   ├── pdfs/                       # Place full-text PDFs here
│   │   └── citations.csv               # Title/abstract CSV for stage 1
│   └── processed/
│       ├── markdown/                   # Converted markdown files
│       ├── abstract_screening_results.qs
│       ├── fulltext_screening_results.qs
│       └── extraction_results.qs
└── outputs/tables/                     # CSV exports of all results
```

## Quick Start

### Two-stage screening

```r
source(here::here("R", "run_pipeline.R"))

results <- run_pipeline(
  citations_csv = here::here("data", "raw", "citations.csv"),
  criteria = "Include papers reporting original epidemiological data on
              disease X in humans. Exclude reviews, editorials, and
              animal studies.",
  criteria_names = c(
    "Original data",
    "Epidemiological study",
    "Human subjects"
  )
)

results$abstract_screening  # tibble of abstract-stage decisions
results$fulltext_screening  # tibble of full-text-stage decisions
```

The `citations.csv` must contain at minimum `title` and `abstract` columns. Use `title_col`, `abstract_col`, and `id_col` to specify non-default column names.

### Full-text screening only

Omit `citations_csv` to skip abstract screening and proceed directly to full-text:

```r
results <- run_pipeline(
  criteria = "...",
  criteria_names = c(...)
)
```

### With data extraction

Pass `extraction_variables` to extract structured fields from each paper that passes full-text screening:

```r
results <- run_pipeline(
  citations_csv = here::here("data", "raw", "citations.csv"),
  criteria = "...",
  extraction_variables = list(
    sample_size  = "Total number of participants enrolled",
    study_design = list(
      type        = "enum",
      values      = c("RCT", "cohort", "case-control", "cross-sectional", "other"),
      description = "Study design"
    ),
    mean_age     = list(type = "number", description = "Mean age of participants in years"),
    blinded      = list(type = "boolean", description = "Whether outcome assessors were blinded")
  )
)

results$extraction  # one row per included paper
```

Each variable is either a plain string (description of a free-text field) or a named list with `type` (`"string"`, `"number"`, `"boolean"`, or `"enum"`), `values` (for enum), and `description`.

### Comparing against human screening

```r
results <- run_pipeline(
  citations_csv  = here::here("data", "raw", "citations.csv"),
  criteria       = "...",
  excel_path     = here::here("data", "raw", "human_screening.xlsx"),
  human_id_col   = "paper_id",
  human_decision_col = "decision"
)

results$summary
#> # A tibble: 2 × 15
#>   stage    n_total n_agree accuracy sensitivity sensitivity_lower …
#>   <chr>      <int>   <int>    <dbl>       <dbl>            <dbl>
#> 1 abstract     …
#> 2 fulltext     …
```

Sensitivity and specificity are reported with Wilson score 95 % CIs.

### Parallel processing

Enable parallel API calls with `parallel = TRUE`:

```r
results <- run_pipeline(
  citations_csv = here::here("data", "raw", "citations.csv"),
  criteria      = "...",
  parallel      = TRUE,
  n_workers     = 4
)
```

## Configuration

| Argument | Description | Default |
|----------|-------------|---------|
| `criteria` | Free-text screening criteria | *(required)* |
| `criteria_names` | Named criteria for per-criterion chain-of-thought | `NULL` |
| `citations_csv` | Path to title/abstract CSV for abstract screening | `NULL` (skip stage 1) |
| `id_col` | Citation ID column in CSV | `NULL` (auto-generated) |
| `title_col` | Title column in CSV | `"title"` |
| `abstract_col` | Abstract column in CSV | `"abstract"` |
| `extraction_variables` | Named list defining variables to extract | `NULL` (skip extraction) |
| `provider` | LLM provider (`"anthropic"`, `"openai"`, `"google"`) | `"anthropic"` |
| `model` | Model name (e.g. `"claude-sonnet-4-20250514"`) | Provider default |
| `parallel` | Enable parallel batch processing via `furrr` | `FALSE` |
| `n_workers` | Number of parallel workers | `4` |
| `excel_path` | Path to human screening Excel file for comparison | `NULL` |
| `human_id_col` | Join column in the Excel sheet | — |
| `human_decision_col` | Decision column in the Excel sheet | — |
| `include_value` / `exclude_value` | Include/exclude codes in Excel | `"include"` / `"exclude"` |
| `overwrite_md` | Re-convert PDFs even if markdown exists | `FALSE` |

## Prompting approach

Both screening stages use a ScreenPrompt-style approach (Cao et al., 2025):

- The LLM is asked to evaluate each named criterion individually before making a final decision (`criteria_evaluation` chain-of-thought field).
- **Inclusion bias**: the system prompt instructs the model to include whenever evidence is absent or ambiguous. At the abstract stage the threshold is very liberal (exclude only on clear, unambiguous grounds); at the full-text stage the bias is moderate (include whenever any doubt remains). This minimises false negatives, prioritising recall over precision.
- On API errors, papers default to `include` / `low` confidence rather than being silently dropped.

## Running Tests

```r
source(here::here("R", "01_pdf_conversion_functions.R"))
source(here::here("R", "02_screening_functions.R"))
source(here::here("R", "03_comparison_functions.R"))
source(here::here("R", "04_extraction_functions.R"))
testthat::test_dir(here::here("tests", "testthat"))
```

## References

Cao, J. et al. (2025). *Otto-SR: An automated AI agent for systematic reviews*. medRxiv. https://doi.org/10.1101/2025.06.13.25329541

## Licence

MIT
