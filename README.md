# AI Systematic Review Screening

An R pipeline that automates the screening step of a systematic review. It converts scientific paper PDFs to markdown, sends them to an LLM for structured screening decisions, and compares the results against a human manual extraction spreadsheet.

## Overview

```
PDFs → opendataloader-pdf → Markdown → ellmer (Claude) → Structured decisions → Comparison with human screening
```

The pipeline produces:

- A per-paper screening decision (`include`/`exclude`) with confidence and rationale
- Agreement metrics against a human reference (accuracy, sensitivity, specificity, precision, F1, Cohen's κ)

## Prerequisites

- **R** ≥ 4.1 with packages: `tidyverse`, `ellmer`, `readxl`, `janitor`, `cli`, `glue`, `qs`, `here`, `testthat`
- **Python** ≥ 3.10 with [`opendataloader-pdf`](https://github.com/opendataloader-project/opendataloader-pdf)
- **Java** ≥ 11 (required by opendataloader-pdf)

```bash
pip install opendataloader-pdf
```

Set your API key in `.Renviron`:

```
ANTHROPIC_API_KEY=sk-ant-...
```

## Project Structure

```
├── R/
│   ├── 01_pdf_conversion_functions.R   # PDF → markdown wrappers
│   ├── 02_screening_functions.R        # LLM screening via ellmer
│   ├── 03_comparison_functions.R       # Agreement metrics
│   └── run_pipeline.R                  # Full pipeline orchestration
├── tests/testthat/
│   ├── test-pdf_conversion_functions.R
│   ├── test-screening_functions.R
│   └── test-comparison_functions.R
├── data/
│   ├── raw/pdfs/                       # Place PDFs here
│   └── processed/
│       ├── markdown/                   # Converted markdown output
│       └── screening_results.qs        # Saved results
└── outputs/tables/                     # CSV results
```

## Quick Start

```r
source(here::here("R", "run_pipeline.R"))

# Screen papers (without human comparison)
results <- run_pipeline(
  criteria = "Include papers reporting original epidemiological data on
              disease X in humans. Exclude reviews, editorials, and
              animal studies.",
  criteria_names = c(
    "Original data",
    "Epidemiological study",
    "Human subjects"
  )
)
```

## Comparing Against Human Screening

```r
results <- run_pipeline(
  criteria = "...",
  excel_path = here::here("data", "raw", "human_screening.xlsx"),
  human_id_col = "paper_id",
  human_decision_col = "decision"
)

results$summary
#> # A tibble: 1 × 12
#>   n_total n_agree accuracy sensitivity specificity precision    f1 kappa …
```

## Configuration

| Argument | Description | Default |
|----------|-------------|---------|
| `criteria` | Free-text screening criteria | *(required)* |
| `criteria_names` | Named criteria for per-criterion tracking | `NULL` |
| `provider` | LLM provider (`"anthropic"`, `"openai"`, `"google"`) | `"anthropic"` |
| `model` | Model name (e.g. `"claude-sonnet-4-20250514"`) | Provider default |
| `excel_path` | Path to human screening Excel file | `NULL` (skip comparison) |
| `human_id_col` | Join column in the Excel sheet | — |
| `human_decision_col` | Decision column in the Excel sheet | — |
| `include_value` / `exclude_value` | How include/exclude are coded in Excel | `"include"` / `"exclude"` |
| `overwrite_md` | Re-convert PDFs even if markdown exists | `FALSE` |

## Running Tests

```r
source(here::here("R", "01_pdf_conversion_functions.R"))
source(here::here("R", "02_screening_functions.R"))
source(here::here("R", "03_comparison_functions.R"))
testthat::test_dir(here::here("tests", "testthat"))
```

## Licence

MIT
