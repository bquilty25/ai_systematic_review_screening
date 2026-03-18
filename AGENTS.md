# AGENTS: Guiding Principles for Data Analysis

Rules and conventions for this project. All code must be **reproducible**, **readable**, **efficient**, and **tested**.

Requires `tidyverse >= 2.0.0` (for native pipe support, `reframe()`, etc.).

-----

## Core Principles

  * **Reproducibility**: Use `here::here()` for all file paths. Never use absolute paths or `setwd()`. Use `renv` to lock package versions.
  * **Vectorised Operations**: Favour vectorised operations and `tidyverse` verbs over explicit loops (`for`, `while`).
  * **Modularity**: Keep custom functions in `R/` scripts. Analysis files (`.qmd`) should source these and focus on narrative and results.
  * **Style**: Follow the `tidyverse` style guide. Use `snake_case` and the native pipe `|>`. Use `styler::style_file()` to auto-format before committing.
  * **Testing**: Write `testthat` unit tests for all custom functions in `R/`. Tests live in `tests/testthat/`.
  * **Data Storage**: Use `qs::qsave()` and `qs::qread()` for intermediate/processed data objects, not `saveRDS()`.
  * **Version Control**: Use descriptive commit messages (imperative mood, e.g. "Add sensitivity analysis"). Never commit `.qs`, `.Rds`, or large data files to git.

-----

## Key Packages & Functions

| Package    | Role                 | Key functions                                                      |
| ---------- | -------------------- | ------------------------------------------------------------------ |
| `dplyr`    | Data manipulation    | `mutate()`, `filter()`, `summarise()`, `group_by()`, `left_join()` |
| `ggplot2`  | Visualisation        | `ggplot()`, `geom_*()`, `facet_wrap()`, `theme()`                  |
| `readr`    | File I/O             | `read_csv()`, `write_csv()`                                        |
| `purrr`    | Functional iteration | `map()`, `map_dfr()`, `walk()`                                     |
| `glue`     | String interpolation | `glue()`, `glue_data()`                                            |
| `here`     | Path management      | `here::here()`                                                     |
| `testthat` | Unit testing         | `test_that()`, `expect_equal()`, `expect_true()`, `expect_error()` |
| `qs`       | Fast serialisation   | `qs::qsave()`, `qs::qread()`                                       |
| `styler`   | Code formatting      | `style_file()`, `style_dir()`                                      |
| `renv`     | Dependency lock      | `renv::init()`, `renv::snapshot()`, `renv::restore()`              |
| `quarto`   | Dynamic reports      | Inline code: `` `r expr` ``                                        |

-----

## Quarto Writing Style

  * Use concise, scientific prose — paragraphs over bullet points.
  * Structure: Abstract, Introduction, Methods, Results, Discussion.
  * Use UK English throughout.
  * Tone: objective, professional, analytically sharp.
  * Results section: state findings only; reserve interpretation for the Discussion.
  * Use inline code (`` `r ...` ``) to embed dynamic results directly in text.
  * All code chunks must have unique, descriptive labels (e.g. `#| label: fig-survival-curve`).

-----

## Project Structure

```
.
├── my_project.Rproj
├── README.md
├── AGENTS.md
├── .gitignore          # *.qs, *.Rds, data/raw/, renv/library/
├── renv.lock
├── R/
│   ├── 01_data_cleaning_functions.R
│   └── 02_analysis_helper_functions.R
├── tests/
│   ├── testthat.R
│   └── testthat/
│       └── test-data_cleaning_functions.R
├── data/
│   ├── raw/
│   │   └── source_data.csv
│   └── processed/
│       └── cleaned_data.qs
└── outputs/
    ├── plots/
    │   └── distribution_plot.png
    ├── tables/
    │   └── summary_table.csv
    └── docs/
        └── report.qmd
```

-----

## Workflow

1. **Setup**: Load packages and `source()` functions from `R/`.
2. **Import**: Read raw data with `readr` and `here::here()`.
3. **Process**: Clean and transform with `dplyr`; save with `qs::qsave()`.
4. **Test**: Run `testthat::test_dir(here::here("tests"))` to validate functions.
5. **Analyse**: Use vectorised `dplyr` and `purrr` operations.
6. **Report**: Present results in Quarto with `ggplot2` plots and inline code.