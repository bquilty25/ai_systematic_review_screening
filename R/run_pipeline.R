#' Main Pipeline Orchestration
#'
#' Two-stage automated systematic review pipeline:
#'   Stage 1 — Abstract screening (title + abstract from CSV)
#'   Stage 2 — Full-text screening + data extraction (PDFs → markdown)
#'
#' This script sources all function files and provides `run_pipeline()` which
#' orchestrates the full workflow. It can be run in full (both stages) or in
#' single-stage mode (full-text only, for backwards compatibility).

# Load packages
library(tidyverse)
library(here)
library(ellmer)
library(cli)
library(glue)

# Source function files
source(here::here("R", "01_pdf_conversion_functions.R"))
source(here::here("R", "02_screening_functions.R"))
source(here::here("R", "03_comparison_functions.R"))
source(here::here("R", "04_extraction_functions.R"))

#' Run the full two-stage systematic review screening pipeline
#'
#' Implements a two-stage screening workflow matching the otto-SR architecture
#' (Cao et al. 2025):
#'   1. **Abstract screening** — reads a CSV of title+abstract citations and
#'      screens each using an enhanced ScreenPrompt approach. Only citations
#'      screened as "include" proceed to stage 2.
#'   2. **Full-text screening** — converts PDFs (for included citations only)
#'      to markdown and performs a second, confirmatory screen.
#'   3. **Data extraction** (optional) — extracts user-defined variables from
#'      papers that passed full-text screening.
#'   4. **Comparison** (optional) — compares LLM decisions against a human
#'      reference standard and reports accuracy, sensitivity, specificity,
#'      precision, F1, Cohen's kappa, and 95% Wilson CIs.
#'
#' Both screening stages apply strong **inclusion bias**: citations are only
#' excluded when there is clear, unambiguous evidence against eligibility.
#' This maximises sensitivity, consistent with systematic review methodology.
#'
#' @section Backwards compatibility:
#'   If `citations_csv = NULL`, the abstract screening stage is skipped and
#'   the pipeline runs full-text screening only (original behaviour).
#'
#' @param citations_csv Character or NULL. Path to a CSV **or RIS** file
#'   containing title+abstract citation data for stage-1 abstract screening.
#'   File type is detected automatically by extension (`.ris` → RIS parser
#'   via `synthesisr`; anything else → CSV parser). For CSV files, at
#'   minimum `title` and `abstract` columns are required (configurable via
#'   `title_col` and `abstract_col`). If NULL, skips abstract screening.
#'   Default NULL.
#' @param pdf_dir Character. Path to directory containing PDF files.
#'   Default `data/raw/pdfs`.
#' @param md_dir Character. Path to directory for markdown output.
#'   Default `data/processed/markdown`.
#' @param excel_path Character or NULL. Path to human screening Excel file
#'   for comparison. If NULL, skips the comparison step. Default NULL.
#' @param criteria Character. Text description of the screening criteria
#'   (inclusion and exclusion rules). Required.
#' @param criteria_names Character vector or NULL. Individual criterion names
#'   for per-criterion tracking (adds boolean columns to output).
#' @param extraction_variables Named list or NULL. Variable definitions for
#'   data extraction (see `define_extraction_schema()`). If NULL, skips data
#'   extraction. Default NULL.
#' @param human_id_col Character or NULL. Column in the Excel sheet to join
#'   on. Required if `excel_path` is provided.
#' @param human_decision_col Character or NULL. Column in the Excel sheet
#'   with the include/exclude decision. Required if `excel_path` is provided.
#' @param include_value Character. Value meaning "include" in the human
#'   sheet. Default `"include"`.
#' @param exclude_value Character. Value meaning "exclude" in the human
#'   sheet. Default `"exclude"`.
#' @param provider Character. LLM provider (`"anthropic"`, `"openai"`,
#'   `"google"`). Default `"anthropic"`.
#' @param model Character or NULL. Model name; uses provider default if NULL.
#' @param id_col Character or NULL. Column in `citations_csv` to use as the
#'   citation identifier. If NULL, sequential integers are generated.
#' @param title_col Character. Name of the title column in `citations_csv`
#'   (after `janitor::clean_names()` normalisation). Default `"title"`.
#' @param abstract_col Character. Name of the abstract column in
#'   `citations_csv`. Default `"abstract"`.
#' @param overwrite_md Logical. Re-convert PDFs even if markdown exists.
#'   Default FALSE.
#' @param parallel Logical. Use `furrr` for concurrent API calls. Requires
#'   `furrr` installed and a `future` plan to be set before calling.
#'   Default FALSE.
#' @param n_workers Integer. Number of parallel workers when
#'   `parallel = TRUE`. Default 4.
#' @param save_results Logical. Save results to disk. Default TRUE.
#'
#' @return A named list with:
#'   - `abstract_screening` (if `citations_csv` provided): tibble of
#'     abstract screening decisions
#'   - `fulltext_screening`: tibble of full-text screening decisions
#'   - `extraction` (if `extraction_variables` provided): tibble of
#'     extracted values
#'   - `comparison` (if `excel_path` provided): per-paper comparison tibble
#'   - `summary` (if `excel_path` provided): metrics tibble (one row per
#'     screened stage)
run_pipeline <- function(citations_csv = NULL,
                         pdf_dir = here::here("data", "raw", "pdfs"),
                         md_dir = here::here("data", "processed", "markdown"),
                         excel_path = NULL,
                         criteria,
                         criteria_names = NULL,
                         extraction_variables = NULL,
                         human_id_col = NULL,
                         human_decision_col = NULL,
                         include_value = "include",
                         exclude_value = "exclude",
                         provider = "anthropic",
                         model = NULL,
                         id_col = NULL,
                         title_col = "title",
                         abstract_col = "abstract",
                         overwrite_md = FALSE,
                         parallel = FALSE,
                         n_workers = 4L,
                         save_results = TRUE) {
  cli::cli_h1("AI Systematic Review Screening Pipeline")

  # Set up parallel backend if requested
  if (parallel) {
    rlang::check_installed("furrr",
      reason = "is required for parallel screening"
    )
    rlang::check_installed("future",
      reason = "is required for parallel screening"
    )
    future::plan(future::multisession, workers = n_workers)
    cli::cli_alert_info(
      "Parallel mode enabled: {n_workers} worker{?s}."
    )
    on.exit(future::plan(future::sequential), add = TRUE)
  }

  output <- list()

  # ----------------------------------------------------------------
  # Stage 1 (optional): Abstract screening
  # ----------------------------------------------------------------
  if (!is.null(citations_csv)) {
    cli::cli_h2("Stage 1: Abstract Screening")

    is_ris <- grepl("\\.ris$", citations_csv, ignore.case = TRUE)
    if (is_ris) {
      citations_df <- read_citation_ris(ris_path = citations_csv, id_col = id_col)
    } else {
      citations_df <- read_citation_csv(
        csv_path     = citations_csv,
        id_col       = id_col,
        title_col    = title_col,
        abstract_col = abstract_col
      )
    }

    abstract_results <- screen_abstracts_batch(
      citations_df   = citations_df,
      criteria       = criteria,
      criteria_names = criteria_names,
      provider       = provider,
      model          = model,
      parallel       = parallel
    )

    n_included_abstract <- sum(
      abstract_results$decision == "include",
      na.rm = TRUE
    )
    n_excluded_abstract <- sum(
      abstract_results$decision == "exclude",
      na.rm = TRUE
    )
    cli::cli_alert_success(
      "Abstract screening complete: {n_included_abstract} included, ",
      "{n_excluded_abstract} excluded from {nrow(abstract_results)} citation{?s}."
    )

    output$abstract_screening <- abstract_results

    # Save abstract screening results
    if (save_results) {
      out_dir <- here::here("data", "processed")
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      qs::qsave(
        abstract_results,
        here::here("data", "processed", "abstract_screening_results.qs")
      )
      tbl_dir <- here::here("outputs", "tables")
      if (!dir.exists(tbl_dir)) dir.create(tbl_dir, recursive = TRUE)
      readr::write_csv(
        abstract_results,
        here::here("outputs", "tables", "abstract_screening_results.csv")
      )
      cli::cli_alert_success(
        "Saved abstract screening results to {.path outputs/tables/abstract_screening_results.csv}"
      )
    }

    # Only process PDFs for citations that passed abstract screening
    included_ids <- abstract_results |>
      dplyr::filter(decision == "include") |>
      dplyr::pull(citation_id)

    cli::cli_alert_info(
      "{length(included_ids)} citation{?s} proceed to full-text screening."
    )
  }

  # ----------------------------------------------------------------
  # Full-text prep: PDF → Markdown conversion
  # ----------------------------------------------------------------
  cli::cli_h2(
    if (!is.null(citations_csv)) {
      "Stage 2: Full-Text Screening"
    } else {
      "Step 1: Full-Text Screening"
    }
  )

  cli::cli_h3("PDF to Markdown Conversion")
  conversion_results <- convert_pdfs_to_markdown(
    pdf_dir    = pdf_dir,
    output_dir = md_dir,
    overwrite  = overwrite_md
  )
  cli::cli_alert_success(
    "Conversion: {sum(conversion_results$status == 'success')}/{nrow(conversion_results)} succeeded."
  )

  papers_df <- read_all_markdown_papers(md_dir)
  cli::cli_alert_success("Read {nrow(papers_df)} paper{?s}.")

  if (nrow(papers_df) == 0) {
    cli::cli_abort(
      "No papers to screen. Check your PDF directory and conversion step."
    )
  }

  # ----------------------------------------------------------------
  # Full-text screening
  # ----------------------------------------------------------------
  cli::cli_h3("Full-Text LLM Screening")
  fulltext_results <- screen_papers_batch(
    papers_df      = papers_df,
    criteria       = criteria,
    criteria_names = criteria_names,
    provider       = provider,
    model          = model,
    parallel       = parallel
  )

  n_included_ft <- sum(fulltext_results$decision == "include", na.rm = TRUE)
  n_excluded_ft <- sum(fulltext_results$decision == "exclude", na.rm = TRUE)
  cli::cli_alert_success(
    "Full-text screening complete: {n_included_ft} included, ",
    "{n_excluded_ft} excluded."
  )

  output$fulltext_screening <- fulltext_results

  if (save_results) {
    tbl_dir <- here::here("outputs", "tables")
    if (!dir.exists(tbl_dir)) dir.create(tbl_dir, recursive = TRUE)
    qs::qsave(
      fulltext_results,
      here::here("data", "processed", "fulltext_screening_results.qs")
    )
    readr::write_csv(
      fulltext_results,
      here::here("outputs", "tables", "fulltext_screening_results.csv")
    )
    cli::cli_alert_success(
      "Saved full-text screening to {.path outputs/tables/fulltext_screening_results.csv}"
    )
  }

  # ----------------------------------------------------------------
  # Data extraction (optional)
  # ----------------------------------------------------------------
  if (!is.null(extraction_variables)) {
    cli::cli_h2(
      if (!is.null(citations_csv)) {
        "Stage 3: Data Extraction"
      } else {
        "Step 2: Data Extraction"
      }
    )

    included_papers <- fulltext_results |>
      dplyr::filter(decision == "include") |>
      dplyr::pull(file_name)

    papers_for_extraction <- papers_df |>
      dplyr::filter(file_name %in% included_papers)

    if (nrow(papers_for_extraction) == 0) {
      cli::cli_warn("No included papers to extract data from.")
    } else {
      extraction_results <- extract_papers_batch(
        papers_df = papers_for_extraction,
        variables = extraction_variables,
        provider  = provider,
        model     = model,
        parallel  = parallel
      )
      cli::cli_alert_success(
        "Data extraction complete for {nrow(extraction_results)} paper{?s}."
      )

      output$extraction <- extraction_results

      if (save_results) {
        qs::qsave(
          extraction_results,
          here::here("data", "processed", "extraction_results.qs")
        )
        readr::write_csv(
          extraction_results,
          here::here("outputs", "tables", "extraction_results.csv")
        )
        cli::cli_alert_success(
          "Saved extraction to {.path outputs/tables/extraction_results.csv}"
        )
      }
    }
  }

  # ----------------------------------------------------------------
  # Comparison against human reference (optional)
  # ----------------------------------------------------------------
  if (!is.null(excel_path)) {
    cli::cli_h2("Comparison with Human Reference Standard")

    if (is.null(human_id_col) || is.null(human_decision_col)) {
      cli::cli_abort(
        "Provide {.arg human_id_col} and {.arg human_decision_col} to compare."
      )
    }

    human_data <- load_human_screening(excel_path)

    # Full-text stage comparison
    ft_comparison <- compare_screening_decisions(
      llm_results        = fulltext_results,
      human_results      = human_data,
      llm_id_col         = "file_name",
      human_id_col       = human_id_col,
      human_decision_col = human_decision_col,
      include_value      = include_value,
      exclude_value      = exclude_value
    )
    ft_summary <- summarise_comparison(ft_comparison, stage = "fulltext")

    output$comparison <- ft_comparison
    summary_list <- list(ft_summary)

    # Abstract stage comparison (if available)
    if (!is.null(output$abstract_screening)) {
      abs_comparison <- compare_screening_decisions(
        llm_results = output$abstract_screening |>
          dplyr::rename(file_name = citation_id),
        human_results = human_data,
        llm_id_col = "file_name",
        human_id_col = human_id_col,
        human_decision_col = human_decision_col,
        include_value = include_value,
        exclude_value = exclude_value
      )
      abs_summary <- summarise_comparison(abs_comparison, stage = "abstract")
      summary_list <- c(list(abs_summary), summary_list)
    }

    summary_metrics <- dplyr::bind_rows(summary_list)

    cli::cli_h3("Agreement Metrics")
    purrr::walk(seq_len(nrow(summary_metrics)), function(i) {
      row <- summary_metrics[i, ]
      stg <- if ("stage" %in% names(row)) glue::glue(" [{row$stage}]") else ""
      cli::cli_alert_info(
        "{stg} Accuracy: {row$accuracy}  Sensitivity: {row$sensitivity} [{row$sensitivity_lower}\u2013{row$sensitivity_upper}]  Specificity: {row$specificity} [{row$specificity_lower}\u2013{row$specificity_upper}]  Kappa: {row$kappa}  F1: {row$f1}"
      )
    })

    output$summary <- summary_metrics

    if (save_results) {
      readr::write_csv(
        output$comparison,
        here::here("outputs", "tables", "comparison_detail.csv")
      )
      readr::write_csv(
        summary_metrics,
        here::here("outputs", "tables", "comparison_summary.csv")
      )
      cli::cli_alert_success(
        "Saved comparison summary to {.path outputs/tables/comparison_summary.csv}"
      )
    }
  } else {
    cli::cli_alert_info(
      "No human reference file provided \u2014 skipping comparison."
    )
  }

  cli::cli_h1("Pipeline Complete")
  output
}
