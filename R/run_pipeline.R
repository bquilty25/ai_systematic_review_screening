#' Main Pipeline Orchestration
#'
#' Run the full screening pipeline: convert PDFs, screen with LLM,
#' compare against human extraction sheet.
#'
#' This script sources the function files and provides `run_pipeline()`
#' which orchestrates the full workflow.

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

#' Run the full screening pipeline
#'
#' @param pdf_dir Character. Path to directory of PDFs.
#'   Default `data/raw/pdfs`.
#' @param md_dir Character. Path to directory for markdown output.
#'   Default `data/processed/markdown`.
#' @param excel_path Character or NULL. Path to human screening Excel file.
#'   If NULL, skips the comparison step.
#' @param criteria Character. Text description of screening criteria.
#' @param criteria_names Character vector or NULL. Individual criterion names.
#' @param human_id_col Character. Column in the Excel sheet to join on.
#' @param human_decision_col Character. Column in the Excel sheet with the
#'   include/exclude decision.
#' @param include_value Character. Value meaning "include" in the human sheet.
#' @param exclude_value Character. Value meaning "exclude" in the human sheet.
#' @param provider Character. LLM provider. Default "anthropic".
#' @param model Character or NULL. Model name.
#' @param overwrite_md Logical. Re-convert PDFs even if markdown exists.
#' @param save_results Logical. Save results to disk. Default TRUE.
#'
#' @return A list with `screening_results`, `comparison` (if Excel provided),
#'   and `summary` (if Excel provided).
run_pipeline <- function(pdf_dir = here::here("data", "raw", "pdfs"),
                         md_dir = here::here("data", "processed", "markdown"),
                         excel_path = NULL,
                         criteria,
                         criteria_names = NULL,
                         human_id_col = NULL,
                         human_decision_col = NULL,
                         include_value = "include",
                         exclude_value = "exclude",
                         provider = "anthropic",
                         model = NULL,
                         overwrite_md = FALSE,
                         save_results = TRUE) {
  cli::cli_h1("AI Systematic Review Screening Pipeline")

  # ---- Step 1: Convert PDFs to markdown ----
  cli::cli_h2("Step 1: PDF to Markdown Conversion")
  conversion_results <- convert_pdfs_to_markdown(
    pdf_dir = pdf_dir,
    output_dir = md_dir,
    overwrite = overwrite_md
  )
  cli::cli_alert_success(
    "Conversion complete: {sum(conversion_results$status == 'success')}/{nrow(conversion_results)} succeeded."
  )

  # ---- Step 2: Read markdown papers ----
  cli::cli_h2("Step 2: Reading Converted Papers")
  papers_df <- read_all_markdown_papers(md_dir)
  cli::cli_alert_success("Read {nrow(papers_df)} paper{?s}.")

  if (nrow(papers_df) == 0) {
    cli::cli_abort("No papers to screen. Check your PDF directory and conversion step.")
  }

  # ---- Step 3: Screen with LLM ----

  cli::cli_h2("Step 3: LLM Screening")
  screening_results <- screen_papers_batch(
    papers_df = papers_df,
    criteria = criteria,
    criteria_names = criteria_names,
    provider = provider,
    model = model
  )
  cli::cli_alert_success("Screening complete.")

  # Print summary
  if ("decision" %in% names(screening_results)) {
    decision_counts <- screening_results |>
      dplyr::count(decision) |>
      glue::glue_data("{decision}: {n}") |>
      paste(collapse = ", ")
    cli::cli_alert_info("Decisions: {decision_counts}")
  }

  # ---- Step 4: Compare with human extraction (if available) ----
  output <- list(screening_results = screening_results)

  if (!is.null(excel_path)) {
    cli::cli_h2("Step 4: Comparison with Human Extraction")

    if (is.null(human_id_col) || is.null(human_decision_col)) {
      cli::cli_abort(
        "Please specify {.arg human_id_col} and {.arg human_decision_col} to compare."
      )
    }

    human_data <- load_human_screening(excel_path)

    comparison <- compare_screening_decisions(
      llm_results = screening_results,
      human_results = human_data,
      human_id_col = human_id_col,
      human_decision_col = human_decision_col,
      include_value = include_value,
      exclude_value = exclude_value
    )

    summary_metrics <- summarise_comparison(comparison)

    cli::cli_h3("Agreement Metrics")
    cli::cli_alert_info("Accuracy: {summary_metrics$accuracy}")
    cli::cli_alert_info("Sensitivity: {summary_metrics$sensitivity}")
    cli::cli_alert_info("Specificity: {summary_metrics$specificity}")
    cli::cli_alert_info("Kappa: {summary_metrics$kappa}")
    cli::cli_alert_info("F1: {summary_metrics$f1}")

    output$comparison <- comparison
    output$summary <- summary_metrics
  } else {
    cli::cli_alert_info("No human extraction file provided. Skipping comparison.")
  }

  # ---- Step 5: Save results ----
  if (save_results) {
    cli::cli_h2("Step 5: Saving Results")

    qs::qsave(
      screening_results,
      here::here("data", "processed", "screening_results.qs")
    )
    cli::cli_alert_success("Saved screening results to {.path data/processed/screening_results.qs}")

    readr::write_csv(
      screening_results,
      here::here("outputs", "tables", "screening_results.csv")
    )
    cli::cli_alert_success("Saved CSV to {.path outputs/tables/screening_results.csv}")

    if (!is.null(excel_path)) {
      readr::write_csv(
        output$summary,
        here::here("outputs", "tables", "comparison_summary.csv")
      )
      cli::cli_alert_success("Saved comparison to {.path outputs/tables/comparison_summary.csv}")

      readr::write_csv(
        output$comparison,
        here::here("outputs", "tables", "comparison_detail.csv")
      )
    }
  }

  cli::cli_h1("Pipeline Complete")
  output
}
