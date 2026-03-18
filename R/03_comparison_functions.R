#' Comparison Functions
#'
#' Compare LLM screening decisions against a human manual extraction
#' spreadsheet to assess agreement and performance metrics.

#' Load the human screening/extraction Excel sheet
#'
#' Reads a `.xlsx` or `.xls` file and standardises column names using
#' `janitor::clean_names()`.
#'
#' @param excel_path Character. Path to the Excel file.
#' @param sheet Character or integer. Sheet name or index. Default 1.
#'
#' @return A tibble with cleaned column names.
load_human_screening <- function(excel_path, sheet = 1) {
  if (!file.exists(excel_path)) {
    cli::cli_abort("Excel file not found: {.path {excel_path}}")
  }

  raw <- readxl::read_excel(excel_path, sheet = sheet)

  cli::cli_alert_success(
    "Loaded {nrow(raw)} rows from {.path {basename(excel_path)}} (sheet: {sheet})."
  )

  raw |>
    janitor::clean_names()
}

#' Compare LLM and human screening decisions
#'
#' Joins LLM results with human results and computes agreement metrics.
#'
#' @param llm_results A tibble with at least `file_name` and `decision`.
#' @param human_results A tibble with at least an identifier column and a
#'   decision column.
#' @param llm_id_col Character. Column in `llm_results` to join on.
#'   Default "file_name".
#' @param human_id_col Character. Column in `human_results` to join on.
#' @param human_decision_col Character. Column in `human_results` containing
#'   the human include/exclude decision.
#' @param include_value Character. Value in the human decision column that
#'   means "include". Default "include".
#' @param exclude_value Character. Value in the human decision column that
#'   means "exclude". Default "exclude".
#'
#' @return A tibble with columns: identifier, `llm_decision`, `human_decision`,
#'   `agree`.
compare_screening_decisions <- function(llm_results,
                                        human_results,
                                        llm_id_col = "file_name",
                                        human_id_col,
                                        human_decision_col,
                                        include_value = "include",
                                        exclude_value = "exclude") {
  # Standardise human decisions to include/exclude

  human_clean <- human_results |>
    dplyr::mutate(
      human_decision = dplyr::case_when(
        tolower(.data[[human_decision_col]]) %in% tolower(include_value) ~ "include",
        tolower(.data[[human_decision_col]]) %in% tolower(exclude_value) ~ "exclude",
        TRUE ~ "unknown"
      )
    ) |>
    dplyr::select(
      join_id = dplyr::all_of(human_id_col),
      "human_decision"
    )

  llm_clean <- llm_results |>
    dplyr::select(
      join_id = dplyr::all_of(llm_id_col),
      llm_decision = "decision",
      llm_confidence = "confidence",
      llm_reason = "reason"
    )

  comparison <- dplyr::inner_join(llm_clean, human_clean, by = "join_id")

  if (nrow(comparison) == 0) {
    cli::cli_warn(c(
      "!" = "No matching records found between LLM and human results.",
      "i" = "Check that {.arg {llm_id_col}} and {.arg {human_id_col}} contain
             matching identifiers."
    ))
  }

  comparison |>
    dplyr::mutate(agree = llm_decision == human_decision)
}

#' Compute a confusion matrix from comparison results
#'
#' @param comparison_df A tibble from `compare_screening_decisions()` with
#'   columns `llm_decision` and `human_decision`.
#' @param positive Character. Which decision is the "positive" class.
#'   Default "include".
#'
#' @return A named list with `tp`, `fp`, `tn`, `fn`, and the `table`.
compute_confusion_matrix <- function(comparison_df,
                                     positive = "include") {
  negative <- ifelse(positive == "include", "exclude", "include")

  valid <- comparison_df |>
    dplyr::filter(
      llm_decision %in% c("include", "exclude"),
      human_decision %in% c("include", "exclude")
    )

  tp <- sum(valid$llm_decision == positive & valid$human_decision == positive)
  fp <- sum(valid$llm_decision == positive & valid$human_decision == negative)
  tn <- sum(valid$llm_decision == negative & valid$human_decision == negative)
  fn <- sum(valid$llm_decision == negative & valid$human_decision == positive)

  list(
    tp = tp,
    fp = fp,
    tn = tn,
    fn = fn,
    table = matrix(
      c(tp, fp, fn, tn),
      nrow = 2,
      dimnames = list(
        LLM = c(positive, negative),
        Human = c(positive, negative)
      )
    )
  )
}

#' Summarise screening comparison metrics
#'
#' Computes accuracy, sensitivity, specificity, precision, F1, and
#' Cohen's kappa from the comparison.
#'
#' @param comparison_df A tibble from `compare_screening_decisions()`.
#' @param positive Character. The positive class. Default "include".
#'
#' @return A tibble with one row and metric columns.
summarise_comparison <- function(comparison_df, positive = "include") {
  cm <- compute_confusion_matrix(comparison_df, positive = positive)

  n <- cm$tp + cm$fp + cm$tn + cm$fn
  accuracy <- (cm$tp + cm$tn) / n
  sensitivity <- cm$tp / max(cm$tp + cm$fn, 1)
  specificity <- cm$tn / max(cm$tn + cm$fp, 1)
  precision <- cm$tp / max(cm$tp + cm$fp, 1)
  f1 <- 2 * (precision * sensitivity) / max(precision + sensitivity, 1e-10)

  # Cohen's kappa
  p_observed <- accuracy
  p_expected <- (
    ((cm$tp + cm$fp) / n) * ((cm$tp + cm$fn) / n) +
    ((cm$tn + cm$fn) / n) * ((cm$tn + cm$fp) / n)
  )
  kappa <- (p_observed - p_expected) / max(1 - p_expected, 1e-10)

  tibble::tibble(
    n_total = n,
    n_agree = cm$tp + cm$tn,
    accuracy = round(accuracy, 3),
    sensitivity = round(sensitivity, 3),
    specificity = round(specificity, 3),
    precision = round(precision, 3),
    f1 = round(f1, 3),
    kappa = round(kappa, 3),
    tp = cm$tp,
    fp = cm$fp,
    tn = cm$tn,
    fn = cm$fn
  )
}
