#' PDF-to-Markdown Conversion Functions
#'
#' Wrappers around the Python `opendataloader-pdf` tool for converting
#' scientific paper PDFs to markdown format suitable for LLM processing.

#' Check that opendataloader-pdf is installed and accessible
#'
#' @return TRUE invisibly if the tool is available, errors otherwise.
check_opendataloader_installed <- function() {
  result <- tryCatch(
    system2("pip", args = c("show", "opendataloader-pdf"), stdout = TRUE, stderr = TRUE),
    error = function(e) NULL
  )
  if (is.null(result) || !any(grepl("Name: opendataloader-pdf", result))) {
    cli::cli_abort(c(
      "x" = "{.pkg opendataloader-pdf} is not installed.",
      "i" = "Install it with: {.code pip install opendataloader-pdf}",
      "i" = "Requires Python 3.10+ and Java 11+."
    ))
  }
  invisible(TRUE)
}

#' Convert all PDFs in a directory to markdown
#'
#' Calls `opendataloader-pdf` via `system2()` to batch-convert PDFs.
#' Outputs markdown files into `output_dir`.
#'
#' @param pdf_dir Character. Path to directory containing PDF files.
#' @param output_dir Character. Path to directory for markdown output.
#' @param overwrite Logical. If FALSE, skip PDFs that already have a
#'   corresponding markdown file in `output_dir`. Default FALSE.
#'
#' @return A tibble with columns: `pdf_file`, `md_file`, `status`.
convert_pdfs_to_markdown <- function(pdf_dir, output_dir, overwrite = FALSE) {
  check_opendataloader_installed()

  pdf_dir <- normalizePath(pdf_dir, mustWork = TRUE)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  output_dir <- normalizePath(output_dir)

  pdf_files <- list.files(pdf_dir,
    pattern = "\\.pdf$", full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(pdf_files) == 0) {
    cli::cli_warn("No PDF files found in {.path {pdf_dir}}.")
    return(tibble::tibble(
      pdf_file = character(), md_file = character(),
      status = character()
    ))
  }

  cli::cli_alert_info("Found {length(pdf_files)} PDF file{?s} in {.path {pdf_dir}}.")

  # Determine which PDFs need conversion
  expected_md <- file.path(
    output_dir,
    paste0(tools::file_path_sans_ext(basename(pdf_files)), ".md")
  )

  if (!overwrite) {
    already_done <- file.exists(expected_md)
    if (any(already_done)) {
      cli::cli_alert_success(
        "Skipping {sum(already_done)} already-converted file{?s}."
      )
    }
    to_convert <- pdf_files[!already_done]
  } else {
    to_convert <- pdf_files
  }

  # Run opendataloader-pdf conversion
  if (length(to_convert) > 0) {
    cli::cli_alert_info("Converting {length(to_convert)} PDF{?s} to markdown...")

    exit_code <- system2(
      "python",
      args = c(
        "-c",
        shQuote(paste0(
          "import opendataloader_pdf; ",
          "opendataloader_pdf.convert(",
          "input_path=", deparse(to_convert), ", ",
          "output_dir='", output_dir, "', ",
          "format='markdown'",
          ")"
        ))
      ),
      stdout = TRUE,
      stderr = TRUE
    )

    status_attr <- attr(exit_code, "status")
    if (!is.null(status_attr) && status_attr != 0) {
      cli::cli_warn(c(
        "!" = "opendataloader-pdf exited with status {status_attr}.",
        "i" = "Output: {paste(exit_code, collapse = '\\n')}"
      ))
    }
  }

  # Build results tibble
  actual_md <- file.path(
    output_dir,
    paste0(tools::file_path_sans_ext(basename(pdf_files)), ".md")
  )
  status <- dplyr::case_when(
    file.exists(actual_md) & !overwrite & file.exists(expected_md) &
      pdf_files %in% pdf_files[file.exists(expected_md)] ~ "skipped",
    file.exists(actual_md) ~ "converted",
    TRUE ~ "failed"
  )

  # Simpler status logic
  status <- ifelse(file.exists(actual_md), "success", "failed")

  tibble::tibble(
    pdf_file = basename(pdf_files),
    md_file = basename(actual_md),
    status = status
  )
}

#' Read a single markdown paper
#'
#' @param md_path Character. Path to a markdown file.
#'
#' @return A single character string with the full paper content.
read_markdown_paper <- function(md_path) {
  if (!file.exists(md_path)) {
    cli::cli_abort("Markdown file not found: {.path {md_path}}")
  }
  paste(readLines(md_path, warn = FALSE), collapse = "\n")
}

#' Read citation data from a CSV file for abstract screening
#'
#' Reads a CSV file containing title and abstract data for stage-1 abstract
#' screening. Column names are standardised via `janitor::clean_names()`.
#'
#' @param csv_path Character. Path to the CSV file.
#' @param id_col Character or NULL. Column to use as citation identifier.
#'   If NULL (default), a sequential integer `citation_id` is generated.
#' @param title_col Character. Name of the title column (after
#'   `janitor::clean_names()` normalisation). Default `"title"`.
#' @param abstract_col Character. Name of the abstract column (after
#'   `janitor::clean_names()` normalisation). Default `"abstract"`.
#'
#' @return A tibble with columns: `citation_id`, `title`, `abstract`, plus
#'   any additional columns present in the CSV.
read_citation_csv <- function(csv_path,
                              id_col = NULL,
                              title_col = "title",
                              abstract_col = "abstract") {
  if (!file.exists(csv_path)) {
    cli::cli_abort("Citation CSV not found: {.path {csv_path}}")
  }

  raw <- readr::read_csv(csv_path, show_col_types = FALSE) |>
    janitor::clean_names()

  required <- c(title_col, abstract_col)
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "x" = "Required columns not found in {.path {basename(csv_path)}}: {.val {missing}}",
      "i" = "Available columns: {.val {names(raw)}}",
      "i" = "Use {.arg title_col} and {.arg abstract_col} to specify the correct column names."
    ))
  }

  # Generate or extract citation ID
  if (!is.null(id_col)) {
    if (!id_col %in% names(raw)) {
      cli::cli_abort(c(
        "x" = "ID column {.val {id_col}} not found.",
        "i" = "Available columns: {.val {names(raw)}}"
      ))
    }
    raw <- raw |> dplyr::rename(citation_id = dplyr::all_of(id_col))
  } else {
    raw <- raw |> dplyr::mutate(citation_id = dplyr::row_number(), .before = 1)
  }

  # Ensure title and abstract are character columns
  raw <- raw |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(title_col, abstract_col)),
        as.character
      )
    )

  # Standardise to expected column names
  if (title_col != "title") {
    raw <- raw |> dplyr::rename(title = dplyr::all_of(title_col))
  }
  if (abstract_col != "abstract") {
    raw <- raw |> dplyr::rename(abstract = dplyr::all_of(abstract_col))
  }

  cli::cli_alert_success(
    "Loaded {nrow(raw)} citation{?s} from {.path {basename(csv_path)}}."
  )

  raw
}

#' Read all markdown papers from a directory
#'
#' @param md_dir Character. Path to directory containing markdown files.
#'
#' @return A tibble with columns: `file_name`, `md_path`, `content`.
read_all_markdown_papers <- function(md_dir) {
  md_dir <- normalizePath(md_dir, mustWork = TRUE)
  md_files <- list.files(md_dir, pattern = "\\.md$", full.names = TRUE)

  if (length(md_files) == 0) {
    cli::cli_warn("No markdown files found in {.path {md_dir}}.")
    return(tibble::tibble(
      file_name = character(), md_path = character(),
      content = character()
    ))
  }

  tibble::tibble(
    file_name = tools::file_path_sans_ext(basename(md_files)),
    md_path = md_files,
    content = purrr::map_chr(md_files, read_markdown_paper)
  )
}
