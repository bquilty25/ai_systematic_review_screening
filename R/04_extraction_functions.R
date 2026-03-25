#' Data Extraction Functions
#'
#' LLM-based structured data extraction from full-text papers after full-text
#' screening. Uses ellmer's typed schema approach (analogous to the screening
#' functions) to extract user-defined variables from each paper.
#'
#' Variable definitions are provided as a named list. Each element describes
#' one variable to extract and can be a:
#'   - character string: treated as a free-text string variable with that
#'     description
#'   - named list with `type` and `description` (and `values` for enums)
#'
#' Supported variable types:
#'   - `"string"`  : free-text character value
#'   - `"number"`  : numeric value
#'   - `"boolean"` : TRUE/FALSE value
#'   - `"enum"`    : one of a fixed set of values (requires `values` element)
#'
#' @examples
#' variables <- list(
#'     sample_size = "Total sample size (integer count of participants)",
#'     study_design = list(
#'         type = "enum",
#'         values = c("RCT", "cohort", "case-control", "cross-sectional", "other"),
#'         description = "Study design classification"
#'     ),
#'     reported_outcome = list(
#'         type = "string",
#'         description = "Primary outcome as reported by the authors"
#'     ),
#'     randomised = list(
#'         type = "boolean",
#'         description = "Was randomisation used?"
#'     )
#' )
#'
#' schema <- define_extraction_schema(variables)
# ============================================================
# Extraction Schema Builder
# ============================================================

#' Define a data extraction output schema
#'
#' Builds an ellmer `type_object()` schema from a user-supplied named list
#' of variable definitions. Variable names are cleaned via
#' `janitor::make_clean_names()` for use as tibble column names.
#'
#' @param variables Named list. Each element is either:
#'   - A character string: free-text description of the variable (type
#'     defaults to `"string"`).
#'   - A named list with elements:
#'     - `type`: one of `"string"`, `"number"`, `"boolean"`, `"enum"`.
#'     - `description`: variable description (used in the schema prompt).
#'     - `values`: character vector of allowed values (required for `"enum"`).
#'
#' @return An ellmer `type_object` specification.
define_extraction_schema <- function(variables) {
    if (!is.list(variables) || is.null(names(variables))) {
        cli::cli_abort(
            "{.arg variables} must be a named list of variable definitions."
        )
    }

    fields <- purrr::imap(variables, function(spec, var_name) {
        # Allow shorthand: a plain string is treated as a string variable
        if (is.character(spec)) {
            return(ellmer::type_string(spec))
        }

        if (!is.list(spec) || is.null(spec$type)) {
            cli::cli_abort(
                "Variable {.val {var_name}}: each element must be a character string
        or a list with at least a {.field type} element."
            )
        }

        desc <- spec$description %||% var_name

        switch(spec$type,
            "string" = ellmer::type_string(desc),
            "number" = ellmer::type_number(desc),
            "boolean" = ellmer::type_boolean(desc),
            "enum" = {
                if (is.null(spec$values) || length(spec$values) == 0) {
                    cli::cli_abort(
                        "Variable {.val {var_name}} (type enum): must provide a
            {.field values} character vector."
                    )
                }
                ellmer::type_enum(desc, values = spec$values)
            },
            cli::cli_abort(
                "Variable {.val {var_name}}: unknown type {.val {spec$type}}.
        Use one of: string, number, boolean, enum."
            )
        )
    })

    # Clean variable names to valid R column names
    names(fields) <- janitor::make_clean_names(names(variables))

    do.call(ellmer::type_object, c(
        list("Extracted data fields from a scientific paper"),
        fields
    ))
}

# ============================================================
# Prompt Builders
# ============================================================

#' Build a system prompt for data extraction
#'
#' @param variable_descriptions Named character vector or named list. Maps
#'   variable names (cleaned) to their description strings. Typically derived
#'   from the `variables` argument of `define_extraction_schema()`.
#'
#' @return A character string for the LLM system prompt.
build_extraction_system_prompt <- function(variable_descriptions) {
    # Render variable list as numbered descriptions
    var_lines <- purrr::imap_chr(
        variable_descriptions,
        function(desc, name) {
            # Normalise spec to string description if it is a list
            if (is.list(desc)) {
                desc_text <- desc$description %||% name
                type_text <- desc$type %||% "string"
                if (type_text == "enum" && !is.null(desc$values)) {
                    glue::glue(
                        "  - **{name}** ({type_text}; one of: {paste(desc$values, collapse = ', ')}): {desc_text}"
                    )
                } else {
                    glue::glue("  - **{name}** ({type_text}): {desc_text}")
                }
            } else {
                glue::glue("  - **{name}** (string): {desc}")
            }
        }
    )

    vars_text <- paste(var_lines, collapse = "\n")

    glue::glue(
        "You are an expert systematic reviewer performing DATA EXTRACTION from\n",
        "scientific papers for a systematic review.\n\n",
        "## Variables to Extract\n\n",
        "{vars_text}\n\n",
        "## Instructions\n\n",
        "- Extract each variable from the MAIN TEXT of the paper only.\n",
        "- If a value is not reported or cannot be determined from the main text,\n",
        "  return an empty string (\"\") for string/number fields, or null for\n",
        "  boolean/enum fields.\n",
        "- Do NOT infer or estimate values not explicitly stated.\n",
        "- Extract exact values as reported (e.g., exact sample sizes, exact p-values,\n",
        "  exact outcome labels) \u2014 do not paraphrase numeric results.\n",
        "- For numeric fields, return the number only (no units, no confidence\n",
        "  intervals \u2014 report those in the relevant string field if needed).\n",
        "- Be precise and consistent across all papers."
    )
}

#' Build a user prompt for data extraction from a single paper
#'
#' @param paper_text Character. The full markdown text of the paper.
#'
#' @return A character string for the user message.
build_extraction_paper_prompt <- function(paper_text) {
    glue::glue(
        "Please extract the required data from the following scientific paper.\n\n",
        "## Paper Text\n\n",
        "{paper_text}"
    )
}

# ============================================================
# Single-item & Batch Extraction
# ============================================================

#' Extract data from a single paper
#'
#' @param chat An ellmer chat object initialised with the extraction system
#'   prompt (via `init_chat()` and `build_extraction_system_prompt()`).
#' @param paper_text Character. Full markdown text of the paper.
#' @param schema An ellmer type specification (from
#'   `define_extraction_schema()`).
#'
#' @return A named list of extracted values.
extract_single_paper <- function(chat, paper_text, schema) {
    prompt <- build_extraction_paper_prompt(paper_text)
    chat_clone <- chat$clone()
    chat_clone$chat_structured(prompt, type = schema)
}

#' Extract data from multiple papers in batch
#'
#' Iterates over included papers and extracts user-defined variables. Uses
#' `purrr::map()` (or `furrr::future_map()` for parallel execution) with a
#' CLI progress bar. Each paper is wrapped in `tryCatch()` so a single
#' failure does not abort the batch.
#'
#' @param papers_df A tibble with at least columns `file_name` and `content`
#'   (from `read_all_markdown_papers()`). Typically the subset of papers that
#'   passed full-text screening.
#' @param variables Named list of variable definitions (see
#'   `define_extraction_schema()`).
#' @param provider Character. LLM provider. Default `"anthropic"`.
#' @param model Character or NULL. Model name. If NULL, uses the provider
#'   default.
#' @param parallel Logical. If TRUE, use `furrr::future_map()` for concurrent
#'   processing. Requires a `future` plan to be set. Default FALSE.
#'
#' @return A tibble with one row per paper: `file_name` plus one column per
#'   extracted variable. Papers where extraction failed have NA in all
#'   variable columns.
extract_papers_batch <- function(papers_df,
                                 variables,
                                 provider = "anthropic",
                                 model = NULL,
                                 parallel = FALSE) {
    schema <- define_extraction_schema(variables)
    sys_msg <- build_extraction_system_prompt(variables)
    chat <- init_chat(
        system_prompt = sys_msg,
        provider = provider,
        model = model
    )

    cli::cli_alert_info(
        "Extracting data from {nrow(papers_df)} paper{?s} with {provider}..."
    )

    map_fn <- if (parallel) {
        rlang::check_installed("furrr",
            reason = "is required for parallel extraction"
        )
        furrr::future_map
    } else {
        purrr::map
    }

    results <- map_fn(
        cli::cli_progress_along(papers_df$content, name = "Extracting data"),
        \(i) {
            tryCatch(
                extract_single_paper(chat, papers_df$content[[i]], schema),
                error = function(e) {
                    cli::cli_warn(
                        "Extraction failed for {.val {papers_df$file_name[[i]]}}: {e$message}"
                    )
                    # Return NA for every variable on failure
                    purrr::map(variables, \(.) NA_character_) |>
                        purrr::set_names(janitor::make_clean_names(names(variables)))
                }
            )
        }
    )

    results_df <- purrr::map_dfr(results, \(x) tibble::as_tibble(x))

    dplyr::bind_cols(
        papers_df |> dplyr::select("file_name"),
        results_df
    )
}
