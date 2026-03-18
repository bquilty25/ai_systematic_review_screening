#' LLM Screening Functions
#'
#' Uses the `ellmer` package to send paper text to an LLM (Claude via
#' Anthropic) and extract a structured screening decision.

#' Define the screening output schema
#'
#' Builds an ellmer `type_object()` schema for structured extraction.
#' The schema is flexible: it always includes `decision` and `reason`,
#' and optionally includes additional criteria-level fields.
#'
#' @param criteria_names Character vector. Optional names of individual
#'   screening criteria to track. Each gets a boolean field indicating
#'   whether the criterion was met.
#'
#' @return An ellmer `type_object` specification.
define_screening_schema <- function(criteria_names = NULL) {
  base_fields <- list(
    decision = ellmer::type_enum(
      "Screening decision: include or exclude this paper.",
      values = c("include", "exclude")
    ),
    confidence = ellmer::type_enum(
      "Confidence in the decision: high, medium, or low.",
      values = c("high", "medium", "low")
    ),
    reason = ellmer::type_string(
      "Brief rationale for the screening decision (1-3 sentences)."
    )
  )

  # Add per-criterion boolean fields if criteria names are provided
  if (!is.null(criteria_names) && length(criteria_names) > 0) {
    criteria_fields <- purrr::map(criteria_names, \(name) {
      ellmer::type_boolean(
        glue::glue("Does this paper meet the criterion: '{name}'?")
      )
    })
    names(criteria_fields) <- janitor::make_clean_names(criteria_names)
    base_fields <- c(base_fields, criteria_fields)
  }

  do.call(ellmer::type_object, c(
    list("Screening decision for a scientific paper"),
    base_fields
  ))
}

#' Build the system prompt for screening
#'
#' @param criteria Character. A text description of the screening criteria
#'   (inclusion and exclusion rules).
#'
#' @return A character string for the system prompt.
build_system_prompt <- function(criteria) {
  glue::glue(
    "You are an expert systematic reviewer screening scientific papers.
    Your task is to decide whether each paper should be INCLUDED or EXCLUDED
    from a systematic review based on the given screening criteria.

    ## Screening Criteria

    {criteria}

    ## Instructions

    - Read the full text of the paper provided.
    - Evaluate the paper against each screening criterion.
    - Return your structured decision: include or exclude.
    - Provide a brief, specific rationale citing evidence from the paper.
    - If the paper is ambiguous or borderline, lean towards inclusion and
      flag your confidence as 'low' or 'medium'.
    - Be consistent and thorough in your evaluation."
  )
}

#' Build the user prompt for a single paper
#'
#' @param paper_text Character. The full markdown text of the paper.
#'
#' @return A character string for the user message.
build_paper_prompt <- function(paper_text) {
  glue::glue(
    "Please screen the following scientific paper against the criteria.

    ## Paper Text

    {paper_text}"
  )
}

#' Initialise a chat object for screening
#'
#' Creates an ellmer chat object with the appropriate system prompt.
#' Defaults to Claude via Anthropic.
#'
#' @param criteria Character. Screening criteria text.
#' @param provider Character. LLM provider to use. One of "anthropic",
#'   "openai", "google". Default "anthropic".
#' @param model Character or NULL. Model name. If NULL, uses the provider
#'   default.
#'
#' @return An ellmer chat object.
init_screening_chat <- function(criteria,
                                provider = c("anthropic", "openai", "google"),
                                model = NULL) {
  provider <- match.arg(provider)
  sys_prompt <- build_system_prompt(criteria)

  chat <- switch(provider,
    anthropic = ellmer::chat_anthropic(system_prompt = sys_prompt, model = model),
    openai = ellmer::chat_openai(system_prompt = sys_prompt, model = model),
    google = ellmer::chat_google(system_prompt = sys_prompt, model = model)
  )

  chat
}

#' Screen a single paper
#'
#' @param chat An ellmer chat object (from `init_screening_chat()`).
#' @param paper_text Character. Full markdown text of the paper.
#' @param schema An ellmer type specification (from `define_screening_schema()`).
#'
#' @return A named list with screening results.
screen_single_paper <- function(chat, paper_text, schema) {
  prompt <- build_paper_prompt(paper_text)

  # Clone chat so each paper gets a fresh conversation
  chat_clone <- chat$clone()
  result <- chat_clone$chat_structured(prompt, type = schema)

  result
}

#' Screen multiple papers in batch
#'
#' Iterates over papers and screens each one. Uses `purrr::map()` with
#' progress reporting via `cli`.
#'
#' @param papers_df A tibble with at least columns `file_name` and `content`.
#' @param criteria Character. Screening criteria text.
#' @param criteria_names Character vector or NULL. Individual criterion names
#'   for per-criterion tracking.
#' @param provider Character. LLM provider. Default "anthropic".
#' @param model Character or NULL. Model name.
#'
#' @return A tibble with one row per paper, including screening results.
screen_papers_batch <- function(papers_df,
                                criteria,
                                criteria_names = NULL,
                                provider = "anthropic",
                                model = NULL) {
  schema <- define_screening_schema(criteria_names)
  chat <- init_screening_chat(criteria, provider = provider, model = model)

  cli::cli_alert_info(
    "Screening {nrow(papers_df)} paper{?s} with {provider}..."
  )

  results <- purrr::map(
    cli::cli_progress_along(papers_df$content, name = "Screening papers"),
    \(i) {
      tryCatch(
        screen_single_paper(chat, papers_df$content[[i]], schema),
        error = function(e) {
          cli::cli_warn(
            "Failed to screen {.val {papers_df$file_name[[i]]}}: {e$message}"
          )
          list(decision = NA_character_, confidence = NA_character_,
               reason = glue::glue("Error: {e$message}"))
        }
      )
    }
  )

  # Bind results into a tibble
  results_df <- purrr::map_dfr(results, \(x) tibble::as_tibble(x))

  # Combine with paper metadata
  dplyr::bind_cols(
    papers_df |> dplyr::select("file_name"),
    results_df
  )
}
