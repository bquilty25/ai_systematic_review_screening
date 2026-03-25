#' LLM Screening Functions
#'
#' Uses the `ellmer` package to send title/abstract and full paper text to an
#' LLM for two-stage systematic review screening (abstract stage then full-text
#' stage), and to perform data extraction. Implements an enhanced ScreenPrompt
#' approach adapted from Cao et al. 2025 (otto-SR) for high-sensitivity
#' screening that errs strongly toward inclusion.
#'
#' Key design principles:
#'   1. Inclusion bias: only exclude when a paper CLEARLY violates criteria.
#'   2. Decomposed evaluation: assess each criterion individually before
#'      making the overall decision (chain-of-thought).
#'   3. Structured output via ellmer typed schemas for reproducibility.
#'   4. Optional parallel execution via `furrr` for large citation sets.

# ============================================================
# Screening Schemas
# ============================================================

#' Define the abstract screening output schema
#'
#' Builds an ellmer `type_object()` schema for abstract-stage screening.
#' Includes a `criteria_evaluation` field for per-criterion chain-of-thought
#' reasoning, which is key to the ScreenPrompt approach.
#'
#' @param criteria_names Character vector or NULL. Optional names of individual
#'   screening criteria. Each gets a boolean field indicating whether the
#'   criterion was met based on the abstract.
#'
#' @return An ellmer `type_object` specification.
define_abstract_screening_schema <- function(criteria_names = NULL) {
  base_fields <- list(
    criteria_evaluation = ellmer::type_string(
      paste(
        "Step-by-step evaluation of each screening criterion based on the",
        "title and abstract. For each criterion, state: (1) what the",
        "abstract says, and (2) whether the criterion is met, not met, or",
        "unclear. This reasoning must precede the final decision."
      )
    ),
    decision = ellmer::type_enum(
      "Final screening decision: include or exclude this citation.",
      values = c("include", "exclude")
    ),
    confidence = ellmer::type_enum(
      "Confidence in the decision: high, medium, or low.",
      values = c("high", "medium", "low")
    ),
    reason = ellmer::type_string(
      "Brief rationale for the final decision (1-2 sentences)."
    )
  )

  if (!is.null(criteria_names) && length(criteria_names) > 0) {
    criteria_fields <- purrr::map(criteria_names, \(name) {
      ellmer::type_boolean(
        glue::glue("Does this citation meet the criterion: '{name}'?")
      )
    })
    names(criteria_fields) <- janitor::make_clean_names(criteria_names)
    base_fields <- c(base_fields, criteria_fields)
  }

  do.call(ellmer::type_object, c(
    list("Abstract screening decision for a systematic review citation"),
    base_fields
  ))
}

#' Define the full-text screening output schema
#'
#' Builds an ellmer `type_object()` schema for full-text screening.
#' Includes a `criteria_evaluation` field for per-criterion chain-of-thought
#' reasoning.
#'
#' @param criteria_names Character vector or NULL. Optional names of individual
#'   screening criteria. Each gets a boolean field indicating whether the
#'   criterion was met.
#'
#' @return An ellmer `type_object` specification.
define_screening_schema <- function(criteria_names = NULL) {
  base_fields <- list(
    criteria_evaluation = ellmer::type_string(
      paste(
        "Step-by-step evaluation of each screening criterion based on the",
        "full paper text. For each criterion, cite specific evidence from the",
        "paper and state whether the criterion is met, not met, or unclear.",
        "This reasoning must precede the final decision."
      )
    ),
    decision = ellmer::type_enum(
      "Final screening decision: include or exclude this paper.",
      values = c("include", "exclude")
    ),
    confidence = ellmer::type_enum(
      "Confidence in the decision: high, medium, or low.",
      values = c("high", "medium", "low")
    ),
    reason = ellmer::type_string(
      "Brief rationale for the screening decision (1-3 sentences), citing specific evidence from the paper."
    )
  )

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
    list("Full-text screening decision for a systematic review paper"),
    base_fields
  ))
}

# ============================================================
# System & User Prompt Builders
# ============================================================

#' Build the system prompt for abstract-stage screening
#'
#' Implements a ScreenPrompt-style approach (adapted from otto-SR / Cao et al.
#' 2025) for title+abstract screening. The prompt decomposes criteria into
#' numbered evaluation steps, requires per-criterion reasoning before the
#' final decision, and applies a strong inclusion bias — correct for systematic
#' reviews where missing a relevant paper (false negative) is far worse than
#' over-including an irrelevant one (false positive).
#'
#' @param criteria Character. A text description of the screening criteria
#'   (inclusion and exclusion rules).
#'
#' @return A character string for the LLM system prompt.
build_abstract_system_prompt <- function(criteria) {
  glue::glue(
    "You are an expert systematic reviewer performing TITLE AND ABSTRACT
    SCREENING for a systematic review. You will receive the title and abstract
    of a citation and must decide whether to include or exclude it.

    ## Your Role

    Your job is to identify citations that are POTENTIALLY eligible based on
    the title and abstract alone. You are NOT doing a final eligibility
    decision — that will be confirmed at full-text screening. Therefore,
    you must be highly sensitive and cautious about exclusion.

    ## Screening Criteria

    {criteria}

    ## How to Evaluate

    Work through EACH criterion systematically in your `criteria_evaluation`
    field before making your final decision:
    1. State what the title/abstract reveals about each criterion.
    2. State whether the criterion appears met, not met, or unclear/not
       reported.
    3. After evaluating all criteria, make your final decision.

    ## Decision Rule — CRITICAL

    - INCLUDE if the citation MIGHT be eligible, even if uncertain.
    - INCLUDE if the abstract does not provide enough information to
      determine eligibility for any criterion.
    - INCLUDE if the citation is borderline or ambiguous.
    - EXCLUDE ONLY if the title and abstract provide CLEAR, UNAMBIGUOUS
      evidence that at least one exclusion criterion is met and the citation
      is definitively ineligible.
    - When in doubt, ALWAYS include. Missing a relevant paper is a serious
      error; over-including requires only a short full-text review.

    ## Confidence

    - high: you are certain of your decision based on clear abstract evidence
    - medium: you are reasonably confident but some ambiguity exists
    - low: limited information; inclusion by default"
  )
}

#' Build the user prompt for abstract-stage screening
#'
#' @param title Character. The citation title.
#' @param abstract Character. The citation abstract (may be NA or empty).
#'
#' @return A character string for the user message.
build_abstract_paper_prompt <- function(title, abstract) {
  abstract_text <- if (is.na(abstract) || nchar(trimws(abstract)) == 0) {
    "[No abstract available — include by default unless the title clearly
    indicates ineligibility]"
  } else {
    abstract
  }

  glue::glue(
    "Please screen the following citation.

    ## Title

    {title}

    ## Abstract

    {abstract_text}"
  )
}

#' Build the system prompt for full-text screening
#'
#' Implements a ScreenPrompt-style approach for full-text stage screening.
#' Requires per-criterion chain-of-thought evaluation citing specific paper
#' sections before making the final decision. Applies inclusion bias — at this
#' stage, a higher evidence threshold applies than at abstract screening, but
#' ambiguous cases still favour inclusion.
#'
#' @param criteria Character. A text description of the screening criteria
#'   (inclusion and exclusion rules).
#'
#' @return A character string for the LLM system prompt.
build_fulltext_system_prompt <- function(criteria) {
  glue::glue(
    "You are an expert systematic reviewer performing FULL-TEXT SCREENING
    for a systematic review. You will receive the full text of a scientific
    paper (in markdown format) and must make a definitive eligibility decision.

    ## Screening Criteria

    {criteria}

    ## How to Evaluate

    Work through EACH criterion systematically in your `criteria_evaluation`
    field before making your final decision:
    1. Identify and quote the relevant section(s) of the paper for each
       criterion.
    2. State clearly whether the criterion is met, not met, or cannot be
       determined from the paper.
    3. After evaluating all criteria, make your overall decision.

    ## Decision Rule

    - INCLUDE if the paper meets all inclusion criteria and no exclusion
      criteria are clearly violated.
    - INCLUDE if the paper is ambiguous or borderline on any criterion —
      it is better to include for data extraction review than to miss an
      eligible paper.
    - EXCLUDE only if there is clear, specific evidence in the paper that
      one or more exclusion criteria are definitively met.
    - If a criterion cannot be assessed from the available text (e.g.,
      data in supplement not available), treat it as met and include.

    ## Confidence

    - high: unambiguous evidence supports the decision
    - medium: mostly clear but minor uncertainty remains
    - low: significant uncertainty; defaulting to inclusion"
  )
}

#' Build the system prompt for full-text screening (backwards compatible alias)
#'
#' @inheritParams build_fulltext_system_prompt
#' @return A character string for the LLM system prompt.
build_system_prompt <- function(criteria) {
  build_fulltext_system_prompt(criteria)
}

#' Build the user prompt for a full-text paper
#'
#' @param paper_text Character. The full markdown text of the paper.
#'
#' @return A character string for the user message.
build_fulltext_paper_prompt <- function(paper_text) {
  glue::glue(
    "Please screen the following scientific paper against the criteria.

    ## Paper Text

    {paper_text}"
  )
}

#' Build the user prompt for a full-text paper (backwards compatible alias)
#'
#' @inheritParams build_fulltext_paper_prompt
#' @return A character string for the user message.
build_paper_prompt <- function(paper_text) {
  build_fulltext_paper_prompt(paper_text)
}

# ============================================================
# Chat Initialisation
# ============================================================

#' Initialise a chat object for LLM screening or extraction
#'
#' Creates an ellmer chat object with the appropriate system prompt.
#' Defaults to Claude via Anthropic.
#'
#' @param system_prompt Character. The system prompt to use.
#' @param provider Character. LLM provider. One of `"anthropic"`,
#'   `"openai"`, `"google"`. Default `"anthropic"`.
#' @param model Character or NULL. Model name. If NULL, uses the provider
#'   default.
#'
#' @return An ellmer chat object.
init_chat <- function(system_prompt,
                      provider = c("anthropic", "openai", "google"),
                      model = NULL) {
  provider <- match.arg(provider)

  switch(provider,
    anthropic = ellmer::chat_anthropic(system_prompt = system_prompt,
                                       model = model),
    openai    = ellmer::chat_openai(system_prompt = system_prompt,
                                    model = model),
    google    = ellmer::chat_google(system_prompt = system_prompt,
                                    model = model)
  )
}

#' Initialise a chat object for full-text screening (backwards compatible)
#'
#' @param criteria Character. Screening criteria text.
#' @param provider Character. LLM provider. Default `"anthropic"`.
#' @param model Character or NULL. Model name.
#'
#' @return An ellmer chat object.
init_screening_chat <- function(criteria,
                                provider = c("anthropic", "openai", "google"),
                                model = NULL) {
  provider <- match.arg(provider)
  init_chat(
    system_prompt = build_fulltext_system_prompt(criteria),
    provider = provider,
    model = model
  )
}

# ============================================================
# Single-item Screeners
# ============================================================

#' Screen a single citation by title and abstract
#'
#' @param chat An ellmer chat object with abstract screening system prompt.
#' @param title Character. The citation title.
#' @param abstract Character. The citation abstract (may be NA).
#' @param schema An ellmer type specification (from
#'   `define_abstract_screening_schema()`).
#'
#' @return A named list with abstract screening results.
screen_single_abstract <- function(chat, title, abstract, schema) {
  prompt <- build_abstract_paper_prompt(title, abstract)
  chat_clone <- chat$clone()
  chat_clone$chat_structured(prompt, type = schema)
}

#' Screen a single paper by full text
#'
#' @param chat An ellmer chat object (from `init_screening_chat()`).
#' @param paper_text Character. Full markdown text of the paper.
#' @param schema An ellmer type specification (from `define_screening_schema()`).
#'
#' @return A named list with screening results.
screen_single_paper <- function(chat, paper_text, schema) {
  prompt <- build_fulltext_paper_prompt(paper_text)
  chat_clone <- chat$clone()
  chat_clone$chat_structured(prompt, type = schema)
}

# ============================================================
# Batch Screeners
# ============================================================

#' Screen a batch of citations by title and abstract
#'
#' Stage-1 abstract screening. Iterates over a citation tibble (from
#' `read_citation_csv()`) and screens each citation's title+abstract using
#' the enhanced ScreenPrompt approach with strong inclusion bias.
#'
#' @param citations_df A tibble with at least columns `citation_id`, `title`,
#'   and `abstract` (from `read_citation_csv()`).
#' @param criteria Character. Screening criteria text.
#' @param criteria_names Character vector or NULL. Individual criterion names
#'   for per-criterion tracking.
#' @param provider Character. LLM provider. Default `"anthropic"`.
#' @param model Character or NULL. Model name.
#' @param parallel Logical. If TRUE, use `furrr::future_map()` for concurrent
#'   processing. Requires a `future` plan to be set (e.g.
#'   `future::plan(future::multisession, workers = n_workers)`).
#'   Default FALSE.
#'
#' @return A tibble with one row per citation: `citation_id`, `title`,
#'   `decision`, `confidence`, `reason`, `criteria_evaluation`, plus any
#'   per-criterion boolean columns.
screen_abstracts_batch <- function(citations_df,
                                   criteria,
                                   criteria_names = NULL,
                                   provider = "anthropic",
                                   model = NULL,
                                   parallel = FALSE) {
  schema <- define_abstract_screening_schema(criteria_names)
  chat   <- init_chat(
    system_prompt = build_abstract_system_prompt(criteria),
    provider = provider,
    model = model
  )

  cli::cli_alert_info(
    "Abstract screening {nrow(citations_df)} citation{?s} with {provider}..."
  )

  map_fn <- if (parallel) {
    rlang::check_installed("furrr",
      reason = "is required for parallel screening")
    furrr::future_map
  } else {
    purrr::map
  }

  results <- map_fn(
    cli::cli_progress_along(citations_df$title,
                            name = "Screening abstracts"),
    \(i) {
      tryCatch(
        screen_single_abstract(
          chat,
          citations_df$title[[i]],
          citations_df$abstract[[i]],
          schema
        ),
        error = function(e) {
          cli::cli_warn(
            "Failed abstract screening for citation {citations_df$citation_id[[i]]}: {e$message}"
          )
          list(
            criteria_evaluation = glue::glue("Error: {e$message}"),
            decision   = "include",  # default to include on error
            confidence = "low",
            reason     = glue::glue("Screening error — included by default: {e$message}")
          )
        }
      )
    }
  )

  results_df <- purrr::map_dfr(results, \(x) tibble::as_tibble(x))

  dplyr::bind_cols(
    citations_df |> dplyr::select("citation_id", "title"),
    results_df
  )
}

#' Screen multiple full-text papers in batch
#'
#' Stage-2 full-text screening. Iterates over a papers tibble (from
#' `read_all_markdown_papers()`) and screens each paper using the enhanced
#' ScreenPrompt approach.
#'
#' @param papers_df A tibble with at least columns `file_name` and `content`.
#' @param criteria Character. Screening criteria text.
#' @param criteria_names Character vector or NULL. Individual criterion names
#'   for per-criterion tracking.
#' @param provider Character. LLM provider. Default `"anthropic"`.
#' @param model Character or NULL. Model name.
#' @param parallel Logical. If TRUE, use `furrr::future_map()`. Requires a
#'   `future` plan. Default FALSE.
#'
#' @return A tibble with one row per paper: `file_name`, `decision`,
#'   `confidence`, `reason`, `criteria_evaluation`, plus any per-criterion
#'   boolean columns.
screen_papers_batch <- function(papers_df,
                                criteria,
                                criteria_names = NULL,
                                provider = "anthropic",
                                model = NULL,
                                parallel = FALSE) {
  schema <- define_screening_schema(criteria_names)
  chat   <- init_screening_chat(criteria, provider = provider, model = model)

  cli::cli_alert_info(
    "Full-text screening {nrow(papers_df)} paper{?s} with {provider}..."
  )

  map_fn <- if (parallel) {
    rlang::check_installed("furrr",
      reason = "is required for parallel screening")
    furrr::future_map
  } else {
    purrr::map
  }

  results <- map_fn(
    cli::cli_progress_along(papers_df$content, name = "Screening papers"),
    \(i) {
      tryCatch(
        screen_single_paper(chat, papers_df$content[[i]], schema),
        error = function(e) {
          cli::cli_warn(
            "Failed to screen {.val {papers_df$file_name[[i]]}}: {e$message}"
          )
          list(
            criteria_evaluation = glue::glue("Error: {e$message}"),
            decision   = NA_character_,
            confidence = NA_character_,
            reason     = glue::glue("Error: {e$message}")
          )
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
