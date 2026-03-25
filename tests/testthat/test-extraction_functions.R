# Tests for R/04_extraction_functions.R

# ============================================================
# Define extraction schema
# ============================================================

test_that("define_extraction_schema accepts string shorthand for variables", {
    variables <- list(
        sample_size  = "Total sample size (integer)",
        country      = "Country where the study was conducted"
    )
    schema <- define_extraction_schema(variables)
    expect_true(!is.null(schema))
})

test_that("define_extraction_schema accepts typed list definitions", {
    variables <- list(
        study_design = list(
            type        = "enum",
            values      = c("RCT", "cohort", "case-control"),
            description = "Study design"
        ),
        randomised = list(
            type        = "boolean",
            description = "Was randomisation used?"
        ),
        n_patients = list(
            type        = "number",
            description = "Number of patients enrolled"
        )
    )
    schema <- define_extraction_schema(variables)
    expect_true(!is.null(schema))
})

test_that("define_extraction_schema errors on invalid type", {
    variables <- list(
        weird_field = list(type = "table", description = "Not a valid type")
    )
    expect_error(define_extraction_schema(variables), "unknown type")
})

test_that("define_extraction_schema errors when enum has no values", {
    variables <- list(
        design = list(type = "enum", description = "No values provided")
    )
    expect_error(define_extraction_schema(variables), "values")
})

test_that("define_extraction_schema errors on non-named list input", {
    expect_error(
        define_extraction_schema(list("unnamed")),
        "named list"
    )
})

# ============================================================
# Prompt builders
# ============================================================

test_that("build_extraction_system_prompt includes variable descriptions", {
    variables <- list(
        sample_size = "Total sample size",
        study_design = list(
            type        = "enum",
            values      = c("RCT", "cohort"),
            description = "Study design type"
        )
    )
    prompt <- build_extraction_system_prompt(variables)
    expect_type(prompt, "character")
    expect_true(grepl("sample_size", prompt, ignore.case = TRUE))
    expect_true(grepl("Total sample size", prompt))
    expect_true(grepl("study_design", prompt, ignore.case = TRUE))
    # Enum values should appear in the prompt
    expect_true(grepl("RCT", prompt))
})

test_that("build_extraction_system_prompt instructs to return NA when not found", {
    prompt <- build_extraction_system_prompt(list(x = "A variable"))
    expect_true(
        grepl("not reported|not found|cannot be determined", prompt,
            ignore.case = TRUE
        ) ||
            grepl("empty string|NA|null", prompt, ignore.case = TRUE)
    )
})

test_that("build_extraction_paper_prompt includes paper text", {
    prompt <- build_extraction_paper_prompt("This RCT enrolled 120 patients.")
    expect_type(prompt, "character")
    expect_true(grepl("120 patients", prompt))
    expect_true(grepl("Paper Text", prompt))
})
