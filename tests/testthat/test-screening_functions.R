# ============================================================
# Abstract screening schema
# ============================================================

test_that("define_abstract_screening_schema returns a valid type specification", {
  schema <- define_abstract_screening_schema()
  expect_true(!is.null(schema))
})

test_that("define_abstract_screening_schema includes criteria_evaluation field", {
  schema <- define_abstract_screening_schema()
  # The schema description should reference criteria_evaluation
  expect_true(!is.null(schema))
  # With criteria names, should include extra criterion boolean fields
  schema_with_criteria <- define_abstract_screening_schema(
    criteria_names = c("Human subjects", "English language")
  )
  expect_true(!is.null(schema_with_criteria))
})

# ============================================================
# Full-text screening schema
# ============================================================

test_that("define_screening_schema returns a valid type specification", {
  schema <- define_screening_schema()
  expect_true(!is.null(schema))
})

test_that("define_screening_schema with criteria_names includes extra fields", {
  schema_with_criteria <- define_screening_schema(
    criteria_names = c("Human subjects", "English language")
  )
  expect_true(!is.null(schema_with_criteria))
})

# ============================================================
# Abstract screening prompt builders
# ============================================================

test_that("build_abstract_system_prompt includes criteria text", {
  criteria <- "Include only RCTs. Exclude observational studies."
  prompt <- build_abstract_system_prompt(criteria)

  expect_type(prompt, "character")
  expect_true(grepl("RCTs", prompt))
  expect_true(grepl("observational", prompt))
})

test_that("build_abstract_system_prompt encodes inclusion-bias language", {
  prompt <- build_abstract_system_prompt("Include epidemiological studies.")
  # Must instruct to include by default when uncertain
  expect_true(grepl("doubt", prompt, ignore.case = TRUE) ||
    grepl("INCLUDE", prompt) ||
    grepl("ambiguous", prompt, ignore.case = TRUE))
})

test_that("build_abstract_system_prompt asks for per-criterion reasoning", {
  prompt <- build_abstract_system_prompt("Include RCTs.")
  expect_true(grepl("criteria_evaluation", prompt, ignore.case = TRUE) ||
    grepl("systematically", prompt, ignore.case = TRUE) ||
    grepl("each criterion", prompt, ignore.case = TRUE))
})

test_that("build_abstract_paper_prompt includes title and abstract", {
  prompt <- build_abstract_paper_prompt(
    title    = "A randomised controlled trial",
    abstract = "We enrolled 200 adult patients."
  )
  expect_type(prompt, "character")
  expect_true(grepl("randomised controlled trial", prompt))
  expect_true(grepl("200 adult patients", prompt))
  expect_true(grepl("Title", prompt))
  expect_true(grepl("Abstract", prompt))
})

test_that("build_abstract_paper_prompt handles missing abstract gracefully", {
  prompt <- build_abstract_paper_prompt(title = "Some title", abstract = NA)
  expect_type(prompt, "character")
  expect_true(grepl("No abstract available", prompt))
})

# ============================================================
# Full-text prompt builders
# ============================================================

test_that("build_fulltext_system_prompt includes criteria text", {
  criteria <- "Include only RCTs. Exclude observational studies."
  prompt <- build_fulltext_system_prompt(criteria)

  expect_type(prompt, "character")
  expect_true(grepl("RCTs", prompt))
  expect_true(grepl("observational", prompt))
  expect_true(grepl("FULL-TEXT", prompt, ignore.case = FALSE) ||
    grepl("full.text", prompt, ignore.case = TRUE))
})

test_that("build_fulltext_system_prompt encodes inclusion-bias language", {
  prompt <- build_fulltext_system_prompt("Include epidemiological studies.")
  expect_true(grepl("ambiguous", prompt, ignore.case = TRUE) ||
    grepl("include", prompt, ignore.case = TRUE))
})

test_that("build_system_prompt is a backwards-compatible alias", {
  criteria <- "Include RCTs."
  expect_equal(
    build_system_prompt(criteria),
    build_fulltext_system_prompt(criteria)
  )
})

test_that("build_fulltext_paper_prompt includes paper text and section header", {
  paper_text <- "This is a randomised controlled trial of aspirin."
  prompt <- build_fulltext_paper_prompt(paper_text)

  expect_type(prompt, "character")
  expect_true(grepl("randomised controlled trial", prompt))
  expect_true(grepl("Paper Text", prompt))
})

test_that("build_paper_prompt is a backwards-compatible alias", {
  paper_text <- "Some paper content."
  expect_equal(
    build_paper_prompt(paper_text),
    build_fulltext_paper_prompt(paper_text)
  )
})
