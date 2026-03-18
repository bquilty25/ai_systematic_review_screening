test_that("define_screening_schema returns a valid type specification", {
  schema <- define_screening_schema()

  # Should be a list-like type object
  expect_true(!is.null(schema))

  # With criteria names, should include extra fields
  schema_with_criteria <- define_screening_schema(
    criteria_names = c("Human subjects", "English language")
  )
  expect_true(!is.null(schema_with_criteria))
})

test_that("build_system_prompt includes criteria text", {
  criteria <- "Include only RCTs. Exclude observational studies."
  prompt <- build_system_prompt(criteria)

  expect_type(prompt, "character")
  expect_true(grepl("RCTs", prompt))
  expect_true(grepl("observational", prompt))
  expect_true(grepl("Screening Criteria", prompt))
})

test_that("build_paper_prompt includes paper text", {
  paper_text <- "This is a randomised controlled trial of aspirin."
  prompt <- build_paper_prompt(paper_text)

  expect_type(prompt, "character")
  expect_true(grepl("randomised controlled trial", prompt))
  expect_true(grepl("Paper Text", prompt))
})
