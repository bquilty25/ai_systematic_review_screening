test_that("read_markdown_paper reads a file correctly", {
  # Create a temporary markdown file
  tmp <- tempfile(fileext = ".md")
  writeLines(c("# Title", "", "Some content here."), tmp)

  result <- read_markdown_paper(tmp)
  expect_type(result, "character")
  expect_length(result, 1)
  expect_true(grepl("# Title", result))
  expect_true(grepl("Some content here.", result))

  unlink(tmp)
})

test_that("read_markdown_paper errors on missing file", {
  expect_error(
    read_markdown_paper("/nonexistent/path/file.md"),
    "not found"
  )
})

test_that("read_all_markdown_papers returns expected tibble structure", {
  tmp_dir  <- tempdir()
  md_subdir <- file.path(tmp_dir, "test_md_papers")
  dir.create(md_subdir, showWarnings = FALSE)

  writeLines(c("# Paper 1", "Content 1"), file.path(md_subdir, "paper1.md"))
  writeLines(c("# Paper 2", "Content 2"), file.path(md_subdir, "paper2.md"))

  result <- read_all_markdown_papers(md_subdir)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_named(result, c("file_name", "md_path", "content"))
  expect_true(all(grepl("paper", result$file_name)))

  unlink(md_subdir, recursive = TRUE)
})

test_that("read_all_markdown_papers warns on empty directory", {
  tmp_dir <- file.path(tempdir(), "empty_md_dir")
  dir.create(tmp_dir, showWarnings = FALSE)

  expect_warning(
    result <- read_all_markdown_papers(tmp_dir),
    "No markdown files"
  )
  expect_equal(nrow(result), 0)

  unlink(tmp_dir, recursive = TRUE)
})

# ============================================================
# CSV citation ingestion
# ============================================================

test_that("read_citation_csv reads CSV and returns expected structure", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      title    = c("Study A", "Study B"),
      abstract = c("Abstract of A", "Abstract of B"),
      year     = c(2020L, 2021L)
    ),
    tmp
  )

  result <- read_citation_csv(tmp)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true(all(c("citation_id", "title", "abstract") %in% names(result)))
  # citation_id should be auto-generated sequential integers
  expect_equal(result$citation_id, 1:2)
  # Extra columns should be preserved
  expect_true("year" %in% names(result))

  unlink(tmp)
})

test_that("read_citation_csv uses supplied id_col", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      pmid     = c("PMC001", "PMC002"),
      title    = c("Study A", "Study B"),
      abstract = c("Abstract A", "Abstract B")
    ),
    tmp
  )

  result <- read_citation_csv(tmp, id_col = "pmid")

  expect_equal(result$citation_id, c("PMC001", "PMC002"))

  unlink(tmp)
})

test_that("read_citation_csv errors on missing file", {
  expect_error(
    read_citation_csv("/nonexistent/citations.csv"),
    "not found"
  )
})

test_that("read_citation_csv errors when required columns are missing", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(title = "Some title", year = 2020),
    tmp  # no 'abstract' column
  )

  expect_error(
    read_citation_csv(tmp),
    "Required column"
  )

  unlink(tmp)
})

test_that("read_citation_csv accepts custom column name mappings", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      study_title    = c("Study A"),
      study_abstract = c("Abstract A")
    ),
    tmp
  )

  result <- read_citation_csv(
    tmp,
    title_col    = "study_title",
    abstract_col = "study_abstract"
  )

  expect_true(all(c("title", "abstract") %in% names(result)))

  unlink(tmp)
})
