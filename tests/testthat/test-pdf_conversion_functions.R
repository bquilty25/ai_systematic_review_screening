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
  tmp_dir <- tempdir()
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
