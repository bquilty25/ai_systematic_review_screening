test_that("compare_screening_decisions produces correct comparison", {
  llm_results <- tibble::tibble(
    file_name = c("paper1", "paper2", "paper3", "paper4"),
    decision = c("include", "exclude", "include", "exclude"),
    confidence = c("high", "high", "medium", "high"),
    reason = c("Relevant", "Not relevant", "Possibly relevant", "Off topic")
  )

  human_results <- tibble::tibble(
    paper_id = c("paper1", "paper2", "paper3", "paper4"),
    screening_decision = c("include", "exclude", "exclude", "exclude")
  )

  comparison <- compare_screening_decisions(
    llm_results = llm_results,
    human_results = human_results,
    llm_id_col = "file_name",
    human_id_col = "paper_id",
    human_decision_col = "screening_decision"
  )

  expect_s3_class(comparison, "tbl_df")
  expect_equal(nrow(comparison), 4)
  expect_true("agree" %in% names(comparison))
  expect_equal(sum(comparison$agree), 3) # paper3 is the disagreement
})

test_that("summarise_comparison computes correct metrics", {
  # Create a known comparison: 1 TP, 1 FP, 2 TN, 0 FN
  comparison_df <- tibble::tibble(
    join_id = c("a", "b", "c", "d"),
    llm_decision = c("include", "include", "exclude", "exclude"),
    human_decision = c("include", "exclude", "exclude", "exclude"),
    llm_confidence = rep("high", 4),
    llm_reason = rep("test", 4),
    agree = c(TRUE, FALSE, TRUE, TRUE)
  )

  metrics <- summarise_comparison(comparison_df)

  expect_s3_class(metrics, "tbl_df")
  expect_equal(metrics$tp, 1)
  expect_equal(metrics$fp, 1)
  expect_equal(metrics$tn, 2)
  expect_equal(metrics$fn, 0)
  expect_equal(metrics$n_total, 4)
  expect_equal(metrics$accuracy, 0.75)
  expect_equal(metrics$sensitivity, 1.0)
})

test_that("compute_confusion_matrix returns correct structure", {
  comparison_df <- tibble::tibble(
    llm_decision = c("include", "include", "exclude", "exclude"),
    human_decision = c("include", "exclude", "exclude", "include")
  )

  cm <- compute_confusion_matrix(comparison_df)

  expect_type(cm, "list")
  expect_named(cm, c("tp", "fp", "tn", "fn", "table"))
  expect_equal(cm$tp, 1)
  expect_equal(cm$fp, 1)
  expect_equal(cm$tn, 1)
  expect_equal(cm$fn, 1)
})

test_that("compare_screening_decisions warns on empty join", {
  llm_results <- tibble::tibble(
    file_name = "paper_x",
    decision = "include",
    confidence = "high",
    reason = "test"
  )

  human_results <- tibble::tibble(
    paper_id = "paper_y",
    screening_decision = "include"
  )

  expect_warning(
    compare_screening_decisions(
      llm_results = llm_results,
      human_results = human_results,
      llm_id_col = "file_name",
      human_id_col = "paper_id",
      human_decision_col = "screening_decision"
    ),
    "No matching records"
  )
})
