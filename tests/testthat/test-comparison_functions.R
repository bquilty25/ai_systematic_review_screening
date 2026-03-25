test_that("compare_screening_decisions produces correct comparison", {
  llm_results <- tibble::tibble(
    file_name  = c("paper1", "paper2", "paper3", "paper4"),
    decision   = c("include", "exclude", "include", "exclude"),
    confidence = c("high", "high", "medium", "high"),
    reason     = c("Relevant", "Not relevant", "Possibly relevant", "Off topic")
  )

  human_results <- tibble::tibble(
    paper_id            = c("paper1", "paper2", "paper3", "paper4"),
    screening_decision  = c("include", "exclude", "exclude", "exclude")
  )

  comparison <- compare_screening_decisions(
    llm_results        = llm_results,
    human_results      = human_results,
    llm_id_col         = "file_name",
    human_id_col       = "paper_id",
    human_decision_col = "screening_decision"
  )

  expect_s3_class(comparison, "tbl_df")
  expect_equal(nrow(comparison), 4)
  expect_true("agree" %in% names(comparison))
  expect_equal(sum(comparison$agree), 3) # paper3 is the disagreement
})

test_that("summarise_comparison computes correct metrics", {
  # Known comparison: 1 TP, 1 FP, 2 TN, 0 FN
  comparison_df <- tibble::tibble(
    join_id        = c("a", "b", "c", "d"),
    llm_decision   = c("include", "include", "exclude", "exclude"),
    human_decision = c("include", "exclude", "exclude", "exclude"),
    llm_confidence = rep("high", 4),
    llm_reason     = rep("test", 4),
    agree          = c(TRUE, FALSE, TRUE, TRUE)
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

test_that("summarise_comparison includes confidence intervals", {
  comparison_df <- tibble::tibble(
    join_id        = c("a", "b", "c", "d"),
    llm_decision   = c("include", "include", "exclude", "exclude"),
    human_decision = c("include", "exclude", "exclude", "exclude"),
    llm_confidence = rep("high", 4),
    llm_reason     = rep("test", 4),
    agree          = c(TRUE, FALSE, TRUE, TRUE)
  )

  metrics <- summarise_comparison(comparison_df)
  expect_true(all(c(
    "sensitivity_lower", "sensitivity_upper",
    "specificity_lower", "specificity_upper"
  ) %in% names(metrics)))
  # CIs should be numeric and within [0, 1]
  expect_true(metrics$sensitivity_lower >= 0 && metrics$sensitivity_lower <= 1)
  expect_true(metrics$sensitivity_upper >= 0 && metrics$sensitivity_upper <= 1)
  expect_true(metrics$sensitivity_lower <= metrics$sensitivity_upper)
})

test_that("summarise_comparison accepts a stage label", {
  comparison_df <- tibble::tibble(
    join_id        = c("a", "b"),
    llm_decision   = c("include", "exclude"),
    human_decision = c("include", "exclude"),
    llm_confidence = c("high", "high"),
    llm_reason     = c("ok", "ok"),
    agree          = c(TRUE, TRUE)
  )
  metrics <- summarise_comparison(comparison_df, stage = "abstract")
  expect_true("stage" %in% names(metrics))
  expect_equal(metrics$stage, "abstract")
})

test_that("wilson_ci returns valid 95% CI", {
  ci <- wilson_ci(8, 10)
  expect_named(ci, c("lower", "upper"))
  expect_true(ci[["lower"]] >= 0 && ci[["lower"]] <= 1)
  expect_true(ci[["upper"]] >= 0 && ci[["upper"]] <= 1)
  expect_true(ci[["lower"]] <= ci[["upper"]])
})

test_that("wilson_ci handles edge cases", {
  # n = 0 returns NA
  expect_true(is.na(wilson_ci(0, 0)[["lower"]]))
  # x = 0 out of 10 — CI lower should be 0
  expect_equal(wilson_ci(0, 10)[["lower"]], 0)
  # x = n — CI upper should be 1
  expect_equal(wilson_ci(10, 10)[["upper"]], 1)
})

test_that("compute_confusion_matrix returns correct structure", {
  comparison_df <- tibble::tibble(
    llm_decision   = c("include", "include", "exclude", "exclude"),
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
    file_name  = "paper_x",
    decision   = "include",
    confidence = "high",
    reason     = "test"
  )

  human_results <- tibble::tibble(
    paper_id           = "paper_y",
    screening_decision = "include"
  )

  expect_warning(
    compare_screening_decisions(
      llm_results        = llm_results,
      human_results      = human_results,
      llm_id_col         = "file_name",
      human_id_col       = "paper_id",
      human_decision_col = "screening_decision"
    ),
    "No matching records"
  )
})
