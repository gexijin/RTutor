# =============================================================================
# Thorough tests for "Revert to Original Data" in mod_15_data_types.R
#
# Run:  testthat::test_file("tests/testthat/test-revert_data_button.R")
#       devtools::test()
#
# Design: tests are fully self-contained — no source() or load_all() required.
#   - Inline-logic tests: reproduce the relevant handler and mod_05
#     update_environment() logic in pure R.  Always runnable.
#   - testServer() tests: exercise the live module.  Require the RTutor package
#     to be loaded; skipped otherwise.
#
# Architecture recap (necessary to understand what "revert" touches):
#
#   current_data  — reactiveVal holding the active data frame used by the UI
#   original_data — reactiveVal holding the immutable copy set at upload time
#   run_env       — reactiveVal wrapping the R environment where LLM code runs
#   run_env_start — reactiveVal snapshot of run_env taken before each execution
#
#   The "Revert to Original Data" handler (mod_15_data_types.R) does:
#     1. current_data(original_data())          — resets current_data
#     2. updateSelectInput() for every column   — resets UI dropdowns
#     3. existing_vars <- as.list(run_env())
#        run_env(list2env(existing_vars))        — copies run_env unchanged
#        run_env_start(as.list(run_env()))       — snapshots it
#
#   Before every LLM submission, mod_05_llms.R::update_environment() runs:
#     existing_vars$df  <- current_data()
#     existing_vars$df2 <- current_data_2()
#     run_env(list2env(existing_vars))
#     run_env_start(as.list(run_env()))
#
#   Consequence: run_env$df is NOT immediately reset by the revert handler,
#   but it IS corrected by update_environment() before the next LLM call.
#   So the end-to-end flow (revert -> submit prompt) operates on original data.
#
# Test sections:
#   1.  current_data() after revert — inline logic (row filters)
#   2.  current_data() after revert — inline logic (column removal)
#   3.  current_data() after revert — inline logic (value mutations)
#   4.  current_data() after revert — inline logic (data type changes)
#   5.  current_data() after revert — inline logic (combined changes)
#   6.  run_env$df intermediate state (immediately after revert, before next prompt)
#   7.  End-to-end: revert -> update_environment() -> run_env$df
#   8.  run_env_start after revert
#   9.  Preservation of non-df run_env variables during revert
#   10. Second dataset (revert_data2) — inline logic
#   11. testServer() — current_data() after revert_data click
#   12. testServer() — run_env$df state after revert_data click
#   13. testServer() — run_env_start state after revert_data click
#   14. testServer() — second dataset (revert_data2)
#   15. testServer() — edge cases
# =============================================================================

library(testthat)

# ---------------------------------------------------------------------------
# Shared test data
# ---------------------------------------------------------------------------

# A representative data frame with mixed column types
make_orig_df <- function() {
  data.frame(
    id        = 1:10,
    score     = c(88.5, 72.0, 95.1, 60.3, 81.7, 77.2, 90.0, 55.5, 66.6, 100.0),
    grade     = c("A", "C", "A", "D", "B", "C", "A", "F", "D", "A"),
    pass      = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
}

make_orig_df2 <- function() {
  data.frame(
    sample_id = paste0("S", 1:6),
    value     = c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6),
    group     = c("X", "X", "Y", "Y", "Z", "Z"),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Pure-R replicas of the handler logic (no Shiny bindings needed)
# Used for inline tests.
# ---------------------------------------------------------------------------

# Simulates clicking "Revert to Original Data" for df1.
# Returns a list: current_data, run_env_list, run_env_start_list
simulate_revert_df1 <- function(current_data, original_data, run_env_list) {
  # --- Handler body (mod_15_data_types.R observeEvent revert_data) ---
  current_data    <- original_data                  # current_data(original_data())
  existing_vars   <- run_env_list                   # existing_vars <- as.list(run_env())
  run_env_list    <- existing_vars                  # run_env(list2env(existing_vars))
  run_env_start   <- run_env_list                   # run_env_start(as.list(run_env()))
  list(
    current_data    = current_data,
    run_env_list    = run_env_list,
    run_env_start   = run_env_start
  )
}

# Simulates clicking "Revert to Original Data" for df2.
simulate_revert_df2 <- function(current_data_2, original_data_2, run_env_list) {
  current_data_2  <- original_data_2
  existing_vars   <- run_env_list
  run_env_list    <- existing_vars
  run_env_start   <- run_env_list
  list(
    current_data_2  = current_data_2,
    run_env_list    = run_env_list,
    run_env_start   = run_env_start
  )
}

# Simulates mod_05_llms.R::update_environment() — called before every LLM request.
# This is the step that syncs run_env$df from current_data().
simulate_update_environment <- function(run_env_list, current_data,
                                        current_data_2 = NULL) {
  existing_vars        <- run_env_list
  existing_vars$df     <- current_data
  existing_vars$df2    <- current_data_2
  run_env_list         <- existing_vars
  run_env_start        <- run_env_list
  list(
    run_env_list  = run_env_list,
    run_env_start = run_env_start
  )
}


# =============================================================================
# 1. current_data() after revert — row filtering scenarios
# =============================================================================

test_that(
  "revert restores current_data row count after LLM row filter",
  {
    orig_df        <- make_orig_df()                       # 10 rows
    filtered_df    <- orig_df[orig_df$pass == TRUE, ]      # 7 rows
    run_env_list   <- list(df = filtered_df)

    result <- simulate_revert_df1(filtered_df, orig_df, run_env_list)

    expect_equal(nrow(result$current_data), 10,
      label = "current_data row count equals original after row-filter revert")
  }
)

test_that(
  "revert restores current_data to be identical to original_data after row filter",
  {
    orig_df     <- make_orig_df()
    filtered_df <- orig_df[1:3, ]
    run_env_list <- list(df = filtered_df)

    result <- simulate_revert_df1(filtered_df, orig_df, run_env_list)

    expect_identical(result$current_data, orig_df,
      label = "current_data is byte-identical to original after row-filter revert")
  }
)

test_that(
  "revert restores current_data when filter removed all but one row",
  {
    orig_df      <- make_orig_df()
    one_row_df   <- orig_df[1, ]
    run_env_list <- list(df = one_row_df)

    result <- simulate_revert_df1(one_row_df, orig_df, run_env_list)

    expect_equal(nrow(result$current_data), 10)
    expect_identical(result$current_data, orig_df)
  }
)

test_that(
  "revert restores current_data after row reordering (not just count)",
  {
    orig_df        <- make_orig_df()
    reordered_df   <- orig_df[order(orig_df$score, decreasing = TRUE), ]
    rownames(reordered_df) <- NULL
    run_env_list   <- list(df = reordered_df)

    result <- simulate_revert_df1(reordered_df, orig_df, run_env_list)

    expect_identical(result$current_data, orig_df,
      label = "current_data row order matches original after reorder revert")
  }
)


# =============================================================================
# 2. current_data() after revert — column removal scenarios
# =============================================================================

test_that(
  "revert restores current_data column count after LLM dropped a column",
  {
    orig_df        <- make_orig_df()
    fewer_cols_df  <- orig_df[, c("id", "score")]          # 2 of 4 columns
    run_env_list   <- list(df = fewer_cols_df)

    result <- simulate_revert_df1(fewer_cols_df, orig_df, run_env_list)

    expect_equal(ncol(result$current_data), ncol(orig_df),
      label = "current_data column count restored after column-drop revert")
  }
)

test_that(
  "revert restores all original column names after LLM dropped columns",
  {
    orig_df        <- make_orig_df()
    fewer_cols_df  <- orig_df[, c("id", "grade")]
    run_env_list   <- list(df = fewer_cols_df)

    result <- simulate_revert_df1(fewer_cols_df, orig_df, run_env_list)

    expect_equal(sort(names(result$current_data)), sort(names(orig_df)),
      label = "all original column names present after column-drop revert")
  }
)

test_that(
  "revert restores current_data after LLM added extra columns",
  {
    orig_df       <- make_orig_df()
    extra_col_df  <- orig_df
    extra_col_df$new_col <- seq_len(nrow(orig_df))         # LLM added a column
    run_env_list  <- list(df = extra_col_df)

    result <- simulate_revert_df1(extra_col_df, orig_df, run_env_list)

    expect_equal(names(result$current_data), names(orig_df),
      label = "extra columns removed after revert")
    expect_false("new_col" %in% names(result$current_data),
      label = "LLM-added column is gone after revert")
  }
)


# =============================================================================
# 3. current_data() after revert — value mutation scenarios
# =============================================================================

test_that(
  "revert restores current_data values after LLM mutated a column",
  {
    orig_df       <- make_orig_df()
    mutated_df    <- orig_df
    mutated_df$score <- mutated_df$score * 2               # score doubled
    run_env_list  <- list(df = mutated_df)

    result <- simulate_revert_df1(mutated_df, orig_df, run_env_list)

    expect_equal(result$current_data$score, orig_df$score,
      label = "score column values restored to original after value-mutation revert")
  }
)

test_that(
  "revert restores current_data values after LLM replaced NAs",
  {
    orig_df      <- make_orig_df()
    orig_df$score[c(2, 5)] <- NA                           # original has NAs
    na_filled_df <- orig_df
    na_filled_df$score[is.na(na_filled_df$score)] <- 0    # LLM filled NAs
    run_env_list <- list(df = na_filled_df)

    result <- simulate_revert_df1(na_filled_df, orig_df, run_env_list)

    expect_true(is.na(result$current_data$score[2]),
      label = "NA in row 2 of score column is restored after revert")
    expect_true(is.na(result$current_data$score[5]),
      label = "NA in row 5 of score column is restored after revert")
  }
)


# =============================================================================
# 4. current_data() after revert — data type change scenarios
# =============================================================================

test_that(
  "revert restores current_data numeric column after user changed it to factor",
  {
    orig_df      <- make_orig_df()                         # id is integer
    typed_df     <- orig_df
    typed_df$id  <- as.factor(typed_df$id)                 # simulate modal change
    run_env_list <- list(df = typed_df)

    result <- simulate_revert_df1(typed_df, orig_df, run_env_list)

    expect_equal(class(result$current_data$id), class(orig_df$id),
      label = "id column class restored to original after factor-revert")
    expect_false(is.factor(result$current_data$id),
      label = "id column is no longer a factor after revert")
  }
)

test_that(
  "revert restores current_data character column after user changed it to factor",
  {
    orig_df        <- make_orig_df()                        # grade is character
    typed_df       <- orig_df
    typed_df$grade <- as.factor(typed_df$grade)            # simulate modal change
    run_env_list   <- list(df = typed_df)

    result <- simulate_revert_df1(typed_df, orig_df, run_env_list)

    expect_equal(class(result$current_data$grade), "character",
      label = "grade column restored to character after factor-revert")
  }
)

test_that(
  "revert restores current_data logical column after user changed it to character",
  {
    orig_df       <- make_orig_df()                        # pass is logical
    typed_df      <- orig_df
    typed_df$pass <- as.character(typed_df$pass)
    run_env_list  <- list(df = typed_df)

    result <- simulate_revert_df1(typed_df, orig_df, run_env_list)

    expect_equal(class(result$current_data$pass), "logical",
      label = "pass column restored to logical after type-revert")
  }
)

test_that(
  "revert restores all column classes simultaneously after multiple type changes",
  {
    orig_df        <- make_orig_df()
    typed_df       <- orig_df
    typed_df$id    <- as.factor(typed_df$id)               # int -> factor
    typed_df$score <- as.character(typed_df$score)         # dbl -> char
    typed_df$grade <- as.factor(typed_df$grade)            # chr -> factor
    run_env_list   <- list(df = typed_df)

    result <- simulate_revert_df1(typed_df, orig_df, run_env_list)

    for (col in names(orig_df)) {
      expect_equal(
        class(result$current_data[[col]]),
        class(orig_df[[col]]),
        label = paste("column", col, "class restored after multi-type-change revert")
      )
    }
  }
)

test_that(
  "revert restores current_data column classes to exactly match original_data classes",
  {
    orig_df      <- make_orig_df()
    typed_df     <- orig_df
    typed_df$id  <- as.numeric(typed_df$id)                # int -> dbl
    run_env_list <- list(df = typed_df)

    result <- simulate_revert_df1(typed_df, orig_df, run_env_list)

    orig_classes    <- sapply(orig_df, class)
    reverted_classes <- sapply(result$current_data, class)
    expect_equal(reverted_classes, orig_classes,
      label = "all column classes match original_data after revert")
  }
)


# =============================================================================
# 5. current_data() after revert — combined change scenarios
# =============================================================================

test_that(
  "revert handles combined row filter + type change: current_data fully restored",
  {
    orig_df      <- make_orig_df()
    combo_df     <- orig_df[orig_df$pass == TRUE, ]        # filtered rows
    combo_df$id  <- as.factor(combo_df$id)                 # type changed
    run_env_list <- list(df = combo_df)

    result <- simulate_revert_df1(combo_df, orig_df, run_env_list)

    expect_equal(nrow(result$current_data), nrow(orig_df),
      label = "row count restored after combined-change revert")
    expect_equal(class(result$current_data$id), class(orig_df$id),
      label = "id column type restored after combined-change revert")
    expect_identical(result$current_data, orig_df,
      label = "current_data fully identical to original after combined-change revert")
  }
)

test_that(
  "revert handles combined column removal + value mutation: current_data fully restored",
  {
    orig_df       <- make_orig_df()
    combo_df      <- orig_df[, c("id", "score")]           # columns removed
    combo_df$score <- combo_df$score + 100                 # values mutated
    run_env_list  <- list(df = combo_df)

    result <- simulate_revert_df1(combo_df, orig_df, run_env_list)

    expect_equal(ncol(result$current_data), ncol(orig_df),
      label = "column count restored after combined-change revert")
    expect_equal(result$current_data$score, orig_df$score,
      label = "score values restored after combined-change revert")
    expect_true("grade" %in% names(result$current_data),
      label = "removed column 'grade' is back after combined-change revert")
  }
)


# =============================================================================
# 6. run_env$df intermediate state — immediately after revert, before next prompt
# =============================================================================
# The revert handler does NOT explicitly reset run_env$df.
# run_env$df is corrected by mod_05::update_environment() before the next LLM call.
# These tests document the intermediate state.

test_that(
  "immediately after revert: run_env$df still contains pre-revert (filtered) data",
  {
    orig_df      <- make_orig_df()
    filtered_df  <- orig_df[1:3, ]
    run_env_list <- list(df = filtered_df, some_var = 99)

    result <- simulate_revert_df1(filtered_df, orig_df, run_env_list)

    # current_data is reset
    expect_equal(nrow(result$current_data), 10,
      label = "current_data is correctly reset to 10 rows after revert")

    # run_env$df is NOT yet reset — it still holds the pre-revert state
    # This is expected behavior: mod_05::update_environment() will fix it
    # before the next LLM prompt runs.
    expect_equal(nrow(result$run_env_list$df), 3,
      label = paste(
        "run_env$df still has 3 rows immediately after revert (before next prompt).",
        "This is expected — mod_05::update_environment() will sync it before LLM runs."
      ))
  }
)

test_that(
  "immediately after revert: run_env$df column types not yet updated",
  {
    orig_df       <- make_orig_df()
    typed_df      <- orig_df
    typed_df$id   <- as.factor(typed_df$id)
    run_env_list  <- list(df = typed_df)

    result <- simulate_revert_df1(typed_df, orig_df, run_env_list)

    # current_data is restored to integer id
    expect_false(is.factor(result$current_data$id),
      label = "current_data$id is NOT a factor after revert")

    # run_env$df still has the factor — will be fixed by update_environment()
    expect_true(is.factor(result$run_env_list$df$id),
      label = paste(
        "run_env$df$id is still a factor immediately after revert.",
        "This is corrected by mod_05::update_environment() before next LLM call."
      ))
  }
)


# =============================================================================
# 7. End-to-end: revert -> update_environment() -> run_env$df
# =============================================================================
# These tests replicate the full flow a user experiences: revert, then submit
# a new prompt.  mod_05::update_environment() bridges the gap.

test_that(
  "after revert + update_environment(): run_env$df row count matches original",
  {
    orig_df      <- make_orig_df()
    filtered_df  <- orig_df[1:3, ]
    run_env_list <- list(df = filtered_df)

    # Step 1: revert
    after_revert <- simulate_revert_df1(filtered_df, orig_df, run_env_list)

    # Step 2: user submits a prompt -> update_environment() runs
    after_update <- simulate_update_environment(
      after_revert$run_env_list,
      after_revert$current_data
    )

    expect_equal(nrow(after_update$run_env_list$df), 10,
      label = "run_env$df has 10 rows after revert + update_environment()")
  }
)

test_that(
  "after revert + update_environment(): run_env$df is identical to original_data",
  {
    orig_df      <- make_orig_df()
    filtered_df  <- orig_df[1:5, ]
    run_env_list <- list(df = filtered_df, extra = "keep_me")

    after_revert <- simulate_revert_df1(filtered_df, orig_df, run_env_list)
    after_update <- simulate_update_environment(
      after_revert$run_env_list,
      after_revert$current_data
    )

    expect_identical(after_update$run_env_list$df, orig_df,
      label = "run_env$df identical to original_data after revert + update_environment()")
  }
)

test_that(
  "after revert + update_environment(): run_env$df column types match original",
  {
    orig_df      <- make_orig_df()
    typed_df     <- orig_df
    typed_df$id  <- as.factor(typed_df$id)
    typed_df$score <- as.character(typed_df$score)
    run_env_list <- list(df = typed_df)

    after_revert <- simulate_revert_df1(typed_df, orig_df, run_env_list)
    after_update <- simulate_update_environment(
      after_revert$run_env_list,
      after_revert$current_data
    )

    orig_classes     <- sapply(orig_df, class)
    post_run_classes <- sapply(after_update$run_env_list$df, class)
    expect_equal(post_run_classes, orig_classes,
      label = "all column types in run_env$df match original after revert + update_environment()")
  }
)

test_that(
  "after revert + update_environment(): run_env$df values match original after mutation",
  {
    orig_df      <- make_orig_df()
    mutated_df   <- orig_df
    mutated_df$score <- 0
    run_env_list <- list(df = mutated_df)

    after_revert <- simulate_revert_df1(mutated_df, orig_df, run_env_list)
    after_update <- simulate_update_environment(
      after_revert$run_env_list,
      after_revert$current_data
    )

    expect_equal(after_update$run_env_list$df$score, orig_df$score,
      label = "run_env$df score values restored after revert + update_environment()")
  }
)

test_that(
  "after revert + update_environment(): run_env_start$df also reflects original",
  {
    orig_df      <- make_orig_df()
    filtered_df  <- orig_df[1:2, ]
    run_env_list <- list(df = filtered_df)

    after_revert <- simulate_revert_df1(filtered_df, orig_df, run_env_list)
    after_update <- simulate_update_environment(
      after_revert$run_env_list,
      after_revert$current_data
    )

    expect_equal(nrow(after_update$run_env_start$df), 10,
      label = "run_env_start$df has 10 rows after revert + update_environment()")
    expect_identical(after_update$run_env_start$df, orig_df,
      label = "run_env_start$df identical to original after revert + update_environment()")
  }
)


# =============================================================================
# 8. run_env_start after revert (immediately, before update_environment)
# =============================================================================

test_that(
  "run_env_start mirrors run_env immediately after revert (both hold pre-revert df)",
  {
    orig_df      <- make_orig_df()
    filtered_df  <- orig_df[1:3, ]
    run_env_list <- list(df = filtered_df)

    result <- simulate_revert_df1(filtered_df, orig_df, run_env_list)

    expect_equal(result$run_env_list, result$run_env_start,
      label = "run_env_start is a copy of run_env immediately after revert")
  }
)


# =============================================================================
# 9. Non-df variables in run_env are preserved during revert
# =============================================================================

test_that(
  "revert preserves non-df variables in run_env",
  {
    orig_df      <- make_orig_df()
    filtered_df  <- orig_df[1:3, ]
    run_env_list <- list(
      df            = filtered_df,
      model_fit     = lm(score ~ id, data = filtered_df),
      custom_string = "some intermediate result",
      numeric_val   = 42L
    )

    result <- simulate_revert_df1(filtered_df, orig_df, run_env_list)

    # Non-df vars should be unchanged immediately after revert
    expect_equal(result$run_env_list$custom_string, "some intermediate result",
      label = "custom_string preserved in run_env after revert")
    expect_equal(result$run_env_list$numeric_val, 42L,
      label = "numeric_val preserved in run_env after revert")
    expect_true(inherits(result$run_env_list$model_fit, "lm"),
      label = "model_fit (lm object) preserved in run_env after revert")
  }
)

test_that(
  "after revert + update_environment(): non-df run_env variables are preserved",
  {
    orig_df      <- make_orig_df()
    filtered_df  <- orig_df[1:3, ]
    run_env_list <- list(df = filtered_df, helper_val = "abc", counter = 5L)

    after_revert <- simulate_revert_df1(filtered_df, orig_df, run_env_list)
    after_update <- simulate_update_environment(
      after_revert$run_env_list,
      after_revert$current_data
    )

    expect_equal(after_update$run_env_list$helper_val, "abc",
      label = "helper_val preserved through revert + update_environment()")
    expect_equal(after_update$run_env_list$counter, 5L,
      label = "counter preserved through revert + update_environment()")
  }
)


# =============================================================================
# 10. Second dataset (revert_data2) — inline logic
# =============================================================================

test_that(
  "revert_data2 restores current_data_2 row count after LLM row filter",
  {
    orig_df2     <- make_orig_df2()                        # 6 rows
    filtered_df2 <- orig_df2[orig_df2$group == "X", ]     # 2 rows
    run_env_list <- list(df2 = filtered_df2)

    result <- simulate_revert_df2(filtered_df2, orig_df2, run_env_list)

    expect_equal(nrow(result$current_data_2), 6,
      label = "current_data_2 row count restored to 6 after df2 row-filter revert")
  }
)

test_that(
  "revert_data2 restores current_data_2 to be identical to original_data_2",
  {
    orig_df2     <- make_orig_df2()
    filtered_df2 <- orig_df2[1:2, ]
    run_env_list <- list(df2 = filtered_df2)

    result <- simulate_revert_df2(filtered_df2, orig_df2, run_env_list)

    expect_identical(result$current_data_2, orig_df2,
      label = "current_data_2 identical to original_data_2 after revert")
  }
)

test_that(
  "revert_data2 restores current_data_2 column types after type change",
  {
    orig_df2        <- make_orig_df2()
    typed_df2       <- orig_df2
    typed_df2$group <- as.factor(typed_df2$group)
    run_env_list    <- list(df2 = typed_df2)

    result <- simulate_revert_df2(typed_df2, orig_df2, run_env_list)

    expect_equal(class(result$current_data_2$group), "character",
      label = "group column restored to character in df2 after type-change revert")
  }
)

test_that(
  "after revert_data2 + update_environment(): run_env$df2 has original rows",
  {
    orig_df2     <- make_orig_df2()
    filtered_df2 <- orig_df2[1:2, ]
    run_env_list <- list(df = make_orig_df(), df2 = filtered_df2)

    after_revert <- simulate_revert_df2(filtered_df2, orig_df2, run_env_list)
    after_update <- simulate_update_environment(
      after_revert$run_env_list,
      current_data   = make_orig_df(),
      current_data_2 = after_revert$current_data_2
    )

    expect_equal(nrow(after_update$run_env_list$df2), 6,
      label = "run_env$df2 has 6 rows after df2 revert + update_environment()")
    expect_identical(after_update$run_env_list$df2, orig_df2,
      label = "run_env$df2 identical to original_data_2 after revert + update_environment()")
  }
)

test_that(
  "reverting df1 does not affect df2 state",
  {
    orig_df      <- make_orig_df()
    orig_df2     <- make_orig_df2()
    filtered_df  <- orig_df[1:3, ]
    filtered_df2 <- orig_df2[1:2, ]    # also filtered, but we only revert df1
    run_env_list <- list(df = filtered_df, df2 = filtered_df2)

    result <- simulate_revert_df1(filtered_df, orig_df, run_env_list)

    # df1 reverted
    expect_equal(nrow(result$current_data), 10,
      label = "current_data (df1) reverted to 10 rows")
    # df2 NOT reverted — we only clicked revert_data, not revert_data2
    expect_equal(nrow(result$run_env_list$df2), 2,
      label = "df2 in run_env unchanged when only df1 revert is clicked")
  }
)


# =============================================================================
# 11-15. testServer() tests — require RTutor package to be loaded
# =============================================================================

# mod_15_data_types_serv is an internal (unexported) function.
# devtools::test() calls load_all() first, making it available as a bare name.
# Installing the package via remotes::install_github() does NOT export it, so
# mod_15_data_types_serv would fail.  We use exists() to detect load_all().
has_module <- exists("mod_15_data_types_serv") &&
              requireNamespace("shiny", quietly = TRUE)

if (!has_module) {
  test_that("testServer revert tests skipped: mod_15_data_types_serv not in scope", {
    skip("Run devtools::load_all() (or devtools::test()) to enable testServer-based tests")
  })
} else {

  # Helper: build standard args for mod_15_data_types_serv
  make_mod15_args <- function(current_df, original_df,
                              current_df2 = NULL, original_df2 = NULL,
                              run_env_extra = list()) {

    e <- new.env()
    e$df  <- current_df
    e$df2 <- current_df2
    for (nm in names(run_env_extra)) assign(nm, run_env_extra[[nm]], envir = e)

    list(
      modal_closed    = shiny::reactiveVal(FALSE),
      run_env         = shiny::reactiveVal(e),
      run_env_start   = shiny::reactiveVal(as.list(e)),
      current_data    = shiny::reactiveVal(current_df),
      current_data_2  = shiny::reactiveVal(current_df2),
      original_data   = shiny::reactiveVal(original_df),
      original_data_2 = shiny::reactiveVal(original_df2),
      ch              = shiny::reactiveValues(code_history = list()),
      user_file       = shiny::reactive(NULL),
      user_file_2     = shiny::reactive(NULL)
    )
  }

  # -------------------------------------------------------------------------
  # Section 11: testServer — current_data() after revert_data click
  # -------------------------------------------------------------------------

  test_that(
    "testServer: after revert_data click, current_data() equals original_data()",
    {
      orig_df     <- make_orig_df()
      filtered_df <- orig_df[1:3, ]
      args        <- make_mod15_args(filtered_df, orig_df)

      shiny::testServer(mod_15_data_types_serv, args = args, {
        session$setInputs(revert_data = 1)
        expect_identical(current_data(), original_data(),
          label = "current_data() is identical to original_data() after revert")
      })
    }
  )

  test_that(
    "testServer: after revert_data click, current_data() row count equals original",
    {
      orig_df     <- make_orig_df()
      filtered_df <- orig_df[1:4, ]
      args        <- make_mod15_args(filtered_df, orig_df)

      shiny::testServer(mod_15_data_types_serv, args = args, {
        session$setInputs(revert_data = 1)
        expect_equal(nrow(current_data()), nrow(original_data()),
          label = "current_data() row count matches original after row-filter revert")
      })
    }
  )

  test_that(
    "testServer: after revert_data click, current_data() column names match original",
    {
      orig_df       <- make_orig_df()
      fewer_col_df  <- orig_df[, c("id", "score")]
      args          <- make_mod15_args(fewer_col_df, orig_df)

      shiny::testServer(mod_15_data_types_serv, args = args, {
        session$setInputs(revert_data = 1)
        expect_equal(sort(names(current_data())), sort(names(original_data())),
          label = "all original column names present in current_data() after column-drop revert")
      })
    }
  )

  test_that(
    "testServer: after revert_data click, current_data() column classes match original",
    {
      orig_df      <- make_orig_df()
      typed_df     <- orig_df
      typed_df$id  <- as.factor(typed_df$id)
      typed_df$score <- as.character(typed_df$score)
      args         <- make_mod15_args(typed_df, orig_df)

      shiny::testServer(mod_15_data_types_serv, args = args, {
        session$setInputs(revert_data = 1)
        orig_classes    <- sapply(original_data(), class)
        current_classes <- sapply(current_data(), class)
        expect_equal(current_classes, orig_classes,
          label = "column classes in current_data() match original after type-change revert")
      })
    }
  )

  test_that(
    "testServer: after revert_data click, current_data() values match original",
    {
      orig_df      <- make_orig_df()
      mutated_df   <- orig_df
      mutated_df$score <- 0
      args         <- make_mod15_args(mutated_df, orig_df)

      shiny::testServer(mod_15_data_types_serv, args = args, {
        session$setInputs(revert_data = 1)
        expect_equal(current_data()$score, original_data()$score,
          label = "score values in current_data() match original after value-mutation revert")
      })
    }
  )

  test_that(
    "testServer: after revert_data click, modal_closed reactive fires",
    {
      orig_df  <- make_orig_df()
      args     <- make_mod15_args(orig_df[1:3, ], orig_df)

      shiny::testServer(mod_15_data_types_serv, args = args, {
        session$setInputs(revert_data = 1)
        # After the revert handler fires modal_closed(TRUE) then immediately
        # the observeEvent(modal_closed()) sets it back to FALSE
        expect_false(modal_closed(),
          label = "modal_closed reactive is reset to FALSE after revert handler completes")
      })
    }
  )

  # -------------------------------------------------------------------------
  # Section 12: testServer — run_env()$df after revert_data click
  # -------------------------------------------------------------------------

  test_that(
    "testServer: immediately after revert_data, run_env()$df row count is pre-revert",
    {
      orig_df     <- make_orig_df()
      filtered_df <- orig_df[1:3, ]
      args        <- make_mod15_args(filtered_df, orig_df)

      shiny::testServer(mod_15_data_types_serv, args = args, {
        session$setInputs(revert_data = 1)
        # current_data() is reset correctly
        expect_equal(nrow(current_data()), 10,
          label = "current_data() has 10 rows after revert")
        # run_env()$df is NOT immediately reset; update_environment() (mod_05) fixes it
        expect_equal(nrow(run_env()$df), 3,
          label = paste(
            "run_env()$df still has 3 rows immediately after revert.",
            "mod_05::update_environment() will sync it before the next LLM prompt."
          ))
      })
    }
  )

  # -------------------------------------------------------------------------
  # Section 13: testServer — run_env_start after revert_data click
  # -------------------------------------------------------------------------

  test_that(
    "testServer: run_env_start() mirrors run_env() immediately after revert",
    {
      orig_df     <- make_orig_df()
      filtered_df <- orig_df[1:3, ]
      args        <- make_mod15_args(filtered_df, orig_df)

      shiny::testServer(mod_15_data_types_serv, args = args, {
        session$setInputs(revert_data = 1)
        expect_equal(
          nrow(run_env_start()$df),
          nrow(run_env()$df),
          label = "run_env_start()$df row count equals run_env()$df immediately after revert"
        )
      })
    }
  )

  # -------------------------------------------------------------------------
  # Section 14: testServer — second dataset (revert_data2)
  # -------------------------------------------------------------------------

  test_that(
    "testServer: after revert_data2 click, current_data_2() equals original_data_2()",
    {
      orig_df2     <- make_orig_df2()
      filtered_df2 <- orig_df2[1:2, ]
      orig_df      <- make_orig_df()
      args         <- make_mod15_args(orig_df, orig_df, filtered_df2, orig_df2)

      shiny::testServer(mod_15_data_types_serv, args = args, {
        session$setInputs(revert_data2 = 1)
        expect_identical(current_data_2(), original_data_2(),
          label = "current_data_2() identical to original_data_2() after revert_data2")
      })
    }
  )

  test_that(
    "testServer: after revert_data2 click, current_data_2() row count restored",
    {
      orig_df2     <- make_orig_df2()
      filtered_df2 <- orig_df2[orig_df2$group == "X", ]   # 2 rows
      orig_df      <- make_orig_df()
      args         <- make_mod15_args(orig_df, orig_df, filtered_df2, orig_df2)

      shiny::testServer(mod_15_data_types_serv, args = args, {
        session$setInputs(revert_data2 = 1)
        expect_equal(nrow(current_data_2()), 6,
          label = "current_data_2() has 6 rows after df2 row-filter revert")
      })
    }
  )

  test_that(
    "testServer: reverting df1 does not change current_data_2()",
    {
      orig_df      <- make_orig_df()
      orig_df2     <- make_orig_df2()
      filtered_df  <- orig_df[1:3, ]
      filtered_df2 <- orig_df2[1:2, ]   # also filtered, should NOT be reverted
      args         <- make_mod15_args(filtered_df, orig_df, filtered_df2, orig_df2)

      shiny::testServer(mod_15_data_types_serv, args = args, {
        session$setInputs(revert_data = 1)
        # df1 reverted
        expect_equal(nrow(current_data()), 10,
          label = "current_data() reverted to 10 rows")
        # df2 NOT reverted — only df1 revert was clicked
        expect_equal(nrow(current_data_2()), 2,
          label = "current_data_2() unchanged when only revert_data (not revert_data2) is clicked")
      })
    }
  )

  # -------------------------------------------------------------------------
  # Section 15: testServer — edge cases
  # -------------------------------------------------------------------------

  test_that(
    "testServer: revert when data already matches original has no adverse effect",
    {
      orig_df  <- make_orig_df()
      args     <- make_mod15_args(orig_df, orig_df)  # no changes made

      shiny::testServer(mod_15_data_types_serv, args = args, {
        session$setInputs(revert_data = 1)
        expect_identical(current_data(), original_data(),
          label = "current_data() still equals original when reverted with no changes")
        expect_equal(nrow(current_data()), nrow(orig_df),
          label = "no rows lost when reverting already-original data")
      })
    }
  )

  test_that(
    "testServer: non-df run_env variables preserved after revert",
    {
      orig_df  <- make_orig_df()
      args     <- make_mod15_args(
        orig_df[1:3, ], orig_df,
        run_env_extra = list(intermediate_result = "kept", count = 7L)
      )

      shiny::testServer(mod_15_data_types_serv, args = args, {
        session$setInputs(revert_data = 1)
        expect_equal(run_env()$intermediate_result, "kept",
          label = "non-df variable 'intermediate_result' preserved in run_env after revert")
        expect_equal(run_env()$count, 7L,
          label = "non-df variable 'count' preserved in run_env after revert")
      })
    }
  )

  test_that(
    "testServer: double revert (clicking revert twice) — current_data still correct",
    {
      orig_df     <- make_orig_df()
      filtered_df <- orig_df[1:3, ]
      args        <- make_mod15_args(filtered_df, orig_df)

      shiny::testServer(mod_15_data_types_serv, args = args, {
        session$setInputs(revert_data = 1)
        session$setInputs(revert_data = 2)   # second click increments button
        expect_identical(current_data(), original_data(),
          label = "current_data() still equals original after second revert click")
      })
    }
  )

}
