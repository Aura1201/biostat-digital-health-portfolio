# ============================================================
# R testthat Templates — Survival, ADaM, TLF, Shiny
# Author: Aura
# Note: Place this file under tests/testthat/ in a package repo,
#       or under tests/ in a non-package analysis repo.
# ============================================================

# Load required packages for tests
suppressPackageStartupMessages({
  library(testthat)
  # Load optional packages if available
  if (requireNamespace("survival", quietly = TRUE)) library(survival)
  if (requireNamespace("survminer", quietly = TRUE)) library(survminer)
  if (requireNamespace("shiny", quietly = TRUE)) library(shiny)
})

# ------------------------------------------------------------
# 0) Helper skips — keep tests robust in different environments
# ------------------------------------------------------------
skip_if_no_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    skip(paste0("Package '", pkg, "' not installed"))
  }
}

# ============================================================
# 1) Survival Analysis — Kaplan–Meier / Cox
# ============================================================
test_that("KM fit object is valid", {
  skip_if_no_pkg("survival")
  # Use built-in dataset as a minimal reproducible example
  lung <- survival::lung
  surv_obj <- Surv(time = lung$time, event = lung$status)
  fit <- survfit(surv_obj ~ 1, data = lung)

  expect_s3_class(fit, "survfit")        # correct class
  expect_gt(fit$n.event, 0)              # at least one event
  expect_true(all(fit$time > 0))         # times are positive
})

test_that("Cox model runs and contains expected coefficients", {
  skip_if_no_pkg("survival")
  lung <- survival::lung
  # Minimal Cox model for demonstration
  cox_model <- coxph(Surv(time, status) ~ age + sex, data = lung)

  expect_s3_class(cox_model, "coxph")
  coefs <- coef(cox_model)
  expect_true(all(c("age", "sex") %in% names(coefs)))
  expect_true(is.numeric(coefs[["age"]]))
})

# ============================================================
# 2) ADaM-like Data Structure Checks (example skeleton)
# ============================================================
test_that("ADaM-like dataset structure is correct", {
  # Example placeholder data.frame for demonstration
  adsl <- data.frame(
    USUBJID = sprintf("SUBJ%03d", 1:10),
    TRT01A  = sample(c("A", "B"), 10, TRUE),
    AVISIT  = factor(sample(c("Baseline","Week 4","Week 8"), 10, TRUE)),
    AVAL    = runif(10, 0, 100),
    stringsAsFactors = FALSE
  )

  expect_true(all(c("USUBJID", "TRT01A", "AVISIT", "AVAL") %in% names(adsl)))
  expect_false(any(is.na(adsl$USUBJID)))
  expect_true(all(adsl$AVAL >= 0))
  expect_gt(nrow(adsl), 0)
})

# ============================================================
# 3) TLF Verification — Table and Plot QC
# ============================================================
test_that("AE summary table structure is valid", {
  ae_table <- data.frame(
    "Preferred Term" = c("Headache","Fatigue"),
    "n (%)" = c("12 (5.3%)", "9 (4.0%)"),
    stringsAsFactors = FALSE
  )
  expect_true("Preferred Term" %in% names(ae_table))
  expect_true("n (%)" %in% names(ae_table))
  expect_equal(nrow(ae_table), 2)
})

test_that("KM plot file is generated (if exists)", {
  plot_path <- "outputs/km_curve_day2.png"
  skip_if_not(file.exists(plot_path), message = "KM plot not found; skip file checks.")
  expect_gt(file.info(plot_path)$size, 10000)  # ~10 KB minimal sanity check
})

# ============================================================
# 4) Statistical Consistency Checks
# ============================================================
test_that("Summary statistics are within tolerance", {
  set.seed(123)
  x <- rnorm(1000, mean = 25.6, sd = 3.2)
  expect_equal(mean(x), 25.6, tolerance = 0.15)
  expect_equal(sd(x), 3.2, tolerance = 0.15)
})

test_that("Percentages sum to ~100%", {
  df <- data.frame(percent = c(33.4, 33.3, 33.3))
  expect_equal(sum(df$percent), 100, tolerance = 0.5)
})

# ============================================================
# 5) Shiny App Sanity Checks (optional)
# ============================================================
test_that("Shiny app can be initialized (if present)", {
  skip_if_no_pkg("shiny")
  app_dir <- "app"
  skip_if_not(dir.exists(app_dir), message = "No Shiny app dir; skipping.")
  # Do not actually run the app; just check ui/server existence
  expect_true(file.exists(file.path(app_dir, "ui.R")) || file.exists(file.path(app_dir, "app.R")))
})

# ============================================================
# 6) GxP-style logging suggestions (comments)
#    - Use descriptive test names (traceable in reports)
#    - Separate unit vs. integration tests if needed
#    - Keep tests deterministic; use set.seed() for randomness
#    - Record environment info with sessionInfo() when archiving logs
# ============================================================
