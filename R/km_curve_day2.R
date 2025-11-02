##############################################################
# Kaplan–Meier Survival Analysis (Day 2)
# Author: Aura
# Purpose: To demonstrate basic time-to-event analysis using
#          the 'lung' dataset from the survival package.
# Date: YYYY-MM-DD
##############################################################

# ============================================================
# 1. Load required libraries
# ============================================================
# 'survival' provides core survival analysis functions.
# 'survminer' helps visualize survival curves (ggplot2-based).
library(survival)
library(survminer)

# ============================================================
# 2. Load example dataset
# ============================================================
# The 'lung' dataset is built into the survival package.
# It contains time-to-event data for patients with advanced lung cancer.
lung <- survival::lung   # safer way to ensure data is loaded
head(lung)               # preview first few rows

# Variable meanings:
# time   = survival time in days
# status = event indicator (1=censored, 2=death)
# sex    = 1=male, 2=female
# age, ph.ecog, etc. = baseline covariates

# ============================================================
# 3. Create survival object
# ============================================================
# Surv(time, event) creates the key data structure for survival analysis.
# It combines time and status variables into a single object.
surv_obj <- Surv(time = lung$time, event = lung$status)

# Optional: inspect first few elements
head(surv_obj)

# ============================================================
# 4. Fit Kaplan–Meier model
# ============================================================
# survfit() estimates survival probabilities over time.
# '~ sex' fits separate KM curves by gender.
fit <- survfit(surv_obj ~ sex, data = lung)
# fit <- survfit(surv_obj ~ 1, data = lung)

# Display summary of survival estimates
summary(fit)
summary(fit)$table

# ============================================================
# 5. Plot survival curve
# ============================================================
# ggsurvplot() automatically generates a KM plot with confidence intervals
# and log-rank p-value (tests for group difference).
km_plot <- ggsurvplot(
  fit,
  conf.int = TRUE,             # show 95% confidence interval
  pval = TRUE,                 # display log-rank test p-value
  risk.table = TRUE,           # show number-at-risk table
  legend.title = "Sex",        # legend title
  legend.labs = c("Male", "Female"),  # group labels
  xlab = "Time (days)",
  ylab = "Survival Probability",
  palette = c("#0072B2", "#D55E00")  # custom colors
)

# Print plot to RStudio viewer
print(km_plot)

# ============================================================
# 6. Save plot to file
# ============================================================
# Save KM curve as high-resolution PNG
ggsave("outputs/km_curve_day2.png", km_plot$plot, width = 7, height = 5, dpi = 300)

# ============================================================
# 7. Unit test (simple QC check)
# ============================================================
# This ensures that the 'fit' object is of class 'survfit'.
# It’s a lightweight GxP-style verification step.
library(testthat)

test_that("KM fit object is valid", {
  expect_s3_class(fit, "survfit")
})

# ============================================================
# 8. Optional extensions
# ============================================================
# subgrouping by another variable, e.g., age > 65:
fit_age <- survfit(Surv(time, status) ~ age > 65, data = lung)
ggsurvplot(fit_age, conf.int = TRUE, pval = TRUE)
##############################################################