# =============================================================================
# sensitivity_excl20-48.R
# Sensitivity analysis: participants 20 (M) and 48 (F) excluded instead of
# 33 (F) and 50 (M). Everything else is identical to analysis_R3.R.
#
# Usage: place this file in the same folder as analysis_R3.R and errors.xlsx,
# run analysis_R3.R first (results_R3/), then:
#   source("sensitivity_excl20-48.R")
# Outputs: results_sens_20-48/ (results_R3/ is not overwritten).
# =============================================================================

src <- readLines("analysis_R3_v4.R", encoding = "UTF-8")

# 1. Exclusion set: 20 and 48 instead of 33 and 50
hit <- grep("^EXCLUDE_IDS <- c\\(33, 50\\)", src)
stopifnot(length(hit) == 1)
src[hit] <- "EXCLUDE_IDS <- c(20, 48)   # SENSITIVITY: exclusions of the previous version"

# 2. Separate output folder
src <- gsub("results_R3", "results_sens_20-48", src, fixed = TRUE)

# 3. Run the full analysis with the sensitivity exclusions
dir.create("results_sens_20-48", showWarnings = FALSE)
eval(parse(text = src), envir = globalenv())

# 4. Side-by-side comparison with the main analysis
#    (main: 33 and 50 excluded; sensitivity: 20 and 48 excluded)
main_dir <- "results_R3"
sens_dir <- "results_sens_20-48"
tables <- c("gender_gap_by_condition.csv", "contrasts.csv",
            "primary_accuracy_anova.csv", "primary_rt_anova.csv",
            "bf_interaction.csv", "validation_rt_slope.csv")

for (f in tables) {
  if (file.exists(file.path(main_dir, f)) && file.exists(file.path(sens_dir, f))) {
    a <- read.csv(file.path(main_dir, f))
    b <- read.csv(file.path(sens_dir, f))
    comp <- cbind(setNames(a, paste0("main_", names(a))),
                  setNames(b, paste0("sens_", names(b))))
    write.csv(comp, file.path(sens_dir, paste0("comparison_", f)), row.names = FALSE)
  }
}
cat("\nComparison tables written to", sens_dir, "\n")

