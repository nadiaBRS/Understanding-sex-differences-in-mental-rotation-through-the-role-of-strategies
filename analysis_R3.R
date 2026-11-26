# =============================================================================
# analysis_R3.R
# Serial mental rotation: Gender x strategy condition
# Reproduces the analyses of the revised manuscript (resubmission, round 3).
#
# Input : errors.xlsx  (trial-level; 60 participants x 60 trials x 3 conditions)
# Output: results_R3/*.csv and session_info.txt
#
# Conventions
#   - Accuracy = proportion correct over all trials of a condition.
#   - RT = correct trials only, anticipations (< 200 ms) and responses at the
#     4.000-s recording ceiling removed.
#   - Congruent trials = rot_type == "correct" (the second figure matched the
#     instructed outcome). The RT validation uses congruent trials only.
#   - Gender is between-subjects; Condition is within-subjects.
#   - Sum-to-zero contrasts (Type-III tests index main effects).
#   - Multiple comparisons are corrected by family (see section 6).
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(effectsize)
})

options(contrasts = c("contr.sum", "contr.poly"))
set.seed(2026)
dir.create("results_R3", showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 0. Settings
# -----------------------------------------------------------------------------
DATA_FILE <- "errors.xlsx"  # trial-level file (3600 rows = 60 participants x 60 trials)

# Excluded participants: chance-level performance, as stated in the Methods.
# The sensitivity analysis with the previous exclusions (20 and 48) is in
# sensitivity_excl20-48.R.
EXCLUDE_IDS <- c(33, 50)

ANTICIPATION_S   <- 0.200
RT_CEILING_S     <- 4.000
TRIALS_PER_BLOCK <- 20

# -----------------------------------------------------------------------------
# 1. Read and reshape to long format (one row per trial)
# -----------------------------------------------------------------------------
raw <- read_excel(DATA_FILE)

trials <- raw %>%
  group_by(id) %>%
  mutate(trial = row_number()) %>%
  ungroup()

# The object-based RT column is named "allor_rt" in the source file.
piece <- function(cond, corr, rt, rot, angle_abs) {
  trials %>%
    transmute(
      id, gender, trial,
      cond    = cond,
      correct = as.numeric(.data[[corr]]),
      rt      = as.numeric(.data[[rt]]),
      rot     = .data[[rot]],
      angle   = as.numeric(.data[[angle_abs]])
    )
}

long <- bind_rows(
  piece("Control",  "ctrl_corr_ans", "ctrl_rt",  "ctrl_rot_type", "ctrl_angle_abs"),
  piece("Object",   "allo_corr_ans", "allor_rt", "allo_type",     "allo_angle_abs"),
  piece("Effector", "ego_corr_ans",  "ego_rt",   "ego_rot_type",  "ego_angle_abs")
)

# Object-based trials with a signed-angle magnitude of 80 deg are not one of the
# ten designed disparities (30, 60, 90, 120, 150). They are recoded to 60 deg
# (see README, "Data cleaning"). The number of recoded trials is printed.
n_recoded <- sum(long$cond == "Object" & long$angle == 80)
message("Recoded ", n_recoded, " object-based trials from 80 to 60 deg.")
long <- long %>%
  mutate(angle = if_else(cond == "Object" & angle == 80, 60, angle))

# Exclusions and factor coding
long <- long %>%
  filter(!id %in% EXCLUDE_IDS) %>%
  mutate(
    id     = factor(id),
    gender = factor(gender, levels = c("F", "M")),
    cond   = factor(cond, levels = c("Control", "Object", "Effector"))
  ) %>%
  group_by(id, cond) %>%
  mutate(block = ceiling(row_number() / TRIALS_PER_BLOCK)) %>%
  ungroup()

cat("Participants analysed:", nlevels(long$id), "\n")
cat("Per gender:", paste(names(table(distinct(long, id, gender)$gender)),
                         table(distinct(long, id, gender)$gender), collapse = ", "), "\n")

# Reaction-time subset: correct trials, RT within [0.2 s, 4.0 s)
rt_dat <- long %>%
  filter(correct == 1, !is.na(rt), rt >= ANTICIPATION_S, rt < RT_CEILING_S)

cat("RT trials removed (anticipations/ceiling/no response, correct only):",
    sum(long$correct == 1) - nrow(rt_dat), "\n")

# -----------------------------------------------------------------------------
# 2. Participant-level means (one observation per participant and condition)
# -----------------------------------------------------------------------------
acc_p <- long %>%
  group_by(id, gender, cond) %>%
  summarise(acc = mean(correct), .groups = "drop")

rt_p <- rt_dat %>%
  group_by(id, gender, cond) %>%
  summarise(rt = mean(rt), .groups = "drop")

desc <- acc_p %>%
  group_by(cond, gender) %>%
  summarise(M_acc = mean(acc), SD_acc = sd(acc), .groups = "drop") %>%
  left_join(rt_p %>% group_by(cond, gender) %>%
              summarise(M_rt = mean(rt), SD_rt = sd(rt), .groups = "drop"),
            by = c("cond", "gender"))
write.csv(desc, "results_R3/descriptives.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 3. Primary analysis (Aim 2): Gender x Condition on participant means
#    Equivalent to a 2 x 3 mixed ANOVA: F(1,56) for Gender,
#    F(2,112) for Condition and Gender x Condition.
# -----------------------------------------------------------------------------
m_acc <- lmer(acc ~ gender * cond + (1 | id), data = acc_p)
anova_acc <- as.data.frame(anova(m_acc))
anova_acc$term <- rownames(anova_acc)
write.csv(anova_acc, "results_R3/primary_accuracy_anova.csv", row.names = FALSE)
print(anova_acc)

m_rt <- lmer(rt ~ gender * cond + (1 | id), data = rt_p)
anova_rt <- as.data.frame(anova(m_rt))
anova_rt$term <- rownames(anova_rt)
write.csv(anova_rt, "results_R3/primary_rt_anova.csv", row.names = FALSE)
print(anova_rt)

# Partial eta-squared for every term
eta_acc <- eta_squared(m_acc, partial = TRUE)
eta_rt  <- eta_squared(m_rt,  partial = TRUE)
write.csv(as.data.frame(eta_acc), "results_R3/eta_accuracy.csv", row.names = FALSE)
write.csv(as.data.frame(eta_rt),  "results_R3/eta_rt.csv",       row.names = FALSE)

# -----------------------------------------------------------------------------
# 4. Gender gap per condition (family F1), corrected across the 3 conditions
# -----------------------------------------------------------------------------
gap_one <- function(df, var) {
  m <- df[[var]][df$gender == "M"]
  f <- df[[var]][df$gender == "F"]
  tt <- t.test(m, f, var.equal = TRUE)
  sp <- sqrt((var(m) + var(f)) / 2)
  tibble(
    gap = mean(m) - mean(f),
    t = unname(tt$statistic), df = unname(tt$parameter), p = tt$p.value,
    d = (mean(m) - mean(f)) / sp
  )
}

gap_tab <- acc_p %>%
  group_split(cond) %>%
  map_dfr(~ gap_one(.x, "acc") %>% mutate(cond = as.character(.x$cond[1]))) %>%
  mutate(
    p_holm = p.adjust(p, method = "holm"),
    p_bonf = p.adjust(p, method = "bonferroni")
  ) %>%
  select(cond, gap, t, df, p, p_holm, p_bonf, d)
write.csv(gap_tab, "results_R3/gender_gap_by_condition.csv", row.names = FALSE)
print(gap_tab)

# -----------------------------------------------------------------------------
# 5. Hypothesis-derived and exploratory contrasts of the gender gap
#    Welch t-tests on participant-level difference scores.
#    EO: effector - object (a priori); EC: effector - control (derived);
#    OC: object - control (exploratory).
# -----------------------------------------------------------------------------
wide <- acc_p %>%
  select(id, gender, cond, acc) %>%
  pivot_wider(names_from = cond, values_from = acc) %>%
  mutate(
    EC = Effector - Control,
    EO = Effector - Object,
    OC = Object   - Control
  )

contrast_one <- function(score) {
  m <- wide[[score]][wide$gender == "M"]
  f <- wide[[score]][wide$gender == "F"]
  tt <- t.test(m, f, var.equal = FALSE)
  sp <- sqrt(((length(m) - 1) * var(m) + (length(f) - 1) * var(f)) /
               (length(m) + length(f) - 2))
  tibble(
    contrast = score,
    diff = mean(m) - mean(f),
    ci_lo = tt$conf.int[1], ci_hi = tt$conf.int[2],
    t = unname(tt$statistic), df = unname(tt$parameter), p = tt$p.value,
    d = (mean(m) - mean(f)) / sp
  )
}

contr_tab <- map_dfr(c("EO", "EC", "OC"), contrast_one)
# Holm across the two hypothesis-derived contrasts (EO, EC).
# OC is exploratory and reported uncorrected.
contr_tab$p_holm2 <- NA_real_
contr_tab$p_holm2[contr_tab$contrast %in% c("EO", "EC")] <-
  p.adjust(contr_tab$p[contr_tab$contrast %in% c("EO", "EC")], method = "holm")
write.csv(contr_tab, "results_R3/contrasts.csv", row.names = FALSE)
print(contr_tab)

# -----------------------------------------------------------------------------
# 6. Correction families (as stated in the Methods)
#    F1: gender gap in the three conditions           -> Bonferroni (section 4)
#    F2: the two hypothesis-derived contrasts (EO, EC) -> Holm       (section 5)
#    F3: quadratic covariate tests, testosterone and 2D:4D, both
#        dependent variables, both genders (8 tests)  -> Bonferroni
#        NOT computed in this script; see README, "Not included".
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 7. Sensitivity (smallest detectable effects and power), closed form
# -----------------------------------------------------------------------------
mde_80   <- power.t.test(n = 29, sd = 1, sig.level = .05, power = .80)$delta
n_d037   <- power.t.test(delta = .37, sd = 1, sig.level = .05, power = .80)$n
power_d037 <- power.t.test(n = 29, delta = .37, sd = 1, sig.level = .05)$power
power_d066 <- power.t.test(n = 29, delta = .66, sd = 1, sig.level = .05)$power
n_d066   <- power.t.test(delta = .66, sd = 1, sig.level = .05, power = .80)$n
sens <- tibble(
  quantity = c("MDE d (n=29/group, 80%)", "n/group for d=0.37 (80%)", "power d=0.37 (n=29)",
               "n/group for d=0.66 (80%)", "power d=0.66 (n=29)"),
  value = c(mde_80, n_d037, power_d037, n_d066, power_d066)
)
write.csv(sens, "results_R3/sensitivity.csv", row.names = FALSE)
print(sens)

# -----------------------------------------------------------------------------
# 8. Aim 1 (validation): angular disparity, congruent trials
# -----------------------------------------------------------------------------
val_rt <- rt_dat %>%
  filter(rot == "correct") %>%
  mutate(angle_c = angle - mean(angle))

m_val_rt <- lmer(rt ~ angle_c * gender * cond + (1 + angle_c | id), data = val_rt)
write.csv(as.data.frame(anova(m_val_rt)) %>% tibble::rownames_to_column("term"),
          "results_R3/validation_rt_anova.csv", row.names = FALSE)

# Participant-level RT slopes (ms per degree), one-sample test
slopes <- val_rt %>%
  group_by(id, gender) %>%
  summarise(slope = coef(lm(rt ~ angle))[["angle"]] * 1000, .groups = "drop")
tt_slope <- t.test(slopes$slope)
write.csv(tibble(mean_slope_ms_per_deg = mean(slopes$slope),
                 t = unname(tt_slope$statistic), df = unname(tt_slope$parameter),
                 p = tt_slope$p.value, dz = mean(slopes$slope) / sd(slopes$slope),
                 n_positive = sum(slopes$slope > 0)),
          "results_R3/validation_rt_slope.csv", row.names = FALSE)

# Accuracy vs angle (all trials), binomial GLMM
acc_val <- long %>% mutate(angle_c = angle - mean(angle))
m_val_acc <- glmer(correct ~ angle_c * gender * cond + (1 + angle_c | id),
                   data = acc_val, family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))
write.csv(as.data.frame(summary(m_val_acc)$coefficients) %>%
            tibble::rownames_to_column("term"),
          "results_R3/validation_acc_glmm.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 9. Secondary (exploratory): Block as a within-subject factor
#     Condition order is fixed (control first), so this is descriptive.
#     NOTE: the random structure below is (1 | id/cond). The manuscript
#     (Section 2.5) describes (1 | Sub). See README, "Known issues".
# -----------------------------------------------------------------------------
acc_b <- long %>%
  group_by(id, gender, cond, block) %>%
  summarise(acc = mean(correct), .groups = "drop")
m_block <- lmer(acc ~ gender * cond * block + (1 | id/cond), data = acc_b)
write.csv(as.data.frame(anova(m_block)) %>% tibble::rownames_to_column("term"),
          "results_R3/secondary_block_anova.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 10. Session log
# -----------------------------------------------------------------------------
writeLines(capture.output(sessionInfo()), "results_R3/session_info.txt")
cat("Done. Outputs in results_R3/\n")
