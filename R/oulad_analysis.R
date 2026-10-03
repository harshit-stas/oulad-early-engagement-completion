# =============================================================================
# Predictors of Course Completion in Online Learning: OULAD analysis
# Author: Harshit Raman
#
# Data: Open University Learning Analytics Dataset (Kuzilek, Hlosta & Zdrahal,
# 2017, Scientific Data 4:170171), CC BY 4.0.
#   Option A: download the CSVs from https://analyse.kmi.open.ac.uk/open_dataset
#             and unzip into ./OULAD/ (needs studentInfo.csv, studentRegistration.csv,
#             courses.csv, studentVle.csv)
#   Option B: the authors' R package: remotes::install_github("jakubkuzilek/oulad")
#
# Design
#   Sample A  = all enrollments that were still registered on day 0 (module start)
#               -> descriptives and timing of withdrawal (Kaplan-Meier from day 0)
#   Sample B  = enrollments still registered on day 14 (landmark)
#               -> multilevel logistic regression, landmark Cox model, random forest
#   Using a day-14 landmark means early-engagement predictors are measured before
#   the outcome period starts, avoiding immortal-time bias and outcome leakage.
# =============================================================================

pkgs <- c("dplyr", "tidyr", "readr", "ggplot2", "lme4", "car", "survival",
          "ranger", "pROC", "fastshap", "scales")
miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) install.packages(miss)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(lme4)
  library(survival); library(ranger); library(pROC)
})
set.seed(2026)
DATA_DIR <- "OULAD"
OUT_DIR  <- "oulad_outputs"
LANDMARK <- 14   # days
dir.create(OUT_DIR, showWarnings = FALSE)
LOG <- character()
say <- function(...) { l <- paste0(...); cat(l, "\n"); LOG <<- c(LOG, l) }

# ---- 1. Load -----------------------------------------------------------------
if (file.exists(file.path(DATA_DIR, "studentInfo.csv"))) {
  rd <- function(f) read.csv(file.path(DATA_DIR, f), na.strings = c("", "?", "NA"))
  student <- rd("studentInfo.csv"); student_registration <- rd("studentRegistration.csv")
  course <- rd("courses.csv"); student_vle <- rd("studentVle.csv")
} else {
  library(oulad)
  data(student, student_registration, course, student_vle, package = "oulad")
}
student$imd_band[student$imd_band %in% c("", "?")] <- NA
student$imd_band[student$imd_band == "10-20"] <- "10-20%"   # label typo in source
student$disability <- as.character(student$disability) %in% c("Y", "TRUE")

d <- student %>%
  inner_join(student_registration, by = c("code_module", "code_presentation", "id_student")) %>%
  inner_join(course, by = c("code_module", "code_presentation")) %>%
  mutate(mp = paste(code_module, code_presentation, sep = "_"))
say("=== SAMPLE ===")
say("Enrollments in OULAD: ", nrow(d), " | students: ", n_distinct(d$id_student),
    " | modules: ", n_distinct(d$code_module), " | module-presentations: ", n_distinct(d$mp))
say("Final results (all): ", paste(names(table(d$final_result)), table(d$final_result), collapse = "; "))

# ---- 2. Derived variables -----------------------------------------------------
early <- student_vle %>%
  group_by(code_module, code_presentation, id_student) %>%
  summarise(pre_clicks   = sum(sum_click[date < 0]),
            early_clicks = sum(sum_click[date >= 0 & date < LANDMARK]),
            early_days   = n_distinct(date[date >= 0 & date < LANDMARK]),
            .groups = "drop")
d <- d %>% left_join(early, by = c("code_module", "code_presentation", "id_student")) %>%
  mutate(across(c(pre_clicks, early_clicks, early_days), ~ coalesce(.x, 0L)))

d <- d %>% mutate(
  completed   = as.integer(final_result %in% c("Pass", "Distinction")),
  withdrawn   = as.integer(final_result == "Withdrawn"),
  domain      = factor(ifelse(code_module %in% c("CCC", "DDD", "EEE", "FFF"), "STEM", "Social sciences"),
                       levels = c("Social sciences", "STEM")),
  semester    = factor(ifelse(substr(code_presentation, 5, 5) == "B", "February start", "October start"),
                       levels = c("October start", "February start")),
  gender      = factor(ifelse(gender == "F", "Female", "Male"), levels = c("Male", "Female")),
  age         = factor(age_band, levels = c("0-35", "35-55", "55<=")),
  education   = factor(case_when(
                  highest_education %in% c("No Formal quals", "Lower Than A Level") ~ "Below A level",
                  highest_education == "A Level or Equivalent" ~ "A level or equivalent",
                  TRUE ~ "HE qualification or higher"),
                  levels = c("A level or equivalent", "Below A level", "HE qualification or higher")),
  imd_low     = as.numeric(sub("-.*", "", imd_band)),
  deprivation = factor(case_when(imd_low < 30 ~ "Most deprived (0-30%)",
                                 imd_low < 70 ~ "Middle (30-70%)",
                                 !is.na(imd_low) ~ "Least deprived (70-100%)"),
                       levels = c("Least deprived (70-100%)", "Middle (30-70%)", "Most deprived (0-30%)")),
  disability  = factor(ifelse(disability, "Yes", "No"), levels = c("No", "Yes")),
  prev_attempt = factor(ifelse(num_of_prev_attempts > 0, "Yes", "No"), levels = c("No", "Yes")),
  reg_lead    = -date_registration          # days registered before module start
)

# Sample A: still registered at day 0 (exclude withdrawals with no date)
A <- d %>% filter(!(withdrawn == 1 & is.na(date_unregistration)),
                  is.na(date_unregistration) | date_unregistration > 0)
say("Excluded: unregistered on/before day 0 = ", sum(d$date_unregistration <= 0, na.rm = TRUE),
    "; withdrawn with no date = ", sum(d$withdrawn == 1 & is.na(d$date_unregistration)))
say("[Sample A] started the module: ", nrow(A))
B <- A %>% filter(is.na(date_unregistration) | date_unregistration > LANDMARK,
                  !is.na(reg_lead), !is.na(deprivation))
say("[Sample B] still registered at day ", LANDMARK, " with complete covariates: ", nrow(B),
    " (students: ", n_distinct(B$id_student), ")")

# ---- 3. Descriptives (Sample A and B) ----------------------------------------
say("\n=== DESCRIPTIVES ===")
say("Sample A: completion ", round(100 * mean(A$completed), 1), "%, withdrawal ",
    round(100 * mean(A$withdrawn), 1), "%, fail ", round(100 * mean(A$final_result == "Fail"), 1), "%")
say("Sample B: completion ", round(100 * mean(B$completed), 1), "%, withdrawal ",
    round(100 * mean(B$withdrawn), 1), "%")
mpr <- A %>% group_by(mp) %>% summarise(p = 100 * mean(completed))
say("Completion range across module-presentations (A): ", round(min(mpr$p), 1), "% (",
    mpr$mp[which.min(mpr$p)], ") to ", round(max(mpr$p), 1), "% (", mpr$mp[which.max(mpr$p)], ")")

desc_vars <- c("gender", "age", "education", "deprivation", "disability", "prev_attempt",
               "domain", "semester")
tab2 <- bind_rows(lapply(desc_vars, function(v) {
  s <- B[!is.na(B[[v]]), ]
  p <- chisq.test(table(s[[v]], s$completed))$p.value
  s %>% group_by(category = as.character(.data[[v]])) %>%
    summarise(n = n(), pct_of_sample = NA_real_, completed_pct = round(100 * mean(completed), 1),
              .groups = "drop") %>%
    mutate(pct_of_sample = round(100 * n / nrow(s), 1), variable = v,
           chisq_p = c(signif(p, 3), rep(NA, n() - 1)), .before = 1)
}))
tab2 <- bind_rows(tab2, tibble(variable = "Total", category = "", n = nrow(B), pct_of_sample = 100,
                               completed_pct = round(100 * mean(B$completed), 1)))
write.csv(tab2, file.path(OUT_DIR, "table2_completion_by_group.csv"), row.names = FALSE)
print(as.data.frame(tab2))

cont <- c("reg_lead", "studied_credits", "pre_clicks", "early_clicks", "early_days")
mw <- bind_rows(lapply(cont, function(v) tibble(
  variable = v, median_completed = median(B[[v]][B$completed == 1]),
  median_not = median(B[[v]][B$completed == 0]),
  p = signif(wilcox.test(B[[v]] ~ B$completed)$p.value, 3))))
write.csv(mw, file.path(OUT_DIR, "continuous_by_outcome.csv"), row.names = FALSE)
print(as.data.frame(mw))
for (i in seq_len(nrow(mw))) say("Median ", mw$variable[i], ": completers ", mw$median_completed[i],
                                 " vs non-completers ", mw$median_not[i])

# ---- 4. Multilevel logistic regression (Sample B) ----------------------------
say("\n=== MULTILEVEL LOGISTIC REGRESSION (Sample B) ===")
zs <- function(x) as.numeric(scale(x))
B <- B %>% mutate(credits_z = zs(log(studied_credits)), reg_lead_z = zs(reg_lead),
                  pre_clicks_z = zs(log1p(pre_clicks)), early_clicks_z = zs(log1p(early_clicks)),
                  early_days_z = zs(early_days))
say("Correlation log early clicks vs early active days: ",
    round(cor(B$early_clicks_z, B$early_days_z), 2))

demo  <- c("gender", "age", "education", "deprivation", "disability", "prev_attempt", "credits_z")
engag <- c("reg_lead_z", "pre_clicks_z", "early_clicks_z", "early_days_z")
crs   <- c("domain", "semester")
re    <- "(1 | mp) + (1 | region)"
ctrl  <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
fit <- function(x) glmer(reformulate(c(x, re), "completed"), data = B, family = binomial, control = ctrl)
M0 <- fit("1"); M1 <- fit(demo); M2 <- fit(c(demo, engag)); M3 <- fit(c(demo, engag, crs))

vc <- function(m) { v <- as.data.frame(VarCorr(m)); setNames(v$vcov, v$grp) }
for (nm in c("M0", "M1", "M2", "M3")) {
  m <- get(nm); v <- vc(m); tot <- sum(v) + pi^2 / 3
  say(nm, ": var(module-pres) = ", round(v["mp"], 3), ", var(region) = ", round(v["region"], 3),
      " | ICC module-pres = ", round(v["mp"] / tot, 3), ", ICC region = ", round(v["region"] / tot, 3),
      " | AIC = ", round(AIC(m), 1), if (isSingular(m)) " (singular)" else "")
}
lrt <- anova(M0, M1, M2, M3); print(lrt)
write.csv(as.data.frame(lrt), file.path(OUT_DIR, "model_comparison_LRT.csv"))

ort <- function(m, lab) {
  co <- summary(m)$coefficients; se <- co[, 2]
  tibble(term = rownames(co),
         !!paste0(lab, "_OR") := round(exp(co[, 1]), 2),
         !!paste0(lab, "_CI") := paste0(sprintf("%.2f", exp(co[, 1] - 1.96 * se)), "-",
                                        sprintf("%.2f", exp(co[, 1] + 1.96 * se))),
         !!paste0(lab, "_p")  := signif(co[, 4], 3))
}
tab3 <- ort(M1, "M1") %>% full_join(ort(M2, "M2"), by = "term") %>% full_join(ort(M3, "M3"), by = "term")
write.csv(tab3, file.path(OUT_DIR, "table3_multilevel_odds_ratios.csv"), row.names = FALSE)
print(as.data.frame(tab3))
say("Full model (M3) odds ratios:")
for (i in which(tab3$term != "(Intercept)"))
  say("  ", tab3$term[i], ": OR ", sprintf("%.2f", tab3$M3_OR[i]), " (", tab3$M3_CI[i], "), p = ", tab3$M3_p[i])

vm <- glm(reformulate(c(demo, engag, crs), "completed"), data = B, family = binomial)
v <- car::vif(vm); v <- if (is.matrix(v)) v[, 3]^2 else v
write.csv(data.frame(VIF = round(v, 2)), file.path(OUT_DIR, "vif.csv"))
say("Max VIF (GVIF^(1/2df) squared): ", round(max(v), 2), " [", names(which.max(v)), "]")

# ---- 5. Timing of withdrawal (Sample A, from day 0) --------------------------
say("\n=== TIMING OF WITHDRAWAL (Sample A) ===")
A <- A %>% mutate(time = ifelse(withdrawn == 1, date_unregistration, module_presentation_length),
                  time = pmin(time, module_presentation_length))
wd <- A$time[A$withdrawn == 1]
say("Withdrawals after start: ", length(wd), "; median day of withdrawal = ", median(wd),
    "; within first 28 days = ", round(100 * mean(wd <= 28), 1), "%; within first 14 days = ",
    round(100 * mean(wd <= 14), 1), "%; within first 90 days = ", round(100 * mean(wd <= 90), 1), "%")
km0 <- survfit(Surv(time, withdrawn) ~ 1, data = A)
s0 <- summary(km0, times = c(14, 28, 90, 180))
say("KM cumulative withdrawal at day 14/28/90/180: ",
    paste(sprintf("%.1f%%", 100 * (1 - s0$surv)), collapse = " / "))
wk <- A %>% filter(withdrawn == 1) %>% mutate(week = ceiling(time / 7)) %>% count(week)
write.csv(wk, file.path(OUT_DIR, "withdrawals_by_week.csv"), row.names = FALSE)
lr0 <- survdiff(Surv(time, withdrawn) ~ domain, data = A)
say("Log-rank by domain (day 0): chi-sq = ", round(lr0$chisq, 1), ", p = ",
    signif(pchisq(lr0$chisq, 1, lower.tail = FALSE), 3))

# ---- 6. Landmark Cox model (Sample B, from day 14) ---------------------------
say("\n=== LANDMARK COX MODEL (Sample B, time from day ", LANDMARK, ") ===")
B <- B %>% mutate(ltime = pmin(ifelse(withdrawn == 1, date_unregistration, module_presentation_length),
                               module_presentation_length) - LANDMARK,
                  ltime = pmax(ltime, 0.5),
                  eng_q = cut(early_clicks, quantile(early_clicks, 0:4 / 4), include.lowest = TRUE,
                              labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")))
kmq <- survfit(Surv(ltime, withdrawn) ~ eng_q, data = B)
lrq <- survdiff(Surv(ltime, withdrawn) ~ eng_q, data = B)
say("Log-rank by early-click quartile: chi-sq = ", round(lrq$chisq, 1), ", df = 3, p = ",
    signif(pchisq(lrq$chisq, 3, lower.tail = FALSE), 3))
end_wd <- B %>% group_by(eng_q) %>% summarise(w = round(100 * mean(withdrawn), 1))
say("Withdrawal by early-click quartile: ", paste(end_wd$eng_q, end_wd$w, "%", collapse = "; "))

kdf <- data.frame(time = kmq$time + LANDMARK, surv = kmq$surv,
                  grp = rep(sub(".*=", "", names(kmq$strata)), kmq$strata))
p1 <- ggplot(kdf, aes(time, 1 - surv, colour = grp)) + geom_step(linewidth = 0.8) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
  scale_colour_manual(values = c("#C0392B", "#E67E22", "#2E86C1", "#1B4F72")) +
  labs(x = "Day of module", y = "Cumulative withdrawal", colour = "Clicks in first 14 days",
       title = "Figure 1. Cumulative withdrawal by early engagement quartile (landmark day 14)") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(OUT_DIR, "figure1_withdrawal_by_engagement.png"), p1, width = 7.5, height = 5, dpi = 300)

cox <- coxph(as.formula(paste("Surv(ltime, withdrawn) ~", paste(c(demo, engag), collapse = " + "),
                              "+ strata(code_module) + cluster(mp)")), data = B)
cs <- summary(cox)$coefficients; ci <- summary(cox)$conf.int
tab4 <- tibble(term = rownames(cs), HR = round(cs[, "exp(coef)"], 2),
               CI = paste0(sprintf("%.2f", ci[, 3]), "-", sprintf("%.2f", ci[, 4])),
               robust_p = signif(cs[, ncol(cs)], 3))
write.csv(tab4, file.path(OUT_DIR, "table4_cox_hazard_ratios.csv"), row.names = FALSE)
print(as.data.frame(tab4))
say("Cox HRs (stratified by module, clustered SE):")
for (i in seq_len(nrow(tab4))) say("  ", tab4$term[i], ": HR ", sprintf("%.2f", tab4$HR[i]), " (", tab4$CI[i], "), p = ", tab4$robust_p[i])
zph <- cox.zph(cox)
write.csv(as.data.frame(zph$table), file.path(OUT_DIR, "cox_ph_test.csv"))
say("PH test global p = ", signif(zph$table["GLOBAL", "p"], 3),
    "; terms with p < .05: ", paste(rownames(zph$table)[zph$table[, "p"] < .05 & rownames(zph$table) != "GLOBAL"], collapse = ", "))

# ---- 7. Prediction: random forest vs logistic + SHAP -------------------------
say("\n=== PREDICTIVE CHECK ===")
X <- c("gender", "age", "education", "deprivation", "disability", "prev_attempt", "studied_credits",
       "reg_lead", "pre_clicks", "early_clicks", "early_days", "domain", "semester")
R <- B[, c("completed", X)]; R$completed <- factor(R$completed, 0:1, c("No", "Yes"))
idx <- sample(nrow(R), floor(.7 * nrow(R))); tr <- R[idx, ]; te <- R[-idx, ]
rf <- ranger(completed ~ ., data = tr, num.trees = 500, probability = TRUE, seed = 2026)
p_rf <- predict(rf, te)$predictions[, "Yes"]
lg <- glm(completed ~ ., data = tr, family = binomial)
p_lg <- predict(lg, te, type = "response")
r_rf <- roc(te$completed, p_rf, levels = c("No", "Yes"), direction = "<", quiet = TRUE)
r_lg <- roc(te$completed, p_lg, levels = c("No", "Yes"), direction = "<", quiet = TRUE)
fmt <- function(r) { c <- ci.auc(r); sprintf("%.3f (95%% CI %.3f-%.3f)", c[2], c[1], c[3]) }
say("Test-set AUC: random forest ", fmt(r_rf), "; logistic regression ", fmt(r_lg),
    "; DeLong p = ", signif(roc.test(r_rf, r_lg)$p.value, 3))

pw <- function(object, newdata) predict(object, newdata)$predictions[, "Yes"]
Xs <- as.data.frame(te[sample(nrow(te), 500), X])
sh <- fastshap::explain(rf, X = as.data.frame(tr[, X]), newdata = Xs, pred_wrapper = pw, nsim = 30)
imp <- sort(colMeans(abs(as.matrix(sh))), decreasing = TRUE)
sdf <- tibble(variable = names(imp), mean_abs_shap = round(imp, 4))
write.csv(sdf, file.path(OUT_DIR, "shap_importance.csv"), row.names = FALSE)
say("Mean |SHAP| ranking: ", paste0(sdf$variable, " (", sdf$mean_abs_shap, ")", collapse = ", "))
lab <- c(early_clicks = "Clicks, days 0-13", early_days = "Active days, days 0-13",
         pre_clicks = "Clicks before start", reg_lead = "Days registered before start",
         studied_credits = "Credits studied", education = "Prior education", deprivation = "Area deprivation",
         prev_attempt = "Previous attempt", domain = "Domain (STEM)", semester = "Start month",
         age = "Age band", gender = "Gender", disability = "Disability")
sdf$label <- lab[sdf$variable]
p2 <- ggplot(sdf, aes(reorder(label, mean_abs_shap), mean_abs_shap)) + geom_col(fill = "#2C6E91") +
  coord_flip() + labs(x = NULL, y = "Mean |SHAP| (change in predicted probability of completion)",
                      title = "Figure 2. Predictor importance in the random forest (SHAP)") +
  theme_minimal(base_size = 11)
ggsave(file.path(OUT_DIR, "figure2_shap.png"), p2, width = 7.5, height = 5, dpi = 300)

writeLines(c("OULAD RESULTS SUMMARY", paste("Generated:", Sys.time()), "", LOG),
           file.path(OUT_DIR, "results_summary.txt"))
writeLines(capture.output(sessionInfo()), file.path(OUT_DIR, "session_info.txt"))
cat("\nDone.\n")
