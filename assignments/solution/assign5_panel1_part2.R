# setwd("~/Documents/AQM2")
options(stringsAsFactors = FALSE)

# ============================================================
# Assignment 5 -- Part 2: Teaching Evaluations (Panel Data I)
# Applied Quantitative Methods for the Social Sciences II
# ============================================================

# List of packages
library(readstata13)
library(ggplot2)
library(fixest)
library(plm)
library(modelsummary)

# Load data
df = read.dta13("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/teaching_evals/teaching_evals.dta")

# --------
## 1. Data exploration

# a) Panel dimensions
length(unique(df$InstrID))
length(unique(df$CourseID))
nrow(df) / length(unique(df$InstrID))
# Moderately long panel: each instructor appears in multiple course-year pairs.
# Panel index uses InstrID + CourseID (not Year) because instructors teach
# multiple courses in the same year.

# b) Scatter of evaluations vs grading generosity
ggplot(df, aes(x = Apct, y = Eval)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "Percent receiving A or A- (%)", y = "Average course evaluation (1-5)")
# Positive cross-sectional relationship: instructors who give more A grades
# tend to receive higher evaluations. Could reflect grading leniency OR
# that better teachers legitimately give more A grades AND get better reviews.

# --------
## 2. Pooled OLS baseline

# a) Pooled OLS
m1 = lm(Eval ~ Apct + Enrollment + Required, data = df)
summary(m1)
# Apct coefficient: positive association between grading generosity and
# evaluation scores, pooling all instructors and years.
# Larger enrollment and required courses are associated with lower evaluations.

# b) Expected bias in OLS: upward bias
# Unobserved instructor characteristics (talent, charisma) drive both grading
# generosity and evaluations. Examples:
# (1) Highly skilled instructors may give more deserved A grades and also
#     receive better evaluations independently of their grading.
# (2) Popular instructors may give higher grades to maintain popularity,
#     inflating both Apct and Eval.
# => OLS overstates the causal effect of grading leniency.

# --------
## 3. Fixed effects models

# a) Instructor FE and two-way FE
m_instr = feols(Eval ~ Apct + Enrollment + Required | InstrID, data = df)
m_twfe  = feols(Eval ~ Apct + Enrollment + Required | InstrID + Year, data = df)

# b) Three-model comparison with clustered SEs
modelsummary(
  list("Pooled OLS" = m1, "Instructor FE" = m_instr, "Two-Way FE" = m_twfe),
  vcov = ~InstrID,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))

# c) Instructor FE controls for all time-invariant instructor characteristics:
# baseline teaching quality, personality, subject area, grading culture, etc.
# If the FE coefficient on Apct is smaller than pooled OLS, the pooled
# estimate was inflated by unobserved instructor quality (better instructors
# give more A grades AND get better evaluations for other reasons).
# => Direction of OVB: upward (more lenient graders are unobservably better
#    evaluators, not because of leniency itself).

# --------
## 4. Random effects and Hausman test

# a) Random effects model
pdata = pdata.frame(df, index = c("InstrID", "CourseID"))
m_re = plm(Eval ~ Apct + Enrollment + Required,
           data = pdata, model = "random")
summary(m_re)

# b) Hausman test: FE vs RE
m_fe_plm = plm(Eval ~ Apct + Enrollment + Required,
               data = pdata, model = "within")
phtest(m_fe_plm, m_re)

# c) Null hypothesis of Hausman test: RE is consistent (unobserved instructor
# heterogeneity is uncorrelated with the regressors).
# If p < 0.05: reject null => FE is preferred.
# Substantively, instructor unobservables (quality, charisma) plausibly
# correlate with Apct and Eval, violating the RE assumption. Even without
# a significant test result, FE is the more defensible estimator here because
# it controls for all time-invariant instructor characteristics and directly
# addresses the endogeneity concern.
