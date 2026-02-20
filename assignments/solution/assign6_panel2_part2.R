# setwd("~/Documents/AQM2")
options(stringsAsFactors = FALSE)

# ============================================================
# Assignment 6 -- Part 2: Staggered DiD with the did Package (Panel Data II)
# Applied Quantitative Methods for the Social Sciences II
# ============================================================

# List of packages
library(did)
library(dplyr)
library(ggplot2)
library(fixest)

# Load data
data(mpdta)

# --------
## 1. Data structure and visualization

## a)
length(unique(mpdta$countyreal))
table(mpdta$first.treat)
# 500 counties, years 2003-2007.
# first.treat takes values 0 (never treated), 2004, 2006, 2007 -- four cohorts.
# Staggered treatment adoption: different counties adopted a minimum wage policy
# in different years. Simply comparing treated vs. untreated ignores this timing
# variation; counties adopting earlier may differ from later adopters, and
# treatment effects may accumulate or fade, so a single treatment dummy is
# insufficient.

## b)
mpdta_avg = mpdta %>%
  mutate(cohort = factor(first.treat,
    levels = c(0, 2004, 2006, 2007),
    labels = c("Never treated", "Adopted 2004",
               "Adopted 2006", "Adopted 2007"))) %>%
  group_by(year, cohort) %>%
  summarise(mean_lemp = mean(lemp, na.rm = TRUE))

ggplot(mpdta_avg, aes(x = year, y = mean_lemp, color = cohort)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Year", y = "Log teen employment", color = "Treatment cohort")

ggsave("mpdta_cohort_trends.pdf", width = 7, height = 4)
# Pre-treatment trends look roughly parallel across cohorts -- encouraging for
# parallel trends. After treatment, counties that adopted the minimum wage
# (especially the 2004 cohort) show lower teen employment relative to
# never-treated counties, consistent with a negative employment effect.

# --------
## 2. Naive TWFE vs. Callaway-Sant'Anna estimator

## a)
# Create a time-varying treatment indicator (treat is time-invariant -- collinear
# with unit FEs; treated_post = 1 only when the county is in its post period)
mpdta = mpdta %>%
  mutate(treated_post = as.integer(first.treat > 0 & year >= first.treat))

m_twfe = feols(lemp ~ treated_post | countyreal + year,
               data = mpdta, cluster = ~countyreal)
summary(m_twfe)
# The coefficient on treated_post is the TWFE estimate pooling all cohorts.
# Implicit assumption: treatment effect is constant across cohorts and over time.
# If effects are heterogeneous, TWFE can be biased -- it assigns negative weights
# to some group-time comparisons (forbidden comparisons using already-treated
# units as controls).

## b)
cs_out = att_gt(
  yname         = "lemp",
  gname         = "first.treat",
  idname        = "countyreal",
  tname         = "year",
  xformla       = ~ lpop,
  data          = mpdta,
  control_group = "nevertreated")

aggte(cs_out, type = "simple")
# CS overall ATT uses only clean comparisons: each cohort vs. never-treated
# counties. Compare to naive TWFE -- may differ if treatment effects are
# heterogeneous across cohorts or over time.

## c)
cs_dyn = aggte(cs_out, type = "dynamic")
ggdid(cs_dyn)

ggsave("mpdta_event_study.pdf", width = 7, height = 4)
# Pre-treatment estimates (leads, negative periods) should be close to zero
# and statistically indistinguishable from zero -- supporting parallel trends.
# Post-treatment estimates (lags) show the dynamic effect. A growing negative
# effect would indicate progressive reduction in teen employment after adoption.

# --------
## 3. Pre-testing the parallel trends assumption

## a)
cs_out_bt = att_gt(
  yname         = "lemp",
  gname         = "first.treat",
  idname        = "countyreal",
  tname         = "year",
  xformla       = ~ lpop,
  data          = mpdta,
  control_group = "nevertreated",
  bstrap        = TRUE,
  cband         = TRUE)

summary(cs_out_bt)
# The summary includes a p-value for the joint pre-test of parallel trends.
# Null hypothesis: all pre-treatment ATT(g,t) are jointly equal to zero.
# A large p-value (e.g. > 0.05) means we fail to reject -- consistent with PT.
# In mpdta the p-value is typically well above 0.05.

## b)
ggdid(cs_out_bt)
ggsave("mpdta_att_gt.pdf", width = 10, height = 6)
# Each panel = one treatment cohort. Negative event-time = pre-treatment.
# Pre-treatment estimates should scatter around zero with CIs including zero.
# This is more granular than the aggregated event study: it shows cohort-specific
# pre-trends rather than a single pooled pre-trend.

## c)
# Limitation of pre-testing: failure to reject parallel trends in the pre-period
# does not guarantee the assumption holds post-treatment. The pre-test only
# examines observable pre-treatment trajectories; divergence that begins exactly
# at treatment (due to confounders or anticipation) would go undetected.
# The pre-test is a necessary but not sufficient diagnostic for credible DiD.

# --------
## 4. Comparing control group specifications

## a)
cs_out_nyt = att_gt(
  yname         = "lemp",
  gname         = "first.treat",
  idname        = "countyreal",
  tname         = "year",
  xformla       = ~ lpop,
  data          = mpdta,
  control_group = "notyettreated")

aggte(cs_out_nyt, type = "simple")
# Uses not-yet-treated counties as controls in addition to never-treated.
# Compare to never-treated ATT from Section 2b -- typically close in mpdta,
# suggesting control group choice does not dramatically alter conclusions here.

## b)
cs_dyn_nyt = aggte(cs_out_nyt, type = "dynamic")
ggdid(cs_dyn_nyt)
ggsave("mpdta_event_study_nyt.pdf", width = 7, height = 4)
# Pre-trends and post-treatment patterns should look broadly similar to the
# never-treated event study. Minor differences arise because not-yet-treated
# counties may already be adjusting in anticipation of their own future treatment,
# potentially contaminating the comparison.

## c)
# Trade-off: never-treated is more conservative (no anticipation contamination)
# but leaves fewer control observations if most units eventually get treated.
# Not-yet-treated offers more power and is preferred when never-treated is small
# or unrepresentative, but requires the additional assumption of no anticipation
# effects. With a reasonably large never-treated group in mpdta, either is
# defensible and estimates are close.

# --------
## 5. Discussion: why does TWFE fail in staggered settings?

## a)
# In staggered DiD, naive TWFE uses already-treated units as implicit controls
# for later-treated units -- the "forbidden comparison." If a county that adopted
# in 2004 is used as a control for a county adopting in 2006, its outcome in 2006
# already reflects its own (potentially growing) treatment effect. When treatment
# effects are heterogeneous across cohorts or over time, these forbidden
# comparisons contaminate the TWFE estimate, assigning negative weights to some
# group-time pairs. The result may not correspond to any meaningful average
# treatment effect and can even have the wrong sign.

## b)
# Compare TWFE coefficient (2.2a) to CS overall ATT (2.2b). If they differ,
# treatment effect heterogeneity is distorting TWFE. Given the event-study
# pre-trends (2.2c) and the formal pre-test p-value (3a): if pre-treatment
# estimates are near zero and the joint test fails to reject, parallel trends
# holds and CS is more credible -- it uses only valid never-treated comparisons
# and allows cohort-specific effects. TWFE should be treated with skepticism
# in staggered settings with heterogeneous timing.
