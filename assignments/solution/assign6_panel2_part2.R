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
## 2. Naive TWFE vs. Callaway-Santa'Anna estimator

## a)
m_twfe = feols(lemp ~ treat | countyreal + year,
               data = mpdta, cluster = ~countyreal)
summary(m_twfe)
# The coefficient on treat is the TWFE estimate pooling all cohorts.
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
## 3. Discussion: why does TWFE fail in staggered settings?

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
# pre-trends (2.2c): if pre-treatment estimates are near zero, parallel trends
# holds and CS is more credible -- it uses only valid never-treated comparisons
# and allows cohort-specific effects. TWFE should be treated with skepticism
# in staggered settings with heterogeneous timing.
