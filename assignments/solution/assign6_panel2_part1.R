# setwd("~/Documents/AQM2")
options(stringsAsFactors = FALSE)

# ============================================================
# Assignment 6 -- Part 1: Card-Krueger Minimum Wage (Panel Data II)
# Applied Quantitative Methods for the Social Sciences II
# ============================================================

# List of packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(modelsummary)

# Load data
df = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/minwage.csv")

# --------
## 1. Data setup and exploration

## a)
df = df %>% mutate(NJ = ifelse(location != "PA", 1, 0))
table(df$NJ)
# NJ (coded 1) outnumbers PA (coded 0) because NJ spans four sub-regions
# (centralNJ, northNJ, shoreNJ, southNJ) while PA is a single region.

df %>%
  group_by(NJ) %>%
  summarise(
    mean_wage_before = mean(wageBefore, na.rm = TRUE),
    mean_wage_after  = mean(wageAfter,  na.rm = TRUE))
# Before the policy change, wages were nearly identical in NJ and PA.
# After the NJ minimum wage rose to $5.05, NJ wages increased noticeably
# while PA wages stayed flat -- confirming the policy raised wages in NJ.

## b)
means = df %>%
  group_by(NJ) %>%
  summarise(
    before = mean(fullBefore, na.rm = TRUE),
    after  = mean(fullAfter,  na.rm = TRUE),
    change = after - before)
means

nj_change = means$change[means$NJ == 1]
pa_change = means$change[means$NJ == 0]
did_est   = nj_change - pa_change
cat("DiD estimate:", round(did_est, 3), "\n")
# A positive DiD means full-time employment grew more (or fell less) in NJ
# than in PA after the minimum wage increase -- contrary to the standard
# prediction that higher minimum wages reduce employment.

## c)
df_long = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(fullBefore, fullAfter),
    names_to = "period",
    values_to = "full_emp") %>%
  mutate(
    post = ifelse(period == "fullAfter", 1, 0),
    NJ   = ifelse(location != "PA", 1, 0))

nrow(df_long)
nrow(df)
# Long format has exactly twice as many rows as the original.
# The DiD regression requires long format so that the post*NJ interaction
# (the DiD estimator) can be estimated across person-period observations.

# --------
## 2. DiD regression

## a)
m_did = feols(full_emp ~ post * NJ, data = df_long, cluster = ~id)
modelsummary(m_did, stars = TRUE, gof_map = c("nobs", "r.squared"))
# The post:NJ interaction coefficient is the DiD estimator and should match
# the manual calculation from 1b. post = PA pre-to-post change (counterfactual
# trend); NJ = baseline NJ-PA gap; post:NJ = additional change in NJ.

## b)
m_did_fe = feols(full_emp ~ post * NJ | chain, data = df_long, cluster = ~id)
modelsummary(
  list("DiD" = m_did, "DiD + Chain FE" = m_did_fe),
  stars = TRUE, gof_map = c("nobs", "r.squared"))
# Chain FEs absorb baseline staffing differences across fast-food chains
# (e.g., Wendy's vs KFC). Since chains are roughly balanced across states,
# this changes the DiD estimate only slightly.

## c)
# Parallel trends assumption: absent the NJ minimum wage increase, employment
# trends in NJ and PA fast-food restaurants would have been the same from
# February to November 1992. This is plausible given the short time window
# and similar economic environment. A concrete violation: if NJ experienced
# an independent economic shock (e.g., a major employer opening/closing plants)
# between the two survey waves, this would bias the DiD estimate.

# --------
## 3. Wages as a validation check

## a)
df_long_wage = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(wageBefore, wageAfter),
    names_to = "period",
    values_to = "wage") %>%
  mutate(
    post = ifelse(period == "wageAfter", 1, 0),
    NJ   = ifelse(location != "PA", 1, 0))

m_wage = feols(wage ~ post * NJ, data = df_long_wage, cluster = ~id)
modelsummary(m_wage, stars = TRUE, gof_map = c("nobs", "r.squared"))
# The post:NJ coefficient is positive and significant: wages rose substantially
# in NJ relative to PA, consistent with the $0.80 minimum wage increase.
# This is the expected sign and magnitude if the law was binding.

## b)
# The wage DiD is a "first stage" or manipulation check. If wages had NOT risen
# in NJ, we could not interpret the employment DiD as a minimum wage effect --
# the law might not have been binding. The positive wage result confirms the
# treatment actually occurred, so the employment DiD can be causally
# interpreted as a response to the minimum wage increase.
