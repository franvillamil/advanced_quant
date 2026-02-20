# setwd("~/Documents/AQM2")
options(stringsAsFactors = FALSE)

# ============================================================
# Assignment 6: Panel Data II
# Applied Quantitative Methods for the Social Sciences II
# ============================================================

# List of packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(modelsummary)
library(did)

# ==========================================================================
# Part 1: Card-Krueger Minimum Wage (In-Class)
# ==========================================================================

df = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/minwage.csv")

# ----------------------------------------------------------
## 1. Data setup and exploration

# a)
df = df %>% mutate(NJ = ifelse(location != "PA", 1, 0))
table(df$NJ)

df %>%
  group_by(NJ) %>%
  summarise(
    mean_wage_before = mean(wageBefore, na.rm = TRUE),
    mean_wage_after  = mean(wageAfter,  na.rm = TRUE))

# b)
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

# c)
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

# ----------------------------------------------------------
## 2. DiD regression

# a)
m_did = feols(full_emp ~ post * NJ, data = df_long, cluster = ~id)
modelsummary(m_did, stars = TRUE, gof_map = c("nobs", "r.squared"))

# b)
m_did_fe = feols(full_emp ~ post * NJ | chain, data = df_long, cluster = ~id)
modelsummary(
  list("DiD" = m_did, "DiD + Chain FE" = m_did_fe),
  stars = TRUE, gof_map = c("nobs", "r.squared"))

# ----------------------------------------------------------
## 3. Wages as a validation check

# a)
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

# ==========================================================================
# Part 2: Staggered DiD with the did Package (Take-Home)
# ==========================================================================

data(mpdta)

# ----------------------------------------------------------
## 1. Data structure and visualization

# a)
length(unique(mpdta$countyreal))
table(mpdta$first.treat)

# b)
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

# ----------------------------------------------------------
## 2. Naive TWFE vs. Callaway-Santa'Anna estimator

# a)
m_twfe = feols(lemp ~ treat | countyreal + year,
               data = mpdta, cluster = ~countyreal)
summary(m_twfe)

# b)
cs_out = att_gt(
  yname         = "lemp",
  gname         = "first.treat",
  idname        = "countyreal",
  tname         = "year",
  xformla       = ~ lpop,
  data          = mpdta,
  control_group = "nevertreated")

aggte(cs_out, type = "simple")

# c)
cs_dyn = aggte(cs_out, type = "dynamic")
ggdid(cs_dyn)

ggsave("mpdta_event_study.pdf", width = 7, height = 4)
