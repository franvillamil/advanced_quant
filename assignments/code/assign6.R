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
m_did_fe = feols(full_emp ~ post * NJ | chain,
  data = df_long, cluster = ~id)
modelsummary(
  list("DiD" = m_did, "DiD + Chain FE" = m_did_fe),
  stars = TRUE, gof_map = c("nobs", "r.squared"))

m_did_feb = feols(full_emp ~ postNJ | id + post^chain,
  data = df_long, cluster = ~id)
modelsummary(
  list("DiD" = m_did, "DiD + Time*Chain FE" = m_did_feb),
  stars = TRUE, gof_map = c("nobs", "r.squared"))


##### SAME WHEN USING TWFE
df_long$postNJ = df_long$post * df_long$NJ
twfe1 = lm(full_emp ~ postNJ + factor(id) + factor(post),
  data = df_long)
twfe2 = feols(full_emp ~ postNJ | id + post, data = df_long)
modelsummary(
  list("twfe (lm)" = twfe1, "twfe (feols)" = twfe2),
  stars = TRUE, gof_map = c("nobs", "r.squared"))




########################################################
### --- THE PROBLEM WITH GROUP FE IN TWFE --- ###
########################################################

#### 
prop.table(table(df_long$chain, df_long$NJ), 1)
prop.table(table(df_long$chain, df_long$NJ), 2)

#### WHAT HAPPENS IF THERE IS A SHOCK TO BK? PTA violation?
#### 40% of stores in NJ are BK, and 80% of BK stores are in NJ

#### let's say that burger king had diff treatment trends, only in NJ
df_long2 = df_long %>%
  mutate(full_emp = ifelse(
    chain == "burgerking" & post == 1,
    full_emp*0.75, full_emp)) %>%
  rowwise() %>%
  mutate(postNJ = post * NJ)

m_did2 = feols(full_emp ~ postNJ | id + post, data = df_long2)
m_did_fe2a = feols(full_emp ~ postNJ | id + post + chain,
  data = df_long2)
m_did_fe2b = feols(full_emp ~ postNJ | id + post^chain,
  data = df_long2)
modelsummary(
  list(
    "TWFE" = m_did2,
    "TWFE + Chain FE" = m_did_fe2a,
    "TWFE with time*chain FE" = m_did_fe2b),
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



# ============================================================
# ============================================================
# ============================================================
### trying this for DiD and TWFE


# Parameters
set.seed(42)
n_states <- 10
counties_per_state <- 5
true_effect <- 5
treat_year <- 2006

# Build panel skeleton
df <- expand.grid(
    state  = 1:n_states,
    county = 1:counties_per_state,
    year   = 2001:2010) %>%
  mutate(county_id = (state - 1) * counties_per_state + county)

# Assign treatment: ~half of counties treated, all starting in 2006
treatment_info <- df %>%
  distinct(county_id, state) %>%
  mutate(treated = rbinom(n(), 1, 0.5))
df <- df %>%
  left_join(treatment_info, by = c("county_id", "state")) %>%
  mutate(post = as.integer(year >= treat_year),
         treat_post = treated * post)

# Simulate outcome with county FE and state-year shocks
county_effects    <- rnorm(max(df$county_id))
state_year_shocks <- matrix(rnorm(n_states * 10), nrow = n_states)

df <- df %>%
  mutate(
    county_fe        = county_effects[county_id],
    state_year_shock = state_year_shocks[cbind(state, year - 2000)],
    epsilon          = rnorm(n()),
    outcome          = 20 + true_effect * treat_post +
      county_fe + state_year_shock + epsilon)


# Estimate
ml = list(
  feols(outcome ~ treat_post | county_id + year, data = df),
  feols(outcome ~ treat_post | county_id + state^year, data = df),
  feols(outcome ~ treat_post | county_id + year + state, data = df),
  lm(outcome ~ treated * post, data = df),
  lm(outcome ~ treated * post + factor(state), data = df))

modelsummary(ml,coef_map = c("treated:post"="treatment","treat_post"="treatment"))



# ============================================================
# ============================================================
# ============================================================
### minimal staggered DiD

set.seed(1234)

# Parameters
n_states           <- 10
counties_per_state <- 5
true_effect        <- 5

# Build panel with staggered treatment
df <- expand.grid(
  state  = 1:n_states,
  county = 1:counties_per_state,
  year   = 2001:2010) %>%
  mutate(county_id = (state - 1) * counties_per_state + county)

treatment_info <- df %>%
  distinct(county_id, state) %>%
  mutate(
    treated    = rbinom(n(), 1, 0.5),
    treat_year = ifelse(treated,
      sample(2004:2008, n(), replace = TRUE),
      Inf)
  )

df <- df %>%
  left_join(treatment_info, by = c("county_id", "state")) %>%
  mutate(
    post    = as.integer(year >= treat_year),
    outcome = 20 + true_effect * post + rnorm(n())) %>%
  # numeric treat_year with 0 for never-treated (not Inf)
  mutate(treat_year_cs = ifelse(treated == 0, 0, treat_year))

###### MODELS

# TWFE
twfe <- feols(outcome ~ post | county_id + year,
  data = df, cluster = ~state)
# Callaway & Sant'Anna
cs <- att_gt(
  yname  = "outcome",
  tname  = "year",
  idname = "county_id",
  gname  = "treat_year_cs",
  data   = df,
  control_group = "notyettreated"  # or "nevertreated"
)

# Aggregate to a single ATT
cs_agg <- aggte(cs, type = "simple")

# Compare
summary(twfe)
summary(cs_agg)

########
#### Opening up CS
cohort = 2004
base_year = 2003
eval_year = 2005


## Example cohort
df_2x2 <- df %>%
  filter(
    year %in% c(base_year,eval_year),
    # 2004 cohort vs never treated
    treat_year_cs == cohort | treat_year_cs == 0) %>%
  mutate(
    in_cohort = as.integer(treat_year_cs == g),
    post      = as.integer(year >= cohort)
  )

# Manual 2x2 DiD
m <- df_2x2 %>%
  group_by(in_cohort, post) %>%
  summarise(mean_y = mean(outcome), .groups = "drop")

change_treated = m$mean_y[m$in_cohort==1 & m$post==1] -
  m$mean_y[m$in_cohort==1 & m$post==0]
change_control = m$mean_y[m$in_cohort==0 & m$post==1] -
  m$mean_y[m$in_cohort==0 & m$post==0]

att = change_treated - change_control
cs_results <- tidy(cs) %>%
  select(att=estimate,se=std.error,time,group)

att
subset(cs_results, time==eval_year & group==cohort)

### SIMPLE Aggregation

# number of treated units per cohort
cohort_sizes <- df %>%
  filter(treated == 1) %>%
  distinct(county_id, treat_year) %>%
  count(treat_year, name = "cohort_n")

simple <- cs_results %>%
  filter(time >= group) %>%
  left_join(cohort_sizes, by = c("group" = "treat_year")) %>%
  summarise(att = weighted.mean(att, w = cohort_n))

simple
aggte(cs, type = "simple")$overall.att

### DYNAMIC Aggregation

dynamic <- cs_results %>%
  mutate(e = time - group) %>% # relative time
  filter(time >= group - 3) %>% # keep some pre-periods too
  left_join(cohort_sizes, by = c("group" = "treat_year")) %>%
  group_by(e) %>%
  summarise(att = weighted.mean(att, w = cohort_n))

dynamic

### GROUP aggregation

group_att <- cs_results %>%
  filter(time >= group) %>%
  group_by(group) %>%
  summarise(att_g = mean(att)) %>% # step 1: average within cohort
  summarise(att = mean(att_g)) # step 2: simple average across cohorts
group_att

# in column 2, cohorts with more post-treatment periods observed get more weight (because they contribute more (g,t) cells). In column 3 each cohort gets equal weight regardless of how many post-periods it has. This matters when cohorts are treated at very different times and thus have very different numbers of observable post-treatment periods.
