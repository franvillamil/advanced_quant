# setwd("~/Documents/AQM2")
options(stringsAsFactors = FALSE)

# ============================================================
# Assignment 5 -- Part 1: Presidential Approval (Panel Data I)
# Applied Quantitative Methods for the Social Sciences II
# ============================================================

# List of packages
library(readstata13)
library(dplyr)
library(ggplot2)
library(fixest)
library(modelsummary)

# Load data
df = read.dta13("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/presidential_approval/presidential_approval.dta")

# --------
## 1. Setup and data exploration

# a) Panel structure
length(unique(df$State))
length(unique(df$Year))
table(table(df$State))
# Unbalanced panel: most states appear 73 times but a few appear fewer times.

# b) Summary statistics and time-series plot
summary(df$PresApprov)
summary(df$UnemPct)

df_sub = df %>%
  filter(State %in% c("California", "Texas", "NewYork"))

ggplot(df_sub, aes(x = Year, y = PresApprov, color = State)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Year", y = "Presidential approval (%)", color = "State")
# States move closely together over time, tracking the same large swings in
# approval. Common national factors dominate over state-level differences.

# c) Cross-sectional scatter
ggplot(df, aes(x = UnemPct, y = PresApprov)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "Unemployment rate (%)", y = "Presidential approval (%)")
# Higher unemployment is associated with lower approval across state-year
# observations, but this cross-sectional pattern conflates within- and
# between-state variation.

# --------
## 2. Pooled OLS

# a) Bivariate pooled OLS
m_pooled = lm(PresApprov ~ UnemPct, data = df)
summary(m_pooled)
# Negative coefficient: higher unemployment is associated with lower approval.
# Conflates within- and between-state variation; subject to omitted variable bias.

# b) Add South control
m_pooled2 = lm(PresApprov ~ UnemPct + South, data = df)
summary(m_pooled2)
# Controlling for South changes the UnemPct coefficient only modestly.
# Southern state status is associated with approval differences but is not
# a strong confounder of the unemployment-approval relationship in pooled OLS.

# c) Limitations of pooled OLS:
# - Ignores time-invariant, unobserved state-level differences that may be
#   correlated with unemployment: e.g., (1) structurally weaker economies
#   with higher baseline unemployment and different political cultures;
#   (2) regional partisan leanings that affect how residents evaluate the
#   president independently of economic conditions; (3) unionization rates
#   that affect both unemployment sensitivity and baseline approval.

# --------
## 3. Entity fixed effects

# a) State fixed effects model
m_fe = feols(PresApprov ~ UnemPct | State, data = df)

modelsummary(
  list("Pooled OLS" = m_pooled, "State FE" = m_fe),
  vcov = ~State,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))
# UnemPct coefficient changes relative to pooled OLS. FE compares approval
# within the same state across years, removing time-invariant confounders.

# b) South drops because it does not vary within a state over time.
# Any time-invariant variable is collinear with the state dummies and
# its effect cannot be separately identified.

# c) The FE coefficient identifies a within-state effect: how approval
# changes in a given state when its unemployment rises or falls, relative
# to that state's own average. This differs from pooled OLS which compares
# states with different unemployment levels to each other.

# --------
## 4. Two-way fixed effects

# a) Add year fixed effects
m_twfe = feols(PresApprov ~ UnemPct | State + Year, data = df)

# b) Three-model comparison with clustered SEs
modelsummary(
  list("Pooled OLS" = m_pooled, "State FE" = m_fe, "Two-Way FE" = m_twfe),
  vcov = ~State,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))

# c) Year fixed effects absorb common time shocks: national economic cycles,
# presidential scandals, wars, etc. They identify the within-state effect of
# unemployment relative to the national average in each year, removing the
# confounding role of aggregate trends. If the coefficient changes after adding
# year FEs, common time trends were partly driving the state-FE estimate.
