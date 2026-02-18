# setwd("~/Documents/AQM2")
options(stringsAsFactors = FALSE)

# ============================================================
# Assignment 4 -- Part 1: Corruption and Wealth
# Applied Quantitative Methods II, UC3M
# ============================================================

# List of packages
library(dplyr)
library(broom)
library(ggplot2)
library(modelsummary)
library(marginaleffects)
library(readstata13)

# --------
## 1. Setup and data exploration

# a) Load data
df = read.dta13("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/corruption.dta")

# b) Drop missing values
df = df %>% filter(!is.na(ti_cpi) & !is.na(undp_gdp))
nrow(df)

# c) Summary statistics
summary(df$ti_cpi)
sd(df$ti_cpi)
summary(df$undp_gdp)
sd(df$undp_gdp)
# GDP per capita is right-skewed: the maximum is far above the median
# and the standard deviation is large relative to the mean.

# --------
## 2. Exploratory visualization

# a) Scatter plot (levels)
ggplot(df, aes(x = undp_gdp, y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "GDP per capita (PPP)", y = "Corruption Perceptions Index")

# b) The relationship is positive but clearly non-linear.
# Most countries cluster at low GDP values.

# c) Scatter plot with log GDP
ggplot(df, aes(x = log(undp_gdp), y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "log(GDP per capita)", y = "Corruption Perceptions Index")
# The log transformation improves the linearity of the relationship.

# --------
## 3. Bivariate regression

# a) Level-level model
m1 = lm(ti_cpi ~ undp_gdp, data = df)

# b) Print results
tidy(m1)
# Predicted change for a $10,000 increase in GDP:
coef(m1)["undp_gdp"] * 10000

# c) Predictions at 25th and 75th percentiles
q25 = quantile(df$undp_gdp, 0.25)
q75 = quantile(df$undp_gdp, 0.75)
c(q25, q75)

predictions(m1, newdata = datagrid(undp_gdp = c(q25, q75)))

# --------
## 4. Non-linear specifications

# a) Log model
m2 = lm(ti_cpi ~ log(undp_gdp), data = df)
tidy(m2)

# b) Predicted change for a doubling of GDP:
coef(m2)["log(undp_gdp)"] * log(2)

# c) Quadratic model
m3 = lm(ti_cpi ~ undp_gdp + I(undp_gdp^2), data = df)
tidy(m3)

# d) Compare R-squared
r2 = c(
  "Level-Level" = summary(m1)$r.squared,
  "Level-Log"   = summary(m2)$r.squared,
  "Quadratic"   = summary(m3)$r.squared)
r2
# The log specification fits best. Non-linear spec is appropriate because
# the marginal return to additional GDP diminishes at higher income levels.

# --------
## 5. Marginal effects

# a) AME of GDP in the log model
avg_slopes(m2, variables = "undp_gdp")

# b) The AME differs from the raw coefficient because in a level-log model,
# the marginal effect is beta/x, which depends on x. The AME averages this
# over all observed values.

# c) Marginal effects at specific GDP values (quadratic model)
slopes(m3, variables = "undp_gdp",
       newdata = datagrid(undp_gdp = c(2000, 10000, 30000)))
# The marginal effect diminishes as countries become richer.

# --------
## 6. Prediction plots

# a) Prediction plot for log model
p1 = plot_predictions(m2, condition = "undp_gdp")
ggsave("pred_plot_m2.png", p1, width = 6, height = 4)

# b) Prediction plot for quadratic model
p2 = plot_predictions(m3, condition = "undp_gdp")
ggsave("pred_plot_m3.png", p2, width = 6, height = 4)

# c) Both models tell a similar story: corruption decreases sharply with
# initial GDP increases then levels off. The log model is smoother; the
# quadratic can curve back up at very high GDP values.

# --------
## 7. Residual diagnostics

# a) Residuals vs fitted: level-level
m1_aug = augment(m1)
ggplot(m1_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Level (m1)")
# Clear curved pattern and possible heteroskedasticity.

# b) Residuals vs fitted: log model
m2_aug = augment(m2)
ggplot(m2_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Log (m2)")
# Much improved residual pattern.

# c) Cook's distance
n = nrow(df)
threshold = 4 / n

cooks_d = cooks.distance(m2)
influential = which(cooks_d > threshold)
df$cname[influential]

plot(m2, which = 4)

# d) Influential observations should not be removed automatically. They may
# be genuine cases. A recommended robustness check is to re-estimate
# excluding them and see if coefficients change meaningfully.

# --------
## 8. Publication-quality table

# a) Regression table with robust SEs
modelsummary(
  list("Level-Level" = m1, "Level-Log" = m2, "Quadratic" = m3),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))

# b) The level-log model (m2) is preferred: highest R-squared, best residual
# diagnostics, and a clear substantive interpretation (diminishing returns).
