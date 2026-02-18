# setwd("~/Documents/AQM2")
options(stringsAsFactors = FALSE)

# ============================================================
# Assignment 4 -- Part 2: Wealth and Infant Mortality
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
## 1. Data exploration

# a) Load data and summary statistics
df = read.dta13("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/infantmortality.dta")
summary(df)
nrow(df)

# b) Histograms
ggplot(df, aes(x = infant)) +
  geom_histogram(bins = 20) +
  labs(x = "Infant mortality (per 1,000 live births)", y = "Count")

ggplot(df, aes(x = income)) +
  geom_histogram(bins = 20) +
  labs(x = "Per-capita income (dollars)", y = "Count")
# Both variables are right-skewed.

# c) Scatter plot colored by region
ggplot(df, aes(x = income, y = infant, color = region)) +
  geom_point() +
  labs(x = "Per-capita income", y = "Infant mortality")
# Clear negative non-linear relationship. African countries have
# highest mortality and lowest income.

# d) Log-log scatter plot
ggplot(df, aes(x = log(income), y = log(infant), color = region)) +
  geom_point() +
  labs(x = "log(income)", y = "log(infant mortality)")
# The log-log relationship looks much more linear.

# --------
## 2. Comparing specifications

# a) Level-level model
m1 = lm(infant ~ income, data = df)
tidy(m1)

# b) Log-log model
m2 = lm(log(infant) ~ log(income), data = df)
tidy(m2)

# c) Interpretation
# m1: predicted change in infant mortality for a $1,000 increase:
coef(m1)["income"] * 1000
# m2: the coefficient is an elasticity. A 10% increase in income is
# associated with a change of approx. 10 * beta_1 percent in infant mortality.

# d) Residual plots
m1_aug = augment(m1)
ggplot(m1_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals: Level-Level (m1)")
# Strong curved pattern and heteroskedasticity.

m2_aug = augment(m2)
ggplot(m2_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals: Log-Log (m2)")
# Much improved residual pattern.

# --------
## 3. Multiple regression with controls

# a) Log-log model with controls
m3 = lm(log(infant) ~ log(income) + region + oil, data = df)
tidy(m3)

# b) Controlling for region and oil changes the income coefficient,
# indicating that some of the bivariate association was driven by
# regional differences and oil wealth.

# c) The Africa coefficient shows that African countries have higher
# infant mortality than the reference group, even controlling for income.

# d) Average marginal effects
avg_slopes(m3)

# --------
## 4. Interaction: oil status and income

# a) Interaction model
m4 = lm(log(infant) ~ log(income) * oil + region, data = df)
tidy(m4)

# b) Marginal effect of income by oil status
avg_slopes(m4, variables = "income", by = "oil")

# c) Oil-exporting countries may have high income from resources without
# the broader development that normally accompanies income growth,
# weakening the income-mortality link ("resource curse").

# d) Plot marginal effects by oil status
p1 = plot_slopes(m4, variables = "income", condition = "oil")
ggsave("slopes_oil.png", p1, width = 6, height = 4)

# --------
## 5. Predicted values for specific scenarios

# a) Predictions for three scenarios
preds = predictions(m3,
  newdata = datagrid(
    income = c(1000, 20000, 10000),
    region = c("Africa", "Europe", "Americas"),
    oil = c("no", "no", "yes")))
preds

# Exponentiate to get original scale
preds$estimate_original = exp(preds$estimate)
preds %>% select(income, region, oil, estimate, estimate_original)

# b) Predicted values are plausible. The gap between the African
# low-income and European high-income scenarios is substantial.

# --------
## 6. Publication-quality visualization

# a) Prediction plot by region
p2 = plot_predictions(m3, condition = c("income", "region")) +
  labs(
    x = "Per-capita income (dollars)",
    y = "Predicted log(infant mortality)",
    title = "Predicted infant mortality by income and region",
    color = "Region") +
  theme_minimal()
ggsave("pred_plot_region.png", p2, width = 7, height = 5)

# b) The plot shows infant mortality decreases with income across all
# regions but at different levels. African countries have consistently
# higher predicted mortality. The gradient is steepest at low income.
# Limitations: cross-sectional (no causation), omitted variables,
# reverse causality, ecological fallacy.

# --------
## 7. Diagnostics and robust inference

# a) Residuals vs fitted for m3
m3_aug = augment(m3)
ggplot(m3_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: m3")
# Reasonably random, some heteroskedasticity possible.

# b) Regression table with robust SEs
modelsummary(
  list("Level" = m1, "Log-Log" = m2,
       "Controls" = m3, "Interaction" = m4),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))

# c) Compare robust and default SEs for m3
modelsummary(
  list("Default SEs" = m3, "Robust SEs" = m3),
  vcov = list("classical", "robust"),
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))
# Robust SEs guard against heteroskedasticity, common in cross-country data.
