# setwd("~/Documents/AQM2")
options(stringsAsFactors = FALSE)

# ============================================================
# Assignment 4: Model Interpretation and Diagnostics
# Applied Quantitative Methods II, UC3M
# ============================================================

# List of packages
library(dplyr)
library(broom)
library(ggplot2)
library(modelsummary)
library(marginaleffects)
library(readstata13)

# ==========================================================================
# Part 1: Corruption and Wealth
# ==========================================================================

# ----------------------------------------------------------
## 1. Setup and data exploration

# a)
df = read.dta13("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/corruption.dta")

# b)
df = df %>% filter(!is.na(ti_cpi) & !is.na(undp_gdp))
nrow(df)

# c)
summary(df$ti_cpi)
sd(df$ti_cpi)
summary(df$undp_gdp)
sd(df$undp_gdp)
## right-skewness: mean > median

# ----------------------------------------------------------
## 2. Exploratory visualization

# a)
ggplot(df, aes(x = undp_gdp, y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    x = "GDP per capita (PPP)",
    y = "Corruption Perceptions Index")

# c)
ggplot(df, aes(x = log(undp_gdp), y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    x = "log(GDP per capita)",
    y = "Corruption Perceptions Index") +
  theme_bw()

# ----------------------------------------------------------
## 3. Bivariate regression

# a)
m1 = lm(ti_cpi ~ undp_gdp, data = df)

# b)
tidy(m1)
coef(m1)["undp_gdp"] * 10000

# c)
q25 = quantile(df$undp_gdp, 0.25)
q75 = quantile(df$undp_gdp, 0.75)
c(q25, q75)
predictions(m1, newdata = datagrid(undp_gdp = c(q25, q75)))

# ----------------------------------------------------------
## 4. Non-linear specifications

# a)
m2 = lm(ti_cpi ~ log(undp_gdp), data = df)
tidy(m2)

# b)
plot_predictions(m2, condition = "undp_gdp")

# c)
m3 = lm(ti_cpi ~ undp_gdp + I(undp_gdp^2), data = df)
tidy(m3)
plot_predictions(m3, condition = "undp_gdp")

# d)
r2 = c(
  "Level-Level" = summary(m1)$r.squared,
  "Level-Log"   = summary(m2)$r.squared,
  "Quadratic"   = summary(m3)$r.squared)
r2

# ----------------------------------------------------------
## 5. Marginal effects

# a)
avg_slopes(m2, variables = "undp_gdp")

# c)
slopes(m3, variables = "undp_gdp",
       newdata = datagrid(undp_gdp = c(2000, 10000, 30000)))

### SHOW WHAT DATAGRID DOES
smalldf = data.frame(x = 1:10, y = 11:20, z = 101:110)
datagrid(newdata = smalldf, x = 5)
datagrid(newdata = smalldf, x = c(1,5))


# ----------------------------------------------------------
## 6. Prediction plots

# a)
p1 = plot_predictions(m2, condition = "undp_gdp")
ggsave("pred_plot_m2.png", p1, width = 6, height = 4)

# b)
p2 = plot_predictions(m3, condition = "undp_gdp")
ggsave("pred_plot_m3.png", p2, width = 6, height = 4)

# ----------------------------------------------------------
## 7. Residual diagnostics

# a)
m1_aug = augment(m1)
ggplot(m1_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Level (m1)")

# b)
m2_aug = augment(m2)
ggplot(m2_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Log (m2)")

# c)
n = nrow(df)
threshold = 4 / n
cooks_d = cooks.distance(m2)
influential = which(cooks_d > threshold)
df$cname[influential]
plot(m2, which = 4)

# ----------------------------------------------------------
## 8. Publication-quality table

# a)
modelsummary(
  list("Level-Level" = m1, "Level-Log" = m2, "Quadratic" = m3),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))

modelsummary(
  list("Level-Level" = m1, "Level-Log" = m2, "Quadratic" = m3),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output = "latex")

modelsummary(
  list("Level-Level" = m1, "Level-Log" = m2, "Quadratic" = m3),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  coef_rename = c(
    "undp_gdp" = "GDPpc",
    "log(undp_gdp)" = "Log. GDPpc"))

modelsummary(
  list("Level-Level" = m1, "Level-Log" = m2, "Quadratic" = m3),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  coef_map = c(
    "undp_gdp" = "GDPpc",
    "log(undp_gdp)" = "Log. GDPpc"))

# ==========================================================================
# Part 2: Wealth and Infant Mortality
# ==========================================================================

# ----------------------------------------------------------
## 1. Data exploration

# a)
df = read.dta13("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/infantmortality.dta")
summary(df)
nrow(df)

# b)
ggplot(df, aes(x = infant)) +
  geom_histogram(bins = 20) +
  labs(x = "Infant mortality (per 1,000 live births)", y = "Count")

ggplot(df, aes(x = income)) +
  geom_histogram(bins = 20) +
  labs(x = "Per-capita income (dollars)", y = "Count")

# c)
ggplot(df, aes(x = income, y = infant, color = region)) +
  geom_point() +
  labs(x = "Per-capita income", y = "Infant mortality")

# d)
ggplot(df, aes(x = log(income), y = log(infant), color = region)) +
  geom_point() +
  labs(x = "log(income)", y = "log(infant mortality)")

# ----------------------------------------------------------
## 2. Comparing specifications

# a)
m1 = lm(infant ~ income, data = df)
tidy(m1)

# b)
m2 = lm(log(infant) ~ log(income), data = df)
tidy(m2)

# c)
coef(m1)["income"] * 1000

# d)
m1_aug = augment(m1)
ggplot(m1_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals: Level-Level (m1)")

m2_aug = augment(m2)
ggplot(m2_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals: Log-Log (m2)")

# ----------------------------------------------------------
## 3. Multiple regression with controls

# a)
m3 = lm(log(infant) ~ log(income) + region + oil, data = df)
tidy(m3)

# d)
avg_slopes(m3)

# ----------------------------------------------------------
## 4. Interaction: oil status and income

# a)
m4 = lm(log(infant) ~ log(income) * oil + region, data = df)
tidy(m4)

# b)
avg_slopes(m4, variables = "income", by = "oil")

# d)
p1 = plot_slopes(m4, variables = "income", condition = "oil")
ggsave("slopes_oil.png", p1, width = 6, height = 4)

# ----------------------------------------------------------
## 5. Predicted values for specific scenarios

# a)
preds = predictions(m3,
  newdata = datagrid(
    income = c(1000, 20000, 10000),
    region = c("Africa", "Europe", "Americas"),
    oil = c("no", "no", "yes")))
preds

preds$estimate_original = exp(preds$estimate)
preds %>% select(income, region, oil, estimate, estimate_original)

# ----------------------------------------------------------
## 6. Publication-quality visualization

# a)
p2 = plot_predictions(m3, condition = c("income", "region")) +
  labs(
    x = "Per-capita income (dollars)",
    y = "Predicted log(infant mortality)",
    title = "Predicted infant mortality by income and region",
    color = "Region") +
  theme_minimal()
ggsave("pred_plot_region.png", p2, width = 7, height = 5)

# ----------------------------------------------------------
## 7. Diagnostics and robust inference

# a)
m3_aug = augment(m3)
ggplot(m3_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: m3")

# b)
modelsummary(
  list("Level" = m1, "Log-Log" = m2,
       "Controls" = m3, "Interaction" = m4),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))

# c)
modelsummary(
  list("Default SEs" = m3, "Robust SEs" = m3),
  vcov = list("classical", "robust"),
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))
