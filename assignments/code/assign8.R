# ============================================================
# Assignment 8: Spatial Data II
# Applied Quantitative Methods for the Social Sciences II
# ============================================================

# List of packages
library(sf)
library(spData)
library(spdep)
library(spatialreg)
library(ggplot2)

# ==========================================================================
# Part 1: In-Class (Detecting Spatial Autocorrelation)
# ==========================================================================

data(world)

# ----------------------------------------------------------
## 1. Setup and OLS baseline

# a)
world = world[!is.na(world$gdpPercap) & !is.na(world$lifeExp), ]
world = world[world$continent != "Antarctica", ]
world$log_gdp = log(world$gdpPercap)
nrow(world)

# b)
ols_fit = lm(lifeExp ~ log_gdp, data = world)
summary(ols_fit)

# c)
world$ols_resid = residuals(ols_fit)

ggplot(world) +
  geom_sf(aes(fill = ols_resid), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d",
                       midpoint = 0, name = "OLS residual") +
  theme_void() +
  labs(title = "OLS residuals: life expectancy ~ log GDP per capita")
ggsave("ols_residuals_map.pdf", width = 10, height = 5)

# ----------------------------------------------------------
## 2. Spatial weights matrix

# a)
nb = poly2nb(world, queen = TRUE)
listw = nb2listw(nb, style = "W", zero.policy = TRUE)
summary(nb)

# b)
moran.test(world$ols_resid, listw = listw, zero.policy = TRUE)

# ----------------------------------------------------------
## 3. Lagrange Multiplier tests

lm_tests = lm.LMtests(ols_fit, listw = listw,
                       test = c("LMerr", "LMlag", "RLMerr", "RLMlag"),
                       zero.policy = TRUE)
summary(lm_tests)

# a)
# LMerr: spatial dependence in error term (lambda != 0)
# LMlag: spatially lagged dependent variable (rho != 0)

# b)
# RLMerr and RLMlag: robust versions controlling for the other type.
# Decision rule: prefer the model whose robust test is more significant.

# ----------------------------------------------------------
## 4. Spatial Error Model (SEM)

sem_fit = errorsarlm(lifeExp ~ log_gdp, data = world,
                     listw = listw, zero.policy = TRUE)
summary(sem_fit)

# a)
# Compare log_gdp coefficient to OLS; inspect lambda and its p-value.

# b)
# Lambda governs u = lambda*W*u + epsilon: positive lambda means unmeasured
# factors driving life expectancy are themselves spatially clustered.

# c)
world$sem_resid = residuals(sem_fit)
moran.test(world$sem_resid, listw = listw, zero.policy = TRUE)

# ==========================================================================
# Part 2: Take-Home (Spatial Lag Model and Model Comparison)
# ==========================================================================

# ----------------------------------------------------------
## 1. Spatial Lag Model (SLM)

slm_fit = lagsarlm(lifeExp ~ log_gdp, data = world,
                   listw = listw, zero.policy = TRUE)
summary(slm_fit)

# a)
# Report rho and its p-value; report log_gdp coefficient.

# b)
# Positive significant rho: countries surrounded by high-life-expectancy neighbors
# tend to have higher life expectancy themselves, beyond own GDP level.

# c)
# Raw log_gdp coefficient != marginal effect.
# y = (I - rho*W)^{-1}*(X*beta + eps): change in x_i propagates to all units.
# Use impacts() for correct marginal effects.

# ----------------------------------------------------------
## 2. Direct and Indirect Effects

# a)
set.seed(42)
impacts_slm = impacts(slm_fit, listw = listw, R = 500)
summary(impacts_slm, zstats = TRUE)

# b)
# Indirect effect = average spillover to all other countries' life expectancy
# when one country's log GDP rises by 1 unit, after spatial equilibration.

# c)
# Total > direct whenever rho > 0.
# As rho -> 0: indirect -> 0, total -> direct.
# As rho grows: feedback multiplier amplifies indirect effect.

# ----------------------------------------------------------
## 3. Model Comparison

# a)
AIC(ols_fit)
AIC(sem_fit)
AIC(slm_fit)

# b)
# Summary: OLS residuals showed strong positive spatial autocorrelation
# (Moran's I positive, p < 0.001). LM tests and robust versions identified
# preferred spatial model. log_gdp coefficient broadly stable across OLS/SEM/SLM.
# SLM reveals life expectancy spillovers via positive significant rho and
# non-trivial indirect effects. Queen contiguity limitation: island nations have
# no neighbors; weights ignore actual cross-border linkage strength.

# ----------------------------------------------------------
## 4. Extension: Spatial Durbin Model (optional/bonus)

# a)
sdm_fit = lagsarlm(lifeExp ~ log_gdp, data = world,
                   listw = listw, Durbin = TRUE, zero.policy = TRUE)
summary(sdm_fit)

# b)
AIC(sdm_fit)
AIC(sem_fit)
AIC(slm_fit)
# SDM adds lag.log_gdp (W * log_gdp). Compare AIC to SEM/SLM to assess
# whether the extra parameter is justified (rule of thumb: > 2 AIC improvement).
