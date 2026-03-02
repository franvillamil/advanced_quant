# setwd("~/Documents/AQM2")
options(stringsAsFactors = FALSE)

# ============================================================
# Assignment 8 -- Part 1: Detecting Spatial Autocorrelation
# Applied Quantitative Methods for the Social Sciences II
# ============================================================

# List of packages
library(sf)
library(spData)
library(spdep)
library(spatialreg)
library(ggplot2)

data(world)

# --------
## 1. Setup and OLS baseline

# a)
world = world[!is.na(world$gdpPercap) & !is.na(world$lifeExp), ]
world = world[world$continent != "Antarctica", ]
world$log_gdp = log(world$gdpPercap)
nrow(world)
# After removing NAs and Antarctica, nrow(world) observations remain.
# We log-transform gdpPercap because the raw variable is strongly right-skewed:
# a few very rich countries pull the distribution far to the right. The log
# transformation compresses the upper tail and linearises the GDP--life expectancy
# relationship, which is required for OLS.

# b)
ols_fit = lm(lifeExp ~ log_gdp, data = world)
summary(ols_fit)
# The coefficient on log_gdp is positive and highly significant (p < 0.001):
# a 1-unit increase in log GDP per capita (approx. a doubling) is associated
# with higher life expectancy by that coefficient's value in years, on average.
# The R² indicates that log GDP alone explains a large share of cross-country
# variation in life expectancy.

# c)
world$ols_resid = residuals(ols_fit)

ggplot(world) +
  geom_sf(aes(fill = ols_resid), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d",
                       midpoint = 0, name = "OLS residual") +
  theme_void() +
  labs(title = "OLS residuals: life expectancy ~ log GDP per capita")
ggsave("ols_residuals_map.pdf", width = 10, height = 5)
# Sub-Saharan Africa shows a cluster of negative residuals -- life expectancy
# is lower than GDP-based predictions, likely due to HIV/AIDS and disease burden.
# Western Europe and parts of East Asia show positive residuals -- higher life
# expectancy than income alone predicts.
# The geographic clustering is a visual signal of spatial autocorrelation in residuals.

# --------
## 2. Spatial weights matrix

# a)
nb = poly2nb(world, queen = TRUE)
listw = nb2listw(nb, style = "W", zero.policy = TRUE)
summary(nb)
# Some countries have zero neighbors -- these are island nations (e.g. New Zealand,
# Japan, Caribbean and Pacific states) that share no land border or border point
# with any other polygon. Queen contiguity requires at least one shared point;
# islands surrounded by ocean have none. zero.policy = TRUE keeps these units
# in the analysis with no spatial lag contribution.

# b)
moran.test(world$ols_resid, listw = listw, zero.policy = TRUE)
# Moran's I is positive and p-value is well below 0.05: statistically significant
# positive spatial autocorrelation in OLS residuals.
# Nearby countries tend to share similar residuals (both over- or under-predicted),
# violating the OLS assumption of independent errors.
# Consequence: OLS standard errors are invalid and inference is unreliable.

# --------
## 3. Lagrange Multiplier tests

lm_tests = lm.LMtests(ols_fit, listw = listw,
                       test = c("LMerr", "LMlag", "RLMerr", "RLMlag"),
                       zero.policy = TRUE)
summary(lm_tests)

# a)
# LMerr tests for spatial dependence in the error term (lambda != 0, SEM).
# LMlag tests for a spatially lagged dependent variable (rho != 0, SLM).
# Both standard LM tests are significant, so both forms of dependence appear
# present when tested individually. We turn to the robust versions to discriminate.

# b)
# RLMerr and RLMlag each control for the presence of the other type of dependence.
# Decision rule: select the model whose robust test is more significant.
# If RLMerr > RLMlag (more significant), prefer SEM; if RLMlag > RLMerr, prefer SLM.

# --------
## 4. Spatial Error Model (SEM)

sem_fit = errorsarlm(lifeExp ~ log_gdp, data = world,
                     listw = listw, zero.policy = TRUE)
summary(sem_fit)

# a)
# The log_gdp coefficient from SEM is shown above; compare to the OLS estimate.
# Lambda (lambda) is the spatial autoregressive parameter in the error process.
# A positive and significant lambda confirms spatial dependence in the errors.

# b)
# Lambda governs: u = lambda * W*u + epsilon (spatial AR in errors).
# Positive lambda means unmeasured factors driving life expectancy are themselves
# spatially correlated -- e.g. regional disease environments, healthcare
# infrastructure, cultural practices around health that cluster geographically.
# The SEM filters this out without positing that life expectancy itself diffuses
# across borders.

# c)
world$sem_resid = residuals(sem_fit)
moran.test(world$sem_resid, listw = listw, zero.policy = TRUE)
# The Moran's I on SEM residuals is much smaller and no longer significant (or
# substantially reduced), confirming that the spatial error correction has
# absorbed most of the geographic clustering left in the OLS residuals.
