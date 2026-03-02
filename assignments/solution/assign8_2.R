# setwd("~/Documents/AQM2")
options(stringsAsFactors = FALSE)

# ============================================================
# Assignment 8 -- Part 2: Spatial Lag Model and Model Comparison
# Applied Quantitative Methods for the Social Sciences II
# ============================================================

# List of packages
library(sf)
library(spData)
library(spdep)
library(spatialreg)
library(ggplot2)

data(world)

# Rebuild all objects from Part 1 so this script is self-contained
world = world[!is.na(world$gdpPercap) & !is.na(world$lifeExp), ]
world = world[world$continent != "Antarctica", ]
world$log_gdp = log(world$gdpPercap)

ols_fit = lm(lifeExp ~ log_gdp, data = world)
world$ols_resid = residuals(ols_fit)

nb = poly2nb(world, queen = TRUE)
listw = nb2listw(nb, style = "W", zero.policy = TRUE)

sem_fit = errorsarlm(lifeExp ~ log_gdp, data = world,
                     listw = listw, zero.policy = TRUE)

# --------
## 1. Spatial Lag Model (SLM)

slm_fit = lagsarlm(lifeExp ~ log_gdp, data = world,
                   listw = listw, zero.policy = TRUE)
summary(slm_fit)

# a)
# Rho (rho) and its p-value are shown above, along with the log_gdp coefficient.
# A positive and significant rho confirms that a country's life expectancy is
# partly determined by its neighbors' life expectancy.

# b)
# Positive significant rho means countries surrounded by high-life-expectancy
# neighbors tend to have higher life expectancy themselves, over and above their
# own GDP level. Cross-border spillovers via regional health systems, disease
# environments, migration, and shared institutions create geographic clusters
# that GDP alone cannot capture.

# c)
# The raw log_gdp coefficient in the SLM output is NOT the marginal effect.
# Solving y = rho*W*y + X*beta + eps for y gives y = (I - rho*W)^{-1}*(X*beta + eps).
# A change in x_i ripples through (I - rho*W)^{-1} to all other units via
# the spatial feedback loop. The marginal effect must be computed with impacts().

# --------
## 2. Direct and Indirect Effects

# a)
set.seed(42)
impacts_slm = impacts(slm_fit, listw = listw, R = 500)
summary(impacts_slm, zstats = TRUE)
# Direct effect: average effect of own log GDP on own life expectancy (with feedback).
# Indirect effect: average spillover to all other countries' life expectancy.
# Total effect: direct + indirect (sum across the equilibrium matrix).
# Direct is slightly larger than the raw coefficient due to own-feedback.
# Total is larger than the OLS coefficient because it includes network-propagated
# spillovers that OLS ignores entirely.

# b)
# The indirect effect = average change in all other countries' life expectancy when
# one country's log GDP rises by 1 unit, after the spatial feedback loop equilibrates.
# It reflects both the strength of rho and the density of the neighborhood graph.

# c)
# Total > direct is expected whenever rho > 0.
# As rho -> 0: (I - rho*W)^{-1} -> I, indirect effect -> 0, total -> direct.
# As rho grows larger: the feedback multiplier amplifies shocks further,
# producing bigger indirect effects and a wider gap between total and direct.
# The indirect effect is larger when rho is larger and countries are more densely
# connected in the weights graph.

# --------
## 3. Model Comparison

# a)
AIC(ols_fit)
AIC(sem_fit)
AIC(slm_fit)
# Lower AIC = better fit penalized for complexity.
# Both spatial models improve on OLS.
# The spatial model with the lower AIC should agree with the LM-test decision
# from Part 1 (question 1.3b): robust LM tests and AIC cross-validate each other.

# b) Summary paragraph (5--8 sentences):
# Moran's I on OLS residuals was positive and statistically significant (p < 0.001),
# confirming that the OLS independence-of-errors assumption is violated -- nearby
# countries share systematically similar residuals.
# The LM tests showed both SEM and SLM as candidates; the robust versions (RLMerr,
# RLMlag) pointed to the preferred model by controlling for the other form of dependence.
# The log_gdp coefficient is broadly similar across OLS, SEM, and SLM; the SEM
# corrects for spatial confounding in the error term, while the SLM direct effect
# accounts for network feedback through the equilibrium matrix.
# The SLM reveals that life expectancy spillovers across borders are real and
# non-trivial: rho is positive and significant, and the indirect effect from
# impacts() shows a meaningful cross-country propagation channel.
# One key limitation of queen contiguity for country-level data is that island
# nations have no neighbors and are excluded from the spatial structure; contiguity
# also ignores the actual strength of linkages (trade, migration, institutions),
# treating all shared-border pairs as equally connected.

# --------
## 4. Extension: Spatial Durbin Model (optional/bonus)

# a)
sdm_fit = lagsarlm(lifeExp ~ log_gdp, data = world,
                   listw = listw, Durbin = TRUE, zero.policy = TRUE)
summary(sdm_fit)
# The SDM adds lag.log_gdp (W * log_gdp) alongside own log_gdp and the spatial lag
# of life expectancy. A significant lag.log_gdp coefficient means a neighbor's
# GDP per capita predicts own life expectancy beyond what own GDP already explains --
# a direct economic spillover to health, separate from outcome diffusion (rho).

# b)
AIC(sdm_fit)
AIC(sem_fit)
AIC(slm_fit)
# If SDM AIC is not meaningfully lower than the best of SEM/SLM (rule of thumb:
# < 2 AIC units improvement), the extra parameter is not justified and the more
# parsimonious model is preferred.
# A clearly lower SDM AIC would support the conclusion that neighbors' GDP
# directly matters for own life expectancy, beyond the outcome-diffusion channel.
