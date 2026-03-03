options(stringsAsFactors = FALSE)

library(pscl)
library(MASS)
library(AER)
library(marginaleffects)
library(ggplot2)
data(bioChemists)

# ==========================================================================
# 3. Poisson regression: publication counts
# ==========================================================================

# ----------------------------------------------------------
# a) Explore outcome variable
# ----------------------------------------------------------

summary(bioChemists$art)
var(bioChemists$art)

ggplot(bioChemists, aes(x = art)) +
  geom_histogram(binwidth = 1, fill = "#294b66", color = "white") +
  theme_minimal() +
  labs(title = "Publications in last 3 years of PhD",
       x = "Number of articles", y = "Count")
ggsave("hist_art.pdf", width = 6, height = 4)

# Mean ~1.69, variance ~3.71 -- variance is roughly twice the mean.
# Under Poisson, variance should equal mean. Ratio >> 1 signals overdispersion.
# Distribution is right-skewed with a mode at zero and a long upper tail.

# ----------------------------------------------------------
# b) Fit Poisson regression
# ----------------------------------------------------------

m_pois = glm(art ~ fem + mar + kid5 + phd + ment,
             data = bioChemists, family = poisson)
summary(m_pois)

exp(coef(m_pois)["ment"])

# IRR for ment: each additional mentor article multiplies expected student
# articles by ~1.026. Modest but positive effect of mentor productivity.
# Residual deviance >> degrees of freedom (ratio well above 2): clear
# diagnostic of overdispersion in the Poisson model.

# ----------------------------------------------------------
# c) Formal overdispersion test
# ----------------------------------------------------------

dispersiontest(m_pois)

# z ~ 5.78, p < 0.001: strongly reject equidispersion.
# Estimated dispersion ~ 1.82 (variance ~1.82x the mean).
# Consequence: Poisson SEs are too small, p-values anti-conservative.
# A negative binomial model is needed.

# ==========================================================================
# 4. Negative binomial regression
# ==========================================================================

# ----------------------------------------------------------
# a) Fit negative binomial model
# ----------------------------------------------------------

m_nb = glm.nb(art ~ fem + mar + kid5 + phd + ment,
              data = bioChemists)
summary(m_nb)

# ment coefficient similar to Poisson, but SEs are wider (more honest).
# theta (overdispersion parameter) shown in summary: moderate overdispersion.
# Significant predictors: fem (negative), kid5 (negative), ment (positive).
# phd and mar are not statistically significant.

# ----------------------------------------------------------
# b) Compare model fit by AIC
# ----------------------------------------------------------

AIC(m_pois)
AIC(m_nb)

# NB AIC ~3136 vs Poisson AIC ~3314: NB is substantially better.
# Despite one extra parameter (theta), the improvement in fit far outweighs
# the complexity penalty. Overdispersion is real and worth addressing.

# ----------------------------------------------------------
# c) Predicted counts by gender at sample means
# ----------------------------------------------------------

predictions(m_nb, newdata = datagrid(fem = c("Men", "Women")))

# Men: predicted ~1.87 articles; Women: predicted ~1.51 articles.
# Gender gap is meaningful and statistically distinguishable (CIs do not overlap).
# Difference persists after conditioning on marital status, kids, PhD prestige,
# and mentor productivity -- a within-group productivity gap.

# ----------------------------------------------------------
# d) Summary
# ----------------------------------------------------------

# Poisson is inadequate: variance/mean ratio ~2.2, residual deviance >> df,
# and dispersiontest() rejects equidispersion (p < 0.001).
# Negative binomial adds a dispersion parameter, achieves AIC ~180 units lower,
# and produces more reliable (wider) standard errors.
# The mentor's IRR ~1.026 per additional mentor article: modest but positive --
# working with a productive mentor provides a small, real boost to student output.
# Significant predictors in NB: fem (negative), kid5 (negative), ment (positive).
# phd prestige and marital status are not significant.
# Substantive conclusion: early-career publication productivity in biochemistry
# is shaped by mentor environment, gender, and family demands -- consistent with
# broader literature on structural barriers facing women and parents in STEM PhD programs.
