setwd("~/Desktop")
library(dplyr)
library(broom)
library(modelsummary)

# download QoG
qog = read.csv("https://www.qogdata.pol.gu.se/data/qog_std_cs_jan26.csv")

# cleaning
df = qog %>%
    select(country = cname, epi = epi_epi, women_parl = wdi_wip,
        gov_eff = wbgi_gee, green_seats = cpds_lg)

# regression
m1 = lm(epi ~ women_parl, data = df)
m2 = lm(epi ~ women_parl + gov_eff, data = df)

## 1.5 Omitted variable bias

# Extracting coefs using base R
beta1_biva = subset(tidy(m1), term == "women_parl")$estimate
beta1_mult = subset(tidy(m2), term == "women_parl")$estimate
beta2_mult = subset(tidy(m2), term == "gov_eff")$estimate
# Using pipes
delta = tidy(lm(gov_eff ~ women_parl, data = df)) %>%
    filter(term == "women_parl") %>%
    pull(estimate)

# Check
modelsummary(list(m1,m2), output = "html")
round(beta1_mult + beta2_mult * delta, 2)
round(beta1_biva, 2)

