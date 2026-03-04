# setwd("~/Documents/AQM2")
options(stringsAsFactors = FALSE)

# ============================================================
# Assignment 10 -- Part 1: Project Organization
# Applied Quantitative Methods II, UC3M
# ============================================================

# This assignment is about project organization. The code below
# shows the content of each file that students should create.

# --------
## 1. Folder structure

# a) Create folders from the terminal:
# mkdir -p assignment10/data assignment10/analysis/output assignment10/plots/output
# touch assignment10/Makefile assignment10/README.md
# touch assignment10/analysis/models.R assignment10/plots/figures.R

# b) Download corruption.dta and place in data/

# c) README.md should describe:
# - What the project does (examines corruption vs GDP)
# - What each folder contains (data, analysis scripts, plots)
# - How to run it (make)

# --------
## 2. Analysis script (analysis/models.R)

# The script below is what students should put in analysis/models.R.
# Here we load from URL for verification; the actual script uses
# a relative path: read_dta("data/corruption.dta")

library(haven)
library(modelsummary)

df = read_dta("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/corruption.dta")

# Define constants
dep_var = "ti_cpi"
indep_var = "undp_gdp"

# Clean data
df = df[!is.na(df[[dep_var]]) & !is.na(df[[indep_var]]), ]
cat("Observations:", nrow(df), "\n")

# Assertion
if(nrow(df) < 10) stop("Too few observations")

# Estimate models
m1 = lm(ti_cpi ~ undp_gdp, data = df)
m2 = lm(ti_cpi ~ log(undp_gdp), data = df)

# Save table (in the actual script: output = "analysis/output/table_models.tex")
modelsummary(
  list("Level" = m1, "Log" = m2),
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))

cat("Analysis complete. Table saved.\n")

# --------
## 3. Plots script (plots/figures.R)

library(ggplot2)

# Scatter plot: corruption vs log GDP
ggplot(df, aes(x = log(undp_gdp), y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    x = "log(GDP per capita)",
    y = "Corruption Perceptions Index",
    title = "Corruption vs. GDP per capita") +
  theme_minimal()

# In the actual script: ggsave("plots/output/scatter_corruption.pdf", width = 7, height = 5)

cat("Scatter plot saved.\n")

# --------
## 4. Makefile

# The Makefile content (NOTE: recipe lines must use TAB indentation):
#
# all: analysis/output/table_models.tex \
#      plots/output/scatter_corruption.pdf
#
# analysis/output/table_models.tex: analysis/models.R \
#                                   data/corruption.dta
# 	Rscript --no-save analysis/models.R
#
# plots/output/scatter_corruption.pdf: plots/figures.R \
#                                      data/corruption.dta
# 	Rscript --no-save plots/figures.R

# Test by running `make` from the assignment10/ directory.
# Both scripts should execute and produce output files.
# Running `make` again should print "nothing to be done."
