# setwd("~/Documents/AQM2")

# ============================================================
# Assignment 3: Binary Outcomes
# Applied Quantitative Methods II, UC3M
# ============================================================

# List of packages
library(dplyr)
library(broom)
library(ggplot2)
library(modelsummary)
library(marginaleffects)

# ============================================================
# Part 1: ANES Voter Turnout
# ============================================================

# --------
## 1. Setup and data preparation

# a)
raw = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/anes/anes_timeseries_2020.csv")

## Warn
print("NOTE: mutate vs transmute")
print("NOTE: case_when and ifelse")

class(NA_character_)
class(NA_real_)
class(NA)

df = raw %>%
  transmute(
    voted = ifelse(V202109x < 0, NA, V202109x),
    age = ifelse(V201507x < 0, NA, V201507x),
    female = case_when(
      V201600 == 2 ~ 1,
      V201600 == 1 ~ 0,
      TRUE ~ NA_real_),
    education = case_when(
      V201511x == 1 ~ 10, V201511x == 2 ~ 12, V201511x == 3 ~ 14,
      V201511x == 4 ~ 16, V201511x == 5 ~ 20, TRUE ~ NA_real_),
    income = ifelse(V201617x < 0, NA, V201617x),
    party_id = ifelse(V201231x < 0, NA, V201231x)
  )

# b)
df = na.omit(df)
nrow(df)

# c)
mean(df$voted)
summary(df)

# --------
## 2. Exploratory visualization

# a)
turnout_by_edu = df %>%
  group_by(education) %>%
  summarise(turnout = mean(voted))

ggplot(turnout_by_edu, aes(x = factor(education), y = turnout)) +
  geom_col() +
  labs(x = "Years of education", y = "Turnout rate")

# --------
## 3. Linear probability model

# a)
lpm = lm(voted ~ age + education + income + female, data = df)

# b)
tidy(lpm)

# d)
preds_lpm = predict(lpm)
sum(preds_lpm < 0)
sum(preds_lpm > 1)
range(preds_lpm)

# --------
## 4. Logistic regression

# a)
logit = glm(voted ~ age + education + income + female,
            family = binomial, data = df)

# b)
tidy(logit)

# c)
exp(coef(logit))

# d)
preds_logit = predict(logit, type = "response")
range(preds_logit)

# --------
## 5. Comparing LPM and logit

# a)
avg_slopes(logit)

# c)
modelsummary(list("LPM" = lpm, "Logit" = logit),
             vcov = list("robust", NULL))

# --------
## 6. Predicted probabilities

# a)
p1 = plot_predictions(logit, condition = "education")
ggsave("pred_prob_education.png", p1, width = 6, height = 4)

# b)
p2 = plot_predictions(logit, condition = c("age", "female"))
ggsave("pred_prob_age_gender.png", p2, width = 6, height = 4)

# --------
## 7. Presenting results

# a-b)
p3 = modelplot(list("LPM" = lpm, "Logit" = logit),
               vcov = list("robust", NULL))
ggsave("coefplot_lpm_logit.png", p3, width = 6, height = 4)

# ============================================================
# Part 2: STAR --- High School Graduation
# ============================================================

# --------
## 1. Data preparation

# a)
star = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/star/star.csv")

star$classtype = factor(star$classtype,
  levels = 1:3,
  labels = c("Small", "Regular", "Regular+Aide"))

star$race = factor(star$race,
  levels = 1:6,
  labels = c("White", "Black", "Asian", "Hispanic",
             "Native American", "Other"))

# b)
star$small = ifelse(star$classtype == "Small", 1, 0)

# c)
df = star %>% filter(!is.na(hsgrad))
nrow(df)

# d)
mean(df$hsgrad)

df %>%
  group_by(classtype) %>%
  summarise(grad_rate = mean(hsgrad), n = n())

# --------
## 2. LPM and logit

# a)
lpm1 = lm(hsgrad ~ small, data = df)
tidy(lpm1)

# b)
logit1 = glm(hsgrad ~ small, family = binomial, data = df)
tidy(logit1)

# d)
avg_slopes(logit1)

# --------
## 3. Adding controls

# a)
lpm2 = lm(hsgrad ~ small + race + yearssmall, data = df)
logit2 = glm(hsgrad ~ small + race + yearssmall,
             family = binomial, data = df)

# b)
tidy(lpm1) %>% filter(term == "small") %>% select(term, estimate)
tidy(lpm2) %>% filter(term == "small") %>% select(term, estimate)

# c)
tidy(logit2)
avg_slopes(logit2, variables = "yearssmall")

# --------
## 4. Predicted probabilities

# a)
predictions(logit2,
  newdata = datagrid(
    small = c(1, 0),
    race = c("White", "Black"),
    yearssmall = c(3, 0)))

# b)
p1 = plot_predictions(logit2, condition = c("yearssmall", "small"))
ggsave("pred_prob_yearssmall.png", p1, width = 6, height = 4)

# --------
## 5. Interactions

# a)
logit3 = glm(hsgrad ~ small * race + yearssmall,
             family = binomial, data = df)

# b)
avg_slopes(logit3, variables = "small", by = "race")

# --------
## 6. Presenting results and discussion

# a)
modelsummary(
  list("LPM biv." = lpm1, "LPM ctrl." = lpm2,
       "Logit biv." = logit1, "Logit ctrl." = logit2),
  vcov = list("robust", "robust", NULL, NULL))

# b)
p2 = modelplot(
  list("LPM biv." = lpm1, "LPM ctrl." = lpm2,
       "Logit biv." = logit1, "Logit ctrl." = logit2),
  vcov = list("robust", "robust", NULL, NULL))
ggsave("coefplot_star.png", p2, width = 6, height = 4)
