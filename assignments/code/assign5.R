# ============================================================
# Assignment 5: Panel Data I
# Applied Quantitative Methods for the Social Sciences II
# ============================================================

# List of packages
library(haven)
library(dplyr)
library(ggplot2)
library(fixest)
library(plm)
library(modelsummary)

# ==========================================================================
# Part 1: Presidential Approval (In-Class)
# ==========================================================================

# ----------------------------------------------------------
## 1. Setup and data exploration

# a)
df = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/presidential_approval/presidential_approval.csv")
length(unique(df$State))
length(unique(df$Year))
table(table(df$State))

# b)
summary(df$PresApprov)
summary(df$UnemPct)

df_sub = df %>%
  filter(State %in% c("California", "Texas", "NewYork"))

ggplot(df_sub, aes(x = Year, y = PresApprov, color = State)) +
  geom_line() +
  theme_minimal() +
  #theme(legend.position = "none") +
  labs(x = "Year", y = "Presidential approval (%)", color = "State")

# c)
ggplot(df, aes(x = UnemPct, y = PresApprov, color = State)) +
  geom_point(alpha = 0.4) +
#  geom_smooth(method = "lm") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Unemployment rate (%)", y = "Presidential approval (%)")

# ----------------------------------------------------------
## 2. Pooled OLS

# a)
m_pooled = lm(PresApprov ~ UnemPct, data = df)
summary(m_pooled)

# b)
m_pooled2 = lm(PresApprov ~ UnemPct + South, data = df)
summary(m_pooled2)

modelsummary(list(m_pooled, m_pooled2), stars = TRUE)
  
# ----------------------------------------------------------
## 3. Entity fixed effects
## --------------------------------------------------

# a)
m_fe = feols(PresApprov ~ UnemPct | State, data = df)

ols0 = lm(PresApprov ~ UnemPct, data = df)
ols = lm(PresApprov ~ UnemPct + factor(State), data = df)

newdf = df %>%
  group_by(State) %>%
  mutate(PresApprov = PresApprov - mean(PresApprov,na.rm=T),
         UnemPct = UnemPct - mean(UnemPct,na.rm=T)) %>% ungroup

ols2 = lm(PresApprov ~ UnemPct, data = newdf)

summary(ols)
summary(ols2)
modelsummary(list(ols0, ols, ols2))

ols3 = lm(PresApprov ~ UnemPct + factor(State) + factor(Year), data=df)
summary(ols3)

newdf2 = df %>%
  group_by(State) %>%
  mutate(PresApprov = PresApprov - mean(PresApprov,na.rm=T),
         UnemPct = UnemPct - mean(UnemPct,na.rm=T)) %>%
  ungroup() %>% group_by(Year) %>%
  mutate(PresApprov = PresApprov - mean(PresApprov,na.rm=T),
         UnemPct = UnemPct - mean(UnemPct,na.rm=T)) %>%
  ungroup()
newdf2$PresApprov = newdf2$PresApprov +
  mean(newdf2$PresApprov, na.rm=T)
newdf2$UnemPct = newdf2$UnemPct +
  mean(newdf2$UnemPct, na.rm=T)

ols4 = lm(PresApprov ~ UnemPct, data=newdf2)
summary(ols4)




modelsummary(
  list("Pooled OLS" = m_pooled, "OLS + Control" = m_pooled2, "State FE" = m_fe),
  vcov = ~State,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))

# ----------------------------------------------------------
## 4. Two-way fixed effects

# a)
m_twfe = feols(PresApprov ~ UnemPct | State + Year,
               data = df)

# b)
modelsummary(
  list("Pooled OLS" = m_pooled, "State FE" = m_fe, "Two-Way FE" = m_twfe),
  vcov = ~State,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))

# ==========================================================================
# Part 2: Teaching Evaluations (Take-Home)
# ==========================================================================

# ----------------------------------------------------------
## 1. Data exploration

# a)
df = read_dta("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/teaching_evals/teaching_evals.dta")
length(unique(df$InstrID))
length(unique(df$CourseID))
nrow(df) / length(unique(df$InstrID))

# b)
ggplot(df, aes(x = Apct, y = Eval)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "Percent receiving A or A- (%)", y = "Average course evaluation (1-5)")

# ----------------------------------------------------------
## 2. Pooled OLS baseline

# a)
m1 = lm(Eval ~ Apct + Enrollment + Required, data = df)
summary(m1)

# ----------------------------------------------------------
## 3. Fixed effects models

# a)
m_instr = feols(Eval ~ Apct + Enrollment + Required | InstrID, data = df)
m_twfe  = feols(Eval ~ Apct + Enrollment + Required | InstrID + Year, data = df)

# b)
modelsummary(
  list("Pooled OLS" = m1, "Instructor FE" = m_instr, "Two-Way FE" = m_twfe),
  vcov = ~InstrID,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))

# ----------------------------------------------------------
## 4. Random effects and Hausman test

# a)
pdata = pdata.frame(df, index = c("InstrID", "CourseID"))
m_re = plm(Eval ~ Apct + Enrollment + Required,
           data = pdata, model = "random")
summary(m_re)

# b)
m_fe_plm = plm(Eval ~ Apct + Enrollment + Required,
               data = pdata, model = "within")
phtest(m_fe_plm, m_re)
