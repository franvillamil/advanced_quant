setwd("~/Downloads")
library(dplyr)

raw = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/anes/anes_timeseries_2020.csv")

df = raw %>%
    select(voted,
        age,
        female,
        education,
        income,
        party_id)