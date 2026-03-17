setwd("~/Documents/course_materials/AQM2/datasets/")
library(dplyr)

## UCDP GED ##

# Url
gedurl = "https://ucdp.uu.se/downloads/ged/ged251-rds.zip"

# Download and extract
tmp_zip = tempfile(fileext = ".zip")
tmp_dir = tempdir()
download.file(gedurl, destfile = tmp_zip)
unzip(tmp_zip, exdir = tmp_dir)

# Load
file = list.files(tmp_dir, pattern = "*.rds")
raw = readRDS(paste(tmp_dir, file, sep = "/"))

# Subset and remove geometry
df = raw %>%
    filter(region == "Africa") %>%
    as.data.frame() %>%
    select(event_id = id, year, longitude, latitude,
        fatalities = best, event_type = type_of_violence) %>%
    mutate(event_type = recode(as.character(event_type),
        "1" = "state-based",
        "2" = "non-state",
        "3" = "one-sided"))

## Save
write.csv(df, "spatial/conflict_events.csv", row.names = FALSE)


df = read.csv("spatial/conflict_events.csv")