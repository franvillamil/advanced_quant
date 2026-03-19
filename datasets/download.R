setwd("~/Documents/course_materials/AQM2/datasets/")
library(dplyr)
library(terra)
library(geodata)
library(rnaturalearth)
library(ggplot2)
library(sf)
library(tidyterra)

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



events = read.csv("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/spatial/conflict_events.csv")
table(events$event_type)

# ============================

shapefiles = rnaturalearth::ne_states(
    c("Portugal", "Spain", "France"),
    returnclass = "sf")
xlim = c(-12, 8)
ylim = c(38, 47)

points = data.frame(
    id = 1:100,
    x = sample(seq(-8,xlim[2],.01), 100, replace=T),
    y = sample(seq(ylim[1],ylim[2],.05), 100, replace=T))
points = st_as_sf(points, coords = c("x", "y"), crs = 4326)

p = ggplot() +
  geom_sf(data = shapefiles, fill = NA, color = "black") +
  theme_void() +
  coord_sf(xlim = xlim, ylim = ylim)
ggsave("../slides/img/vectorexample1.pdf")

p = ggplot() +
  geom_sf(data = shapefiles, fill = NA, color = "gray99") +
  geom_sf(data = points, color = "red") +
  theme_void() +
  coord_sf(xlim = xlim, ylim = ylim)
ggsave("../slides/img/vectorexample2.pdf")

p = ggplot() +
  geom_sf(data = shapefiles, fill = NA, color = "black") +
  geom_sf(data = points, color = "red") +
  theme_void() +
  coord_sf(xlim = xlim, ylim = ylim)
ggsave("../slides/img/vectorexample3.pdf")

f = "/Users/franvillamil/Documents/course_materials/AQM2/slides/img/vectorexample"
for(i in 1:3){
    fn = paste0(f,i,".pdf")
    system2("pdfcrop", args = c(fn,fn))
}

# ============================

# shapefile
spainshp = rnaturalearth::ne_states("Spain", returnclass = "sf")
spainshp = spainshp[-which(spainshp$region == "Canary Is."), ]

# raster and crop
spainr = geodata::worldclim_country(country = "Spain",
    var = "tavg", res = 5, path = tempdir())
spain_extent = terra::ext(spainshp)
spainr = terra::crop(spainr, spain_extent)
spainr = spainr[[1]] # only january

# low res version
spainrlowres = terra::aggregate(spainr, fact = 20,
    fun = "mean", na.rm = TRUE)
# cropped version
spainrcrop = terra::mask(spainr, vect(spainshp))
# extracting prov means and adding to shapefile
provmean = terra::extract(spainr[[1]], spainshp,
    fun = mean, na.rm = TRUE)
provmin = terra::extract(spainr[[1]], spainshp,
    fun = min, na.rm = TRUE)
spainshp$mean = provmean[[2]]
spainshp$min = provmin[[2]]

p = ggplot() +
  geom_spatraster(data = spainr) +
  scale_fill_gradient(low="white",high="black") +
  theme_void()
ggsave("../slides/img/spain_raster1.pdf")

p = ggplot() +
  geom_spatraster(data = spainrlowres) +
  scale_fill_gradient(low="white",high="black") +
  theme_void()
ggsave("../slides/img/spain_raster2.pdf")

p = ggplot() +
  geom_spatraster(data = spainr) +
  geom_sf(data = spainshp, fill = NA, color = "white") +
  scale_fill_gradient(low="white",high="black") +
  theme_void()
ggsave("../slides/img/spain_raster3.pdf")

p = ggplot() +
  geom_spatraster(data = spainrcrop) +
  geom_sf(data = spainshp, fill = NA, color = "white") +
  scale_fill_gradient(low="white",high="black") +
  theme_void()
ggsave("../slides/img/spain_raster4.pdf")

p = ggplot() +
  geom_spatraster(data = spainrcrop) +
  geom_sf(data = spainshp, aes(fill = mean), color = "white") +
  scale_fill_gradient(low="white",high="black") +
  theme_void() + labs(x="",y="") +
  annotate("text", x = 2, y = 43, label = "Province mean")
ggsave("../slides/img/spain_raster5.pdf")

p = ggplot() +
  geom_spatraster(data = spainrcrop) +
  geom_sf(data = spainshp, aes(fill = min), color = "white") +
  scale_fill_gradient(low="white",high="black") +
  theme_void() + labs(x="",y="") +
  annotate("text", x = 2, y = 43, label = "Province min")
ggsave("../slides/img/spain_raster6.pdf")

f = "/Users/franvillamil/Documents/course_materials/AQM2/slides/img/spain_raster"
for(i in 1:6){
    fn = paste0(f,i,".pdf")
    system2("pdfcrop", args = c(fn,fn))
}

