# setwd("~/Documents/AQM2")
options(stringsAsFactors = FALSE)

# ============================================================
# Assignment 7 -- Part 1: Exploring Spatial Data with sf
# Applied Quantitative Methods for the Social Sciences II
# ============================================================

# List of packages
library(sf)
library(spData)
library(dplyr)
library(ggplot2)

data(world)

# --------
## 1. Inspecting an sf object

# a)
class(world)
names(world)
nrow(world)
# An sf object is a regular data frame with an extra geometry column (sfc)
# that stores spatial shapes. The geometry is "sticky": dplyr operations
# (filter, mutate, select) retain it automatically.

# b)
st_crs(world)
# EPSG:4326 = WGS84 (World Geodetic System 1984), the global GPS standard.
# Coordinates are in decimal degrees of longitude and latitude.
# It is used for global datasets because it provides a common datum
# that works across all regions of the world.

# c)
unique(st_geometry_type(world))
# Geometry type: MULTIPOLYGON.
# A MULTIPOLYGON stores multiple polygons as one feature, needed when a
# country's territory is non-contiguous -- e.g., the USA (includes Alaska
# and Hawaii) or France (includes Caribbean overseas territories).

# d)
pdf("world_gdp_base.pdf")
plot(world["gdpPercap"])
dev.off()
# Western/Northern Europe, North America, and Oceania are wealthiest.
# Sub-Saharan Africa and parts of South/Southeast Asia appear poorest.

# --------
## 2. Attribute operations

# a)
africa = filter(world, continent == "Africa")
nrow(africa)
# The dataset contains nrow(africa) African countries -- slightly below the
# UN-recognised 54, reflecting missing/excluded territories in spData.
pdf("africa_gdp_base.pdf")
plot(africa["gdpPercap"])
dev.off()

# b)
world = world %>%
  mutate(pop_millions = pop / 1e6)

gdp_by_continent = world %>%
  group_by(continent) %>%
  summarise(mean_gdpPercap = mean(gdpPercap, na.rm = TRUE))

print(st_drop_geometry(gdp_by_continent))
# NOTE: summarise() on a grouped sf object unions geometries by group and
# keeps the geometry column. Use st_drop_geometry() first to get a plain
# data frame when spatial information is not needed downstream.

# c)
africa_sorted = africa %>%
  arrange(desc(gdpPercap)) %>%
  select(name_long, gdpPercap)

print(head(st_drop_geometry(africa_sorted), 5))
# Top 5 African countries by GDP per capita shown above.
# Typically: Equatorial Guinea, Gabon, Libya, Botswana, and a North African
# economy -- all reflecting either oil revenues or strong natural-resource
# export bases relative to small populations.

# --------
## 3. Simple visualization with ggplot2

# a)
ggplot(world) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita by country")
ggsave("world_gdp.pdf", width = 10, height = 5)
# Western Europe, North America, and Oceania are the wealthiest regions.
# Sub-Saharan Africa and South Asia cluster at the lowest values.

# b)
ggplot(africa) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita -- Africa")
ggsave("africa_gdp.pdf", width = 7, height = 6)
# North Africa and Southern Africa show higher GDP per capita.
# Central and West Africa (outside oil states) are at the low end.

# c)
ggplot(africa) +
  geom_sf(aes(fill = gdpPercap), color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita -- Africa (with borders)")
ggsave("africa_gdp_borders.pdf", width = 7, height = 6)
# White borders separate adjacent countries without competing with the fill
# scale, making it easier to identify individual countries and compare
# neighbours -- especially useful for smaller countries.
