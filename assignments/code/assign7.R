# ============================================================
# Assignment 7: Spatial Data I
# Applied Quantitative Methods for the Social Sciences II
# ============================================================

# List of packages
library(sf)
library(spData)
library(dplyr)
library(tidyr)
library(ggplot2)

# ==========================================================================
# Part 1: In-Class (Exploring Spatial Data with sf)
# ==========================================================================

data(world)

# ----------------------------------------------------------
## 1. Inspecting an sf object

# a)
class(world)
names(world)
nrow(world)

# b)
st_crs(world)

# c)
unique(st_geometry_type(world))

# d)
plot(world)
pdf("world_gdp_base.pdf")
plot(world["gdpPercap"])
dev.off()

# ----------------------------------------------------------
## 2. Attribute operations

# a)
africa = world %>% filter(continent == "Africa")
# or: filter(world, continent == "Africa")
# or: subset(world, continent == "Africa")

nrow(africa)
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

# c)
africa_sorted = africa %>%
  arrange(desc(gdpPercap)) %>%
  select(name_long, gdpPercap)

print(head(st_drop_geometry(africa_sorted), 5))

# ----------------------------------------------------------
## 3. Simple visualization with ggplot2

# a)
ggplot(world) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita by country")
ggsave("world_gdp.pdf", width = 10, height = 5)

# b)
ggplot(africa) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita -- Africa")
ggsave("africa_gdp.pdf", width = 7, height = 6)

# c)
ggplot(africa) +
  geom_sf(aes(fill = gdpPercap), color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80",
                       name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita -- Africa (with borders)")
ggsave("africa_gdp_borders.pdf", width = 7, height = 6)

# ==========================================================================
# Part 2: Take-Home (Point Data and Spatial Joins)
# ==========================================================================

events = read.csv("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/spatial/conflict_events.csv")


# ----------------------------------------------------------
## 1. Converting tabular data to sf

# a)
events_sf = st_as_sf(events,
                     coords = c("longitude", "latitude"),
                     crs = 4326)
class(events_sf)
st_crs(events_sf)
plot(events_sf)
plot(events_sf["event_id"])
plot(events_sf["event_id"], pch = ".", col = "black")

# b)
nrow(events_sf)
table(events_sf$event_type)

# c)
map = ggplot() +
  geom_sf(data = world, fill = "grey90", color = "white", linewidth = 0.2) +
  geom_sf(data = events_sf, aes(color = event_type),
          size = 0.5, alpha = 0.4) +
  theme_void() +
  labs(title = "Armed conflict events", color = "Event type")
ggsave("conflict_events_map.pdf", width = 10, height = 5)

### Note: you can also limit map using coord_sf(xlim, ylim)
map = map +
  coord_sf(
    xlim = c(-15, 20), # from 15ºW to 20ºE
    ylim = c(-10, 30) # from 10ºS to 30ºN
  )
ggsave("conflict_events_map_reduced.pdf", width = 10, height = 5)

# ----------------------------------------------------------
## 2. Spatial join: events to countries

# a)
st_crs(events_sf) == st_crs(world)

events_joined = st_join(events_sf,
  world[, c("name_long", "continent", "gdpPercap")])

nrow(events_joined)
nrow(events_sf)

# b)
n_unmatched = sum(is.na(events_joined$name_long))
n_unmatched
round(n_unmatched / nrow(events_joined), 3)

# c)
events_by_country = events_joined %>%
  filter(!is.na(name_long)) %>%
  group_by(name_long) %>%
  summarise(n_events = n(),
            total_fatalities = sum(fatalities, na.rm = TRUE)) %>%
  arrange(desc(n_events))

print(head(st_drop_geometry(events_by_country), 10))

# ----------------------------------------------------------
## 3. Choropleth of conflict intensity

# a)
events_by_country_df = st_drop_geometry(events_by_country)

world_conflict = world %>%
  left_join(events_by_country_df, by = "name_long") %>%
  mutate(n_events = replace_na(n_events, 0),
         total_fatalities = replace_na(total_fatalities, 0))

nrow(world_conflict) == nrow(world)

# b)
ggplot(world_conflict) +
  geom_sf(aes(fill = n_events), color = "white", linewidth = 0.2) +
  scale_fill_distiller(palette = "Reds", direction = 1,
                       name = "N events", na.value = "grey80") +
  theme_void() +
  labs(title = "Armed conflict events by country")
ggsave("conflict_by_country.pdf", width = 10, height = 5)

# c)
ggplot(world_conflict) +
  geom_sf(aes(fill = log1p(n_events)), color = "white", linewidth = 0.2) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       name = "Log(events+1)", na.value = "grey80") +
  theme_void() +
  labs(title = "Armed conflict events by country (log scale)")
ggsave("conflict_log_map.pdf", width = 10, height = 5)

# ----------------------------------------------------------
## 4. Bonus, optional: calculate distance to capital in Nigeria and run lm

# from scratch
nigeria = events_sf %>% 
  st_join(world[, c("name_long")]) %>%
  filter(name_long == "Nigeria")

# quick check
plot(nigeria[1])

# we first need to locate Abuja
abuja = data.frame(city = "abuja", lon = 7.5, lat = 9) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# let's check
st_crs(nigeria)$epsg
st_crs(abuja)$epsg
ggplot(nigeria) +
  geom_sf() +
  geom_sf(data = abuja, color = "red")

### calculate distance
# transform to UTM projection (zone 32N: https://epsg.io/?q=nigeria+utm)
nigeria_m = st_transform(nigeria, 32632)
abuja_m = st_transform(abuja, 32632)

# distance (in m)
# first try if it works
st_distance(nigeria_m[1:3,], abuja_m)
# now just save it in the dataframe
nigeria$dist_abuja = as.numeric(st_distance(nigeria_m, abuja_m))

# transform
nigeria = nigeria %>%
  mutate(log_fatalities = log(fatalities + 1),
    dist_abuja_log_km = log((dist_abuja+1)/1000))

# models
m1 = lm(fatalities ~ dist_abuja, data = nigeria)
m2 = lm(log_fatalities ~ dist_abuja_log_km, data = nigeria)
m3 = lm(log_fatalities ~ dist_abuja_log_km + event_type,
    data = nigeria)
m4 = lm(log_fatalities ~ dist_abuja_log_km * event_type,
    data = nigeria)
library(modelsummary)
modelsummary(list(m1,m2,m3,m4),stars=TRUE)