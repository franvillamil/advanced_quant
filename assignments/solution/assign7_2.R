# setwd("~/Documents/AQM2")
options(stringsAsFactors = FALSE)

# ============================================================
# Assignment 7 -- Part 2: Point Data and Spatial Joins
# Applied Quantitative Methods for the Social Sciences II
# ============================================================

# List of packages
library(sf)
library(spData)
library(dplyr)
library(tidyr)
library(ggplot2)

data(world)

# NOTE: Replace this block with real data once conflict_events.csv is available.
# Synthetic data for solution demonstration purposes.
set.seed(42)
n = 500
events = data.frame(
  event_id = 1:n,
  year = sample(2018:2022, n, replace = TRUE),
  longitude = runif(n, -20, 55),   # Africa bounding box
  latitude = runif(n, -35, 38),
  fatalities = rpois(n, lambda = 5),
  event_type = sample(c("Battles", "Violence against civilians",
                        "Protests", "Remote violence"), n,
                      replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1))
)

# --------
## 1. Converting tabular data to sf

# a)
events_sf = st_as_sf(events,
                     coords = c("longitude", "latitude"),
                     crs = 4326)

class(events_sf)
st_crs(events_sf)
# st_as_sf() promotes a data frame to sf by creating a geometry column from
# the columns named in coords (longitude = x, latitude = y). crs = 4326
# assigns EPSG:4326 (WGS84) so R knows how to interpret the degree values.
# The original longitude/latitude columns are replaced by the geometry column.

# b)
nrow(events_sf)
table(events_sf$event_type)
# Battles are the most common event type in this dataset (40% of synthetic
# events). With real ACLED data, Battles and Violence against civilians
# typically dominate in active conflict zones.

# c)
ggplot() +
  geom_sf(data = world, fill = "grey90", color = "white", linewidth = 0.2) +
  geom_sf(data = events_sf, aes(color = event_type),
          size = 0.5, alpha = 0.4) +
  theme_void() +
  labs(title = "Armed conflict events", color = "Event type")
ggsave("conflict_events_map.pdf", width = 10, height = 5)
# Synthetic events are uniformly distributed (including ocean areas).
# Real ACLED events concentrate in the Sahel, Horn of Africa, DRC, and Nigeria.

# --------
## 2. Spatial join: events to countries

# a)
st_crs(events_sf) == st_crs(world)

events_joined = st_join(events_sf, world[, c("name_long", "continent", "gdpPercap")])

nrow(events_joined)
nrow(events_sf)
# st_join() uses geometric relationships (point-in-polygon by default) to match
# rows -- no shared key column is needed. CRS equality must be checked first:
# if the layers use different coordinate systems, spatial operations are
# meaningless. The row count stays the same because the default join is 'left':
# all points are retained, with NA for country attributes when no polygon matches.

# b)
n_unmatched = sum(is.na(events_joined$name_long))
n_unmatched
round(n_unmatched / nrow(events_joined), 3)
# Two reasons a point may not match any polygon:
# 1. The point falls in an ocean or sea area outside all country polygons
#    (common with synthetic uniform coordinates or real at-sea events).
# 2. The point lies exactly on a border where floating-point gaps between
#    adjacent polygons prevent assignment to either country.

# c)
events_by_country = events_joined %>%
  filter(!is.na(name_long)) %>%
  group_by(name_long) %>%
  summarise(n_events = n(),
            total_fatalities = sum(fatalities, na.rm = TRUE)) %>%
  arrange(desc(n_events))

print(head(st_drop_geometry(events_by_country), 10))
# Synthetic data: ranking reflects country land area (large countries receive
# more uniformly random points). Real ACLED data: ranking reflects conflict
# intensity -- typically Ethiopia, Nigeria, DRC, Somalia, Mali dominate.

# --------
## 3. Choropleth of conflict intensity

# a)
events_by_country_df = st_drop_geometry(events_by_country)

world_conflict = world %>%
  left_join(events_by_country_df, by = "name_long") %>%
  mutate(n_events = replace_na(n_events, 0),
         total_fatalities = replace_na(total_fatalities, 0))

nrow(world_conflict) == nrow(world)
# replace_na() converts NA event counts to 0 for countries with no events,
# ensuring the choropleth colour scale anchors correctly at zero rather than
# treating zero-event countries as missing data.

# b)
ggplot(world_conflict) +
  geom_sf(aes(fill = n_events), color = "white", linewidth = 0.2) +
  scale_fill_distiller(palette = "Reds", direction = 1,
                       name = "N events", na.value = "grey80") +
  theme_void() +
  labs(title = "Armed conflict events by country")
ggsave("conflict_by_country.pdf", width = 10, height = 5)
# The country choropleth should show the same geographic pattern as the
# event dot map from 1c -- countries dense with points here appear darkest.

# c)
ggplot(world_conflict) +
  geom_sf(aes(fill = log1p(n_events)), color = "white", linewidth = 0.2) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       name = "Log(events+1)", na.value = "grey80") +
  theme_void() +
  labs(title = "Armed conflict events by country (log scale)")
ggsave("conflict_log_map.pdf", width = 10, height = 5)
# The log transformation compresses the right tail: in the raw map, a few
# high-conflict countries dominate the colour scale and most appear near-zero.
# The log map reveals variation among low-to-medium conflict countries that
# the raw map suppresses. log1p(0) = 0, so zero-event countries still anchor
# the bottom of the scale.

# --------
## 4. Discussion

# a)
# A key limitation of point-in-polygon spatial joins is that points falling
# exactly on borders or just outside polygons due to coordinate imprecision
# receive NA country attributes. This is especially problematic near coastlines
# where small geocoding errors (a few hundred meters) can place a point in the
# sea. Practical fixes: st_nearest_feature() to snap unmatched points to the
# closest polygon, or st_buffer() to slightly expand polygons to catch near-miss
# points.

# b)
# st_join() matches rows using geometric relationships (e.g., point inside
# polygon) -- no shared key column is required. left_join() matches on shared
# attribute values (e.g., a country name or ISO code column). Prefer st_join()
# when data have coordinates but no reliable common key (typical when combining
# geocoded events with administrative polygons). Prefer left_join() when a
# reliable identifier already exists in both datasets, because key-based joins
# are faster, deterministic, and independent of coordinate precision.
