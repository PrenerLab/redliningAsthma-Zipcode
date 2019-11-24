# create zipcode file

# dependencies
library(dplyr)
library(sf)
library(tigris)

# download zcta data
zip <- zctas(state = 29, year = 2010, class = "sf") %>%
  select(ZCTA5CE10) %>%
  rename(
    ZCTA = ZCTA5CE10
  ) %>%
  st_transform(crs = 26915)

# download county data
county <- counties(state = 29, class = "sf") %>%
  filter(GEOID %in% c("29189", "29510")) %>%
  select(GEOID, NAMELSAD) %>%
  st_transform(crs = 26915)

# identify zipcode centroids
zip_centroids <- st_centroid(zip)

zip_centroids <- st_intersection(zip_centroids, county) %>%
  select(ZCTA, NAMELSAD)

st_geometry(zip_centroids) <- NULL

# join centroids to original zip data
zip <- left_join(zip, zip_centroids, by = "ZCTA") %>%
  filter(is.na(NAMELSAD) == FALSE) %>%
  select(ZCTA)

# write data
st_write(zip, "data/raw/zip/zip.shp")

# clean-up
rm(county, zip, zip_centroids)
