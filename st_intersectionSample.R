library(tigris)
library(dplyr)
library(mapview)

counties <- counties(state = 29, class = "sf")

counties %>%
  select(STATEFP, COUNTYFP) %>%
  filter(COUNTYFP %in% c(189,510)) -> counties
counties <- st_transform(counties, crs = 26915)

zips <- tigris::zctas(state = 29, year = 2010, class = "sf")

zips <- select(zips, ZCTA5CE10)
zips <- st_transform(zips, crs = 26915)

crop <- st_crop(zips, counties)

mapview::mapview(crop)

intersect <- st_intersection(zips, counties) %>%
  select(ZCTA5CE10)

intersect <- st_collection_extract(intersect, "POLYGON")

mapview::mapview(intersect)

