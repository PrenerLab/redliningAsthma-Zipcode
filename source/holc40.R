# create 1940 file

# dependencies
library(dplyr)
library(measurements)
library(sf)
library(tidyr)

# load zipcode data
zip <- st_read("data/raw/zip/", crs = 26915, stringsAsFactors = FALSE)

# calulcate area of zipcodes
zip %>%
  mutate(total_area = unclass(st_area(geometry))) %>%
  mutate(total_area = conv_unit(total_area, from = "m2", to = "km2")) %>%
  select(ZCTA, total_area) -> zip

# remove attribute data
attr(zip$total_area, "units") <- NULL

# load and tidy holc data
holc <- st_read("data/raw/holc_sl_1940_bzv", stringsAsFactors = FALSE) %>%
  st_transform(crs = 26915) %>%
  select(area2) %>%
  rename(holc_grade = area2) 

# intersect zipcode and holc data
zip_holc <- st_intersection(holc, zip) %>%
  st_collection_extract()

# calculate area redlined by grade
zip_holc %>% 
  mutate(area_km2 = unclass(st_area(geometry))) %>%
  mutate(area_km2 = conv_unit(area_km2, from = "m2", to = "km2")) -> zip_holc

# remove geometry
st_geometry(zip_holc) <- NULL

# summarize and convert to wide
zip_holc %>%
  group_by(ZCTA, holc_grade) %>%
  summarise(area_km2 = sum(area_km2)) %>%
  pivot_wider(names_from = holc_grade, values_from = area_km2) %>%
  select(ZCTA, A, B, C, D) %>%
  rename(
    area_a = A,
    area_b = B,
    area_c = C,
    area_d = D
  ) %>%
  ungroup()-> zip_holc

# combine and calculate totals and proportions
left_join(zip, zip_holc, by = "ZCTA") %>%
  rowwise() %>%
  mutate(total_graded = sum(area_a, area_b, area_c, area_d, na.rm = TRUE)) %>%
  mutate(total_ab = sum(area_a, area_b, na.rm = TRUE)) %>%
  mutate(total_cd = sum(area_c, area_d, na.rm = TRUE)) %>%
  ungroup() -> zip_clean

zip_clean %>%
  mutate(
    total_graded = ifelse(total_graded == 0, NA, total_graded),
    total_ab = ifelse(total_ab == 0, NA, total_ab),
    total_cd = ifelse(total_cd == 0, NA, total_cd)
  ) %>%
  mutate(
    prop_graded = total_graded/total_area,
    prop_ab = total_ab/total_area,
    prop_cd = total_cd/total_area
  ) %>%
  select(ZCTA, total_area, total_graded, prop_graded, total_ab, prop_ab, total_cd, prop_cd, 
         area_a, area_b, area_c, area_d) -> zip_clean

# join
zip <- select(zip, ZCTA)
zip <- left_join(zip, zip_clean, by = "ZCTA")

# write data
zip <- st_transform(zip, crs = 4269)
st_write(zip, dsn = "data/clean/STL_HOLC_zip40.geoJSON")
