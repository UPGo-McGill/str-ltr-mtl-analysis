#### AIRDNA IMPORT ######################################################

### Load libraries & data #############################################################

library(tidyverse)
library(upgo)
library(strr)
library(future)
library(sf)
library(lubridate)
library(readxl)

load("data/montreal.Rdata")

# Sys.setenv(CM_API_KEY = 'CensusMapper_3f15611c3eafd43d50e284e597cdc606')






### Set global variables #######################################################

memory.limit(size = 48000)
plan(multiprocess, workers = 4)

end_date <- as.Date(max(daily$date))
key_date <- as.Date("2020-03-14")

exchange_rate <- 1
#exchange_rate <- 
#  map_dbl(0:11, ~{ 
#    ex_table <- 
#      fixerapi::fixer_historical(
#        date = (end_date %m-% months(.x)), symbols = c("CAD", "USD"))
#    ex_table[1,]$value / ex_table[2,]$value
#  }) %>% mean()






### Build geometries ###########################################################

## Montreal CSD

city <-
  cancensus::get_census(dataset = "CA16", regions = list(CSD = "2466023"),
                        level = "CSD", geo_format = "sf") %>% 
  st_transform(32618) %>% 
  st_set_agr("constant")






### Census import ##############################################################

CTs <-
  cancensus::get_census(
    dataset = "CA16", regions = list(CSD = "2466023"), level = "CT",
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(GeoUID, CSD_UID, Population, Dwellings) %>% 
  set_names(c("GeoUID", "CMA_UID", "population", "dwellings", "geometry")) %>% 
  st_set_agr("constant")






### Import data from city ######################################################

## Import boroughs, zoning and parcels

boroughs <-
  read_sf("data/shapefiles/montreal_boroughs_2019.shp") %>% 
  select(borough = NOM) %>% 
  # st_set_crs(2950) %>%  ??
  st_transform(32618)


# Add population data from the census

boroughs <- 
  st_join(CTs, boroughs) %>% 
  group_by(borough) %>% 
  summarize(dwellings=sum(dwellings))


borough_geometries <- 
  boroughs %>% 
  st_combine() %>% 
  st_union() %>% 
  st_cast("POLYGON") %>% 
  st_union() %>% 
  smoothr::fill_holes(400)






### Process the property file ##################################################

## Spatial join or run the raffle to assign a DA to each listing

property <-
  property %>% 
  strr_raffle(CTs, GeoUID, dwellings, seed = 1)






### Process multilistings, alculate FREH and GH listings #######################

daily <- 
  daily %>% 
  strr_multi(host)

FREH <- 
  daily %>% 
  strr_FREH("2017-01-01", end_date) %>%
  filter(FREH == TRUE) %>% 
  select(-FREH)

GH <- 
  property %>% 
  strr_ghost(start_date = "2017-01-01", end_date = end_date)






### Add seasonal fields ########################################################
# 
# season_start <- as.Date("2019-05-01")
# season_end <- as.Date("2019-09-30")
# 
# property <-
#   daily %>%
#   filter(listing_type == "Entire home/apt", status %in% c("R", "A")) %>% 
#   group_by(property_ID) %>%
#   summarize(
#     n_ar_2019 = sum(between(date, season_start, season_end)),
#     n_r_2019  = sum(between(date[status == "R"], season_start, season_end)),
#     n_ar_2018 = sum(between(date, 
#                             season_start - years(1), 
#                             season_end - years(1))),
#     n_r_2018  = sum(between(date[status == "R"], 
#                             season_start - years(1), 
#                             season_end - years(1))),
#     n_ar_2017 = sum(between(date, 
#                             season_start - years(2),
#                             season_end - years(2))),
#     n_r_2017  = sum(between(date[status == "R"],
#                             season_start - years(2),
#                             season_end - years(2))),
#     seasonal_2019 = if_else(n_ar_2019 >= 120 & n_r_2019 >= 60, TRUE, FALSE),
#     seasonal_2018 = if_else(n_ar_2018 >= 120 & n_r_2018 >= 60, TRUE, FALSE),
#     seasonal_2017 = if_else(n_ar_2017 >= 120 & n_r_2017 >= 60, TRUE, FALSE)
#   ) %>% 
#   select(property_ID, seasonal_2019:seasonal_2017) %>% 
#   left_join(property, ., by = "property_ID")

### Save files #################################################################

save(city, daily, CTs, FREH, GH, host,
     property, end_date,
     key_date, exchange_rate, #season_start, season_end,
     boroughs, borough_geometries,
     file = "data/montreal_str_processed_a.Rdata")

