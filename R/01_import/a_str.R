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

exchange_rate <- 1.33868019270251
# exchange_rate <-
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
  filter(TYPE == "Arrondissement") %>% 
  select(borough = NOM) %>% 
  st_transform(32618)

# zones <- 
#   read_sf("data/shapefiles/AffectationPU.shp") %>% 
#   st_transform(32618)

# Add population data from the census

boroughs <-
  st_join(boroughs, CTs) %>%
  group_by(borough) %>%
  summarize(dwellings=sum(dwellings))


borough_geometries <- 
  boroughs %>% 
  st_combine() %>% 
  st_union() %>% 
  st_cast("POLYGON") %>% 
  st_union() %>% 
  smoothr::fill_holes(400)

ggplot(boroughs2)+
  geom_sf(aes(fill = borough))




### Process the property file ##################################################

## Spatial join or run the raffle to assign a CT to each listing

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

## Short term FREH (2020)
FREH_2020 <- 
daily %>% 
  filter(housing, date >= "2020-01-01") %>% 
  strr_FREH(start_date = "2020-01-01", end_date = key_date,
            n_days = 73, r_cut = 18, ar_cut = 37)

GH <- 
  property %>% 
  strr_ghost(start_date = "2017-01-01", end_date = end_date)



### Add principal residence fields #############################################


## Experimental principal residence function

strr_principal_residence <- 
  function(property, daily, FREH, GH, start_date, end_date, 
           field_name = principal_residence, sensitivity = 0.1) {
    
    start_date <- as.Date(start_date, origin = "1970-01-01")
    end_date <- as.Date(end_date, origin = "1970-01-01")
    
    sens_n <- 
      round(sensitivity * as.integer((end_date - start_date + 1)))
    
    pr_table <- tibble(property_ID = property$property_ID,
                       listing_type = property$listing_type,
                       host_ID = property$host_ID,
                       housing = property$housing)
    
    pr_ML <- 
      daily %>% 
      group_by(property_ID) %>% 
      summarize(multi = if_else(
        sum(multi * (date >= start_date)) + sum(multi * (date <= end_date)) >= sens_n, 
        TRUE, FALSE))
    
    pr_n <-
      daily %>%  
      filter(status %in% c("R", "A"), date >= start_date, date <= end_date) %>% 
      count(property_ID, status) %>% 
      group_by(property_ID) %>% 
      summarize(n_available = sum(n),
                n_reserved = sum(n[status == "R"]))
    
    pr_table <- 
      pr_table %>% 
      left_join(pr_ML, by = "property_ID") %>% 
      mutate(multi = if_else(is.na(multi), FALSE, multi)) %>% 
      left_join(pr_n, by = "property_ID") %>% 
      group_by(host_ID, listing_type) %>% 
      mutate(LFRML = case_when(
        listing_type != "Entire home/apt" ~ FALSE,
        multi == FALSE                       ~ FALSE,
        n_available == min(n_available)   ~ TRUE,
        TRUE                              ~ FALSE)) %>% 
      ungroup()
    
    pr_table <- 
      pr_table %>%
      filter(LFRML == TRUE) %>%
      group_by(host_ID) %>%
      mutate(prob = sample(0:10000, n(), replace = TRUE),
             LFRML = if_else(
               sum(LFRML) > 1 & prob != max(prob), FALSE, LFRML)) %>%
      ungroup() %>% 
      select(property_ID, LFRML2 = LFRML) %>% 
      left_join(pr_table, ., by = "property_ID") %>% 
      mutate(LFRML = if_else(!is.na(LFRML2), LFRML2, LFRML)) %>% 
      select(-LFRML2)
    
    GH_list <-
      GH %>% 
      filter(date >= start_date, date <= end_date) %>% 
      pull(property_IDs) %>%
      unlist() %>%
      tibble(property_ID = .) %>% 
      group_by(property_ID) %>% 
      filter(n() >= sens_n) %>% 
      ungroup() %>% 
      pull(property_ID) %>% 
      unique()
    
    pr_table <-
      pr_table %>% 
      mutate(GH = if_else(property_ID %in% GH_list, TRUE, FALSE))
    
    pr_table <-
      FREH %>% 
      filter(date >= start_date, date <= end_date) %>% 
      group_by(property_ID) %>% 
      summarize(FREH = if_else(n() >= sens_n, TRUE, FALSE)) %>% 
      left_join(pr_table, ., by = "property_ID") %>% 
      mutate(FREH = if_else(is.na(FREH), FALSE, FREH))
    
    # Add principal_res field
    
    pr_table <- 
      pr_table %>% 
      mutate({{ field_name }} := case_when(
        housing == FALSE               ~ FALSE,
        GH == TRUE                     ~ FALSE,
        listing_type == "Shared room"  ~ TRUE,
        listing_type == "Private room" ~ TRUE,
        FREH == TRUE                   ~ FALSE,
        LFRML == TRUE                  ~ TRUE,
        multi == TRUE                     ~ FALSE,
        TRUE                           ~ TRUE)) %>% 
      select(property_ID, {{ field_name }})
    
    left_join(property, pr_table, by = "property_ID")
    
  }

property <- 
  property %>% 
  strr_principal_residence(daily, FREH, GH, key_date, key_date, 
                           principal_res, 0.5)



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



