#### LTR MATCHES IMPORT ######################################################

### Load libraries #############################################################
library(tidyverse)
library(future)
library(sf)
library(strr)





### Set global variables #######################################################

memory.limit(size = 48000)
plan(multiprocess, workers = 4)







### Load data #############################################################
load("data/montreal.Rdata")
load("data/mtl_ab_final.Rdata")

ab_matches <- mtl_ab_final
rm(mtl_ab_final)




### Clean matches df ######################################################
ab_matches$x_name <- 
  str_replace_all(ab_matches$x_name, c("/Volumes/Data/Scrape photos/mtl/ab/|.jpg" = ""))

ab_matches$y_name <- 
  str_replace_all(ab_matches$y_name, c("/Volumes/Data/Scrape photos/mtl/ab/|.jpg" = ""))

ab_matches <- 
  ab_matches %>% 
  select(x_name, y_name)

### create a list column with all properties to which it matched ##########

# Since I'm unable to do that, here's a left_join which duplicates a lot

property <- 
  left_join(property, rename(ab_matches, property_ID = x_name, ab_match = y_name), by = "property_ID")


### exploring #############################################################

property %>%
  filter(!is.na(ab_match)) %>%
  count(ab_match) %>%
  filter(n == 5) %>%
  arrange(n) %>%
  View()

property %>% 
  filter(ab_match == "ab-31121720") %>% 
  View()

daily %>% 
  filter(property_ID %in% (property %>% filter(property_ID == "ab-11356766"))$property_ID, status == "B" | status == "A") %>% 
  count(date) %>% 
  count(n) %>% 
  arrange(-n)

city <-
  cancensus::get_census(dataset = "CA16", regions = list(CSD = "2466023"),
                        level = "CSD", geo_format = "sf") %>% 
  st_transform(32618) %>% 
  st_set_agr("constant")

property %>% 
  filter(ab_match == "ab-37835100") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326)) %>% 
  ggplot()+
  geom_sf(data = city)+
  geom_sf()


