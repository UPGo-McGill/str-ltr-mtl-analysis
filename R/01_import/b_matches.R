#### LTR MATCHES IMPORT ######################################################

### Load libraries #############################################################
library(tidyverse)
library(sf)
library(ggmap)
library(readxl)
library(future)






### Set global variables #######################################################

memory.limit(size = 48000)
plan(multiprocess, workers = 4)

# register_google(key = "AIzaSyDxSAajMCw_k8E7GsKCFKBMP2RhofvLMX8", write = TRUE)





### Load data #############################################################

#### load data to prepare LTR weekly update ####
load("data/kj_r_mtl.Rdata")
load("data/Canada_cl_2020_06_22.Rdata") ### load new data from CL and KJ
load("data/Canada_kj_2020_06_22.Rdata")

# data on image mataching
load("data/mtl_kj_matches_2020-06-22.Rdata")
load("data/mtl_cl_matches_2020-06-22.Rdata")

# load the data already filtered with mutate_geocode(location) to have the geometry from the previous week
load("data/kj_geo.Rdata")

# rename the dfs
kj_nogeo <- Canada_kj_2020_06_22
cl <- Canada_cl_2020_06_22
rclalq <- kj_r_mtl

rm(Canada_kj_2020_06_22)
rm(kj_r_mtl)
rm(Canada_cl_2020_06_22)


# import data from AirDNA (already raffled, info on FREH, GH, etc.)
load("data/montreal_str_processed_a.Rdata")




### Get geometry for KJ listings ###########################################

# merge the geometry (lat, lon) of previous listings to the new df
kj_geo <- distinct(merge(kj_nogeo, kj_geo[,c("id", "lat", "lon")], by="id", all.x=TRUE))

# retrieve the geometry of the listings that are newly scraped
kj_newgeo <-
  kj_geo %>%
  filter(is.na(lon), is.na(lat), !is.na(location)) %>%
  mutate_geocode(location)
#
kj_newgeo <-
  kj_newgeo %>%
  mutate(lat = lat1,
         lon = lon1) %>%
  distinct(id, .keep_all = T)

# add the new geometry to the df that will combine geometry of previous listings, and geometry of the new ones.

kj_geo <- left_join(kj_geo, kj_newgeo[,c("id", "lat", "lon")], by = "id")
kj_geo <- kj_geo %>%
  mutate(lat = if_else(is.na(lat.x), lat.y, lat.x),
         lon = if_else(is.na(lon.x), lon.y, lon.x)) %>%
  select(-c(lat.x, lat.y, lon.x, lon.y))

# assign it to kj to easily play with it later. kj_geo will be saved in the end of this file to be used next weeks.
kj <- kj_geo







#### needed stringr ############################################################

# clean the image matching ids and put it in only one DF

kj_matches$x_name <- 
  str_replace_all(kj_matches$x_name, c("/Volumes/Data/Scrape photos/mtl/ab/|.jpg" = ""))

kj_matches$y_name <- 
  str_replace_all(kj_matches$y_name, c("/Volumes/Data/Scrape photos/mtl/kj/|-[:digit:]+.jpg" = ""))

cl_matches$x_name <- 
  str_replace_all(cl_matches$x_name, c("/Volumes/Data/Scrape photos/mtl/ab/|.jpg" = ""))

cl_matches$y_name <- 
  str_replace_all(cl_matches$y_name, c("/Volumes/Data/Scrape photos/mtl/cl/|-[:digit:]+.jpg" = ""))


matches <- rbind(kj_matches, cl_matches)
rm(kj_matches)
rm(cl_matches)

# get created date for rclalq file

rclalq$posted <- 
  str_sub(rclalq$posted, start= 16L)

rclalq$posted <- 
  as.numeric(str_replace_all(rclalq$posted, c("jours|jour" = "", 
                                              "plus d'un mois|un mois" = "30",
                                              "[:digit:]+ heures|une heure|[:digit:]+ minutes|moins d'une minute|une minute" = "0",
                                              "un" = "1")))

# arrange kijiji bedrooms column as numeric

kj$bedrooms <- 
  str_replace(kj$bedrooms, pattern = "Bachelor/Studio|Studio", replacement = "0")

kj$bedrooms <- 
  as.numeric(str_sub(kj$bedrooms, end= 1L))

# arrange kijiji bathrooms column as numeric
kj$bathrooms <- 
  str_sub(kj$bathrooms, end= 3L)

kj$bathrooms <- 
  as.numeric(str_replace(kj$bathrooms, pattern = "Ut|U|\\+", replacement = ""))





#### Prepare the 3 dataframes for binding #############################################

cl <- cl %>% 
  select(id, created, scraped, price, city, location, bedrooms, bathrooms, furnished, title, text) %>% 
  separate(location, c("lon", "lat"), sep = ";") %>% 
  mutate(bedrooms = as.numeric(bedrooms),
         bathrooms = as.numeric(bathrooms),
         created = as.Date(created),
         scraped = as.Date(scraped),
         lat = as.numeric(lat), 
         lon = as.numeric(lon),
         short_long = NA,
         location = NA,
         kj = F) %>% 
  filter(!is.na(lon), !is.na(lat)) %>% 
  st_as_sf(coords = c("lat", "lon"), crs = st_crs(4326))

cl$city <-  # change cities character for CL, because they are that way in KJ
  str_replace_all(cl$city, c("montreal" = "Montreal", 
                             "vancouver" = "Vancouver",
                             "toronto" = "Toronto"))

kj <- kj %>% 
  select(id, short_long, created, scraped, price, city, location, bedrooms, bathrooms, furnished, lat, lon, title, text) %>% 
  mutate(bedrooms = as.numeric(bedrooms),
         bathrooms = as.numeric(bathrooms),
         kj = T)

rclalq <- rclalq %>% 
  filter(location == "VilledeMontréal") %>% 
  mutate(city = "Montreal",
         scraped = as.Date(date, tryFormats = c("%m/%d/%Y")),
         details = as.numeric(substr(details, 1, 1)),
         bedrooms = (details - 2),
         bedrooms = case_when(bedrooms <= 0 ~ 0, TRUE ~ bedrooms),
         created = scraped - posted,
         location = address,
         short_long = NA,
         bathrooms = NA,
         furnished = NA,
         text = NA,
         kj = T) %>% 
  select(id, short_long, created, scraped, price, city, location, bedrooms, bathrooms, furnished, lat, lon, title, kj, text)


# merge kj with rclalq

kj <- 
  rclalq %>% 
  filter(!id %in% !!kj$id) %>% 
  rbind(kj)

kj <- left_join(kj, rclalq[,c("id", "bedrooms", "price")], by = "id")

kj <- kj %>% 
  mutate(bedrooms = if_else(is.na(bedrooms.x), bedrooms.y, bedrooms.x),
         price = if_else(is.na(price.x), price.y, price.x)) %>% 
  select(-c(price.x, price.y, bedrooms.x, bedrooms.y))


# kj as sf 
kj <- kj %>% 
  filter(!is.na(lon),
         !is.na(lat)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326))






### bind all LTRs in 1 table, and create a sf table for mtl ###############################

ltr <- rbind(kj, cl)

ltr_mtl <- ltr %>% 
  filter(city == "Montreal") %>% 
  st_transform(32618)

# import montreal's boroughs geograhy
boroughs <-
  read_sf("data/shapefiles/montreal_boroughs_2019.shp") %>% 
  select(borough = NOM) %>%
  st_set_crs(4326) %>% 
  st_transform(32618)

ltr_mtl <- ltr_mtl %>% 
  st_join(boroughs)



# import ensus data to LTR table
CTs <-
  cancensus::get_census(
    dataset = "CA16", regions = list(CSD = c("2466023", "5915022", "3520005")), level = "CT",
    geo_format = "sf") %>% 
  st_transform(4326) %>% 
  select(GeoUID, CSD_UID, Population, Dwellings) %>% 
  set_names(c("GeoUID", "CMA_UID", "population", "dwellings", "geometry")) %>% 
  select(GeoUID, dwellings, CMA_UID) %>% 
  st_set_agr("constant")

CTs$CMA_UID <- 
  str_replace_all(CTs$CMA_UID, c("2466023" = "Montreal",
                                 "3520005" = "Toronto",
                                 "5915022" = "Vancouver"))

ltr_cts <- st_join(ltr, CTs)





### add a column with matched LTR IDs to STR properties and vice versa #######

property <- 
  left_join(property, rename(matches[,c("x_name", "y_name")], property_ID = x_name, ltr_id = y_name), by = "property_ID")

ltr_mtl <- 
  left_join(ltr_mtl, rename(matches[,c("x_name", "y_name")], ab_id = x_name, id = y_name), by = "id")






#### rename columns in matches ##############################################

matches <- 
  matches %>% 
  rename(ab_id = x_name,
         ltr_id = y_name)






#### save ####

save(ltr, ltr_mtl, ltr_cts, CTs, rclalq, matches,
     file = "data/ltr_matches.Rdata")

save(kj_geo,
     file = "data/kj_geo.Rdata")

save(city, daily, CTs, FREH, GH, host,
     property, end_date,
     key_date, exchange_rate, #season_start, season_end,
     boroughs, borough_geometries,
     file = "data/montreal_str_processed_b.Rdata")

