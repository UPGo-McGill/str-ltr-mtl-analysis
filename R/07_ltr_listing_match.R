#### 07 LTR LISTING MATCH ######################################################

#' This script produces the TKTK objects. The script is extremely time-consuming and 
#' memory-intensive to run, so it should only be rerun when image matching needs 
#' to be rebuilt from scratch. In addition, the script downloads hundreds of 
#' thousands of photos to a specified folder, so it requires approximately 50 GB 
#' of free storage space.

#' External dependencies:
#' - Access to the UPGo database
#' - Listings scraped from Kijiji and Craigslist with upgo::upgo_scrape_kj and
#'   upgo::upgo_scrape_cl

source("R/01_startup.R")


# Load previous data ------------------------------------------------------

load("data/str_raw.Rdata")

load("data/matches.Rdata")

load("data/ltr.Rdata")

dl_location <- "/Volumes/Data/Scrape photos/mtl"

rm(daily, host)


# Clean up matches --------------------------------------------------------

cl_matches <- 
  cl_matches %>% 
  filter(confirmation == "match") %>%
  mutate(
    x_name = str_replace_all(x_name, paste0(dl_location, "/ab/|.jpg"), ""),
    y_name = str_replace_all(x_name, 
                             paste0(dl_location, "/cl/|-[:digit:]+.jpg"), "")
    )

kj_matches <- 
  kj_matches %>% 
  filter(confirmation == "match") %>% 
  mutate(
    x_name = str_replace_all(x_name, paste0(dl_location, "/ab/|.jpg"), ""),
    y_name = str_replace_all(x_name, 
                             paste0(dl_location, "/kj/|-[:digit:]+.jpg"), "")
    )

matches <- 
  bind_rows(kj_matches, cl_matches) %>% 
  select(property_ID = x_name, ltr_ID = y_name)

rm(cl_matches, kj_matches)


# Connect STR and LTR listings --------------------------------------------

property_nest <-
  property %>% 
  st_drop_geometry() %>% 
  left_join(matches, by = "property_ID") %>% 
  select(property_ID, ltr_ID)

property <- 
  property_nest %>% 
  group_by(property_ID) %>% 
  summarize(ltr_ID = list(ltr_ID)) %>% 
  left_join(property, .)

ltr_nest <- 
  ltr %>% 
  st_drop_geometry() %>% 
  left_join(matches, by = c("id" = "ltr_ID")) %>% 
  select(id, property_ID)

ltr <-
  ltr_nest %>% 
  group_by(id) %>% 
  summarize(property_ID = list(property_ID)) %>% 
  left_join(ltr, .)

rm(property_nest, ltr_nest)



### Clean matches df ######################################################
ab_matches$x_name <- 
  str_replace_all(ab_matches$x_name, c("/Volumes/Data/Scrape photos/mtl/ab/|.jpg" = ""))

ab_matches$y_name <- 
  str_replace_all(ab_matches$y_name, c("/Volumes/Data/Scrape photos/mtl/ab/|.jpg" = ""))

ab_matches <- 
  ab_matches %>% 
  select(x_name, y_name)













# Save output -------------------------------------------------------------

save(property, ltr, matches)


#### save ###################################################################

save(ltr, ltr_mtl, ltr_mtl_das, DAs, kj_rclalq, matches,
     file = "data/ltr_matches.Rdata")

save(kj_geo,
     file = "data/kj_geo.Rdata")

save(city, daily, DAs, FREH, 
     GH, host, property,
     key_date, exchange_rate,
     boroughs,
     file = "data/str_montreal.Rdata")

