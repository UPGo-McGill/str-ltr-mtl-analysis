#### 07 LTR LISTING MATCH ######################################################

#' This script produces the `str_processed.Rdata` and `matches_processed.Rdata` 
#' objects, and updates the `ltr.Rdata` object. The script runs relatively
#' quickly, and should be rerun anytime the raw STR, LTR or image matching data
#' changes.

source("R/01_startup.R")


# Load previous data ------------------------------------------------------

load("data/str_raw.Rdata")

load("data/matches_raw.Rdata")

load("data/ltr.Rdata")

dl_location <- "/Volumes/Data/Scrape photos/mtl"


# Clean up matches --------------------------------------------------------

cl_matches <- 
  cl_matches %>% 
  filter(confirmation == "match") %>%
  mutate(
    x_name = str_replace_all(x_name, paste0(dl_location, "/ab/|.jpg"), ""),
    y_name = str_replace_all(y_name, 
                             paste0(dl_location, "/cl/|-[:digit:]+.jpg"), "")
    )

kj_matches <- 
  kj_matches %>% 
  filter(confirmation == "match") %>% 
  mutate(
    x_name = str_replace_all(x_name, paste0(dl_location, "/ab/|.jpg"), ""),
    y_name = str_replace_all(y_name, 
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


# Save output -------------------------------------------------------------

save(property, daily, host, file = "data/str_processed.Rdata")
save(ltr, file = "data/ltr.Rdata")
save(matches, ab_matches, file = "data/matches_processed.Rdata")
