#### 60 MATCHED PROPRETY EXPORT ################################################

#' This script produces and exports the CSV with information about STR/LTR
#' matches. It runs quickly.
#' 
#' Output:
#' - `listing_matches.csv`
#' 
#' Script dependencies:
#' - `02_geometry_import.R`
#' - `05_cmhc_data_import.R`
#' - `07_ltr_listing_match.R`
#' - `09_str_processing.R`
#' - `11_FREH_model.R`
#' 
#' External dependencies:
#' - `kj_with_landlord.rds`: A file matching Kijiji listings with role numbers
#'   from the UEF.

source("R/01_startup.R")
library(rebus)

load("output/str_processed.Rdata")
load("output/geometry.Rdata")
load("output/ltr_processed.Rdata")


# Import landlord data ----------------------------------------------------

kj_landlord <- 
  readRDS("output/kj_with_landlord.rds") %>% 
  select(kj_id, Numero_de_matricule) %>%
  rename(id = kj_id, roll_number = Numero_de_matricule)

# Clean ltr locations ---------------------------------------------------------------------

street_no_pat <- START %R% one_or_more(DGT)
postal_code_pat <- WRD %R% DGT %R% WRD %R% optional(SPC) %R% DGT %R% WRD %R% DGT
street_pat <- START %R% one_or_more(WRD) %R% optional(SPC) %R% one_or_more(optional(WRD))

ltr <-
ltr %>% 
  mutate(street_no = str_extract(location, pattern = street_no_pat),
         location = str_remove(location, pattern = street_no_pat %R% optional(" ")),
         
         postal_code = str_extract(location, pattern = postal_code_pat),
         location = str_remove(location, pattern = postal_code_pat),
         
         borough_loc = str_extract(location, pattern = boroughs$borough),
         location = str_remove(location, pattern = boroughs$borough),
         
         location = str_remove_all(location, pattern = "Canada"),
         location = str_remove_all(location, pattern = or("Montréal", "Montreal", "montreal", "montréal")),
         location = str_remove_all(location, pattern = or("QC", "Qc")),
         
         street = str_extract(location, pattern = "[^,]+"),
         street = str_trim(street),
         location = str_remove(location, pattern = "[^,]+" %R% optional(", ") %R% optional(", ") %R% optional(", ")),
         
         location = str_remove_all(location, pattern = ",")
  ) %>% 
  rename(location_related = location) %>% 
  mutate(location = if_else(!is.na(street_no), str_c(street_no, " ", street, ", ", postal_code),
                                   postal_code),
         location = if_else(is.na(postal_code), str_c(street_no, " ", street), location),
         location = if_else(is.na(street_no) & is.na(postal_code), street, location))

# Prepare the basic of the table ----------------------------------------------------------
listings_info <- 
  property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID), scraped >= "2020-01-01") %>% 
  select(property_ID, host_ID, created, scraped,
         housing, listing_type, borough)


# Add a location and roll_number column --------------------------------------------------
ltr$location <-
  str_replace_all(ltr$location, c("Ã©" = "é",
                              "SÃ¨" = "è",
                              "Ã¨" = "è"))

locations_roll_number <- 
  ltr %>% 
  as_tibble() %>% 
  arrange(desc(scraped)) %>% 
  distinct(id, .keep_all = T) %>% 
  filter(!is.na(property_ID)) %>% 
  unnest(property_ID) %>% 
  select(id, property_ID, location) %>% 
  left_join(kj_landlord) %>% 
  select(-id) %>% 
  distinct() %>% 
  filter(!is.na(location) | !is.na(roll_number)) %>% 
  rename(property_ID = property_ID) %>% 
  group_by(property_ID) %>% 
  summarize(roll_number = list(roll_number),
            location = list(location))


listings_info <- 
left_join(listings_info, locations_roll_number)


# Adding a FREH column -----------------------------------------------------------------
# If listing has been following a FREH pattern at least 3 months once in its lifetime
FREH <- 
  daily %>% 
  filter(FREH_3 >= 0.5) %>% 
  select(property_ID) %>% 
  distinct() %>% 
  mutate(FREH = T)

listings_info <- 
  left_join(listings_info, FREH) %>% 
  mutate(FREH = if_else(is.na(FREH), F, T))

# Adding a GH column -------------------------------------------------------------------
# If listing has been part of a GH once in its lifetime
GH_properties <- 
  GH %>% 
  st_drop_geometry() %>% 
  distinct(ghost_ID, .keep_all = T) %>% 
  unnest(property_IDs) %>% 
  distinct(property_IDs) %>% 
  rename(property_ID = property_IDs) %>% 
  mutate(GH = T)

listings_info <- 
  left_join(listings_info, GH_properties) %>% 
  mutate(GH = if_else(is.na(GH), F, T))

# Still on STR platform ----------------------------------------------------------------
listings_info <- 
listings_info %>% 
  mutate(still_listed = if_else(months(scraped) == months(max(scraped)), T, F)) %>% 
  select(-scraped)

# Still active -------------------------------------------------------------------------
still_active_properties <- 
daily %>% 
  filter(date >= "2020-05-01", date < "2020-06-01", status != "B") %>% 
  distinct(property_ID) %>% 
  mutate(still_active = T)

listings_info <- 
left_join(listings_info, still_active_properties) %>% 
  mutate(still_active = if_else(is.na(still_active), F, T))
  
# How many other STR are operated by this host -----------------------------------------
host_properties <- 
property %>% 
  st_drop_geometry() %>% 
  filter(scraped >= "2020-01-01") %>% 
  count(host_ID) 

listings_info <- 
left_join(listings_info, host_properties) %>% 
  mutate(n = n-1) %>% # for it to be all "other" listings of a host
  rename(host_other_listings = n)

# Addition of LTR IDs ------------------------------------------------------------------
ltr_id <- 
ltr %>% 
  st_drop_geometry %>% 
  filter(!is.na(property_ID)) %>% 
  arrange(desc(scraped)) %>% 
  distinct(id, .keep_all = T) %>% 
  unnest(property_ID) %>% 
  inner_join(select(listings_info, property_ID), by = "property_ID") %>% 
  group_by(property_ID) %>% 
  summarize(ltr_ID = list(id))

listings_info <- 
  left_join(listings_info, ltr_id)

  

# ordering columns ---------------------------------------------------------------------
listings_info <- 
listings_info %>% 
  select(property_ID, ltr_ID, housing, listing_type, host_ID, host_other_listings, created, still_listed, still_active, 
         borough, location, roll_number, FREH, GH)


# save the file ------------------------------------------------------------------------

listings_info <- 
listings_info %>%
  mutate(location = map_chr(location, paste0, collapse = "; ")) %>% 
  mutate(roll_number = map_chr(roll_number, paste0, collapse = "; ")) %>% 
  mutate(ltr_ID = map_chr(ltr_ID, paste0, collapse = "; "))

listings_info <- 
listings_info %>% 
  rename(`AirBNB ID` = property_ID, 
         `Kijiji | Craiglist ID` = ltr_ID,
         `In housing unit` = housing,
         `Listing type` = listing_type,
         `Host ID` = host_ID,
         `Other listings from the host` = host_other_listings,
         `Date created` = created,
         `Still listed` = still_listed,
         `Still active` = still_active,
         `Borough` = borough,
         `Street adress` = location,
         `Roll Number` = roll_number,
         `Frequently rented entire home` = FREH,
         `Ghost hostel` = GH)


write.csv(listings_info[1:100,], "output/listings_info.csv")



