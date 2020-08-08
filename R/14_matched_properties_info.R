#### Information about mtached properties #################################################

source("R/01_startup.R")

# load data ------------------------------------------------------------------------------
load("output/str_processed.Rdata")
load("output/ltr_processed.Rdata")

kj_landlord <- readRDS("output/kj_with_landlord.Rds") %>% 
  select(kj_id, Numero_de_matricule) %>%
  rename(id = kj_id,
         roll_number = Numero_de_matricule)

# Preare the basic of the table ----------------------------------------------------------
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
  filter(!is.na(ab_id)) %>% 
  unnest(ab_id) %>% 
  select(id, ab_id, location) %>% 
  left_join(kj_landlord) %>% 
  select(-id) %>% 
  distinct() %>% 
  filter(!is.na(location) | !is.na(roll_number)) %>% 
  rename(property_ID = ab_id) %>% 
  group_by(property_ID) %>% 
  summarize(roll_number = list(roll_number),
            location = list(location))


listings_info <- 
left_join(listings_info, locations_roll_number)


# Adding a FREH column -----------------------------------------------------------------
# If listing has been FREH once in its lifetime
FREH <- 
  daily %>% 
  filter(FREH == 1) %>% 
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

# ordering columns ---------------------------------------------------------------------
listings_info <- 
listings_info %>% 
  select(property_ID, housing, listing_type, host_ID, host_other_listings, created, still_listed, still_active, 
         borough, location, roll_number, FREH, GH)


# save the file ------------------------------------------------------------------------

write.csv(listings_info, "output/listings_info.csv")
