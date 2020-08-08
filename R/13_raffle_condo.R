#### 13. RAFFLE CONDO ##########################################################

#' This script runs slowly, and should be rerun anytime the raw STR data 
#' changes.
#' 
#' Output:
#' - `condo_analysis.Rdata`
#' 
#' Script dependencies:
#' - `09_str_processing.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")
load("output/str_processed.Rdata")


# Get DA variables for raffle ---------------------------------------------

DAs_raffle <-
  cancensus::get_census(
    dataset = "CA16", regions = list(CSD = c("2466023")), level = "DA",
    vectors = c("v_CA16_4840", "v_CA16_4841", "v_CA16_4842", "v_CA16_4836", 
                "v_CA16_4837", "v_CA16_4838"),
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(
    GeoUID, CSD_UID, Population, Dwellings, 
    "v_CA16_4840: Total - Occupied private dwellings by condominium status - 25% sample data",
    "v_CA16_4841: Condominium", "v_CA16_4842: Not condominium", 
    "v_CA16_4836: Total - Private households by tenure - 25% sample data", 
    "v_CA16_4837: Owner", "v_CA16_4838: Renter") %>% 
  set_names(c("GeoUID", "CMA_UID", "population", "dwellings", "parent_condo", 
              "condo", "not_condo", "parent_tenure", "owner", "renter", 
              "geometry")) %>% 
  mutate(p_condo = condo / parent_condo,
         p_not_condo = max(not_condo / parent_condo, 1, na.rm = TRUE),
         p_owner = owner / parent_tenure,
         p_renter = renter / parent_tenure) %>% 
  select(GeoUID, dwellings, CMA_UID, p_condo:p_renter) %>% 
  as_tibble() %>% 
  st_as_sf(agr = "constant")


# Get properties that were only active in 2017, 2018 and 2019 -------------

active_properties_2017 <- 
  daily %>% 
  filter(date >= "2017-01-01", date <= "2017-12-31",
         status %in% c("A", "R")) %>% 
  pull(property_ID) %>% 
  unique() %>% 
  {filter(property, property_ID %in% .)}

active_properties_2018 <- 
  daily %>% 
  filter(date >= "2018-01-01", date <= "2018-12-31",
         status %in% c("A", "R")) %>% 
  pull(property_ID) %>% 
  unique() %>% 
  {filter(property, property_ID %in% .)}

active_properties_2019 <- 
  daily %>% 
  filter(date >= LTM_start_date, date <= LTM_end_date,
         status %in% c("A", "R")) %>% 
  pull(property_ID) %>% 
  unique() %>% 
  {filter(property, property_ID %in% .)}


# Do new raffle with diagnostic == TRUE -----------------------------------

raffle_condo <-
  property %>% 
  filter(property_ID %in% c(active_properties_2017$property_ID,
                            active_properties_2018$property_ID,
                            active_properties_2019$property_ID)) %>% 
  strr_raffle(DAs_raffle, GeoUID, dwellings, seed = 1, diagnostic = TRUE) 


### Add geometries and census variables to the raffle ################################################ 

trial_tenure_2019 <- left_join(st_drop_geometry(raffle_2019), st_drop_geometry(DAs_raffle), by="GeoUID")

unnested_trial_tenure_2019 <- trial_tenure_2019 %>% 
  unnest(candidates)

unnested_trial_tenure_2019 <- unnested_trial_tenure_2019 %>% 
  left_join(., st_drop_geometry(DAs_raffle), by = c("poly_ID" = "GeoUID")) 


### Add geometries and census variables to the raffle ################################################ 

tenure_probabilities_2019 <- unnested_trial_tenure_2019 %>% 
  ungroup() %>% 
  group_by(property_ID) %>% 
  summarize(prob_condo = sum(p_condo.x*(probability/sum(probability))),
            prob_renter = sum(p_renter.x*(probability/sum(probability))),
            prob_owner = sum(p_owner.x*(probability/sum(probability)))
  ) 


### Add final sf to the raffle ################################################ 

tenure_probabilities_sf_2019 <- 
  left_join(tenure_probabilities_2019, trial_tenure_2019, by="property_ID") %>% 
  left_join(., DAs_raffle, by = "GeoUID") %>% 
  select(property_ID, GeoUID, dwellings.x, prob_condo, prob_renter, prob_owner, geometry) %>% #number_condo, number_renter, number_owner,
  rename(dwellings=dwellings.x) %>% 
  st_as_sf()


### Initial proportions for graph ################################################ 

DAs_raffle_p_condo <- DAs_raffle %>% 
  select(GeoUID, p_condo) %>% 
  st_drop_geometry()


# Save output -------------------------------------------------------------

save(tenure_probabilities_sf_2019, DAs_raffle_p_condo, file = "output/raffle_condo.Rdata")


