#### Raffle for condo and ownership type ################################################

source("R/01_startup.R")

### Get DA variables for raffle ################################################

DAs_raffle <-
  cancensus::get_census(
    dataset = "CA16", regions = list(CSD = c("2466023")), level = "DA",
    vectors = c("v_CA16_4840", "v_CA16_4841", "v_CA16_4842", "v_CA16_4836", "v_CA16_4837", "v_CA16_4838"),
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(GeoUID, CSD_UID, Population, Dwellings, "v_CA16_4840: Total - Occupied private dwellings by condominium status - 25% sample data",
         "v_CA16_4841: Condominium", "v_CA16_4842: Not condominium", "v_CA16_4836: Total - Private households by tenure - 25% sample data", 
         "v_CA16_4837: Owner", "v_CA16_4838: Renter") %>% 
  set_names(c("GeoUID", "CMA_UID", "population", "dwellings", "parent_condo", "condo", "not_condo", "parent_tenure",
              "owner", "renter", "geometry")) %>% 
  mutate(p_condo = condo/parent_condo,
         p_not_condo = not_condo/parent_condo,
         p_owner = owner/parent_tenure,
         p_renter = renter/parent_tenure) %>% 
  select(GeoUID, dwellings, CMA_UID, p_condo, p_not_condo, p_owner, p_renter) %>% 
  st_set_agr("constant")

### Get properties that were only active in 2019 ################################################ 

DAs_raffle$p_not_condo[DAs_raffle$p_not_condo >= "1"] <- "1"
DAs_raffle$p_not_condo <- as.numeric(DAs_raffle$p_not_condo)

active_properties_2019 <- 
  daily %>% 
  filter(date >= LTM_start_date, date <= LTM_end_date) %>% 
  filter(status == "A" | status == "R") %>% 
  group_by(property_ID) %>% 
  count()

active_properties_2019 <- property %>%
  filter(property_ID %in% active_properties_2019$property_ID)


### Conduct the raffle ################################################ 

raffle_2019 <- 
  active_properties_2019 %>% 
  strr_raffle(DAs_raffle, GeoUID, dwellings, seed=1, diagnostic = TRUE) 


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


