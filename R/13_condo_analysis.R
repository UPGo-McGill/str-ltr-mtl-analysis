#### 13 CONDO ANALYSIS #########################################################

#' This script runs quickly, and should be rerun anytime the raw STR data 
#' changes.
#' 
#' Output:
#' - `condo_analysis.Rdata`
#' 
#' Script dependencies:
#' - `09_str_processing.R`
#' 
#' External dependencies:
#' - `uniteevaluationfonciere.shp`: Municipal evaluation data shapefile

source("R/01_startup.R")
load("output/str_processed.Rdata")


# Load UEF shapefile ------------------------------------------------------

uef <-
  read_sf("data/shapefiles/uniteevaluationfonciere.shp") %>%
  st_transform(32618) %>%
  filter(!is.na(NOMBRE_LOG)) %>%
  as_tibble() %>%
  distinct(ID_UEV, .keep_all = TRUE) %>%
  st_as_sf()


# Get properties that were only active in 2017 and 2019 -------------------

active_properties_2017 <- 
  daily %>% 
  filter(date >= "2017-01-01", date <= "2017-12-31",
         status %in% c("A", "R")) %>% 
  pull(property_ID) %>% 
  unique()

active_properties_2019 <- 
  daily %>% 
  filter(date >= LTM_start_date, date <= LTM_end_date,
         status %in% c("A", "R")) %>% 
  pull(property_ID) %>% 
  unique()


# Get DA tenure variables -------------------------------------------------

DA_tenure <-
  cancensus::get_census(
    dataset = "CA16", regions = list(CSD = c("2466023")), level = "DA",
    vectors = c("v_CA16_4840", "v_CA16_4841", "v_CA16_4836", "v_CA16_4838"),
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(-c(`Shape Area`:Households, CSD_UID:`Area (sq km)`)) %>% 
  set_names(c("dwellings", "GeoUID", "parent_condo", "condo", "parent_tenure", 
              "renter", "geometry")) %>% 
  mutate(p_condo = condo / parent_condo,
         p_renter = renter / parent_tenure) %>% 
  select(GeoUID, dwellings, p_condo:p_renter) %>% 
  as_tibble() %>% 
  st_as_sf(agr = "constant")


# Probability helper functions --------------------------------------------

calculate_listing_prob <- function(PIDs) {
  property %>% 
    st_drop_geometry() %>%
    filter(property_ID %in% PIDs) %>%
    left_join(uef, by = c("uef" = "ID_UEV")) %>% 
    mutate(condo = if_else(CATEGORIE_ == "Condominium", TRUE, FALSE)) %>% 
    inner_join(st_drop_geometry(DA_tenure), by = "GeoUID") %>% 
    select(property_ID, GeoUID, p_renter, condo)
}

calculate_DA_prob <- function(PIDs) {
  property %>% 
    st_drop_geometry() %>%
    filter(property_ID %in% PIDs) %>%
    left_join(uef, by = c("uef" = "ID_UEV")) %>% 
    mutate(condo = if_else(CATEGORIE_ == "Condominium", TRUE, FALSE)) %>% 
    right_join(st_drop_geometry(DA_tenure), by = "GeoUID") %>% 
    group_by(GeoUID) %>% 
    summarize(n_listings = as.numeric(n()), 
              n_condo = as.numeric(sum(condo)),
              pct_condo = n_condo / n_listings,
              n_renter = sum(p_renter, na.rm = TRUE)) %>% 
    mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, .x))) %>% 
    left_join(DA_tenure) %>% 
    st_as_sf() %>% 
    relocate(GeoUID, dwellings) %>% 
    filter(n_listings > 0)
}


# Calculate probabilities -------------------------------------------------

listing_probabilities_2017 <- calculate_listing_prob(active_properties_2017)
DA_probabilities_2017 <- calculate_DA_prob(active_properties_2017)

listing_probabilities_2019 <- calculate_listing_prob(active_properties_2019)
DA_probabilities_2019 <- calculate_DA_prob(active_properties_2019)


# Save output -------------------------------------------------------------

save(listing_probabilities_2017, DA_probabilities_2017, 
     listing_probabilities_2019, DA_probabilities_2019, 
     file = "output/condo_analysis.Rdata")
