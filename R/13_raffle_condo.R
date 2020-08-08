#### 13. RAFFLE CONDO ##########################################################

#' This script runs slowly, and should be rerun anytime the raw STR data 
#' changes.
#' 
#' Output:
#' - `raffle_condo.Rdata`
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
    vectors = c("v_CA16_4840", "v_CA16_4841", "v_CA16_4836", "v_CA16_4837", 
                "v_CA16_4838"),
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(
    GeoUID, Dwellings, 
    "v_CA16_4840: Total - Occupied private dwellings by condominium status - 25% sample data",
    "v_CA16_4841: Condominium",
    "v_CA16_4836: Total - Private households by tenure - 25% sample data", 
    "v_CA16_4837: Owner", "v_CA16_4838: Renter") %>% 
  set_names(c("GeoUID", "dwellings", "parent_condo", "condo", "parent_tenure", 
              "owner", "renter", "geometry")) %>% 
  mutate(p_condo = condo / parent_condo,
         p_owner = owner / parent_tenure,
         p_renter = renter / parent_tenure) %>% 
  select(GeoUID, dwellings, p_condo:p_renter) %>% 
  as_tibble() %>% 
  st_as_sf(agr = "constant")


# Get properties that were only active in 2017, 2018 and 2019 -------------

active_properties_2017 <- 
  daily %>% 
  filter(date >= "2017-01-01", date <= "2017-12-31",
         status %in% c("A", "R")) %>% 
  pull(property_ID) %>% 
  unique()

active_properties_2018 <- 
  daily %>% 
  filter(date >= "2018-01-01", date <= "2018-12-31",
         status %in% c("A", "R")) %>% 
  pull(property_ID) %>% 
  unique()

active_properties_2019 <- 
  daily %>% 
  filter(date >= LTM_start_date, date <= LTM_end_date,
         status %in% c("A", "R")) %>% 
  pull(property_ID) %>% 
  unique()


# Do new raffle with diagnostic == TRUE -----------------------------------

raffle_condo <-
  property %>% 
  filter(property_ID %in% c(active_properties_2017, active_properties_2018, 
                            active_properties_2019)) %>% 
  strr_raffle(DAs_raffle, GeoUID, dwellings, seed = 1, diagnostic = TRUE) 


# Split results by year ---------------------------------------------------

raffle_condo_2017 <- 
  raffle_condo %>% 
  filter(property_ID %in% active_properties_2017)

raffle_condo_2018 <- 
  raffle_condo %>% 
  filter(property_ID %in% active_properties_2018)

raffle_condo_2019 <- 
  raffle_condo %>% 
  filter(property_ID %in% active_properties_2019)

rm(raffle_condo, active_properties_2017, active_properties_2018,
   active_properties_2019)


# Probability helper functions --------------------------------------------

calculate_listing_prob <- function(df) {
  df %>% 
    st_drop_geometry() %>%
    unnest(candidates) %>% 
    left_join(st_drop_geometry(DAs_raffle), by = c("poly_ID" = "GeoUID")) %>% 
    group_by(property_ID) %>% 
    summarize(across(c(p_condo, p_renter, p_owner), 
                     ~sum(.x * probability / sum(probability)))) 
}

calculate_DA_prob <- function(df) {
  df %>% 
    st_drop_geometry() %>%
    select(property_ID, candidates) %>% 
    unnest(candidates) %>% 
    left_join(st_drop_geometry(DAs_raffle), by = c("poly_ID" = "GeoUID")) %>%
    group_by(property_ID) %>% 
    mutate(across(c(p_condo, p_renter, p_owner), 
                  ~{.x * probability / sum(probability)})) %>% 
    group_by(GeoUID = poly_ID) %>% 
    summarize(across(c(p_condo, p_renter, p_owner), sum, na.rm = TRUE)) %>% 
    rename(n_condo = p_condo, n_renter = p_renter, n_owner = p_owner) %>% 
    mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, .x))) %>% 
    left_join(DAs_raffle) %>% 
    st_as_sf()
  
}


# Calculate probabilities -------------------------------------------------

listing_probabilities_2017 <- calculate_listing_prob(raffle_condo_2017)
DA_probabilities_2017 <- calculate_DA_prob(raffle_condo_2017)

listing_probabilities_2018 <- calculate_listing_prob(raffle_condo_2018)
DA_probabilities_2018 <- calculate_DA_prob(raffle_condo_2018)

listing_probabilities_2019 <- calculate_listing_prob(raffle_condo_2019)
DA_probabilities_2019 <- calculate_DA_prob(raffle_condo_2019)


# Save output -------------------------------------------------------------

save(listing_probabilities_2017, DA_probabilities_2017, 
     listing_probabilities_2018, DA_probabilities_2018, 
     listing_probabilities_2019, DA_probabilities_2019, 
     file = "output/raffle_condo.Rdata")
