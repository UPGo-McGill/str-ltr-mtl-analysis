#### 10 NATIONAL COMPARISON ####################################################

#' This script is time-consuming to run, so it should only be rerun when STR 
#' data needs to be rebuilt from scratch.
#' 
#' Output:
#' - `national_comparison.Rdata`
#' 
#' Script dependencies:
#' - None
#' 
#' External dependencies:
#' - Access to the UPGo database

source("R/01_startup.R")
library(cancensus)


# Get geometries for 10 biggest cities ------------------------------------

CSD <-
  get_census("CA16", list(C = "01"), level = "CSD", geo_format = "sf") %>% 
  arrange(-Population) %>% 
  slice(1:10) %>% 
  mutate(name = str_extract(name, '.*(?= )'),
         name = stringi::stri_trans_general(name, "Latin-ASCII")) %>% 
  as_tibble() %>% 
  st_as_sf()


# Get STR data for same cities --------------------------------------------

upgo_connect()

property_CA <- 
  property_all %>% 
  filter(country == "Canada", city %in% !!CSD$name) %>% 
  collect() %>% 
  strr_as_sf() %>%
  st_filter(CSD)

daily_CA <- 
  daily_all %>% 
  filter(property_ID %in% !!property_CA$property_ID, start_date >= "2019-01-01",
         start_date <= "2019-12-31") %>% 
  collect() %>% 
  strr_expand() %>% 
  # Reconcile geography inconsistencies between property and daily files
  select(-country, -region, -city) %>% 
  left_join(select(st_drop_geometry(property_CA), property_ID, country:city))

upgo_disconnect()


# Calculate figures -------------------------------------------------------

national_comparison <- 
  daily_CA %>% 
  filter(status != "B", housing) %>% 
  group_by(city) %>% 
  summarize(active_daily_listings = n() / 365, .groups = "drop") %>% 
  left_join(select(st_drop_geometry(CSD), name, Dwellings), 
            by = c("city" = "name")) %>% 
  mutate(listings_per_1000 = 1000 * active_daily_listings / Dwellings) %>% 
  select(-Households)

exchange_rates <- convert_currency(start_date = "2019-01-01", 
                                   end_date = "2019-12-31")

national_comparison <- 
  daily_CA %>% 
  filter(status == "R", housing) %>% 
  mutate(year_month = substr(date, 1, 7)) %>% 
  left_join(exchange_rates) %>% 
  mutate(price = price * exchange_rate) %>% 
  select(-year_month, -exchange_rate) %>% 
  group_by(city) %>% 
  summarize(revenue = sum(price)) %>% 
  left_join(national_comparison, .)

national_comparison <- 
  national_comparison %>% 
  mutate(revenue_per_listing = revenue / active_daily_listings)


# Save output -------------------------------------------------------------

save(national_comparison, file = "output/national_comparison.Rdata")
