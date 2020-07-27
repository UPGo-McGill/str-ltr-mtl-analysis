#### 03 STR DATA IMPORT ########################################################

#' This script produces the `str_raw.Rdata` object. It is time-consuming to run, 
#' so it should only be rerun when STR data needs to be rebuilt from scratch.

#' External dependencies:
#' - Access to the UPGo database

source("R/01_startup.R")


# Get data ----------------------------------------------------------------

upgo_connect()

property <- 
  property_all %>% 
  filter(country == "Canada", city == "Montreal") %>% 
  collect()

daily <- 
  daily_all %>% 
  filter(country == "Canada", city == "Montreal") %>% 
  collect() %>% 
  strr_expand()

host <-
  host_all %>% 
  filter(host_ID %in% !!property$host_ID) %>% 
  collect() %>% 
  strr_expand()

upgo_disconnect()


# Remove inactive rows from daily file ------------------------------------

daily <- 
  daily %>% 
  left_join(select(property, property_ID, created, scraped)) %>%
  filter(date >= created, date <= scraped) %>%
  select(-created, -scraped)


# Convert currency --------------------------------------------------------

exchange_rates <- 
  convert_currency(start_date = min(daily$date), 
                   end_date = max(daily$date))

daily <- 
  daily %>% 
  mutate(year_month = substr(date, 1, 7)) %>% 
  left_join(exchange_rates) %>% 
  mutate(price = price * exchange_rate) %>% 
  select(-year_month, -exchange_rate)

rm(exchange_rates)


# Process the property and daily files ------------------------------------

# Run raffle to assign a DA to each listing
property <-
  property %>% 
  strr_raffle(DA, GeoUID, dwellings, seed = 1)

# Spatial join to only keep the properties inside the city of Mtl
property <- 
  property %>% 
  st_intersection(city)

# Trim daily file
daily <- 
  daily %>% 
  filter(property_ID %in% property$property_ID)

# Trim host file
host <- 
  host %>% 
  filter(host_ID %in% property$host_ID)

# Add borough to daily file
daily <- 
  property %>% 
  st_join(boroughs) %>% 
  st_drop_geometry() %>% 
  select(property_ID, borough) %>% 
  left_join(daily, ., by = "property_ID")


# Save output -------------------------------------------------------------

save(property, daily, host, file = "data/str_raw.Rdata")

