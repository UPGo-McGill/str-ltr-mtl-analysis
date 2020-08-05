#### 03 STR DATA IMPORT ########################################################

#' This script is time-consuming to run, so it should only be rerun when STR 
#' data needs to be rebuilt from scratch.
#' 
#' Output:
#' - `str_raw.Rdata`
#' 
#' Script dependencies:
#' - `02_geometry_import.R`
#' 
#' External dependencies:
#' - Access to the UPGo database
#' - `uniteevaluationfonciere.shp`: Municipal evaluation data shapefile

source("R/01_startup.R")
load("output/geometry.Rdata")


# Load parcel data --------------------------------------------------------

uef <- 
  read_sf("data/shapefiles/uniteevaluationfonciere.shp") %>% 
  st_transform(32618) %>% 
  filter(!is.na(NOMBRE_LOG))

# Remove duplicates
uef <- 
  uef %>% 
  as_tibble() %>% 
  distinct(ID_UEV, .keep_all = TRUE) %>% 
  st_as_sf()


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


# Manually fix scraped date issue -----------------------------------------

# Load files for fix
load("data/scrape_fix/changes.Rdata")
load("data/scrape_fix/inactives.Rdata")

# Change scraped date in property file
property <- 
  property %>% 
  left_join(rename(changes, new_scraped = scraped)) %>% 
  mutate(scraped = if_else(new_scraped > scraped, new_scraped, scraped, 
                           scraped)) %>% 
  select(-new_scraped)

# Add inactive rows to daily file then re-trim
daily <- 
  daily %>% 
  bind_rows(inactives) %>% 
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

# Spatial join to only keep the properties inside the city of Mtl
property <- 
  property %>% 
  st_intersection(city)

# Run raffle to assign a DA to each listing
property <-
  property %>% 
  strr_raffle(DA, GeoUID, dwellings, seed = 1)

# Run raffle to assign a parcel to each listing
property <-
  property %>% 
  strr_raffle(uef, ID_UEV, NOMBRE_LOG, seed = 1)

# Trim daily file
daily <- 
  daily %>% 
  filter(property_ID %in% property$property_ID)

# Trim host file
host <- 
  host %>% 
  filter(host_ID %in% property$host_ID)

# Add borough to property file
property <- 
  property %>% 
  st_join(boroughs) %>% 
  select(-dwellings)


# Add borough to daily file
daily <- 
  property %>% 
  st_drop_geometry() %>% 
  select(property_ID, borough) %>% 
  left_join(daily, ., by = "property_ID")


# Fix April plateau -------------------------------------------------------

# Problem dates are 2020-04-03 to 2020-04-17, and the actual bad results have 
# booked_date 2020-04-04, 2020-04-14, 2020-04-15. So we change those status
# values from R to B.

daily <- 
  daily %>% 
  as_tibble() %>% 
  mutate(status = if_else(
    booked_date %in% c(as.Date("2020-04-04"), as.Date("2020-04-14"), 
                       as.Date("2020-04-15")) & 
      date < "2020-04-18" &
      status == "R", "B", status))


# Save output -------------------------------------------------------------

save(property, daily, host, file = "output/str_raw.Rdata")
