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

# Load file for fix
load("data/scrape_fix/changes.Rdata")

# Get inactives
upgo_connect(daily_inactive = TRUE)

inactives <-
  daily_inactive_all %>% 
  filter(property_ID %in% !!changes$property_ID) %>% 
  collect() %>% 
  strr_expand()

upgo_disconnect()

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

rm(changes, inactive)


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


# Process the property and daily files ------------------------------------

# Spatial join to only keep the properties inside the city of Mtl
property <- 
  property %>% 
  strr_as_sf(32618) %>% 
  st_intersection(city)

# Run raffle to assign a DA to each listing
property <-
  property %>% 
  strr_raffle(DA, GeoUID, dwellings, seed = 1)

# # Run raffle to assign a parcel to each listing
# uef <- 
#   read_sf("data/shapefiles/uniteevaluationfonciere.shp") %>% 
#   st_transform(32618) %>% 
#   filter(!is.na(NOMBRE_LOG)) %>% 
#   as_tibble() %>% 
#   distinct(ID_UEV, .keep_all = TRUE) %>% 
#   st_as_sf()
# 
# property <-
#   property %>% 
#   strr_raffle(uef, ID_UEV, NOMBRE_LOG, seed = 1)

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

rm(borough, city, DA)


# Fix April and June plateaus ---------------------------------------------

#' For April, problem dates are 2020-04-03 to 2020-04-17, and the actual bad 
#' results have booked_date 2020-04-04, 2020-04-14, 2020-04-15. So we change 
#' those status values from R to A. For June, problem dates are 2020-06-01 to
#' 2020-06-15, and the relevant booked_date is 2020-05-31. We then shift Bs to
#' As based on activity in the previous ~1 month.

daily <- 
  daily %>% 
  mutate(status = if_else(booked_date %in% c(as.Date("2020-04-04"), 
                                             as.Date("2020-04-14"), 
                                             as.Date("2020-04-15")) & 
                            date < "2020-04-18" & status == "R", "A", status),
         status = if_else(status == "R" & 
                            date >= "2020-06-01" &
                            date <= "2020-06-15" & 
                            booked_date == "2020-05-30" &
                            !is.na(booked_date), "A", status))

all_blocked_june <- 
  daily %>% 
  filter(date >= "2020-06-01", date <= "2020-06-15") %>% 
  group_by(property_ID) %>% 
  count(status) %>% 
  ungroup() %>% 
  filter(status == "B", n == 15) %>% 
  pull(property_ID)

activity_in_may <- 
  daily %>% 
  filter(property_ID %in% all_blocked_june, date >= "2020-04-15", 
         date <= "2020-05-31") %>% 
  group_by(property_ID) %>% 
  count(status) %>% 
  ungroup() %>% 
  filter(status != "B") %>% 
  pull(property_ID) %>% 
  unique()

daily <- 
  daily %>% 
  mutate(status = if_else(status == "B" &
                            property_ID %in% all_blocked_june &
                            property_ID %in% activity_in_may &
                            date >= "2020-06-02" & date <= "2020-06-15",
                          "A", status))

all_blocked_april <- 
  daily %>% 
  filter(date >= "2020-04-03", date <= "2020-04-17") %>% 
  group_by(property_ID) %>% 
  count(status) %>% 
  ungroup() %>% 
  filter(status == "B", n == 15) %>% 
  pull(property_ID)

activity_in_march <- 
  daily %>% 
  filter(property_ID %in% all_blocked_april, date >= "2020-03-01", 
         date <= "2020-03-31") %>% 
  group_by(property_ID) %>% 
  count(status) %>% 
  ungroup() %>% 
  filter(status != "B") %>% 
  pull(property_ID) %>% 
  unique()

A_on_15_16 <- 
  daily %>% 
  filter(date >= "2020-04-15", date <= "2020-04-16", status == "A") %>%
  count(property_ID) %>% 
  # filter(n == 2) %>%
  pull(property_ID)

changed_from_14_to_15 <- 
  daily %>% 
  filter(date >= "2020-04-03", date <= "2020-04-14", status == "B",
         property_ID %in% A_on_15_16) %>%
  count(property_ID) %>% 
  filter(n >= 10) %>% 
  pull(property_ID)

daily <- 
  daily %>% 
  mutate(status = if_else(status == "B" &
                            property_ID %in% all_blocked_april &
                            property_ID %in% activity_in_march &
                            date >= "2020-04-03" & date <= "2020-04-17",
                          "A", status)) %>%
  mutate(status = if_else(property_ID %in% changed_from_14_to_15 &
                            date >= "2020-04-15" & date <= "2020-04-16",
                          "B",
                          status))

rm(all_blocked_june, activity_in_may, all_blocked_april, activity_in_march,
   A_on_15_16, changed_from_14_to_15)


# Save output -------------------------------------------------------------

save(property, daily, host, exchange_rate, file = "output/str_raw.Rdata")
