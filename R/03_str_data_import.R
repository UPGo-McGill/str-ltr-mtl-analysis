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
#' - Old versions of the AirDNA property file to fix problems with last scraped
#'   dates
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
  collect()
  
daily <- daily %>% strr_expand()

host <-
  host_all %>% 
  filter(host_ID %in% !!property$host_ID) %>% 
  collect()
  
host <- host %>% strr_expand()

upgo_disconnect()



# Manually fix January scraped date issue ---------------------------------

# Load old property files
prop_04 <- 
  read_csv(paste0("~/Documents/Academic/Code/global-file-import/", 
                  "data/property_2020_04.gz")) %>% 
  select(property_ID = `Property ID`,
         old_scraped = `Last Scraped Date`)

# Get fixes
jan_fix <- 
  property %>% 
  filter(scraped >= "2020-01-29", scraped <= "2020-01-31") %>% 
  left_join(prop_04) %>% 
  filter(scraped < old_scraped) %>% 
  select(property_ID, old_scraped)

# Change scraped date in property file
property <- 
  property %>% 
  left_join(jan_fix) %>% 
  mutate(scraped = if_else(is.na(old_scraped), scraped, old_scraped)) %>% 
  select(-old_scraped)

# Scrape fixed listings with May scraped date to see which are still active
to_scrape <- jan_fix %>% filter(old_scraped >= "2020-05-01")
upgo_scrape_connect()
new_scrape <- to_scrape %>% upgo_scrape_ab(proxies = .proxy_list, cores = 10)
upgo_scrape_disconnect()
still_active <- new_scrape %>% filter(!is.na(city))

# Update scraped dates for active listings
property <- 
  property %>% 
  mutate(scraped = if_else(property_ID %in% still_active$property_ID,
                           as.Date("2020-08-01"), scraped))

# Get inactives
upgo_connect(daily_inactive = TRUE)

inactives <-
  daily_inactive_all %>% 
  filter(property_ID %in% !!jan_fix$property_ID) %>% 
  collect() %>% 
  strr_expand()

upgo_disconnect()


# Add inactive rows to daily file then re-trim
daily <- 
  daily %>% 
  bind_rows(inactives) %>% 
  left_join(select(property, property_ID, created, scraped)) %>%
  filter(date >= created, date <= scraped) %>%
  select(-created, -scraped)

rm(prop_04, jan_fix, to_scrape, new_scrape, still_active, inactives)


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

# Run raffle to assign a parcel to each listing
uef <-
  read_sf("data/shapefiles/uniteevaluationfonciere.shp") %>%
  st_transform(32618) %>%
  filter(!is.na(NOMBRE_LOG)) %>%
  as_tibble() %>%
  distinct(ID_UEV, .keep_all = TRUE) %>%
  st_as_sf()

property <-
  property %>%
  strr_raffle(uef, ID_UEV, NOMBRE_LOG, seed = 1)

property <- 
  property %>%
  rename(uef = ID_UEV)

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
  st_join(boroughs_raw)

# Add borough to daily file
daily <- 
  property %>% 
  st_drop_geometry() %>% 
  select(property_ID, borough) %>% 
  left_join(daily, ., by = "property_ID")

rm(boroughs, boroughs_raw, city, DA, province, uef)


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

save(property, daily, host, exchange_rates, file = "output/str_raw.Rdata")
