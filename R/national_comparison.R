#### NATIONAL COMPARISON #######################################################

source("R/01_startup.R")
library(cancensus)

# Get geometries for 10 biggest cities ------------------------------------

CSD <-
  get_census("CA16", list(C = "01"), level = "CSD", geo_format = "sf") %>% 
  arrange(-Population) %>% 
  slice(1:10) %>% 
  mutate(name = str_extract(name, '.*(?= )'),
         name = stringi::stri_trans_general(name, "Latin-ASCII")) %>% 
  st_drop_geometry() %>% 
  as_tibble()


# Get STR data for same cities --------------------------------------------

upgo_connect()

property <- 
  property_all %>% 
  filter(country == "Canada", city %in% !!CSD$name) %>% 
  collect()

daily <- 
  daily_all %>% 
  filter(country == "Canada", city %in% !!CSD$name, start_date >= "2019-01-01",
         start_date <= "2019-12-31") %>% 
  collect() %>% 
  strr_expand()



# Calculate figures -------------------------------------------------------

national_comparison <- 
  daily %>% 
  filter(status != "B", housing) %>% 
  group_by(city) %>% 
  summarize(active_daily_listings = n() / 365) %>% 
  left_join(select(CSD, name, Households), by = c("city" = "name")) %>% 
  mutate(listings_per_1000 = 1000 * active_daily_listings / Households) %>% 
  select(-Households)

exchange_rates <- convert_currency(start_date = "2019-01-01", 
                                   end_date = "2019-12-31")

national_comparison <- 
  daily %>% 
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
