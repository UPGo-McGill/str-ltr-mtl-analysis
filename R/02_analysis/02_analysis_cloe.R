library(upgo)
library(strr)
library(dplyr)
library(ggmap)
library(sf)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(lubridate)
library(RColorBrewer)
library(tidyverse)
library(future)
library(osmdata)
library(cancensus)
library(scales)
library(zoo)
library(gt)
library(data.table)

upgo_connect()
upgo_disconnect()

### Housing loss ###############################################################

FREH %>% 
  filter(date == end_date) %>% 
  count()

freh_listings_mtl <- FREH %>% 
  count(date) %>% 
  ggplot() +
  geom_line(aes(date, n), colour = "black", size = 1) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = comma) +
  ggtitle("FREH listings in Montreal")

ggsave("output/freh_listings_mtl.pdf", plot = freh_listings_mtl, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

units_converted_gh_mtl <- GH %>% 
  st_drop_geometry() %>% 
  group_by(date) %>% 
  summarize(GH_units = sum(housing_units)) %>%
  ggplot() +
  geom_line(aes(date, GH_units), colour = "black", size = 1) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = comma) +
  ggtitle("Units converted to ghost hostels in Montreal")

ggsave("output/units_converted_gh_mtl.pdf", plot = units_converted_gh_mtl, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

GH_total <-
  GH %>%
  st_drop_geometry() %>%
  group_by(date) %>%
  summarize(GH_units = sum(housing_units)) %>%
  mutate(GH_average = frollmean(GH_units, 30, align = "right")) %>%
  mutate(GH_average = if_else(is.na(GH_average), 8, GH_average)) %>%
  select(-GH_units)


########## HOUSING LOSS IN MTL ##################
housing_loss %>% 
  group_by(`Listing type`, year=floor_date(date, "year")) %>% 
  summarize(mean=mean(`Housing units`, na.rm=TRUE))

housing_loss <-
  FREH %>%
  group_by(date) %>%
  summarize(`Entire home/apt` = n()) %>%
  left_join(GH_total, by = "date") %>%
  rename(`Private room` = GH_average) %>%
  #mutate(`Summer listings` = case_when(
  #  date >= season_start & date <= season_end ~ seasonal_loss_2019,
  #  date >= season_start - years(1) & date <= season_end - years(1) ~ seasonal_loss_2018,
  #  date >= season_start - years(2) & date <= season_end - years(2) ~ seasonal_loss_2017,
  #  TRUE ~ 0L)) %>% 
  gather(`Entire home/apt`, `Private room`,  #, `Summer listings` 
         key = `Listing type`, value = `Housing units`) 



###### REPLICATING DE CT COMPARATIVE ANALYSIS WITH OTHER MARITIME CITIES, BUT WITH LARGE METROPOLISES ####################################################

library(upgo)
library(dplyr)
library(strr)
library(lubridate)

upgo_connect()

cities <-
  cancensus::get_census(dataset = "CA16", regions = list(CSD = c("2466023", "5915022", "3520005")),
                        level = "CSD", geo_format = "sf") %>% 
  st_transform(32618) %>% 
  st_set_agr("constant")

mtl_properties <- 
  property_all %>% 
  filter(country == "Canada",
         city %in% c("Montreal")) %>% 
  collect()

to_properties <- 
  property_all %>% 
  filter(country == "Canada",
         city %in% c("Toronto")) %>% 
  collect()

van_properties <- 
  property_all %>% 
  filter(country == "Canada",
         city %in% c("Vancouver")) %>% 
  collect()

######## Get daily activity for 3 metropolises ##################

to_daily <- 
  daily_all %>% 
  filter(property_ID %in% !! to_properties$property_ID) %>% ##, start_date >= "2019-01-01"
  collect()

to_daily <- 
  to_daily%>% 
  strr_expand()

van_daily <- 
  daily_all %>% 
  filter(property_ID %in% !! van_properties$property_ID) %>% ##, start_date >= "2019-01-01"
  collect()

van_daily <- 
  van_daily%>% 
  strr_expand()

# Average listings reserved or available / blocked in 2019

filter(daily, housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>% 
  count(date) %>% 
  summarize(mean(n))

filter(to_daily, housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>% 
  count(date) %>% 
  summarize(mean(n))

filter(van_daily, housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>% 
  count(date) %>% 
  summarize(mean(n))

########## Yearly revenue for three metropolises

daily %>% 
  filter(status == "R", housing == TRUE) %>%  ##
  filter(date >= LTM_start_date, date <= LTM_end_date) %>% 
  group_by(city) %>% 
  summarize(revenue = sum(price) * exchange_rate)

to_daily %>% 
  filter(status == "R", housing == TRUE) %>%  ##
  filter(date >= LTM_start_date, date <= LTM_end_date) %>% 
  group_by(city) %>% 
  summarize(revenue = sum(price) * exchange_rate)

van_daily %>% 
  filter(status == "R", housing == TRUE) %>%  ##
  filter(date >= LTM_start_date, date <= LTM_end_date) %>% 
  group_by(city) %>% 
  summarize(revenue = sum(price) * exchange_rate)

save(property, daily, to_properties, to_daily, van_daily, van_properties, LTM_start_date, LTM_end_date,
     file = "pour_cloe")
