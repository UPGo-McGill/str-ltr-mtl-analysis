### load libraries and data ###########################################
library(tidyverse)
library(lubridate)
library(sf)
library(gt)
load("data/airdna.Rdata")
load("data/ltr.Rdata")




###  ####################################################



### Active daily listings ######################################################

## Active listings from property file

# Active listings
property %>% 
  filter(housing, created <= end_date, scraped >= end_date) %>% 
  nrow()

# Hosts active on March 14
property %>% 
  filter(housing, created <= end_date, scraped >= end_date) %>% 
  pull(host_ID) %>% 
  unique() %>% 
  length()

# Housing listings over the last twelve months
LTM_property <-
  property %>%
  filter(housing, created <= end_date, scraped > end_date - years(1))

nrow(LTM_property)


# Number of hosts over last twelve months
length(unique(LTM_property$host_ID))

# Hosts by listing type
# LTM_property %>%
#   filter(housing,listing_type == "Private room") %>%
#   select(host_ID) %>%
#   st_drop_geometry() %>%
#   unique() %>%
#   nrow()/
#   length(unique(LTM_property$host_ID))


# LTM revenue

LTM_revenue <-
  daily %>%
  filter(housing,
         date <= end_date, date > end_date - years(1),
         status == "R") %>%
  group_by(property_ID) %>%
  summarize(revenue_LTM = sum(price) * exchange_rate) %>%
  left_join(LTM_property, .)

sum(LTM_revenue$revenue_LTM, na.rm = TRUE)

# YOY growth rate
#2019
nrow(filter(property, housing == TRUE, created <= end_date, scraped >= end_date)) / 
  nrow(filter(property, created <= end_date - years(1), scraped >= end_date - years(1),
              housing == TRUE))

#2018
nrow(filter(property, created <= end_date - years(1), scraped >= end_date - years(1),
            housing == TRUE)) / 
  nrow(filter(property, created <= end_date - years(2), scraped >= end_date - years(2),
              housing == TRUE))

#2017
nrow(filter(property, created <= end_date - years(2), scraped >= end_date - years(2),
            housing == TRUE)) / 
  nrow(filter(property, created <= end_date - years(3), scraped >= end_date - years(3),
              housing == TRUE))






### Montreal mapping prep ###################################################


### Which STR platforms are used in Montreal? ###################################

# Airbnb and not Homeaway

nrow(filter(LTM_property, !is.na(ab_property), is.na(ha_property)))

# Homeaway and not Airbnb
nrow(filter(LTM_property, !is.na(ha_property), is.na(ab_property)))

# Both Airbnb and Homeaway
nrow(filter(LTM_property, !is.na(ha_property), !is.na(ab_property)))

nrow(LTM_property)


### Listing type prevalence ####################################################

# Listing types for city

LTM_revenue_table <- 
  LTM_revenue %>% 
  filter(housing, created <= end_date, scraped > end_date - years(1)) %>% 
  rename(`Listing type` = listing_type) %>% 
  st_drop_geometry() %>% 
  group_by(`Listing type`) %>% 
  summarize(`Number of listings` = n(),
            `Annual revenue` = sum(revenue_LTM, na.rm = TRUE),
            `Rev. per listing` = `Annual revenue` / n()) %>% 
  mutate(
    `% of all listings` = round(`Number of listings` /
                                  sum(`Number of listings`), 3),
    `% of all listings` = paste0(100 * `% of all listings`, "%"),
    `% of annual revenue` = `Annual revenue` / sum(`Annual revenue`)) %>% 
  mutate(
    `Annual revenue` = round(`Annual revenue`),
    `Annual revenue` = paste0("$", str_sub(`Annual revenue`, 1, -7), ".",
                              str_sub(`Annual revenue`, -6, -6), " million"),
    `% of annual revenue` = round(`% of annual revenue`, 3),
    `% of annual revenue` = paste0(100 * `% of annual revenue`, "%"),
    `Rev. per listing` = round(`Rev. per listing`),
    `Rev. per listing` = paste0("$", str_sub(`Rev. per listing`, 1, -4),
                                ",", str_sub(`Rev. per listing`, -3, -1))
  )

LTM_revenue_table <- # visual table
  LTM_revenue_table %>%
  gt() %>%
  tab_header(
    title = "LTM revenue",
    subtitle = glue::glue("2019-03-31 to 2020-03-31")
  ) %>%
  opt_row_striping() %>%
  gtsave("output/LTM_revenue_table.png", path = NULL)


# By borough
LTM_revenue %>%
  rename(`Listing type` = listing_type) %>%
  st_drop_geometry() %>%
  group_by(neighbourhood) %>%
  summarize(`Number of listings` = n(),
            `Active listings on 2020-03-14` = 
              as.numeric(length(property_ID[created <= key_date & scraped >= key_date])),
            `Annual revenue` = sum(revenue_LTM, na.rm = TRUE),
            `Rev. per listing` = `Annual revenue` / n(),
            `Annual growth ratio` = as.numeric(length(property_ID[housing == TRUE &
                                                                    created <= key_date & scraped >= key_date]) /
                                                 (length(property_ID[housing == TRUE &
                                                                       created <= key_date - years(1) &
                                                                       scraped >= key_date - years(1)])))) %>%
  mutate(
    `% of all listings` = `Number of listings` / sum(`Number of listings`),
    `% of annual revenue` = `Annual revenue` / sum(`Annual revenue`),
    `% annual listing growth` = `Annual growth ratio` - 1
  ) %>%
  drop_na() %>%
  select(-"Annual growth ratio", -"Number of listings") %>%
  # mutate(
  #   `% reservations from May-September` = c(0.7379253, 0.6388024, 0.7693436,
  #                                           0.8985275, 0.6794479, 0.7949283,
  #                                           0.6406896, 0.8268593, 0.7344935,
  #                                           0.8138371)) %>% 
  arrange(desc(`Active listings on 2020-03-14`)) %>% 
  filter(`Active listings on 2020-03-14` > 1000) %>% 
  rename(Borough = neighbourhood) %>% 
  gt() %>% 
  tab_header(
    title = "Boroughs with at least a thousand listing",
    subtitle = glue::glue("2019-03-14 to 2020-03-14")
  ) %>%
  fmt_number(columns = 2:4,
             sep_mark = " ",
             decimals = 0) %>% 
  opt_row_striping() %>% 
  fmt_percent(columns = 5:7)
  




### Bedroom breakdown ##########################################################

LTM_property %>% 
  st_drop_geometry() %>%  
  filter(housing == TRUE,
         listing_type == "Shared room",
         created <= key_date, scraped >= key_date) %>% 
  count(bedrooms) %>% 
  mutate(percentage = n / sum(n)) %>% 
  group_by(bedrooms) %>% 
  summarize(`Percentage of listings` = sum(percentage)) %>% 
  filter(`Percentage of listings` > 0.01) %>% 
  gt() %>% 
  tab_header(
    title = "LTM active properties, entire home/apt",
  ) %>%
  opt_row_striping() %>% 
  fmt_percent(columns = 2)



### Revenue distribution and commercial operators ##############################

## Host revenue percentiles

table_host_revenue <- 
  daily %>%
  filter(housing == TRUE, date > end_date - years(1), status == "R") %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price)*exchange_rate) %>%
  filter(rev > 0) %>%
  summarize(
    `Top 1%`  = sum(rev[rev > quantile(rev, c(0.99))] / sum(rev)),
    `Top 5%`  = sum(rev[rev > quantile(rev, c(0.95))] / sum(rev)),
    `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)),
    `Top 20%` = sum(rev[rev > quantile(rev, c(0.80))] / sum(rev))) 


## Median host income

LTM_revenue %>% 
  filter(revenue_LTM > 0, !is.na(host_ID)) %>% 
  group_by(host_ID) %>% 
  summarize("host_rev" = sum(revenue_LTM)*exchange_rate) %>% 
  pull(host_rev) %>%
  quantile() %>% 
  as.list() %>% 
  as_tibble() %>% 
  select(-`0%`) %>% 
  set_names(c("25th percentile", "Median", "75th percentile", 
              "100th percentile")) %>% 
  mutate_all(round, -2) %>% 
  drop_na() %>% 
  gt() %>% 
  tab_header(
    title = "Host income",
  ) %>%
  opt_row_striping() 


## Top earning host(s)
LTM_revenue %>% 
  st_drop_geometry() %>% 
  group_by(host_ID) %>% 
  summarize(host_rev = sum(revenue_LTM)*exchange_rate) %>% 
  filter(host_rev>0) %>% 
  arrange(-host_rev) %>% 
  drop_na() %>% 
  filter(host_rev >= 500000)  %>% 
  gt() %>% 
  tab_header(
    title = "Hosts that made at least half a million",
    subtitle = glue::glue("2019-03-14 to 2020-03-14")
  ) %>%
  fmt_number(columns = 2,
             sep_mark = " ",
             decimals = 0) %>% 
  opt_row_striping() 





































##### WHERE I'M AT
## Multilistings

ML_table <- 
  daily %>% 
  group_by(date) %>% 
  summarize(Listings = mean(multi),
            Revenue = sum(price * (status == "R") * multi * 
                            exchange_rate, na.rm = TRUE) / 
              sum(price * (status == "R") * exchange_rate, na.rm = TRUE)) %>% 
  gather(Listings, Revenue, key = `Multilisting percentage`, value = Value)

ML_table %>% 
  filter(date == key_date)

# Entire home multilistings

daily %>% 
  filter(listing_type == "Entire home/apt") %>% 
  group_by(date) %>% 
  summarize(Listings = sum(multi)) %>% 
  filter(date == end_date)

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

# Seasonal housing loss

# 2019
FREH_season_2019 <- 
  FREH %>% 
  filter(date >= season_start & date <= season_end)

seasonal_loss_2019 <- 
  nrow(filter(property, seasonal_2019 == TRUE)) - 
  property %>% 
  filter(seasonal_2019, property_ID %in% FREH_season_2019$property_ID) %>% 
  nrow()

#2018
FREH_season_2018 <-
  FREH %>% 
  filter(date >= season_start - years(1) & date <= season_end - years(1))

seasonal_loss_2018 <-
  nrow(filter(property, seasonal_2018 == TRUE)) - 
  property %>% 
  filter(seasonal_2018, property_ID %in% FREH_season_2018$property_ID) %>% 
  nrow()

#2017
FREH_season_2017 <- 
  FREH %>% 
  filter(date >= season_start - years(2) & date <= season_end - years(2))

seasonal_loss_2017 <-
  nrow(filter(property, seasonal_2017 == TRUE)) - 
  property %>% 
  filter(seasonal_2017, property_ID %in% FREH_season_2017$property_ID) %>% 
  nrow()

# Housing loss numbers

housing_loss <-
  FREH %>%
  group_by(date) %>%
  summarize(`Entire home/apt` = n()) %>%
  left_join(GH_total, by = "date") %>%
  rename(`Private room` = GH_average) %>%
  # mutate(`Summer listings` = case_when(
  #   date >= season_start & date <= season_end ~ seasonal_loss_2019,
  #   date >= season_start - years(1) & date <= season_end - years(1) ~ seasonal_loss_2018,
  #   date >= season_start - years(2) & date <= season_end - years(2) ~ seasonal_loss_2017,
  #   TRUE ~ 0L)) %>% 
  gather(`Entire home/apt`, `Private room`, #`Summer listings`, 
         key = `Listing type`, value = `Housing units`) 


# Current housing loss figure
sum(filter(housing_loss, date == end_date)$`Housing units`)

## Daily graphs:

GH %>% 
  filter(date == key_date) %>% 
  count()


## Total nights "R" in 2019
daily %>%
  filter(housing, date > end_date - years(1), status == "R") %>%
  count(property_ID) %>% 
  summarize(sum(n))




## Relate housing loss to rental vacancy rate

# vacancy_rate <- 1.016
# 
# housing <-
#   get_census("CA16", regions = list(CSD = "2466023"), level = "CSD",
#              vectors = c("v_CA16_4897", "v_CA16_405"))
# 
# housing %>%
#   select(
#     `v_CA16_405: Private dwellings occupied by usual residents`,
# `v_CA16_4897: Total - Tenant households in non-farm, non-reserve private dwellings - 25% sample data`
#          ) %>%
#   set_names(c("Dwellings", "Tenants")) %>%
#   pull(Tenants) %>%
#   {. * vacancy_rate * (vacancy_rate - 1)}


## Save files #####################################
save(property, file = "data/Montreal_property.Rdata")
save(housing_loss, file = "data/housing_loss.Rdata")





#### 2020-06-10 Getting data to compare with Cloe's KJ_CL report

daily_r_2020 <- 
  daily %>% 
  filter(date >= "2020-01-01",
         status == "R") %>% 
  left_join(select(st_drop_geometry(property), borough, property_ID), by= "property_ID")

daily_r_2020_plot <- 
  daily_r_2020 %>% 
  filter(date >= "2020-01-23",
         borough %in% !! unique(arrange(count(filter(daily_r_2020, status == "R", !is.na(borough)), borough), desc(n))$borough[1:10])) %>% 
  group_by(date) %>% 
  count(borough) %>% 
  arrange(desc(n)) %>% 
  group_by(borough) %>%
  filter(!is.na(date)) %>% 
  ggplot()+
  geom_smooth(aes(date, n, color = borough), se = F, size = 1.5)+
  scale_color_brewer(palette = "Paired")+
  ggtitle("Number of reservations per day (STR)")

ggsave("output/daily_r_2020_plot.pdf", plot = daily_r_2020_plot, width = 10, 
       height = 5, units = "in", useDingbats = FALSE)

daily %>% 
  filter(borough %in% unique(arrange(count(kj_mtl, borough), desc(n))$borough[1:10])) %>% 
  group_by(created) %>% 
  count(borough) %>% 
  group_by(borough) %>%
  filter(!is.na(created)) %>% 
  ggplot()+
  geom_line(aes(created, n, color = borough), se = F)+
  geom_smooth(aes(created, n, color = borough), se = F, size = 1.5)+
  scale_color_brewer(palette = "Paired")