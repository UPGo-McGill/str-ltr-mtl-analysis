### load libraries and data ###########################################
library(tidyverse)
library(lubridate)
library(sf)
library(gt)
library(future)
library(ggplot2)
library(data.table)

memory.limit(size = 48000)
plan(multiprocess, workers = 4)

load("data/montreal_str_processed_a.Rdata")



#LTM start_date and end_date
LTM_start_date <- as.Date("2019-01-01")
LTM_end_date <- as.Date("2019-12-31")
###  ####################################################

### Active daily listings ######################################################

## Active listings from property file

# Average listings reserved or available / blocked in 2019
filter(daily, housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>% 
  count(date) %>% 
  summarize(mean(n))
  
# Active housing listings in 2019
property_2019 <-
  property %>%
  filter(housing, property_ID %in% filter(daily, housing, status != "B", date >= LTM_start_date, date <= LTM_end_date)$property_ID)

# Average number of hosts (taking out blocked 365 days)
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>%
  count(date, host_ID) %>% 
  count(date) %>% 
  summarize(mean(n))


# 2019 revenue
revenue_2019 <- 
daily %>%
  filter(housing,
         date <= LTM_end_date, date > LTM_start_date,
         status == "R") %>%
  group_by(property_ID) %>%
  summarize(revenue_LTM = sum(price) * exchange_rate) %>% 
  left_join(property_2019, .)

sum(revenue_2019$revenue_LTM, na.rm = T)

# YOY growth rate
#2020-2019
(filter(daily, housing, status != "B", date >= key_date, date <= key_date) %>% 
  nrow() -
  filter(daily, housing, status != "B", date >= key_date - years(1), date <= key_date - years(1)) %>% 
  nrow()) /
  filter(daily, housing, status != "B", date >= key_date - years(1), date <= key_date - years(1)) %>% 
  nrow() *100
#2018-2019
(filter(daily, housing, status != "B", date >= key_date - years(1), date <= key_date - years(1)) %>% 
    nrow() -
    filter(daily, housing, status != "B", date >= key_date - years(2), date <= key_date - years(2)) %>% 
    nrow()) /
  filter(daily, housing, status != "B", date >= key_date - years(2), date <= key_date - years(2)) %>% 
  nrow() *100
#2017-2018
(filter(daily, housing, status != "B", date >= key_date - years(2), date <= key_date - years(2)) %>% 
    nrow() -
    filter(daily, housing, status != "B", date >= key_date - years(3), date <= key_date - years(3)) %>% 
    nrow()) /
  filter(daily, housing, status != "B", date >= key_date - years(3), date <= key_date - years(3)) %>% 
  nrow() *100







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

revenue_2019 %>% 
#  filter(housing, created <= key_date, scraped > key_date - years(1)) %>% 
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
  ) %>% 
  gt() %>%
  tab_header(
    title = "2019 revenue",
    subtitle = glue::glue("")
  ) %>%
  opt_row_striping()


# By borough
boroughs_breakdown <- tibble(Borough = character(length = length(boroughs$borough)), 
                             `Daily active listings (average)` = numeric(length = length(boroughs$borough)),
                             `Annual revenue (CAD)` = numeric(length = length(boroughs$borough)),
                             `% of all listings` = numeric(length = length(boroughs$borough)),
                             `% of annual revenue` = numeric(length = length(boroughs$borough)),
                             `% average daily listing growth` = numeric(length = length(boroughs$borough))
                             )


for (i in 1:length(boroughs$borough)) { # testing a bigger loop, good one with try(), does work. long, since not doing remotely.
      
      boroughs_breakdown[i,1] <- boroughs$borough[[i]]

      boroughs_breakdown[i,2] <- daily_boroughs %>%
        filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date,
               borough == boroughs$borough[[i]]) %>%
        count(date, host_ID) %>%
        count(date) %>%
        summarize(`Active daily listings average` = mean(n, na.rm = T)) %>%
        arrange(desc(`Active daily listings average`))

      boroughs_breakdown[i,3] <- daily_boroughs %>%
        filter(housing, status == "R", date >= LTM_start_date, date <= LTM_end_date,
               borough == boroughs$borough[[i]]) %>%
        summarize(sum(price))

      boroughs_breakdown[i,4] <- daily_boroughs %>%
        filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date,
               borough == boroughs$borough[[i]]) %>%
        nrow() /
        daily_boroughs %>%
        filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>%
        nrow()

      boroughs_breakdown[i,5] <- daily_boroughs %>%
        filter(housing, status == "R", date >= LTM_start_date, date <= LTM_end_date,
               borough == boroughs$borough[[i]]) %>%
        summarize(sum(price)) /
        daily_boroughs %>%
        filter(housing, status == "R", date >= LTM_start_date, date <= LTM_end_date) %>%
        summarize(sum(price))
      
      boroughs_breakdown[i,6] <- (
        daily_boroughs %>%
        filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date,
               borough == boroughs$borough[[i]]) %>%
        count(date, host_ID) %>%
        count(date) %>%
        summarize(`Active daily listings average` = mean(n, na.rm = T)) %>%
        arrange(desc(`Active daily listings average`)) -
        daily_boroughs %>%
        filter(housing, status != "B", date >= LTM_start_date - years(1), date <= LTM_end_date - years(1),
               borough == boroughs$borough[[i]]) %>%
        count(date, host_ID) %>%
        count(date) %>%
        summarize(`Active daily listings average` = mean(n, na.rm = T)) %>%
        arrange(desc(`Active daily listings average`))
        ) /
        daily_boroughs %>%
        filter(housing, status != "B", date >= LTM_start_date - years(1), date <= LTM_end_date - years(1),
               borough == boroughs$borough[[i]]) %>%
        count(date, host_ID) %>%
        count(date) %>%
        summarize(`Active daily listings average` = mean(n, na.rm = T)) %>%
        arrange(desc(`Active daily listings average`))
      

}

boroughs_breakdown %>% 
  filter(`Daily active listings (average)` > 100) %>% 
  arrange(desc(`Daily active listings (average)`)) %>%
  mutate(`Daily active listings (average)` = round(`Daily active listings (average)`, digit=-1),
         `Annual revenue (CAD)` = round(`Annual revenue (CAD)`, digit =-5)) %>% 
  gt() %>% 
  tab_header(
    title = "Borough breakdown",
    subtitle = "2019"
  ) %>%
  opt_row_striping() %>% 
  fmt_percent(columns = 4:6, decimals = 0) %>% 
  fmt_number(columns = 2:3,
             decimals = 0)


# listings per dwellings, boroughs

daily_boroughs %>%
  filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>%
  group_by(borough) %>% 
  count(date, host_ID) %>%
  count(date) %>%
  summarize(`Daily active listings (average)` = mean(n, na.rm = T)) %>%
  left_join(select(st_drop_geometry(boroughs), borough, dwellings), .) %>% 
  mutate(percentage = `Daily active listings (average)` / dwellings,
         `Daily active listings (average)` = round(`Daily active listings (average)`, digit=-1)) %>% 
  arrange(desc(`Daily active listings (average)`)) %>% 
  filter(`Daily active listings (average)`>100) %>% 
  select(borough, `Daily active listings (average)`, dwellings, percentage) %>% 
  gt() %>% 
  fmt_number(columns = 2:3,
             decimals = 0) %>% 
  fmt_percent(columns = 4, decimals = 2)


### Bedroom breakdown ##########################################################

LTM_property %>% 
  st_drop_geometry() %>%  
  filter(housing == TRUE,
         listing_type == "Entire home/apt",
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

daily %>%
  filter(housing == TRUE, date < key_date, date > key_date - years(1), status == "R") %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price)*exchange_rate) %>%
  filter(rev > 0) %>%
  summarize(
    `Top 1%`  = sum(rev[rev > quantile(rev, c(0.99))] / sum(rev)),
    `Top 5%`  = sum(rev[rev > quantile(rev, c(0.95))] / sum(rev)),
    `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)),
    `Top 20%` = sum(rev[rev > quantile(rev, c(0.80))] / sum(rev))) %>% 
  gt() %>% 
  tab_header(
    title = "% of total STR revenue in the hand of x%  ofhosts",
  ) %>%
  opt_row_striping() 


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


## Multilistings

ML_table <- 
  daily %>% 
  group_by(date) %>% 
  summarize(Listings = mean(multi),
            Revenue = sum(price * (status == "R") * multi * 
                            exchange_rate, na.rm = TRUE) / 
              sum(price * (status == "R") * exchange_rate, na.rm = TRUE))

ML_table %>% 
  filter(date == key_date)

# Entire home multilistings

daily %>% 
  filter(listing_type == "Entire home/apt") %>% 
  group_by(date) %>% 
  summarize(Listings = sum(multi)) %>% 
  filter(date == key_date)

### Housing loss ###############################################################

FREH_2020 %>% 
  filter(date == key_date) %>% 
  count()

FREH_2020 %>% 
  count(date) %>% 
  filter(date <= end_date, date >= "2020-01-01") %>% 
  ggplot() +
  geom_line(aes(date, n), colour = "black", size = 1) +
  theme_minimal() +
  scale_y_continuous(name = NULL) +
  ggtitle("FREH_2020 listings in Montreal (in 2020)")

GH %>% 
  st_drop_geometry() %>% 
  group_by(date) %>% 
  summarize(GH_units = sum(housing_units)) %>%
  ggplot() +
  geom_line(aes(date, GH_units), colour = "black", size = 1) +
  theme_minimal() +
  scale_y_continuous(name = NULL) +
  ggtitle("Units converted to ghost hostels in Montreal")


GH_total <-
  GH %>%
  st_drop_geometry() %>%
  group_by(date) %>%
  summarize(GH_units = sum(housing_units)) %>%
  mutate(GH_average = frollmean(GH_units, 30, align = "right")) %>%
  mutate(GH_average = if_else(is.na(GH_average), 8, GH_average)) %>%
  select(-GH_units)

# Housing loss numbers

housing_loss <-
  FREH_2020 %>%
  group_by(date) %>%
  summarize(`Entire home/apt` = n()) %>%
  left_join(GH_total, by = "date") %>%
  rename(`Private room` = GH_average) %>%
  gather(`Entire home/apt`, `Private room`, 
         key = `Listing type`, value = `Housing units`) 


# Current housing loss figure
sum(filter(housing_loss, date == key_date)$`Housing units`)

## Daily graphs:

GH %>% 
  filter(date == key_date) %>% 
  count()


## Total nights "R"
daily %>%
  filter(housing, date > key_date - years(1), date < key_date, status == "R") %>%
  count(property_ID) %>% 
  summarize(sum(n))





## Save files #####################################
save.image(file = "data/montreal_str_processed_c.Rdata")
