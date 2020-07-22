### load libraries and data ###########################################
library(tidyverse)
library(lubridate)
library(sf)
library(gt)
library(future)
library(ggplot2)
library(data.table)

memory.limit(size = 48000)
# plan(multiprocess, workers = 4)

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
  summarize(round(mean(n), digit=-2))
  
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
(
  filter(daily, housing, status != "B", date == LTM_start_date + years(1)) %>% 
  nrow() -
  filter(daily, housing, status != "B", date == LTM_start_date) %>% 
  nrow()
  ) /
  filter(daily, housing, status != "B", date == LTM_start_date) %>% 
  nrow() * 100
#2018-2019
(filter(daily, housing, status != "B", date == LTM_start_date) %>% 
    nrow() -
    filter(daily, housing, status != "B", date == LTM_start_date - years(1)) %>% 
    nrow()) /
  filter(daily, housing, status != "B", date == LTM_start_date - years(1)) %>% 
  nrow() *100
#2017-2018
(filter(daily, housing, status != "B", date == LTM_start_date - years(1)) %>% 
    nrow() -
    (filter(daily, housing, status != "B", date == LTM_start_date - years(2)) %>% 
    nrow() + 960)) /
  (filter(daily, housing, status != "B", date == LTM_start_date - years(2)) %>% 
  nrow() + 960) *100



filter(daily, housing, status != "B") %>%
  count(date) %>% 
  ggplot()+
  geom_line(aes(date, n))+
  ggtitle("Sum of daily active listing per day")






### Montreal mapping prep ###################################################


### Which STR platforms are used in Montreal? ###################################

# Airbnb and not Homeaway

nrow(filter(property_2019, !is.na(ab_property), is.na(ha_property)))

# Homeaway and not Airbnb
nrow(filter(property_2019, !is.na(ha_property), is.na(ab_property)))

# Both Airbnb and Homeaway
nrow(filter(property_2019, !is.na(ha_property), !is.na(ab_property)))

nrow(property_2019)


### Listing type prevalence ####################################################

# Listing types for city

unique_listing_type <- unique(daily$listing_type)[1:3]

listing_type_breakdown <- tibble(`Listing Type` = character(length = length(unique_listing_type)), 
                             `Daily active listings (average)` = numeric(length = length(unique_listing_type)),
                             `Annual revenue (CAD)` = numeric(length = length(unique_listing_type)),
                             `% of all listings` = numeric(length = length(unique_listing_type)),
                             `% of annual revenue` = numeric(length = length(unique_listing_type)),
                             `% average daily listing growth` = numeric(length = length(unique_listing_type))
)


for (i in 1:length(unique_listing_type)) { # testing a bigger loop, good one with try(), does work. long, since not doing remotely.
  
  listing_type_breakdown[i,1] <- unique_listing_type[[i]]
  
  listing_type_breakdown[i,2] <- daily %>%
    filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date,
           listing_type == unique_listing_type[[i]]) %>%
    count(date) %>% 
    summarize(mean(n))
  
  listing_type_breakdown[i,3] <- daily %>%
    filter(housing, status == "R", date >= LTM_start_date, date <= LTM_end_date,
           listing_type == unique_listing_type[[i]]) %>%
    summarize(sum(price * exchange_rate))
  
  listing_type_breakdown[i,4] <- daily %>%
    filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date,
           listing_type == unique_listing_type[[i]]) %>%
    nrow() /
    daily %>%
    filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>%
    nrow()
  
  listing_type_breakdown[i,5] <- daily %>%
    filter(housing, status == "R", date >= LTM_start_date, date <= LTM_end_date,
           listing_type == unique_listing_type[[i]]) %>%
    summarize(sum(price)) /
    daily %>%
    filter(housing, status == "R", date >= LTM_start_date, date <= LTM_end_date) %>%
    summarize(sum(price))
  
  listing_type_breakdown[i,6] <- (
    daily %>%
      filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date,
             listing_type == unique_listing_type[[i]]) %>%
      count(date) %>%
      summarize(mean(n, na.rm = T)) -
      daily %>%
      filter(housing, status != "B", date >= LTM_start_date - years(1), date <= LTM_end_date - years(1),
             listing_type == unique_listing_type[[i]]) %>%
      count(date) %>%
      summarize(mean(n, na.rm = T))
  ) /
    daily %>%
    filter(housing, status != "B", date >= LTM_start_date - years(1), date <= LTM_end_date - years(1),
           listing_type == unique_listing_type[[i]]) %>%
    count(date) %>%
    summarize(mean(n, na.rm = T))
  
  
}

listing_type_breakdown %>% 
  arrange(desc(`Daily active listings (average)`)) %>%
  mutate(`Daily active listings (average)` = round(`Daily active listings (average)`, digit=-1),
         `Annual revenue (CAD)` = round(`Annual revenue (CAD)`),
         `Annual revenue (CAD)` = paste0("$", str_sub(`Annual revenue (CAD)`, 1, -7), ".",
                                         str_sub(`Annual revenue (CAD)`, -6, -6), " million")) %>% 
  rename(`% average daily listing growth (YOY)` = `% average daily listing growth`) %>% 
  gt() %>% 
  tab_header(
    title = "Listing type breakdown",
    subtitle = "2019"
  ) %>%
  opt_row_striping() %>% 
  fmt_percent(columns = 4:6, decimals = 0) %>% 
  fmt_number(columns = 2,
             decimals = 0)


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

      boroughs_breakdown[i,2] <- daily %>%
        filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date,
               borough == boroughs$borough[[i]]) %>%
        count(date) %>% 
        summarize(mean(n))

      boroughs_breakdown[i,3] <- daily %>%
        filter(housing, status == "R", date >= LTM_start_date, date <= LTM_end_date,
               borough == boroughs$borough[[i]]) %>%
        summarize(sum(price * exchange_rate))

      boroughs_breakdown[i,4] <- daily %>%
        filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date,
               borough == boroughs$borough[[i]]) %>%
        nrow() /
        daily %>%
        filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>%
        nrow()

      boroughs_breakdown[i,5] <- daily %>%
        filter(housing, status == "R", date >= LTM_start_date, date <= LTM_end_date,
               borough == boroughs$borough[[i]]) %>%
        summarize(sum(price)) /
        daily %>%
        filter(housing, status == "R", date >= LTM_start_date, date <= LTM_end_date) %>%
        summarize(sum(price))
      
      boroughs_breakdown[i,6] <- (
        daily %>%
        filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date,
               borough == boroughs$borough[[i]]) %>%
        count(date) %>%
        summarize(mean(n, na.rm = T)) -
        daily %>%
        filter(housing, status != "B", date >= LTM_start_date - years(1), date <= LTM_end_date - years(1),
               borough == boroughs$borough[[i]]) %>%
        count(date) %>%
        summarize(mean(n, na.rm = T))
        ) /
        daily %>%
        filter(housing, status != "B", date >= LTM_start_date - years(1), date <= LTM_end_date - years(1),
               borough == boroughs$borough[[i]]) %>%
        count(date) %>%
        summarize(mean(n, na.rm = T))
      

}

boroughs_breakdown %>% 
  filter(`Daily active listings (average)` > 100) %>% 
  arrange(desc(`Daily active listings (average)`)) %>%
  mutate(`Daily active listings (average)` = round(`Daily active listings (average)`, digit=-1),
         `Annual revenue (CAD)` = round(`Annual revenue (CAD)`),
         `Annual revenue (CAD)` = paste0("$", str_sub(`Annual revenue (CAD)`, 1, -7), ".",
                            str_sub(`Annual revenue (CAD)`, -6, -6), " million")) %>% 
  rename(`% average daily listing growth (YOY)` = `% average daily listing growth`) %>% 
  gt() %>% 
  tab_header(
    title = "Borough breakdown",
    subtitle = "2019"
  ) %>%
  opt_row_striping() %>% 
  fmt_percent(columns = 4:6, decimals = 0) %>% 
  fmt_number(columns = 2,
             decimals = 0)


# listings per dwellings, boroughs

daily_boroughs %>%
  filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>%
  group_by(borough) %>% 
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

property_2019 %>% 
  st_drop_geometry() %>%  
  filter(housing == TRUE,
         listing_type == "Entire home/apt") %>% 
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
  filter(housing == TRUE, date <= LTM_end_date, date >= LTM_start_date, status == "R") %>%
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
    title = "% of total STR revenue in the hand of x%  of hosts",
  ) %>%
  opt_row_striping() 


## Median host income

revenue_2019 %>% 
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
revenue_2019 %>% 
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
  filter(status != "B") %>% 
  group_by(date) %>% 
  summarize(Listings = mean(multi),
            Revenue = sum(price * (status == "R") * multi * 
                            exchange_rate, na.rm = TRUE) / 
              sum(price * (status == "R") * exchange_rate, na.rm = TRUE))

ML_table %>% 
  filter(date >= LTM_start_date, date <= LTM_end_date) %>% 
  summarize(mean(Listings), mean(Revenue))

# Entire home multilistings

daily %>% 
  filter(listing_type == "Entire home/apt") %>% 
  group_by(date) %>% 
  summarize(Listings = sum(multi)) %>% 
  filter(date == key_date)

### Housing loss ###############################################################

FREH %>% 
  filter(date == LTM_end_date) %>% 
  nrow()

FREH %>% 
  count(date) %>% 
  filter(date <= LTM_end_date - years(1), date >= LTM_start_date - years(2)) %>% 
  ggplot() +
  geom_line(aes(date, n), colour = "black", size = 1) +
  theme_minimal() +
  scale_y_continuous(name = NULL) +
  ggtitle("FREH listings in Montreal (in 2017-2018)")

## how many units are part of a GH
GH %>% 
  st_drop_geometry() %>% 
  filter(date >= LTM_start_date, date <= LTM_end_date) %>% 
  group_by(date) %>% 
  summarize(GH_units = sum(housing_units)) %>% 
  summarize(mean(GH_units))

property %>% 
  filter(!is.na(ha_property)) %>% 
  arrange(scraped)

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
  mutate(GH_average = frollmean(GH_units, 30, align = "right", fill = 198)) %>%
  select(-GH_units)

# mean(GH_total[1:30,2]$GH_units) = 198.13

GH_total %>% 
  filter(date >= LTM_start_date, date <= LTM_end_date) %>% 
  ggplot()+
  geom_line(aes(date, GH_units)) +
  ggtitle("Sum of housing units, GH (without rolling average)")

daily %>% 
  filter(status != "B", date >= LTM_start_date, date <= LTM_end_date) %>% 
  count(date) %>% 
  ggplot()+
  geom_line(aes(date, n)) +
  ggtitle("Daily (count of R and A)")

# Housing loss numbers

housing_loss <-
  FREH %>%
  group_by(date) %>%
  summarize(`Entire home/apt` = n()) %>%
  left_join(GH_total, by = "date") %>%
  rename(`Private room` = GH_average) %>%
  gather(`Entire home/apt`, `Private room`, 
         key = `Listing type`, value = `Housing units`) 

# housing loss variation
(housing_loss %>% 
  filter(date == LTM_end_date) %>% 
  summarize(sum(`Housing units`)) - 
  housing_loss %>% 
  filter(date == LTM_end_date - years(1)) %>% 
  summarize(sum(`Housing units`))
) /
  housing_loss %>% 
  filter(date == LTM_end_date - years(1)) %>% 
  summarize(sum(`Housing units`))
  

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
# save.image(file = "data/montreal_str_processed_c.Rdata")
