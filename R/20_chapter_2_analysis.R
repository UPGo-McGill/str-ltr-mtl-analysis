#### Chapter 2 ANALYSIS ####################################################

### load libraries ###########################################
library(tidyverse)
library(lubridate)
library(sf)
library(gt)
library(future)
library(ggplot2)
library(data.table)

### LTM start_date and end_date
LTM_start_date <- as.Date("2019-01-01")
LTM_end_date <- as.Date("2019-12-31")

### Active daily listings ######################################################

# Average active daily listings in 2019
filter(daily, housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>% 
  count(date) %>% 
  summarize(round(mean(n), digit=-2))


# Average listings blocked for reservations in 2019
filter(daily, housing, status == "B", date >= LTM_start_date, date <= LTM_end_date) %>% 
  count(date) %>% 
  summarize(round(mean(n), digit=-2))


# Average number of hosts (taking out blocked 365 days)
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>%
  count(date, host_ID) %>% 
  count(date) %>% 
  summarize(round(mean(n), digit=-2))


# How many active listings out of housing
daily %>% 
  filter(housing == F, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>%
  count(date) %>% 
  summarize(round(mean(n), digit=-2))


# Highest sum of daily listing activity
daily %>% 
  filter(housing, status != "B") %>% 
  count(date) %>% 
  summarize(round(max(n), digit =-2))

daily %>% 
  filter(housing, status != "B") %>% 
  count(date) %>% 
  filter(n >= 12300)


# Active housing listings in 2019
property_2019 <-
  property %>%
  filter(housing, property_ID %in% 
           filter(daily, housing, status != "B", date >= LTM_start_date, date <= LTM_end_date)$property_ID) %>% 
  filter(created <= LTM_end_date)


# 2019 revenue
revenue_2019 <- 
  daily %>%
  filter(housing,
         date <= LTM_end_date, date >= LTM_start_date,
         status == "R") %>%
  group_by(property_ID) %>%
  summarize(revenue_LTM = sum(price) ) %>% 
  inner_join(property, .)

sum(revenue_2019$revenue_LTM, na.rm = T)
mean(revenue_2019$revenue_LTM, na.rm = T)

# average revenue by host
revenue_2019 %>% 
  st_drop_geometry() %>% 
  filter(!is.na(host_ID)) %>% 
  group_by(host_ID) %>% 
  summarize("host_rev" = sum(revenue_LTM)) %>% 
  summarize(mean(host_rev))

### Montreal in comparison with other major Canadian cities ######################################################


#


















#

### Location of STR listings and revenues in Montreal ######################################################

# By borough
boroughs_breakdown <- tibble(Borough = character(length = length(boroughs$borough)), 
                             `Daily active listings (average)` = numeric(length = length(boroughs$borough)),
                             `Annual revenue (CAD)` = numeric(length = length(boroughs$borough)),
                             `% of all listings` = numeric(length = length(boroughs$borough)),
                             `% of annual revenue` = numeric(length = length(boroughs$borough)),
                             `% of daily active listings (average) per dwellings` = numeric(length = length(boroughs$borough))
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
    summarize(sum(price ))
  
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
  
  boroughs_breakdown[i,6] <- daily %>%
    filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date,
           borough == boroughs$borough[[i]]) %>%
    group_by(borough) %>% 
    count(date) %>%
    summarize(`Daily active listings (average)` = mean(n, na.rm = T)) %>%
    left_join(select(st_drop_geometry(boroughs), borough, dwellings), by = "borough") %>%
    mutate(percentage = `Daily active listings (average)` / dwellings) %>% 
    summarize(percentage)
  
}

boroughs_breakdown %>% 
  filter(`Daily active listings (average)` > 100) %>% 
  arrange(desc(`Daily active listings (average)`)) %>%
  mutate(`Daily active listings (average)` = round(`Daily active listings (average)`, digit=-1),
         `Annual revenue (CAD)` = round(`Annual revenue (CAD)`),
         `Annual revenue (CAD)` = paste0("$", str_sub(`Annual revenue (CAD)`, 1, -7), ".",
                                         str_sub(`Annual revenue (CAD)`, -6, -6), " million")) %>%
  rename(`Active listings as % of dwellings` = `% of daily active listings (average) per dwellings`) %>% 
  gt() %>% 
  tab_header(
    title = "Borough breakdown",
    subtitle = "Boroughs with more than 100 daily active listings average, 2019"
  ) %>%
  opt_row_striping() %>% 
  fmt_percent(columns = 4:6, decimals = 1) %>% 
  fmt_number(columns = 2,
             decimals = 0)


### STR listing types and sizes in Montreal ######################################################

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
    summarize(sum(price ))
  
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
  rename(`% average daily listing growth (YOY 2018-2019)` = `% average daily listing growth`) %>% 
  gt() %>% 
  tab_header(
    title = "Listing type breakdown",
    subtitle = "2019"
  ) %>%
  opt_row_striping() %>% 
  fmt_percent(columns = 4:6, decimals = 1) %>% 
  fmt_number(columns = 2,
             decimals = 0)


### STRs operating in condos ######################################################

DAs_raffle <-
  cancensus::get_census(
    dataset = "CA16", regions = list(CSD = c("2466023")), level = "DA",
    vectors = c("v_CA16_4840", "v_CA16_4841", "v_CA16_4842", "v_CA16_4836", "v_CA16_4837", "v_CA16_4838"),
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(GeoUID, CSD_UID, Population, Dwellings, "v_CA16_4840: Total - Occupied private dwellings by condominium status - 25% sample data",
         "v_CA16_4841: Condominium", "v_CA16_4842: Not condominium", "v_CA16_4836: Total - Private households by tenure - 25% sample data", 
         "v_CA16_4837: Owner", "v_CA16_4838: Renter") %>% 
  set_names(c("GeoUID", "CMA_UID", "population", "dwellings", "parent_condo", "condo", "not_condo", "parent_tenure",
              "owner", "renter", "geometry")) %>% 
  mutate(p_condo = condo/parent_condo,
         p_not_condo = not_condo/parent_condo,
         p_owner = owner/parent_tenure,
         p_renter = renter/parent_tenure) %>% 
  select(GeoUID, dwellings, CMA_UID, p_condo, p_not_condo, p_owner, p_renter) %>% 
  st_set_agr("constant")

DAs_raffle$p_not_condo[DAs_raffle$p_not_condo >= "1"] <- "1"
DAs_raffle$p_not_condo <- as.numeric(DAs_raffle$p_not_condo)

active_properties_2019 <- 
  daily %>% 
  filter(date >="2019-01-01", date<="2020-01-01") %>% 
  filter(status == "A" | status == "R") %>% 
  group_by(property_ID) %>% 
  count()

active_properties_2019 <- property %>%
  filter(property_ID %in% active_properties_2019$property_ID)

raffle_2019 <- 
  active_properties_2019 %>% 
  strr_raffle(DAs_raffle, GeoUID, dwellings, seed=1, diagnostic = TRUE) 

trial_tenure_2019 <- left_join(st_drop_geometry(raffle_2019), st_drop_geometry(DAs_raffle), by="GeoUID")

unnested_trial_tenure_2019 <- trial_tenure_2019 %>% 
  unnest(candidates)

unnested_trial_tenure_2019 <- unnested_trial_tenure_2019 %>% 
  left_join(., st_drop_geometry(DAs_raffle), by = c("poly_ID" = "GeoUID")) 

tenure_probabilities_2019 <- unnested_trial_tenure_2019 %>% 
  ungroup() %>% 
  group_by(property_ID) %>% 
  summarize(prob_condo = sum(p_condo.x*(probability/sum(probability))),
            prob_renter = sum(p_renter.x*(probability/sum(probability))),
            prob_owner = sum(p_owner.x*(probability/sum(probability)))
  ) 

tenure_probabilities_sf_2019 <- 
  left_join(tenure_probabilities_2019, trial_tenure_2019, by="property_ID") %>% 
  left_join(., DAs_raffle, by = "GeoUID") %>% 
  select(property_ID, GeoUID, dwellings.x, prob_condo, prob_renter, prob_owner, geometry) %>% #number_condo, number_renter, number_owner,
  rename(dwellings=dwellings.x) %>% 
  st_as_sf()


### STR growth rate ######################################################

# YOY growth of average daily active listings
# 2019 to 2020
(filter(daily, housing, status != "B", date >= LTM_start_date + years(1), date <= max(date)) %>% 
   count(date) %>% 
   summarize(mean(n)) - 
   filter(daily, housing, status != "B", date >= LTM_start_date , date <= max(date) - years(1)) %>% 
   count(date) %>% 
   summarize(mean(n))) /
  filter(daily, housing, status != "B", date >= LTM_start_date , date <= max(date) - years(1)) %>% 
  count(date) %>% 
  summarize(mean(n))

# 2018 to 2019
(filter(daily, housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>% 
    count(date) %>% 
    summarize(mean(n)) - 
    filter(daily, housing, status != "B", date >= LTM_start_date - years(1), date <= LTM_end_date - years(1)) %>% 
    count(date) %>% 
    summarize(mean(n))) /
  filter(daily, housing, status != "B", date >= LTM_start_date - years(1), date <= LTM_end_date - years(1)) %>% 
  count(date) %>% 
  summarize(mean(n))

# 2017 to 2019
(filter(daily, housing, status != "B", date >= LTM_start_date - years(1), date <= LTM_end_date - years(1)) %>% 
    count(date) %>% 
    summarize(mean(n)) - 
    filter(daily, housing, status != "B", date >= LTM_start_date - years(2), date <= LTM_end_date - years(2)) %>% 
    count(date) %>% 
    summarize(mean(n)) + 960) /
  filter(daily, housing, status != "B", date >= LTM_start_date - years(2), date <= LTM_end_date - years(2)) %>% 
  count(date) %>% 
  summarize(mean(n) + 960)

daily_variation <- 
  left_join(
    (daily %>% 
       filter(housing, status == "R", date >= "2017-01-01") %>% 
       group_by(date) %>% 
       summarize(daily_rev = sum(price)) %>% 
       mutate(rev_var = as.numeric(NA)) %>% 
       filter(date != "2020-02-29")),
    (daily %>% 
       filter(status != "B", date >= "2017-01-01") %>% 
       count(date) %>% 
       mutate(n = if_else(date <= "2017-05-31", n + 960, as.numeric(n)),
              n_var = as.numeric(NA)) %>% 
       filter(date != "2020-02-29")), by = "date")


for(i in 366:length(daily_variation$date)) {
  year1_n <- 
    daily_variation %>% 
    filter(date == daily_variation$date[[i]] - years(1)) %>% 
    pull(n)
  
  year2_n <- 
    daily_variation %>% 
    filter(date == daily_variation$date[[i]]) %>% 
    pull(n)
  
  daily_variation$n_var[[i]] <- as.numeric((year2_n - year1_n) / year1_n)
  
  
  
  
  year1_rev <- 
    daily_variation %>% 
    filter(date == daily_variation$date[[i]] - years(1)) %>% 
    pull(daily_rev)
  
  year2_rev <- 
    daily_variation %>% 
    filter(date == daily_variation$date[[i]]) %>% 
    pull(daily_rev)
  
  daily_variation$rev_var[[i]] <- as.numeric((year2_rev - year1_rev) / year1_rev)
  
}

daily_variation <- 
  rbind(
    daily_variation %>% 
      rename(variation = rev_var) %>% 
      mutate(group = "Revenue") %>% 
      select(date, variation, group),
    
    daily_variation %>% 
      rename(variation = n_var) %>% 
      mutate(group = "Active Listings") %>% 
      select(date, variation, group)
  ) %>% 
  filter(date >= "2018-01-01")

### STR revenue distribution ######################################################

## Host revenue percentiles
daily %>%
  filter(housing == TRUE, date <= LTM_end_date, date >= LTM_start_date, status == "R") %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price)) %>%
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
  st_drop_geometry() %>% 
  filter(revenue_LTM > 0) %>% 
  group_by(host_ID) %>% 
  summarize("host_rev" = sum(revenue_LTM)) %>% 
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


### Multilistings ######################################################

ML_table <- 
  daily %>% 
  filter(status != "B") %>% 
  group_by(date) %>% 
  summarize(Listings = mean(multi),
            Revenue = sum(price * (status == "R") * multi * 
                            exchange_rate, na.rm = TRUE) / 
              sum(price * (status == "R") , na.rm = TRUE))

ML_table %>% 
  filter(date >= LTM_start_date, date <= LTM_end_date) %>% 
  summarize(mean(Listings), mean(Revenue))

# Entire home multilistings
daily %>% 
  filter(listing_type == "Entire home/apt") %>% 
  group_by(date) %>% 
  summarize(Listings = sum(multi)) %>% 
  filter(date == key_date)




