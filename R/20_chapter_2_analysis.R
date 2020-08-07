#### Chapter 2 ANALYSIS ####################################################

source("R/01_startup.R")

load("output/str_processed.Rdata")
load("output/geometry.Rdata")



daily %>% 
  filter(date >= "2020-01-01") %>% 
  mutate(status = if_else(status == "R" & booked_date == "2020-05-30" & !is.na(booked_date), "B", status)) %>%
  count(date, status) %>% 
  ggplot()+
  geom_line(aes(date, n, color= status))
  
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
  filter(n >= 11800)


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

round(sum(revenue_2019$revenue_LTM), digit =-5)
round(mean(revenue_2019$revenue_LTM), digit =-2)

# average listings revenue per average of daily active listings
(sum(revenue_2019$revenue_LTM) /
  filter(daily, housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>% 
  count(date) %>% 
  summarize(round(mean(n)))
) %>% 
  round(digit=-2)

# average revenue per host (taking out hosts blocked 365 days)
revenue_2019 %>% 
  st_drop_geometry() %>% 
  filter(!is.na(host_ID)) %>% # because if not, grouping the sum of host_ID = NA makes it a top earner, and brings average up
  group_by(host_ID) %>% 
  summarize("host_rev" = sum(revenue_LTM)) %>% 
  summarize(mean(host_rev))
 

(sum(revenue_2019$revenue_LTM) /
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>%
  count(host_ID) %>% 
  nrow()
) %>% 
  round(digit=-2)

(sum(revenue_2019$revenue_LTM) /
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>%
  count(date, host_ID) %>% 
  count(date) %>% 
  summarize(round(mean(n)))
) %>% 
  round(digit=-2)



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
                             `Active listings as % of dwellings` = numeric(length = length(boroughs$borough))
)


for (i in 1:length(boroughs$borough)) {
  
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


for (i in 1:length(unique_listing_type)) { 
  
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



# Bedroom counts
property %>% 
  st_drop_geometry() %>% 
  filter(housing == TRUE,
         listing_type == "Entire home/apt") %>% 
  mutate(bedrooms = ifelse(bedrooms >= 3, "3+", bedrooms)) %>% 
  count(bedrooms) %>% 
  mutate(percentage = n / sum(n)) %>% 
  group_by(bedrooms) %>% 
  summarize(perc_bedrooms = sum(percentage)) 



### STR growth rate ######################################################

# YOY growth of average daily active listings
# 2019 to 2020
(filter(daily, housing, status != "B", date >= LTM_start_date + years(1), date <= max(daily$date)) %>% 
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
  summarize(mean(n) + 960) # +960 to account for HA


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

# More than 500k
revenue_2019 %>% 
  st_drop_geometry() %>% 
  filter(revenue_LTM > 0) %>% 
  group_by(host_ID) %>% 
  summarize("host_rev" = sum(revenue_LTM)) %>% 
  filter(host_rev > 500000) %>% 
  nrow()

### Multilistings ######################################################

ML_table <- 
  daily %>% 
  filter(status != "B") %>% 
  group_by(date) %>% 
  summarize(Listings = mean(multi),
            Revenue = sum(price * (status == "R") * multi, na.rm = TRUE) / 
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




