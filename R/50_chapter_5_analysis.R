#### Chapter 5 ANALYSIS ####################################################

source("R/01_startup.R")

load("output/str_processed.Rdata")
load("output/geometry.Rdata")
load("output/ltr_processed.Rdata")

### Overview of the number of matches #################################################################

# distinct LTR listings
unique_ltr <- 
  ltr %>% 
  st_drop_geometry() %>% 
  arrange(desc(scraped)) %>% 
  distinct(id, .keep_all = T)

unique_ltr %>% 
  nrow()

# number of unique STR listings that matched
property %>% 
  filter(!is.na(ltr_ID)) %>% 
  nrow()

# number of unique STR listings that matched in 2020
property %>% 
  filter(!is.na(ltr_ID), scraped >= "2020-01-01") %>% 
  nrow()

# LTR listings which matched
ltr %>% 
  st_drop_geometry() %>% 
  filter(!is.na(property_ID)) %>% 
  unnest(property_ID) %>% 
  filter(!is.na(property_ID)) %>% 
  count(id)

#unique matching property_ID locations using street address
ltr_unique_property_ID <-
  ltr %>% 
  st_drop_geometry() %>% 
  filter(!is.na(property_ID)) %>% 
  unnest(property_ID) %>% 
  filter(property_ID %in% filter(property, scraped >= "2020-01-01")$property_ID) %>% 
  arrange(desc(scraped)) %>% 
  distinct(property_ID) %>% 
  inner_join(unnest(filter(ltr, !is.na(property_ID)), property_ID), by = "property_ID") %>% 
  arrange(desc(scraped)) %>% 
  distinct(property_ID, .keep_all = T)

# rental agreement and agreement type
ltr_unique_property_ID %>% 
  count(short_long) %>% 
  mutate(perc = n/sum(n))

ltr_unique_property_ID %>% 
  count(type) %>% 
  mutate(perc = n/sum(n))


### Spatial distribution of the matches #################################################################

#by boroughs
ltr_unique_property_ID %>% 
  count(borough) %>% 
  mutate(perc = n/sum(n)) %>% 
  arrange(desc(perc))

ltr %>% 
  filter(kj == F, !is.na(property_ID)) %>% 
  View()

### Unit size of the matches #################################################################

perc_size_units <- tibble(`Number of bedrooms` = numeric(length = 4), 
                          `Island of Montreal` = numeric(length = 4),
                          `STR market (2019)` = numeric(length = 4),
                          `LTR matches` = numeric(length = 4),
                          `LTR non-matches` = numeric(length = 4)
)

perc_size_units$`Number of bedrooms` <- 
  c(0, 1, 2, 3)

perc_size_units$`Island of Montreal` <- 
  c(0.099, 0.272, 0.525, 0.103)



for(i in 1:length(perc_size_units$`Number of bedrooms`)) {
  
  perc_size_units[i,3] <- 
    (property %>% 
       st_drop_geometry() %>% 
       filter(property_ID %in% filter(daily, housing, status != "B", date >= "2019-01-01", date <= "2019-12-31")$property_ID) %>% 
       mutate(bedrooms = ifelse(bedrooms >= 3, 3, bedrooms)) %>% 
       count(bedrooms) %>% 
       mutate(perc = n/sum(n)))[i,3] %>% 
    pull()
  
  
  perc_size_units[i, 4] <- ltr_unique_property_ID %>%
    mutate(bedrooms = ifelse(bedrooms >= 3, 3, bedrooms)) %>% 
    filter(bedrooms == perc_size_units$`Number of bedrooms`[[i]]) %>% 
    nrow() / ltr_unique_property_ID %>% nrow()
  
  perc_size_units[i, 5] <- ltr %>%
    st_drop_geometry() %>% 
    distinct(id, .keep_all = T) %>% 
    mutate(bedrooms = ifelse(bedrooms >= 3, 3, bedrooms)) %>% 
    filter(bedrooms == perc_size_units$`Number of bedrooms`[[i]]) %>% 
    nrow() / 
    ltr %>%
    st_drop_geometry() %>% 
    distinct(id) %>% nrow()
  
}

perc_size_units$`Number of bedrooms`[4] <- c("3+")
perc_size_units$`Number of bedrooms`[1] <- c("Studio")

perc_size_units %>%
  gt() %>% 
  tab_header(
    title = "Market comparison",
    subtitle = "Bedroom breakdown"
  ) %>%
  opt_row_striping() %>% 
  fmt_percent(columns = c(2:5), decimals = 1)

### Amenities (furnished & unfurnished rentals) #################################################################


# LTR furnished & not furnished
unique_ltr %>% 
  count(furnished) %>% 
  mutate(perc = n/sum(n))


ltr_unique_property_ID %>% 
  count(furnished) %>% 
  mutate(perc = n/sum(n))



### Asking rents #################################################################

#representation of matched LTR listings
unique_ltr %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  count(matched) %>% 
  mutate(perc = n/sum(n))


unique_ltr %>% 
  filter(price >425, price <8000) %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(matched) %>%
  summarize(avg_price = mean(price))


### Typical STR units going to LTR #####################################

### Type of STR: FREH #####################################

ltr_unique_property_ID %>% 
  filter(property_ID %in% filter(daily, FREH_3 >= 0.5)$property_ID) %>% 
  nrow() /
  ltr_unique_property_ID %>% 
  nrow()


### Type of STR: GH #####################################

# ltr_unique_property_ID %>% 
#   filter(property_ID %in% (GH %>% 
#                       st_drop_geometry() %>% 
#                       group_by(ghost_ID) %>% 
#                       unnest(property_IDs) %>% 
#                       pull(property_IDs))) %>% 
#   nrow()

### Type of STR:  multilistings #####################################

ltr_unique_property_ID %>% 
  filter(property_ID %in% filter(daily, multi == T)$property_ID) %>% 
  nrow() /
  ltr_unique_property_ID %>% 
  nrow()


### Combined 'commercial characteristics': commercial operations #####################################
(rbind(
  ltr_unique_property_ID %>% 
    filter(property_ID %in% filter(daily, multi == T)$property_ID),
  ltr_unique_property_ID %>% 
    filter(property_ID %in% filter(daily, FREH_3 >= 0.5)$property_ID)) %>% 
    distinct(property_ID)) %>%
  nrow() /
  ltr_unique_property_ID %>% 
  nrow()


daily %>% 
  filter(date >= "2020-01-01", 
         property_ID %in% filter(daily, FREH_3 >= 0.5 | multi == T)$property_ID, 
         property_ID %in% ltr_unique_property_ID$property_ID) %>% 
  count(property_ID) %>% 
  nrow() / 
  daily %>% 
  filter(date >= "2020-01-01",
         property_ID %in% filter(daily, FREH_3 >= 0.5 | multi == T)$property_ID) %>% 
  count(property_ID) %>% 
  nrow()

### Type of host #####################################

# number of hosts
property %>%
  filter(!is.na(ltr_ID)) %>% 
  count(host_ID) %>% 
  nrow() 

# number of hosts having more than one properties on LTR market
property %>%
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID), scraped >= "2020-01-01") %>% 
  count(host_ID) %>% 
  filter(n > 1) %>% 
  arrange(desc(n))

property %>% 
  st_drop_geometry() %>% 
  filter(scraped >= "2020-01-01", host_ID %in% (property %>%
                                                st_drop_geometry() %>% 
                                                filter(!is.na(ltr_ID), scraped >= "2020-01-01") %>% 
                                                count(host_ID) %>% 
                                                filter(n > 1) %>% 
                                                arrange(desc(n)))[1,1]) %>% 
  nrow()


# percentage of hosts properties which made the switch from STR to LTR
property %>%
  filter(!is.na(ltr_ID)) %>% 
  nrow() /
  property %>% 
  st_drop_geometry() %>% 
  filter(host_ID %in% (property %>%
                         st_drop_geometry() %>% 
                         filter(!is.na(ltr_ID)) %>% 
                         pull(host_ID)), 
         scraped >= "2020-01-01") %>% 
  nrow()

### Type of host: revenue #####################################

#revenue distribution of hosts that matched on a LTR platform 
revenue_2019 <- 
  daily %>%
  filter(housing,
         date <= LTM_end_date, date >= LTM_start_date,
         status == "R") %>%
  group_by(property_ID) %>%
  summarize(revenue_LTM = sum(price) ) %>% 
  inner_join(property, .)


revenue_2019 %>% 
  st_drop_geometry() %>%
  filter(revenue_LTM > 0) %>%
  filter(host_ID %in% (property %>%
                         st_drop_geometry() %>%
                         filter(property_ID %in% ltr_unique_property_ID$property_ID) %>%
                         pull(host_ID))) %>%
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


half_mil_ltr <- revenue_2019 %>% # host that matched and made more than 500k
  st_drop_geometry() %>%
  filter(revenue_LTM > 0) %>%
  filter(host_ID %in% (property %>%
                         st_drop_geometry() %>%
                         filter(property_ID %in% ltr_unique_property_ID$property_ID) %>%
                         pull(host_ID))) %>%
  group_by(host_ID) %>% 
  summarize("host_rev" = sum(revenue_LTM)) %>% 
  filter(host_rev > 500000) %>% 
  pull(host_ID) 


property %>% # how many listings matched for the top earning hosts that matched
  st_drop_geometry() %>% 
  filter(host_ID %in% half_mil_ltr, 
         !is.na(ltr_ID), 
         scraped >= "2020-01-01") %>%
  count(host_ID) %>% 
  summarize(mean(n))

property %>% 
  st_drop_geometry() %>% 
  filter(host_ID %in% (property %>%
                         st_drop_geometry() %>% 
                         filter(!is.na(ltr_ID)) %>% 
                         pull(host_ID)), !is.na(ltr_ID), 
         scraped >= "2020-01-01") %>%
  count(host_ID) %>% 
  summarize(mean(n))


### Type of host: Superhost status #####################################

property %>% # listings from hosts that matched
  filter(host_ID %in% (property %>%
                         st_drop_geometry() %>% 
                         filter(!is.na(ltr_ID)) %>% 
                         pull(host_ID)), scraped > "2020-01-01", superhost == T) %>% 
  nrow() /
  property %>% 
  filter(host_ID %in% (property %>%
                         st_drop_geometry() %>% 
                         filter(!is.na(ltr_ID)) %>% 
                         pull(host_ID)), scraped > "2020-01-01") %>% 
  nrow()


property %>% # listings from all hosts
  filter(scraped > "2020-01-01", superhost == T) %>% 
  nrow() /
  property %>% 
  filter(scraped > "2020-01-01") %>% 
  nrow()

### LTR listings exposure ######################################################

# length of availability on LTR platforms
unique_ltr %>% 
  mutate(how_long_they_stay = scraped-created) %>% 
  arrange(desc(how_long_they_stay)) %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(matched) %>% 
  summarize(mean(how_long_they_stay, na.rm = T))

# number of matches that remained on STR platforms
property %>%
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  filter(scraped >= "2020-07-01") %>% 
  nrow() 


property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID),
         scraped >= "2020-01-01") %>% 
  nrow() -
property %>%
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID),
         scraped >= "2020-07-01") %>% 
  nrow() 

# remaining presence on LTR platforms
unique_ltr %>%
  unnest(property_ID) %>%
  filter(property_ID %in% (property %>%
                       st_drop_geometry() %>% 
                       filter(!is.na(ltr_ID),
                              scraped >= "2020-07-01") %>% 
                       pull(property_ID))) %>%
  filter(scraped < max(scraped)) %>% 
  distinct(property_ID) %>% 
  nrow()


### Matched that could potentially be looked as being rented on a LTR platform ######################################################
property_IDs_ltr_rented <- 
  property %>% 
  filter(!is.na(ltr_ID),
         scraped < "2020-07-01",
         scraped >= "2020-01-01") %>% 
  pull(property_ID)

# FREH? 
daily %>% 
  filter(property_ID %in% property_IDs_ltr_rented,
         FREH_3 >= 0.5) %>% 
  distinct(property_ID) %>% 
  nrow()

# how old were these listings compared to plateform
property %>% 
  filter(property_ID %in% property_IDs_ltr_rented) %>%
  mutate(how_old = scraped-created) %>% 
  summarize(mean(how_old, na.rm = T) /30)

property %>% 
  filter(scraped >= "2020-01-01") %>% 
  mutate(how_old = scraped-created) %>% 
  summarize(mean(how_old, na.rm = T) /30)

# breakdown by boroughs
property %>% 
  filter(property_ID %in% property_IDs_ltr_rented) %>% 
  count(borough) %>% 
  st_drop_geometry() %>% 
  mutate(perc = n/sum(n)) %>% 
  arrange(desc(perc))

property %>% 
  filter(property_ID %in% property_IDs_ltr_rented) %>% 
  count(borough) %>% 
  st_drop_geometry() %>% 
  mutate(perc = n/sum(n)) %>% 
  arrange(desc(perc)) %>% 
  filter(n < 25) %>% 
  summarize(sum(n))

# median host revenue of these listings
revenue_2019 %>% 
  st_drop_geometry() %>% 
  filter(host_ID %in% (property %>%
                         st_drop_geometry() %>%
                         filter(property_ID %in% ltr_unique_property_ID$property_ID) %>%
                         filter(scraped < "2020-06-01"))$host_ID) %>%
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

# average host revenue of these listings
revenue_2019 %>% 
  st_drop_geometry() %>% 
  filter(host_ID %in% (property %>%
                         st_drop_geometry() %>%
                         filter(property_ID %in% ltr_unique_property_ID$property_ID) %>%
                         filter(scraped < "2020-06-01"))$host_ID) %>%
  group_by(host_ID) %>% 
  summarize("host_rev" = sum(revenue_LTM)) %>% 
  summarize(mean(host_rev))


# how many hosts?
property %>% 
  filter(property_ID %in% property_IDs_ltr_rented) %>% 
  count(host_ID) %>% 
  nrow()

#### Conclusion ##################################
# percentage of commercial listings matched which are gone from the STR out of all commercial listings
daily %>% 
  filter(date >= "2020-01-01", 
         property_ID %in% filter(daily, FREH_3 >= 0.5 | multi == T)$property_ID,
         property_ID %in% ltr_unique_property_ID$property_ID,
         property_ID %in% filter(property, scraped <= max(scraped) - months(1))$property_ID) %>% 
  count(property_ID) %>% 
  nrow() / 
  daily %>% 
  filter(date >= "2020-01-01", 
         property_ID %in% filter(daily, FREH_3 >= 0.5 | multi == T)$property_ID) %>% 
  count(property_ID) %>% 
  nrow()

# commercial listings turnover during ban and years before between same date
daily %>% 
  filter(date >= "2020-03-28", date <= "2020-06-25", 
         property_ID %in% filter(daily, FREH_3 >= 0.5 | multi == T)$property_ID,
         property_ID %in% filter(st_drop_geometry(property), scraped <= "2020-06-25")$property_ID) %>% 
  count(property_ID) %>% 
  nrow() / 
  daily %>% 
  filter(date >= "2020-03-28", date <= "2020-06-25",
         property_ID %in% filter(daily, FREH_3 >= 0.5 | multi == T)$property_ID) %>% 
  count(property_ID) %>% 
  nrow()

daily %>% 
  filter(date >= "2019-03-28", date <= "2019-06-25",
         property_ID %in% filter(daily, FREH_3 >= 0.5 | multi == T)$property_ID,
         property_ID %in% filter(st_drop_geometry(property), scraped <= "2019-06-25")$property_ID) %>% 
  count(property_ID) %>% 
  nrow() / 
  daily %>% 
  filter(date >= "2019-03-28", date <= "2019-06-25", 
         property_ID %in% filter(daily, FREH_3 >= 0.5 | multi == T)$property_ID) %>% 
  count(property_ID) %>% 
  nrow()

daily %>% 
  filter(date >= "2018-03-28", date <= "2018-06-25",
         property_ID %in% filter(daily, FREH_3 >= 0.5 | multi == T)$property_ID,
         property_ID %in% filter(st_drop_geometry(property), scraped <= "2018-06-25")$property_ID) %>% 
  count(property_ID) %>% 
  nrow() / 
  daily %>% 
  filter(date >= "2018-03-28", date <= "2018-06-25", 
         property_ID %in% filter(daily, FREH_3 >= 0.5 | multi == T)$property_ID) %>% 
  count(property_ID) %>% 
  nrow()

daily %>% 
  filter(date >= "2017-03-28", date <= "2017-06-25", 
         property_ID %in% filter(daily, FREH_3 >= 0.5 | multi == T)$property_ID,
         property_ID %in% filter(st_drop_geometry(property), scraped <= "2017-06-25")$property_ID) %>% 
  count(property_ID) %>% 
  nrow() / 
  daily %>% 
  filter(date >= "2017-03-28", date <= "2017-06-25",
         property_ID %in% filter(daily, FREH_3 >= 0.5 | multi == T)$property_ID) %>% 
  count(property_ID) %>% 
  nrow()
