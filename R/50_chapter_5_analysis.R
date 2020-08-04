#### Chapter 5 ANALYSIS ####################################################

source("R/01_startup.R")

load("data/str_processed.Rdata")
load("data/geometry.Rdata")
load("data/ltr_matches.Rdata")

### Overview of the number of matches #################################################################

# distinct LTR listings
unique_ltr <- 
  ltr %>% 
  st_drop_geometry() %>% 
  arrange(desc(scraped)) %>% 
  distinct(id, .keep_all = T)

unique_ltr %>% 
  nrow()


# number of unique STR listings that matched in 2020
property %>% 
  filter(!is.na(ltr_id), scraped >= "2020-01-01")


#unique matching ab_id locations using street address
ltr_unique_ab_id <- 
  ltr %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ab_id)) %>% 
  unnest(ab_id) %>% 
  filter(ab_id %in% filter(property, scraped >= "2020-01-01")$property_ID) %>% 
  arrange(desc(scraped)) %>% 
  distinct(ab_id) %>% 
  inner_join(unnest(ltr, ab_id), by = "ab_id") %>% 
  arrange(desc(scraped)) %>% 
  distinct(ab_id, .keep_all = T)

### Spatial distribution of the matches #################################################################

#by boroughs
ltr_unique_ab_id %>% 
  count(borough) %>% 
  mutate(perc = n/sum(n)) %>% 
  arrange(desc(perc))

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
  
  
  perc_size_units[i, 4] <- ltr_unique_ab_id %>%
    mutate(bedrooms = ifelse(bedrooms >= 3, 3, bedrooms)) %>% 
    filter(bedrooms == perc_size_units$`Number of bedrooms`[[i]]) %>% 
    nrow() / ltr_unique_ab_id %>% nrow()
  
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
  # filter(!is.na(furnished)) %>% 
  count(furnished) %>% 
  mutate(perc = n/sum(n))

unique_ltr %>% 
  filter(!is.na(ab_id)) %>% 
  nrow() /
  unique_ltr %>% 
  nrow()

unique_ltr %>% 
  filter(!is.na(ab_id)) %>% 
  # filter(!is.na(furnished)) %>% 
  count(furnished) %>% 
  mutate(perc = n/sum(n))

unique_ltr %>% 
  filter(is.na(ab_id)) %>% 
  filter(!is.na(furnished)) %>% 
  count(furnished) %>% 
  mutate(perc = n/sum(n))


### Asking rents #################################################################

unique_ltr %>% 
  filter(price >425, price <8000) %>% 
  mutate(matched = if_else(!is.na(ab_id), TRUE, FALSE)) %>% 
  group_by(matched) %>%
  summarize(avg_price = mean(price))

### Typical STR units going to LTR #####################################

### Date of creation of matched listings #####################################

property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_id),
         scraped >= "2020-01-01") %>%
  summarize(mean(scraped-created, na.rm = T))


### Type of STR: FREH #####################################

ltr_unique_ab_id %>% 
  filter(ab_id %in% filter(FREH, date >= "2020-01-01")$property_ID) %>% 
  nrow() /
  ltr_unique_ab_id %>% 
  nrow()


### Type of STR: GH #####################################

# ltr_unique_ab_id %>% 
#   filter(ab_id %in% (GH %>% 
#                       st_drop_geometry() %>% 
#                       group_by(ghost_ID) %>% 
#                       unnest(property_IDs) %>% 
#                       pull(property_IDs))) %>% 
#   nrow()

### Type of STR:  multilistings #####################################

ltr_unique_ab_id %>% 
  filter(ab_id %in% filter(daily, multi == T)$property_ID)


### Combined 'commercial characteristics': commercial operations #####################################
(rbind(
  ltr_unique_ab_id %>% 
    filter(ab_id %in% filter(daily, multi == T)$property_ID),
  ltr_unique_ab_id %>% 
    filter(ab_id %in% FREH$property_ID),
  ltr_unique_ab_id %>% 
    filter(ab_id %in% unlist(GH$property_IDs))) %>% 
    distinct(ab_id)) %>% 
  nrow() /
  ltr_unique_ab_id %>% 
  nrow()


### Type of host #####################################

# number of hosts
property %>%
  filter(property_ID %in% ltr_unique_ab_id$ab_id) %>% 
  count(host_ID) %>% 
  nrow() /
  property %>%
  filter(property_ID %in% ltr_unique_ab_id$ab_id) %>% 
  nrow()

property %>%
  st_drop_geometry() %>% 
  filter(property_ID %in% ltr_unique_ab_id$ab_id) %>% 
  count(host_ID) %>% 
  filter(n > 1) %>% 
  arrange(desc(n))


# percentage of hosts properties which made the switch from STR to LTR
property %>%
  filter(property_ID %in% ltr_unique_ab_id$ab_id) %>% 
  nrow() /
  property %>% 
  st_drop_geometry() %>% 
  filter(host_ID %in% (property %>%
                         st_drop_geometry() %>% 
                         filter(property_ID %in% ltr_unique_ab_id$ab_id) %>% 
                         pull(host_ID)), scraped >= "2020-01-01") %>% 
  nrow()

### Type of host: revenue #####################################

#revenue distribution of hosts that matched on a LTR platform 
revenue_2019 %>% 
  st_drop_geometry() %>%
  filter(revenue_LTM > 0) %>%
  filter(host_ID %in% (property %>%
                         st_drop_geometry() %>%
                         filter(property_ID %in% ltr_unique_ab_id$ab_id) %>%
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
                         filter(property_ID %in% ltr_unique_ab_id$ab_id) %>%
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
                         filter(property_ID %in% ltr_unique_ab_id$ab_id) %>% 
                         pull(host_ID)), !is.na(ltr_ID), scraped >= "2020-01-01") %>%
  count(host_ID) %>% 
  summarize(mean(n))


### Type of host: Superhost status #####################################

property %>% # listings from hosts that matched
  filter(host_ID %in% (property %>%
                         st_drop_geometry() %>% 
                         filter(property_ID %in% ltr_unique_ab_id$ab_id) %>% 
                         pull(host_ID)), scraped > "2020-01-01", superhost == T) %>% 
  nrow() /
  property %>% 
  filter(host_ID %in% (property %>%
                         st_drop_geometry() %>% 
                         filter(property_ID %in% ltr_unique_ab_id$ab_id) %>% 
                         pull(host_ID)), scraped > "2020-01-01") %>% 
  nrow()


property %>% # listings from hosts that matched
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
  mutate(matched = if_else(!is.na(ab_id), TRUE, FALSE)) %>% 
  group_by(matched) %>% 
  summarize(mean(how_long_they_stay, na.rm = T))

# number of matches that remained on STR platforms
property %>%
  st_drop_geometry() %>% 
  filter(property_ID %in% ltr_unique_ab_id$ab_id) %>% 
  filter(scraped >= "2020-06-01") %>% 
  nrow() /
  property %>%
  st_drop_geometry() %>% 
  filter(property_ID %in% ltr_unique_ab_id$ab_id) %>% 
  nrow()

# remaining presence on LTR platforms
unique_ltr %>%
  unnest(ab_id) %>%
  filter(ab_id %in% (property %>%
                       st_drop_geometry() %>% 
                       filter(property_ID %in% ltr_unique_ab_id$ab_id) %>% 
                       filter(scraped >= "2020-06-01") %>% 
                       pull(property_ID))) %>%
  filter(scraped < max(scraped)) %>% 
  pull(ab_id) %>% 
  unique()


### Matched that could potentially be looked as being rented on a LTR platform ######################################################

# how many hosts?
property %>% 
  filter(property_ID %in% ltr_unique_ab_id$ab_id,
         scraped < "2020-06-01") %>% 
  count(host_ID) %>% 
  nrow()

# breakdown by boroughs
property %>% 
  filter(property_ID %in% ltr_unique_ab_id$ab_id,
         scraped < "2020-06-01") %>% 
  st_join(boroughs) %>% 
  count(borough) %>% 
  st_drop_geometry() %>% 
  mutate(perc = n/sum(n)) %>% 
  arrange(desc(perc)) %>% 
  filter(n < 20) %>% 
  summarize(sum(n))

# median host revenue of these listings
revenue_2019 %>% 
  st_drop_geometry() %>% 
  filter(host_ID %in% (property %>%
                         st_drop_geometry() %>%
                         filter(property_ID %in% ltr_unique_ab_id$ab_id) %>%
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
                         filter(property_ID %in% ltr_unique_ab_id$ab_id) %>%
                         filter(scraped < "2020-06-01"))$host_ID) %>%
  group_by(host_ID) %>% 
  summarize("host_rev" = sum(revenue_LTM)) %>% 
  summarize(mean(host_rev))


# FREH?
property %>% 
  filter(property_ID %in% ltr_unique_ab_id$ab_id,
         scraped < "2020-06-01",
         property_ID %in% FREH$property_ID) 

# how old were these listings compared to plateform
property %>% 
  filter(property_ID %in% ltr_unique_ab_id$ab_id,
         scraped < "2020-06-01") %>%
  mutate(how_old = scraped-created) %>% 
  summarize(mean(how_old, na.rm = T) /30)

property %>% 
  filter(scraped >= "2020-01-01") %>% 
  mutate(how_old = scraped-created) %>% 
  summarize(mean(how_old, na.rm = T) /30)

