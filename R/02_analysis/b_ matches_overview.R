### load libraries and data ###########################################
library(tidyverse)
library(lubridate)
library(sf)
library(gt)
library(future)
library(ggplot2)

memory.limit(size = 48000)
plan(multiprocess, workers = 4)

load("data/montreal_str_processed_b.Rdata")
load("data/ltr_matches.Rdata")

### Overview ##########################################################
#unique airbnb listings that matched
ltr_mtl %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ab_id)) %>% 
  distinct(ab_id) %>% 
  nrow()

#unique listings that matched
ltr_mtl %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ab_id)) %>% 
  distinct(id) %>% 
  nrow()

#unique listings that did not match
ltr_mtl %>% 
  st_drop_geometry() %>% 
  filter(is.na(ab_id)) %>% 
  distinct(id) %>% 
  nrow()

#furnsihed listings that did not match vs matched
ltr_mtl %>% 
  st_drop_geometry() %>% 
  filter(is.na(ab_id),
         is.na(furnished)) %>% 
  distinct(id) %>% 
  nrow() /
  ltr_mtl %>% 
  st_drop_geometry() %>% 
  filter(is.na(ab_id)) %>% 
  distinct(id) %>% 
  nrow()

#number of bedrooms
ltr_mtl %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ab_id),
         !is.na(bedrooms)) %>% 
  distinct(id, .keep_all = T) %>% 
  summarize(mean(bedrooms))

### typical unit that  went on the LTR market ###################################
# % of units that went on LTR for all hosts that put at least 1 listing on LTR
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_id)) %>% 
  distinct(property_ID, .keep_all = T) %>% 
  nrow() /
  property %>% 
  st_drop_geometry() %>% 
  filter(host_ID %in% filter(property, !is.na(ltr_id))$host_ID,
         scraped >= "2020-01-01") %>% 
  distinct(property_ID, .keep_all = T) %>% 
  nrow()

### Who are the hosts that put listings on LTR ##################################
#list of all the hosts that matched
ltr_hosts_id <- 
property %>%
  st_drop_geometry() %>% 
  filter(!is.na(ltr_id)) %>%
  distinct(host_ID)

#median host income
daily %>%
  filter(housing,
         date <= key_date, date > key_date - years(1),
         status == "R", host_ID %in% ltr_hosts_id$host_ID) %>%
  group_by(property_ID) %>%
  summarize(revenue_LTM = sum(price) * exchange_rate) %>% 
  left_join(property, .) %>% 
  distinct(.keep_all = T) %>% 
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


#top earning hosts
daily %>%
  filter(housing,
         date <= key_date, date > key_date - years(1),
         status == "R", host_ID %in% ltr_hosts_id$host_ID) %>%
  group_by(property_ID) %>%
  summarize(revenue_LTM = sum(price) * exchange_rate) %>% 
  left_join(property, .) %>% 
  distinct(.keep_all = T) %>% 
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





### portrait of STR units going on LTR #################################
## GH, FREH, Multi

#GH
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% unlist(GH$property_IDs),
         !is.na(ltr_id), scraped > "2020-01-01") %>% 
  distinct(property_ID, .keep_all = T) %>% 
  nrow() /
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% unlist(GH$property_IDs),
         scraped > "2020-01-01") %>% 
  distinct(property_ID, .keep_all = T) %>% 
  nrow()

#FREH
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% FREH_2020$property_ID,
         !is.na(ltr_id)) %>% 
  distinct(property_ID, .keep_all = T) %>% 
  nrow() /
  property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% unlist(FREH_2020$property_ID),
         scraped > "2020-01-01") %>% 
  distinct(property_ID, .keep_all = T) %>% 
  nrow()

#multi
daily %>%
  filter(date > "2020-01-01", multi == T) %>%
  count(property_ID) %>%
  filter(property_ID %in% matches$ab_id) %>% 
  select(property_ID) %>% 
  nrow() /
daily %>%
  filter(date >= "2020-01-01",
         multi == T) %>%
  count(property_ID) %>%
  nrow() 


#how many of these FREH on all matches
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% FREH_2020$property_ID,
         !is.na(ltr_id)) %>% 
  distinct(property_ID, .keep_all = T) %>% 
  nrow()/
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_id),
         ltr_id %in% ltr_mtl$id) %>% 
  distinct(property_ID) %>% 
  nrow()


### all commercial operations
#commercial listings that matched out of all matches
rbind(property %>% 
        st_drop_geometry() %>% 
        filter(property_ID %in% unlist(GH$property_IDs),
               !is.na(ltr_id), scraped > "2020-01-01") %>% 
        distinct(property_ID),
      property %>% 
        st_drop_geometry() %>% 
        filter(property_ID %in% FREH_2020$property_ID,
               !is.na(ltr_id)) %>% 
        distinct(property_ID),
        daily %>%
        filter(date > "2020-01-01", multi == T) %>%
        count(property_ID) %>%
        filter(property_ID %in% matches$ab_id) %>% 
        select(property_ID)
) %>% distinct(property_ID) %>% nrow() /
  property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_id),
         ltr_id %in% ltr_mtl$id) %>% 
  distinct(property_ID) %>% 
  nrow()


#commercial listings that matched out of all commercial listings
rbind(property %>% 
        st_drop_geometry() %>% 
        filter(property_ID %in% unlist(GH$property_IDs),
               !is.na(ltr_id), scraped > "2020-01-01") %>% 
        distinct(property_ID),
      property %>% 
        st_drop_geometry() %>% 
        filter(property_ID %in% FREH_2020$property_ID,
               !is.na(ltr_id)) %>% 
        distinct(property_ID),
      daily %>%
        filter(date > "2020-01-01", multi == T) %>%
        count(property_ID) %>%
        filter(property_ID %in% matches$ab_id) %>% 
        select(property_ID)
) %>% distinct(property_ID) %>% nrow() /
  rbind(property %>% 
          st_drop_geometry() %>% 
          filter(property_ID %in% unlist(GH$property_IDs),
                 scraped > "2020-01-01") %>% 
          distinct(property_ID),
        property %>% 
          st_drop_geometry() %>% 
          filter(property_ID %in% FREH_2020$property_ID) %>% 
          distinct(property_ID),
        daily %>%
          filter(date > "2020-01-01", multi == T) %>%
          count(property_ID) %>% 
          select(property_ID)
  ) %>% distinct(property_ID) %>% nrow()
