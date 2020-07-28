### load libraries and data ###########################################
library(tidyverse)
library(lubridate)
library(sf)
library(gt)
library(future)
library(ggplot2)
library(magicfor)


load("data/str_montreal.Rdata")
load("data/ltr_matches.Rdata")


### Overview #################################################################

# number of unique STR listings that matched in 2020
property %>% 
  filter(!is.na(ltr_id), scraped >= "2020-01-01")


#unique matching ab_id locations using street address
unique_ab_id <- 
ltr %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ab_id)) %>% 
  unnest(ab_id) %>% 
  filter(ab_id %in% filter(property, scraped >= "2020-01-01")$property_ID) %>% 
  arrange(desc(scraped)) %>% 
  distinct(ab_id) %>% 
  inner_join(unnest(ltr, ab_id), by = "ab_id") %>% 
  distinct(ab_id, .keep_all = T)

#by boroughs
unique(unique_ab_id$ab_id)



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


### the matches that left LTR platforms are still on AirBNB? ########
ltr_mtl %>%
  st_drop_geometry() %>%
  filter(!is.na(ab_id),
         scraped < max(scraped)) %>%
  arrange(desc(scraped)) %>% 
  distinct(ab_id, .keep_all = T) %>%
  select(ab_id)

property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% (ltr_mtl %>%
           st_drop_geometry() %>%
           filter(!is.na(ab_id),
                  scraped < max(scraped)) %>%
           arrange(desc(scraped)) %>% 
           distinct(ab_id, .keep_all = T) %>%
           select(ab_id))$ab_id,
         scraped >= "2020-01-01") %>% 
  distinct(property_ID, .keep_all = T) %>% 
  filter(scraped < max(scraped)) %>% 
  nrow() / 
  property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% (ltr_mtl %>%
                             st_drop_geometry() %>%
                             filter(!is.na(ab_id),
                                    scraped < max(scraped)) %>%
                             arrange(desc(scraped)) %>% 
                             distinct(ab_id, .keep_all = T) %>%
                             select(ab_id))$ab_id,
         scraped >= "2020-01-01") %>% 
  distinct(property_ID, .keep_all = T) %>% 
  nrow()

#are listings both on LTR and AirBNB?


### portrait of matched LTR listings ################################
ltr_mtl %>% 
  st_drop_geometry() %>% 
  filter(is.na(ab_id)) %>%
  arrange(desc(scraped)) %>% 
  distinct(id, .keep_all = T) %>% 
  mutate(how_long_they_stay = (scraped-created)) %>% 
  filter(how_long_they_stay > 0) %>% 
  summarize(mean(how_long_they_stay))

ltr_mtl %>%
  st_drop_geometry() %>%
  filter(!is.na(ab_id)) %>%
  arrange(desc(scraped)) %>% 
  distinct(id, .keep_all = T) %>%
  mutate(how_long_they_stay = (scraped-created)) %>% 
  summarize(mean(how_long_they_stay, na.rm = T))

property %>% 
  filter(scraped == "2020-05-20")

#percentages of units that matched that are long term
ltr_mtl %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ab_id), short_long == "long") %>% 
  distinct(ab_id, .keep_all = T) %>% 
  nrow() /
  ltr_mtl %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ab_id), !is.na(short_long)) %>% 
  distinct(ab_id, .keep_all = T) %>% 
  nrow()



### average rent by bedroom breakdown and variation with data from CMHC. #####
average_rent <- tibble(`Number of bedrooms` = numeric(length = 4), 
                       `2019 - CMHC reported average rent` = numeric(length = 4),
                       `Variation since 2018` = numeric(length = 4),
                       `2020 - Collected data` = numeric(length = 4),
                       `Variation from CMHC 2019 data` = numeric(length = 4)
)

# $660 for bachelor or studios (a 4.3% change from 2018), $752 for one bedrooms (a 4.2% change from 2018),
# $851 for 2 bedrooms (a 3.7% change from 2018), and $1,118 for 3 bedrooms or more (a 3.5% change from 2018)

average_rent$`Number of bedrooms` <- 
  c(0, 1, 2, 3)

average_rent$`2019 - CMHC reported average rent` <- 
  c(660, 752, 851, 1118)

average_rent$`Variation since 2018` <- 
  c(0.043, 0.042, 0.037, 0.035)


for(i in 1:length(average_rent$`Number of bedrooms`)) {
  
  average_rent[i,4] <- ltr_mtl %>% 
    st_drop_geometry() %>% 
    filter(bedrooms == average_rent$`Number of bedrooms`[[i]]) %>% 
    arrange(desc(scraped)) %>% 
    distinct(id, .keep_all = T) %>% 
    summarize(mean(price, na.rm = T))
  
  average_rent[i, 5] <- (ltr_mtl %>% 
                           st_drop_geometry() %>% 
                           filter(bedrooms == average_rent$`Number of bedrooms`[[i]]) %>% 
                           arrange(desc(scraped)) %>% 
                           distinct(id, .keep_all = T) %>% 
                           summarize(mean(price, na.rm = T)) -
                           average_rent$`2019 - CMHC reported average rent`[[i]]) /
    average_rent$`2019 - CMHC reported average rent`[[i]]
  
}

average_rent %>% 
  mutate(`2020 - Collected data` = round(`2020 - Collected data`, digit = -1)) %>%
  gt() %>% 
  tab_header(
    title = "Average rent/asking rent",
    subtitle = "Bedroom breakdown"
  ) %>%
  opt_row_striping() %>% 
  fmt_percent(columns = c(3, 5), decimals = 1) %>% 
  fmt_number(columns = c(2, 4),
             decimals = 0)











#matches and non-matches distribution by number of bedrooms compared to the island of Mtl and the entire STR listings population

bedroom_breakdown <- matrix(c("3.0 %", "13.3 %", "16.4 %", "10.3 %", 
                              "42.0 %", "45.2 %", "35.6 %", "35.6 %", 
                              "37.0 %", "27.2 %", "25.0 %", "32.5 %",
                              "18.0 %", "10.6 %", "22.8 %", "20.5 %"),ncol=4,byrow=TRUE)
colnames(bedroom_breakdown) <- c("Island of Montreal","STRs in Montreal","Matches", "Non-matches")
rownames(bedroom_breakdown) <- c("Studios","1-Bedroom","2-Bedrooms", "3-Bedrooms and more")
grid.table(bedroom_breakdown)
         