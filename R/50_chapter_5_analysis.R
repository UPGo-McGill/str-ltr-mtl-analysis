#### 50 CHAPTER 5 ANALYSIS ####################################################0

#' This script produces the tables and facts for chapter 5. It runs quickly.
#' 
#' Output:
#' - None
#' 
#' Script dependencies:
#' - `02_geometry_import.R`
#' - `07_ltr_listing_match.R`
#' - `09_str_processing.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")


# Load previous data ------------------------------------------------------

load("output/str_processed.Rdata")
load("output/geometry.Rdata")
load("output/ltr_processed.Rdata")


# Prepare new objects -----------------------------------------------------

# distinct LTR listings
ltr_unique <- 
  ltr %>% 
  st_drop_geometry() %>% 
  arrange(desc(scraped)) %>% 
  distinct(id, .keep_all = TRUE)

# Unique STR matches with their information on longer-term market
ltr_unique_property_ID <-
  ltr %>% 
  st_drop_geometry() %>% 
  filter(!is.na(property_ID)) %>% 
  unnest(property_ID) %>% 
  arrange(desc(scraped)) %>% 
  distinct(property_ID) %>% 
  inner_join(unnest(filter(ltr, !is.na(property_ID)), property_ID), by = "property_ID") %>% 
  arrange(desc(scraped)) %>% 
  distinct(property_ID, .keep_all = T)


# How many STR listings have returned to the long-term market? ------------

#' Our image matching algorithm recognized 2,569 [1] unique Airbnb listings 
#' which matched with 4,842 [2] different LTR listings (as some units are posted 
#' multiple times) in the City of Montreal. The matching LTR listings were 
#' evenly split between Kijiji (2,596 [2] listings, or 53.6%) and Craigslist 
#' (2,246 [2] listings, or 46.4%). Out of the 2,569 matching Airbnb listings,
#' 57.6% (1,479 [3] listings) were active STRs in 2020, which establishes a 
#' lower bound for the number of unique housing units that went from the STR 
#' market to the LTR market due to the COVID-19 pandemic.

#' [1] Unique STR matches
property %>% filter(!is.na(ltr_ID)) %>% nrow()

#' [2] Unique LTR matches
ltr %>% 
  st_drop_geometry() %>% 
  filter(!is.na(property_ID)) %>% 
  unnest(property_ID) %>% 
  filter(!is.na(property_ID)) %>%
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  nrow()

ltr %>% 
  st_drop_geometry() %>% 
  filter(!is.na(property_ID)) %>% 
  unnest(property_ID) %>% 
  filter(!is.na(property_ID)) %>%
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(kj) %>% 
  mutate(pct = n/sum(n))

#' [3] Unique STR matches active in 2020
property %>% 
  filter(!is.na(ltr_ID), scraped >= "2020-01-01") %>% 
  nrow()

property %>% 
  filter(!is.na(ltr_ID), scraped >= "2020-01-01") %>% 
  nrow()/
  property %>% 
  filter(!is.na(ltr_ID)) %>% 
  nrow()


#' Out of the 2,569 unique Airbnb listings which matched, 1,690 (66.9%) [1] were observed 
#' on Kijiji. Of these listings, 74.0% [2] were identified by their hosts as 
#' “long-term rentals” and 26.0% [2] were identified as “short-term rentals”. 
#' Among these listings, 50.3% [3] specified lease lengths of 1 year, 21.9% [3] 
#' specified month-to-month, and 27.8% [3] did not specify.

#' [1] Long-term or short-term
ltr_unique_property_ID %>% 
  group_by(kj) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(pct = n/sum(n))

#' [2] Long-term or short-term
ltr_unique_property_ID %>% 
  filter(kj) %>% 
  count(short_long) %>% 
  mutate(perc = n/sum(n))

#' [3] Agreement length
ltr_unique_property_ID %>% 
  filter(kj) %>% 
  count(type) %>% 
  mutate(perc = n/sum(n))

# Spatial distribution of the matches ---------------------------------------------------

#' [1] Total number of unique matches by borough
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  count(borough) %>% 
  mutate(pct = n/sum(n)) %>% 
  arrange(desc(pct))

ltr %>% 
  filter(kj == F, !is.na(property_ID)) %>% 
  View()


# Unit size of the matches ---------------------------------------------------

#' Table 5.1 
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
       mutate(pct = n/sum(n)))[i,3] %>% 
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


# Amenities (furnished or unfurnished status) ---------------------------------------------------
#' To accommodate temporary guests, STR properties are overwhelmingly furnished. 
#' Properties that have moved from the STR to LTR market during the pandemic are 
#' listed as furnished at much higher rates than other LTR properties on LTR platforms 
#' like Kijiji and Craigslist. Of the 82,016 LTR listings scraped and analysed, 
#' 26.8% [1] of the listings were listed as furnished, 55.8% [1] as unfurnished, and 17.4% [1]
#' did not give this information. Listings which matched with STRs had a significantly 
#' higher proportion classified as furnished: 76.7% [2] furnished and 22.7% [2] unfurnished, 
#' while only 0.6% [2] of these listings did not offer this information). STR listings 
#' clearly represent an influx of units with different amenities than is the norm of 
#' LTR listings, suggesting that very little is being changed when STR hosts move 
#' former STRs to the LTR market. The lack of change to the furnishings of these 
#' units might suggest that STR hosts are prepared to return to the STR market as 
#' soon as demand returns.

#' [1] Proportion of ALL furnished and unfurnished longer-term rentals
ltr_unique %>% 
  count(furnished) %>% 
  mutate(pct = n/sum(n))

#' [2] Proportion of MATCHES that are furnished and unfurnished
ltr_unique_property_ID %>% 
  count(furnished) %>% 
  mutate(pct = n/sum(n))


# Asking rents on the LTR platform ---------------------------------------------------
#' The LTR listings which matched with STR listings had higher average prices than 
#' other LTR listings, as depicted in Figure 5.2. While the increase in housing 
#' supply could have lowered asking rents, the addition of these STRs in the LTR 
#' market did not have a great impact on the overall average asking rents 
#' (represented by the grey line), potentially due to their fairly low 
#' representation in the entire dataset (matches make up only 5.9% [1] of the dataset). 
#' If anything, they contributed to a slight increase in the overall average due to 
#' their significantly higher asking rents. The asking rent of STRs which moved to 
#' the LTR market was on average 19.3% [2] higher than the rest of the dataset. Factors 
#' involved in the consistently higher average prices of the matches could be 
#' explained by their overrepresentation in furnished listings and their more 
#' central location, among others. The decrease in asking prices for STR matches 
#' over time suggests a market correction potentially reflecting a lack of demand. 
#' Due to data limitations, we have not been able to calculate the trend in average 
#' asking rents compared to previous years. Indeed, the scrapes have not been 
#' conducted long enough to provide for year-over-year comparisons. 


#' [1] representation of matched LTR listings
ltr_unique %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  count(matched) %>% 
  mutate(pct = n/sum(n))

#' [2] Average asking rent of the matches
ltr_unique %>% 
  filter(price >425, price <8000) %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(matched) %>%
  summarize(avg_price = mean(price)) %>% 
  summarize(pct_diff = (avg_price[2] - avg_price[1])/avg_price[2])


# Analysis of the STRs that turned to the LTR platforms during the pandemic ---------------------------------------------------

#' [1] Type of STR - FREH 
ltr_unique_property_ID %>% 
  filter(property_ID %in% filter(daily, FREH_3 > 0.5)$property_ID) %>% 
  nrow() /
  ltr_unique_property_ID %>% 
  nrow()

#' [2] Type of STR - GH 
# ltr_unique_property_ID %>% 
#   filter(property_ID %in% (GH %>% 
#                       st_drop_geometry() %>% 
#                       group_by(ghost_ID) %>% 
#                       unnest(property_IDs) %>% 
#                       pull(property_IDs))) %>% 
#   nrow()

#' [3] Type of STR - Multilistings 
ltr_unique_property_ID %>% 
  filter(property_ID %in% filter(daily, multi == T)$property_ID) %>% 
  nrow() /
  ltr_unique_property_ID %>% 
  nrow()

#' [4] Combination of FREH, GH and ML to get the listings with commercial characteristics
(rbind(
  ltr_unique_property_ID %>% 
    filter(property_ID %in% filter(daily, multi == T)$property_ID),
  ltr_unique_property_ID %>% 
    filter(property_ID %in% filter(daily, FREH_3 > 0.5)$property_ID)) %>% 
    distinct(property_ID)) %>%
  nrow() /
  ltr_unique_property_ID %>% 
  nrow()

daily %>% 
  filter(date >= "2020-01-01", 
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID, 
         property_ID %in% ltr_unique_property_ID$property_ID) %>% 
  count(property_ID) %>% 
  nrow() / 
  daily %>% 
  filter(date >= "2020-01-01",
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID) %>% 
  count(property_ID) %>% 
  nrow()


# Analysis of the type of host that turned to LTR platforms ---------------------------------------------------

#' [1] number of hosts found on a LTR platform
property %>%
  filter(!is.na(ltr_ID)) %>% 
  count(host_ID) %>% 
  nrow() 

#' [2] number of hosts having more than one properties on LTR market
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


#' [3] percentage of the hosts' properties which made the switch from STR to LTR
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

#' [4] revenue distribution of hosts that matched on a LTR platform 
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

#' [5] host that matched and made more than 500k
half_mil_ltr <- revenue_2019 %>% 
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

#' [6] how many listings matched for the top earning hosts that matched
property %>% 
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

#' [7] Superhost status for host that matched
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

#' [8] Superhost status for ALL hosts 
property %>% 
  filter(scraped > "2020-01-01", superhost == T) %>% 
  nrow() /
  property %>% 
  filter(scraped > "2020-01-01") %>% 
  nrow()


# LTR listing exposure ---------------------------------------------------

#' [1] length of availability on LTR platforms
ltr_unique %>% 
  mutate(how_long_they_stay = scraped-created) %>% 
  arrange(desc(how_long_they_stay)) %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(matched) %>% 
  summarize(mean(how_long_they_stay, na.rm = T))

#' [2] number of matches that remained on STR platforms
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

#' [3] remaining presence on LTR platforms
ltr_unique %>%
  unnest(property_ID) %>%
  filter(property_ID %in% (property %>%
                             st_drop_geometry() %>% 
                             filter(!is.na(ltr_ID),
                                    scraped >= "2020-07-01") %>% 
                             pull(property_ID))) %>%
  filter(scraped < max(scraped)) %>% 
  distinct(property_ID) %>% 
  nrow()

# Estimation of the number of matches that could potentially have returned to the LTR market ------------------------------

#' [1] Matches that could potentially be looked as being rented on a LTR platform
property_IDs_ltr_rented <- 
  property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID),
         scraped >= "2020-01-01",
         scraped <= max(property$scraped) - months(1)) %>% 
  rbind(property %>% 
          st_drop_geometry() %>% 
          filter(property_ID %in% (daily %>% 
                                     filter(date >= "2020-06-01", date <= "2020-06-30",
                                            status != "B",
                                            property_ID %in% ltr_unique_property_ID$property_ID) %>% 
                                     count(property_ID) %>% 
                                     filter(n == 30) %>%
                                     pull(property_ID)))) %>% 
  pull(property_ID) %>% 
  unique()

#' [2] FREH? 
daily %>% 
  filter(property_ID %in% property_IDs_ltr_rented,
         FREH_3 > 0.5) %>% 
  distinct(property_ID) %>% 
  nrow()

#' [4]  how old were these listings compared all listings on the STR platforms
property %>% 
  filter(property_ID %in% property_IDs_ltr_rented) %>%
  mutate(how_old = scraped-created) %>% 
  summarize(mean(how_old, na.rm = T) /30)

property %>% 
  filter(scraped >= "2020-01-01") %>% 
  mutate(how_old = scraped-created) %>% 
  summarize(mean(how_old, na.rm = T) /30)

#' [5] breakdown by boroughs of the potential returning units
property %>% 
  filter(property_ID %in% property_IDs_ltr_rented) %>% 
  count(borough) %>% 
  st_drop_geometry() %>% 
  mutate(pct = n/sum(n)) %>% 
  arrange(desc(pct))

property %>% 
  filter(property_ID %in% property_IDs_ltr_rented) %>% 
  count(borough) %>% 
  st_drop_geometry() %>% 
  mutate(pct = n/sum(n)) %>% 
  arrange(desc(pct)) %>% 
  filter(n < 60) %>% 
  summarize(sum(n))

#' [5] median STR host revenue of the potential returning units
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

#' [6] average host revenue of these listings
revenue_2019 %>% 
  st_drop_geometry() %>% 
  filter(host_ID %in% (property %>%
                         st_drop_geometry() %>%
                         filter(property_ID %in% ltr_unique_property_ID$property_ID) %>%
                         filter(scraped < "2020-06-01"))$host_ID) %>%
  group_by(host_ID) %>% 
  summarize("host_rev" = sum(revenue_LTM)) %>% 
  summarize(mean(host_rev))


#' [7] how many hosts have returned part of their units to the longer-term rental market
property %>% 
  filter(property_ID %in% property_IDs_ltr_rented) %>% 
  count(host_ID) %>% 
  nrow()


# Conclusion ---------------------------------------------------

#' [1] percentage of commercial listings matched which are gone from the STR out of all commercial listings
daily %>% 
  filter(date >= "2020-01-01", 
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID, # listings that were commercial once in lifetime
         property_ID %in% ltr_unique_property_ID$property_ID,
         property_ID %in% filter(property, scraped <= max(scraped) - months(1))$property_ID) %>% 
  count(property_ID) %>% 
  nrow() / 
  daily %>% 
  filter(date >= "2020-01-01", 
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID) %>% 
  count(property_ID) %>% 
  nrow()

#' [2] commercial listings turnover during ban and years before between same date
daily %>% 
  filter(date >= "2020-03-28", date <= "2020-06-25", 
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID,
         property_ID %in% filter(st_drop_geometry(property), scraped <= "2020-06-25")$property_ID) %>% 
  count(property_ID) %>% 
  nrow() / 
  daily %>% 
  filter(date >= "2020-03-28", date <= "2020-06-25",
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID) %>% 
  count(property_ID) %>% 
  nrow()

daily %>% 
  filter(date >= "2019-03-28", date <= "2019-06-25",
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID,
         property_ID %in% filter(st_drop_geometry(property), scraped <= "2019-06-25")$property_ID) %>% 
  count(property_ID) %>% 
  nrow() / 
  daily %>% 
  filter(date >= "2019-03-28", date <= "2019-06-25", 
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID) %>% 
  count(property_ID) %>% 
  nrow()

daily %>% 
  filter(date >= "2018-03-28", date <= "2018-06-25",
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID,
         property_ID %in% filter(st_drop_geometry(property), scraped <= "2018-06-25")$property_ID) %>% 
  count(property_ID) %>% 
  nrow() / 
  daily %>% 
  filter(date >= "2018-03-28", date <= "2018-06-25", 
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID) %>% 
  count(property_ID) %>% 
  nrow()

daily %>% 
  filter(date >= "2017-03-28", date <= "2017-06-25", 
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID,
         property_ID %in% filter(st_drop_geometry(property), scraped <= "2017-06-25")$property_ID) %>% 
  count(property_ID) %>% 
  nrow() / 
  daily %>% 
  filter(date >= "2017-03-28", date <= "2017-06-25",
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID) %>% 
  count(property_ID) %>% 
  nrow()

#' [3] commercial listings blocked during ban and years before between same date
daily %>% 
  filter(date >= "2020-06-01", date <= "2020-06-30",
         status != "B",
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID) %>% 
  count(property_ID) %>% 
  filter(n == 30) %>% 
  nrow() /
  filter(daily, date >= "2020-06-01", date <= "2020-06-30",
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID) %>% 
  count(property_ID) %>% 
  nrow()


daily %>% 
  filter(date >= "2019-06-01", date <= "2019-06-30",
         status != "B",
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID) %>% 
  count(property_ID) %>% 
  filter(n == 30) %>% 
  nrow() /
  filter(daily, date >= "2019-06-01", date <= "2019-06-30",
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID) %>% 
  nrow()


daily %>% 
  filter(date >= "2018-06-01", date <= "2018-06-30",
         status != "B",
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID) %>% 
  count(property_ID) %>% 
  filter(n == 30) %>% 
  nrow() /
  filter(daily, date >= "2018-06-01", date <= "2018-06-30",
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID) %>% 
  nrow()


daily %>% 
  filter(date >= "2017-06-01", date <= "2017-06-30",
         status != "B",
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID) %>% 
  count(property_ID) %>% 
  filter(n == 30) %>% 
  nrow() /
  filter(daily, date >= "2017-06-01", date <= "2017-06-30",
         property_ID %in% filter(daily, FREH_3 > 0.5 | multi == T)$property_ID) %>% 
  nrow()