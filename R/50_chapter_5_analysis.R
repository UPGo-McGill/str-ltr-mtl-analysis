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
load("output/cmhc.Rdata")


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
  inner_join(unnest(filter(ltr, !is.na(property_ID)), property_ID), 
             by = "property_ID") %>% 
  arrange(desc(scraped)) %>% 
  distinct(property_ID, .keep_all = T)


# How many STR listings have returned to the long-term market? ------------

#' Our image matching algorithm recognized 2,526 [1] unique Airbnb listings 
#' which matched with 4,842 [2] different LTR listings (as some units are posted 
#' multiple times) in the City of Montreal. The matching LTR listings were 
#' evenly split between Kijiji (2,596 [3] listings, or 53.6%) and Craigslist 
#' (2,246 [3] listings, or 46.4%). Out of the 2,526 matching Airbnb listings,
#' 51.2% (1,294 [3] listings) were active STRs in 2020, which establishes a 
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

#' [3] KJ/CL split
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

#' [4] Unique STR matches active in 2020
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  count(active = active >= "2020-01-01" | created >= "2020-01-01") %>% 
  mutate(pct = n / sum(n))

#' Out of the 1,690 [1] Airbnb listings which we matched to Kijiji, 74.0% [2]
#' were identified by their hosts as “long-term rentals” and 26.0% [2] were 
#' identified as “short-term rentals”. Among these listings, 50.3% [3] specified 
#' lease lengths of one year, 21.9% [3] specified month-to-month, and 27.8% [3]
#' did not specify.

#' [1] KJ or CL
ltr_unique_property_ID %>% 
  group_by(kj) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(pct = n / sum(n))

#' [2] Long-term or short-term
ltr_unique_property_ID %>% 
  filter(kj) %>% 
  count(short_long) %>% 
  mutate(perc = n / sum(n))

#' [3] Agreement length
ltr_unique_property_ID %>% 
  filter(kj) %>% 
  count(type) %>% 
  mutate(perc = n / sum(n))


# When did STR listings move to the long-term market? ---------------------

#' By the end of the March, more than 60 [1] listings were being transferred 
#' each day. Daily numbers remained high through April, but even from May 
#' through July an average of 5.5 [2] new Airbnb listings were transferred to 
#' Craigslist or Kijiji each day.

#' [1] Peak dailiy listings transfer
ltr %>% 
  st_drop_geometry() %>% 
  unnest(property_ID) %>% 
  filter(!is.na(property_ID)) %>% 
  arrange(created) %>% 
  distinct(property_ID, .keep_all = TRUE) %>% 
  count(created, kj, sort = TRUE) %>% 
  slice(1)

#' [2] Average daily listings transfer, May - July
ltr %>% 
  st_drop_geometry() %>% 
  unnest(property_ID) %>% 
  filter(!is.na(property_ID)) %>% 
  arrange(created) %>% 
  distinct(property_ID, .keep_all = TRUE) %>% 
  count(created, kj) %>% 
  filter(created >= "2020-05-01", created <= "2020-07-31") %>% 
  summarize(avg = mean(n))
  

# Spatial distribution of matched listings --------------------------------

#' Out of the 2,526 [1] unique STR listings matched to LTR listings in the City 
#' of Montreal, nearly half (44.4% [2]) were located in the Ville-Marie borough 
#' and 28.5% [2] in Le Plateau-Mont-Royal, with Le Sud-Ouest (8.2% [2]), 
#' Côte-des-Neiges-Notre-Dame-de-Grâce (5.0% [2]) and Rosemont-La-Petite-Patrie 
#' (4.0% [2]) accounting for most of the remaining matches.... In fact, 
#' the number of STR listings matched to LTR listings in Ville-Marie is 
#' equivalent to nearly half (41.4% [3]) of all the STR listings active in the 
#' borough on March 1, 2020, and 24.7% [4] of all the listings active in the 
#' borough in 2020.

#' [1] Unique STR matches
property %>% filter(!is.na(ltr_ID)) %>% nrow()

#' [2] Total number of unique matches by borough
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  count(borough) %>% 
  mutate(pct = round(n / sum(n), 3)) %>% 
  arrange(-pct) %>% 
  slice(1:6)

#' [3] Ville-Marie active March 1 percentage
{property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  count(borough) %>% 
  mutate(pct = n / sum(n)) %>% 
  slice(18) %>% 
  pull(n)} /
  {daily %>% 
      filter(housing, borough == "Ville-Marie", status != "B", 
             date == "2020-03-01") %>% 
      nrow}

#' [3] Ville-Marie active 2020 percentage
{{property %>% 
    st_drop_geometry() %>% 
    filter(!is.na(ltr_ID)) %>% 
    count(borough) %>% 
    mutate(pct = n / sum(n)) %>% 
    slice(18) %>% 
    pull(n)} /
  {daily %>% 
      filter(housing, borough == "Ville-Marie", status != "B", 
             date >= "2020-01-01") %>% 
      pull(property_ID) %>% 
      unique() %>% 
      length()}} %>% 
  round(3)


# Asking rents ------------------------------------------------------------

asking_rents <- 
  ltr_unique %>% 
  filter(price > 425, price < 8000) %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(matched, created) %>%
  summarize(avg_price = mean(price)) %>% 
  ungroup() %>% 
  mutate(status = if_else(matched, "Matched to STR", "Not matched"), 
         .before = created) %>% 
  select(-matched)

asking_rents <- 
  ltr_unique %>% 
  filter(price > 425, price < 8000) %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(created) %>%
  summarize(avg_price = mean(price)) %>% 
  ungroup() %>% 
  mutate(status = "All listings", .before = created) %>% 
  bind_rows(asking_rents) %>% 
  mutate(geography = "City of Montreal")

asking_rents_vm <- 
  ltr_unique %>% 
  filter(price > 425, price < 8000, borough == "Ville-Marie") %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(matched, created) %>%
  summarize(avg_price = mean(price)) %>% 
  ungroup() %>% 
  mutate(status = if_else(matched, "Matched to STR", "Not matched"), 
         .before = created) %>% 
  select(-matched)

asking_rents <- 
  ltr_unique %>% 
  filter(price > 425, price < 8000, borough == "Ville-Marie") %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(created) %>%
  summarize(avg_price = mean(price)) %>% 
  ungroup() %>% 
  mutate(status = "All listings", .before = created) %>% 
  bind_rows(asking_rents_vm) %>% 
  mutate(geography = "Ville-Marie") %>% 
  bind_rows(asking_rents)

#' On March 13 [1], when the average asking rent on LTR platforms in the City 
#' of Montreal was $1,387 [1], the average asking rent among listings which we 
#' matched to Airbnb was $2,641 [1]—90.4% [1] higher. Over the course of
#' March, average asking rents for LTR listings matched to Airbnb declined 
#' steadily, and since April they have been relatively stable, with a daily 
#' average of $1,746 [2]. This remains a significantly higher (22.0% [2]) 
#' average asking price than the non-matched listings, though, which from 
#' April through July averaged $1,431 [2].

#' [1] Peak difference between matched and city-wide
asking_rents %>% 
  filter(geography == "City of Montreal", created >= "2020-03-13") %>% 
  group_by(created) %>% 
  summarize(city = avg_price[status == "All listings"],
            match = avg_price[status == "Matched to STR"],
            dif = avg_price[status == "Matched to STR"] - 
              avg_price[status == "All listings"],
            dif_pct = match / city - 1) %>% 
  filter(dif == max(dif))

#' [2] April-July averages
asking_rents %>% 
  filter(geography == "City of Montreal", created >= "2020-04-01",
         created <= "2020-07-31") %>% 
  group_by(status) %>% 
  summarize(avg_price = mean(avg_price)) %>% 
  mutate(pct_higher = avg_price / min(avg_price) - 1)
  
#' Even in Ville-Marie, however, LTR listings matched to Airbnb have been on 
#' average 11.4% [1] higher than listings not matched, and the same pattern of 
#' initially extremely high asking rents in March yielding to lower (although 
#' still high) rents from April through July is visible.

#' [1] Ville-Marie overall average rent differences
asking_rents %>% 
  filter(geography == "Ville-Marie", created >= "2020-03-13",
         status != "All listings") %>% 
  group_by(status) %>% 
  summarize(avg_price = mean(avg_price)) %>% 
  summarize(max(avg_price) / min(avg_price) - 1)

#' The average city-wide asking rent on Craigslist and Kijiji has remained 
#' between $1,326 [1] and $1,579 [1] throughout the March-July period we tracked 
#' it.... The daily average asking rent in March was $1,736 [2], while in July 
#' it was $1,627 [2]—a $109 [2] or 6.7% [2] decline.

asking_rents %>% 
  filter(created >= "2020-03-13", created <= "2020-07-31", 
         status == "All listings", geography == "City of Montreal") %>% 
  summarize(min = min(avg_price), max = max(avg_price))

#' [2] Ville-Marie March/July rent differences
asking_rents %>% 
  filter(geography == "Ville-Marie", created >= "2020-03-13",
         created <= "2020-07-31", status == "All listings") %>% 
  filter(created <= "2020-03-31" | created >= "2020-07-01") %>% 
  group_by(created <= "2020-03-31") %>% 
  summarize(avg_price = mean(avg_price)) %>% 
  mutate(dif = avg_price - min(avg_price),
         dif_pct = avg_price / min(avg_price) - 1)


# Listing amenities -------------------------------------------------------

#' Studios were overrepresented among LTR listings which matched to Airbnb 
#' (17.7% [1]) compared with LTR listings which did not match (10.0% [2]), 
#' STR listings (in 2019, 10.0% [3]) and Montreal’s rental stock (9.9% [4]). 
#' Units with three bedrooms or more were overrepresented in LTR listings, 
#' at 22.2% [1] for matches and 21.3% [2] for non-matches, compared to the 
#' City (10.3% [4]) and  the STR market (12.2% [3]). One-bedrooms, which were 
#' considerably overrepresented in STR listings (56.6% [3]) compared to the 
#' overall rental housing stock (27.2% [4]), constituted 36.0% [1] of LTR 
#' listings that matched with STR listings, and a similar proportion among 
#' non-matched listings (35.7% [4]).

#' [1] Bedrooms in LTR matches
ltr_unique_property_ID %>% 
  count(bedrooms) %>% 
  filter(!is.na(bedrooms)) %>% 
  rowwise() %>% 
  mutate(group = min(bedrooms, 3, na.rm = TRUE)) %>% 
  group_by(group) %>% 
  summarize(n = sum(n)) %>% 
  rename(bedrooms = group) %>% 
  mutate(n = round(n / sum(n), 3))

#' [2] Bedrooms in LTR non-matches
ltr %>% 
  st_drop_geometry() %>% 
  filter(is.na(property_ID)) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(bedrooms) %>% 
  filter(!is.na(bedrooms)) %>% 
  rowwise() %>% 
  mutate(group = min(bedrooms, 3, na.rm = TRUE)) %>% 
  group_by(group) %>% 
  summarize(n = sum(n)) %>% 
  rename(bedrooms = group) %>% 
  mutate(n = round(n / sum(n), 3))

#' [3] Bedrooms in 2019 STRs
property %>% 
  st_drop_geometry() %>% 
  filter(housing, active >= "2019-01-01", created <= "2019-12-31") %>% 
  count(bedrooms) %>% 
  filter(!is.na(bedrooms)) %>% 
  rowwise() %>% 
  mutate(group = min(bedrooms, 3, na.rm = TRUE)) %>% 
  group_by(group) %>% 
  summarize(n = sum(n)) %>% 
  rename(bedrooms = group) %>% 
  mutate(n = round(n / sum(n), 3))

#' [4] Bedrooms in City of Montreal rentals
city_units %>% 
  filter(date == 2019) %>% 
  slice(1:4) %>% 
  mutate(pct = round(units / sum(units), 3))

#' Table 5.1 
bedroom_match_table <-
  city_units %>% 
  filter(date == 2019) %>% 
  slice(1:4) %>% 
  mutate(pct = round(units / sum(units), 3)) %>% 
  mutate(bedrooms = c("Studio", "1-bedroom", "2-bedroom", "3+-bedroom")) %>% 
  select(bedrooms, pct) %>% 
  mutate(Category = "City of Montreal rental stock", .before = bedrooms)

bedroom_match_table <-
  property %>% 
  st_drop_geometry() %>% 
  filter(housing, active >= "2019-01-01", created <= "2019-12-31") %>% 
  count(bedrooms) %>% 
  filter(!is.na(bedrooms)) %>% 
  rowwise() %>% 
  mutate(group = min(bedrooms, 3, na.rm = TRUE)) %>% 
  group_by(group) %>% 
  summarize(pct = sum(n)) %>% 
  rename(bedrooms = group) %>% 
  mutate(pct = round(pct / sum(pct), 3)) %>% 
  mutate(bedrooms = c("Studio", "1-bedroom", "2-bedroom", "3+-bedroom")) %>% 
  mutate(Category = "STR market (2019)", .before = bedrooms) %>% 
  bind_rows(bedroom_match_table, .)

bedroom_match_table <-
  ltr_unique_property_ID %>% 
  count(bedrooms) %>% 
  filter(!is.na(bedrooms)) %>% 
  rowwise() %>% 
  mutate(group = min(bedrooms, 3, na.rm = TRUE)) %>% 
  group_by(group) %>% 
  summarize(pct = sum(n)) %>% 
  rename(bedrooms = group) %>% 
  mutate(pct = round(pct / sum(pct), 3)) %>% 
  mutate(bedrooms = c("Studio", "1-bedroom", "2-bedroom", "3+-bedroom")) %>% 
  mutate(Category = "LTRs matched to STR", .before = bedrooms) %>% 
  bind_rows(bedroom_match_table, .)

bedroom_match_table <-
  ltr %>% 
  st_drop_geometry() %>% 
  filter(is.na(property_ID)) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(bedrooms) %>% 
  filter(!is.na(bedrooms)) %>% 
  rowwise() %>% 
  mutate(group = min(bedrooms, 3, na.rm = TRUE)) %>% 
  group_by(group) %>% 
  summarize(pct = sum(n)) %>% 
  rename(bedrooms = group) %>% 
  mutate(pct = round(pct / sum(pct), 3)) %>% 
  mutate(bedrooms = c("Studio", "1-bedroom", "2-bedroom", "3+-bedroom")) %>% 
  mutate(Category = "LTRs not matched", .before = bedrooms) %>% 
  bind_rows(bedroom_match_table, .)

bedroom_match_table <- 
  bedroom_match_table %>% 
  pivot_wider(values_from = pct, names_from = bedrooms)

bedroom_match_table %>% 
  gt() %>% 
  tab_header(title = "STR and LTR listings by bedroom count") %>%
  opt_row_striping() %>% 
  fmt_percent(columns = c(2:5), decimals = 1)

#' Of the 82,016 [1] LTR listings we analyzed, 26.8% [2] were listed as 
#' furnished, 55.8% [2] as unfurnished, and 17.4% [2] did not provide this 
#' information. Listings which matched with STRs had a significantly higher 
#' proportion classified as furnished: 76.7% [3] furnished and 22.7% [3] 
#' unfurnished, with only 0.6% [3] not providing this information.

#' [1] All scraped LTR listings
ltr_unique %>% nrow()

#' [2] Furnished proportion for all LTRs
ltr_unique %>% 
  count(furnished) %>% 
  mutate(pct = round(n / sum(n), 3))

#' [3] Furnished proportion for matches
ltr_unique_property_ID %>% 
  count(furnished) %>% 
  mutate(pct = round(n / sum(n), 3))



# Are matched listings commercial operations? -----------------------------

#' Of the 2,526 [1] unique STR listings that matched with the LTR market, 
#' TKTK [2] (TKTK% [2]) are entire-home listings and TKTK [2] (TKTK% [2]) are 
#' private-room listings. Examining the entire-home listings, 50.4% [3] of them 
#' were identified as frequently rented entire-home (FREH) listings at some 
#' point, which means they were almost certainly operated commercially. 
#' Moreover, 75.3% [4] of entire-home STR listings which matched to LTR listings 
#' were multilistings at some point, which means they were operated by hosts 
#' controlling multiple listings simultaneously. In total, TKTK% [5] of 
#' entire-home listings had one of these two strong indicators of commercial 
#' activity.

#' [1] Unique STR matches
property %>% filter(!is.na(ltr_ID)) %>% nrow()

#' [2] listing_type breakdown among matches




#' As we have seen in a previous section, frequently rented entire-home (FREH) 
#' listings are likely to be operated commercially. They may be well-established 
#' due to frequent reservations and consequently have more reviews. Of the 2,569 
#' unique STR listings that matched with the LTR market, 50.4% [1] had at least three 
#' months of reservations and availability which are consistent with a year-long 
#' full-time operation.

#' [1] Type of STR - FREH 
ltr_unique_property_ID %>% 
  filter(property_ID %in% filter(daily, FREH_3 > 0.5)$property_ID) %>% 
  nrow() /
  ltr_unique_property_ID %>% 
  nrow()

#' As mentioned above, one indication that a listing might be operated for commercial 
#' purposes is when a host operates more than one listing on a given date, as it is 
#' unlikely that both listings could be operated out of their principal residence. 
#' Out of all the STR listings that matched LTR listings, 1,903 (75.3%) [1] were found as 
#' multilisting at least once in their lifetime.

#' [1] Type of STR - Multilistings 
ltr_unique_property_ID %>% 
  filter(property_ID %in% filter(daily, multi == T)$property_ID) %>% 
  nrow()

ltr_unique_property_ID %>% 
  filter(property_ID %in% filter(daily, multi == T)$property_ID) %>% 
  nrow() /
  ltr_unique_property_ID %>% 
  nrow()

#' If we aggregate the characteristics that can be used to determine whether a STR is 
#' operated commercially (multilisting, FREH), there were 2,127 [1] matches that can be 
#' categorized as being commercial operations, making up 84.2% [1] of all unique STR units 
#' that matched to a LTR listing and were active in 2020. (Ghost hostels are not 
#' included in this analysis since only postings for entire units on LTR platforms 
#' were scraped.) However, only 8.4% of all STR listings in Montreal that could be 
#' considered commercial operations in their lifetime were matched on longer-term 
#' rental platforms during the pandemic. While commercial listings make up a 
#' great majority of the matches, the low preponderance of commercial listings 
#' resorting to listing their units on LTR platforms could mean that many commercial 
#' operators decided to remain on STR platforms instead of finding longer-term tenants. 
#' The commercial listings that have a good standing with numerous previous reservations 
#' and reviews might have chosen to remain in the STR market, while mostly newer 
#' commercial listings are popping up on Kijiji and Craigslist.

#' [1] Combination of FREH, GH and ML to get the listings with commercial characteristics
rbind(
  ltr_unique_property_ID %>% 
    filter(property_ID %in% filter(daily, multi == T)$property_ID),
  ltr_unique_property_ID %>% 
    filter(property_ID %in% filter(daily, FREH_3 > 0.5)$property_ID)) %>% 
  distinct(property_ID) %>% 
  nrow()

(rbind(
  ltr_unique_property_ID %>% 
    filter(property_ID %in% filter(daily, multi == T)$property_ID),
  ltr_unique_property_ID %>% 
    filter(property_ID %in% filter(daily, FREH_3 > 0.5)$property_ID)) %>% 
    distinct(property_ID)) %>%
  nrow() /
  ltr_unique_property_ID %>% 
  nrow()

ltr_unique_property_ID %>% 
  nrow() /
  daily %>% 
  filter(status != "B", date == "2020-03-01", FREH_3 > 0.5 | multi == T) %>% 
  nrow()

#' Type of host -----------------------------------------------------------------

#' In Montreal, 1149 [1] unique STR host IDs were linked to the 2,526 LTR matches. 
#' 288 [2] of these hosts posted more than one of their STR units on the LTR platforms. 
#' For example, the top (mentioned in section 2) host posted 239 [2] of its STR units on 
#' the LTR market. Half [3] of the active properties of these 1149 hosts were found on either 
#' Kijiji, Craigslist or both. The fact that only a portion of the hosts’ listings were 
#' found on LTR platforms indicates that these matches are likely to be an underestimation
#'  of the STRs that were posted on LTR platforms. Indeed, it would be intuitive to 
#'  assume that a host that decides to post its listings on a LTR platform would post 
#'  all of them. This could mean that the remainder of the listings were posted and 
#'  rented before the team conducted the weekly scrapes, that only the hosts’ less 
#'  performing STRs were put onto the LTR rental market, hosts limited themselves to 
#'  other LTR listings websites (such as various Facebook housing groups), sold housing 
#'  formerly used as STRs or simply that the hosts used updated pictures for its 
#'  listings, making it impossible to detect these matches through our image matching 
#'  algorithm. This minority of each host’s listings found is thus an example of how 
#'  the matches found are likely substantially lower than the reality.

#' [1] number of hosts found on a LTR platform
property %>%
  filter(!is.na(ltr_ID)) %>% 
  count(host_ID) %>% 
  nrow() 

#' [2] number of hosts having more than one properties on LTR market
property %>%
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  count(host_ID) %>% 
  filter(n > 1) %>% 
  nrow()

(property %>%
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  count(host_ID) %>% 
  filter(n > 1) %>% 
  arrange(desc(n)))[1,]

#' [3] percentage of the hosts' properties which made the switch from STR to LTR
ltr_unique_property_ID %>%
  nrow() /
  property %>% 
  st_drop_geometry() %>% 
  filter(host_ID %in% (property %>%
                         st_drop_geometry() %>% 
                         filter(!is.na(ltr_ID)) %>% 
                         pull(host_ID)), 
         scraped >= "2020-01-01") %>% 
  nrow()

#' Type of hosts: revenue

#' The income distribution of the hosts that matched is greatly higher than the revenue 
#' distribution of all Montreal hosts. Median revenue of all hosts was $4,300 in the 
#' entire City of Montreal in 2019. For the hosts that matched, their yearly median 
#' revenue was $14,300 [1]. Many of the top-earning hosts turned to LTR platforms in light 
#' of the COVID-19 pandemic. For example, 27 [2] of the 37 hosts that made more than $500,000 
#' in the past year moved at least one STR unit to the LTR market (28.9 [3] units in average 
#' for these top earning hosts, versus 2.2 [3] units in average for the totality of hosts). 
#' The hosts which moved one or more units from the STR market to the LTR have higher 
#' revenue in general, and more than the majority of the big players of Montreal’s STR 
#' market also shifted part of their units to the longer-term rental market. Needless 
#' to say, they carry a very heavy weight in this analysis. Figure 5.4 displays the 
#' revenue distribution of hosts that matched, compared to the ones that did not.

#' [1] revenue distribution of hosts that matched on a LTR platform 
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
                         filter(!is.na(ltr_ID)) %>%
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

#' [2] host that matched and made more than 500k
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

half_mil_ltr %>% length()

#' [3] how many listings matched for the top earning hosts that matched
property %>% 
  st_drop_geometry() %>% 
  filter(host_ID %in% half_mil_ltr, 
         !is.na(ltr_ID)) %>%
  count(host_ID) %>% 
  summarize(mean(n))

property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>%
  count(host_ID) %>% 
  summarize(mean(n))


#' Type of host: Superhost

#' Another method of analyzing the type of host is the Superhost status. Hosts can earn S
#' uperhost status through their performance on the site, such as their ratings and 
#' reviews. Superhosts get more exposure in search results and are usually more 
#' trusted by guests based on their standing. Out of all hosts that matched, 18.0% [1] had 
#' Superhost status. This is significantly higher than the general distribution of hosts 
#' in Montreal with superhost status (13.0% [2] of all hosts were Superhosts in 2020). 

#' [1] Superhost status for host that matched
property %>%
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID), superhost == T) %>% 
  count(host_ID) %>% 
  nrow() / 
  property %>%
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  count(host_ID) %>% 
  nrow()

#' [2] Superhost status for ALL hosts 
property %>%
  st_drop_geometry() %>% 
  filter(scraped >= "2020-01-01", superhost == T) %>% 
  count(host_ID) %>% 
  nrow() / 
  property %>%
  st_drop_geometry() %>% 
  filter(scraped >= "2020-01-01") %>% 
  count(host_ID) %>% 
  nrow()
  

# Listing exposure ---------------------------------------------------------------

#' Length of availability on long-term rental platforms

#' On average, STR matches found on LTR platforms stayed longer than the non-matches. 
#' STR matches were listed on average 22.0 [1] days on LTR platforms, whereas the non-matches 
#' stayed only 11.3 [1] days on average. The increased length of presence on LTR platforms 
#' could be due to many factors, one of them being the higher median price (as mentioned 
#' above) or less suitable features for the long-term rental market (i.e furnished units 
#' or bedrooms set up like hotel rooms with multiple beds). Figure 5.5 shows the 
#' distribution of the length of stay for both matches and non-matches.

#' [1] length of availability on LTR platforms
ltr_unique %>% 
  mutate(how_long_they_stay = scraped-created) %>% 
  arrange(desc(how_long_they_stay)) %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(matched) %>% 
  summarize(mean(how_long_they_stay, na.rm = T))



#' Remaining presence on STR platforms

#' Are hosts planning on renting on a longer-term only for a few months, leaving the STR 
#' listing up-and-running for future bookings? Looking at whether the listings that matched 
#' remained on a STR platform is an indicator of the strategy of the hosts. As stated above, 
#' we identified 1,264 STR listings that were still in operation in 2020, from which it is 
#' possible to study recent activity. Out of this number, 635 [1] STR listings were still active 
#' at least once in the month of July 2020. The number of active listings by July 31th, last 
#' day of data, was already down to 420 [2]. It means that a very conservative minimum of 621 [3]
#' of our matches (50.5% were removed from the STR platform.

#' [1] number of matches that remained on STR platforms
property %>%
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  filter(active >= "2020-07-01") %>% 
  nrow()

#' [2] number of matches active on STR platform on last day of data
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID), active == "2020-07-31") %>% 
  nrow()

#' [3] number of matches left for LTR market
property %>%
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  filter(active >= "2020-01-01", !active >= "2020-07-01") %>% 
  nrow()

#' Listings can still be appearing on the platforms, but be inactive. Indeed, hosts who 
#' operate listings that are well established in the STR market do not necessarily want to 
#' remove their listings. Instead, if their units are dedicated to other activities than 
#' STRs during the previous and/or upcoming months, they can simply block their calendar 
#' to make sure no reservations can occur, while keeping their perfectly good listings 
#' intact for when activity in the STR market starts again. The units that matched and still 
#' appear on the STR platforms, but have a blocked calendar on every day of the month of 
#' July 2020, may have successfully made the move to the LTR market. These account for 327 [1]
#' units out of the 621 units (51.9% [2]) which we conservatively consider that could have moved 
#' to the LTR market. The rest are listings deactivated indefinitely. 

#' [1] Number of units inactive in July, but still on the platform
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  filter(scraped >= max(scraped) - months(1), !active >= "2020-07-01") %>% 
  nrow() 

#' [2] Percentage of previous listings on all listings which we considered moved to LTR
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  filter(scraped >= max(scraped) - months(1), !active >= "2020-07-01") %>% 
  nrow() /
  property %>%
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  filter(active >= "2020-01-01", !active >= "2020-07-01") %>% 
  nrow()


#' More info on listings matching

#' Of these 626 units, 368 [1] had at least three months of reservations and availability 
#' which are consistent with a year-long full-time operation. These 626 housing units have 
#' been available on a STR platform for the last 19.5 [2] months on average, while the average 
#' for all listings active in 2020 on the STR platform is 26.2 [2] months. This indicates that 
#' the matches most likely rented through an LTR platform were on average newer listings. As 
#' for spatial distribution of these matches, 45.4% (282) [3] of the 621 housing units that were 
#' rented on the LTR market are located in Ville-Marie, 23.7% (147) [3] in Le Plateau-Mont-Royal, 
#' and 9.0% (55) [3] in le Sud-Ouest. The rest (137) [4] are distributed in 13 [4] other boroughs. The 
#' total 626 STR listings were operated by 348 [5] hosts. These hosts had a median revenue of 
#' $23,600 [6] in 2019, while the median revenue for the entire STR platforms in the City of 
#' Montreal was $4,300. The average revenue of these hosts is $224,000 [7], while it was $16,900 
#' for hosts active on STR platforms in Montreal in 2019. 


#' [1] FREH? 
property %>%
  st_drop_geometry() %>%
  filter(!is.na(ltr_ID)) %>%
  filter(active >= "2020-01-01", !active >= "2020-07-01",
         property_ID %in% (daily %>% 
                             filter(FREH > 0.5))$property_ID) %>%
  nrow()


#' [2]  how old were these listings compared all listings on the STR platforms
property %>%
  st_drop_geometry() %>%
  filter(!is.na(ltr_ID)) %>%
  filter(active >= "2020-01-01", !active >= "2020-07-01") %>% 
  mutate(how_old = scraped-created) %>% 
  summarize(mean(how_old, na.rm = T) /30)

property %>% 
  st_drop_geometry() %>% 
  filter(active >= "2020-01-01") %>% 
  mutate(how_old = scraped-created) %>% 
  summarize(mean(how_old, na.rm = T) /30)


#' [3] breakdown by boroughs of the potential returning units
property %>%
  st_drop_geometry() %>%
  filter(!is.na(ltr_ID)) %>%
  filter(active >= "2020-01-01", !active >= "2020-07-01") %>% 
  count(borough) %>% 
  mutate(pct = n/sum(n)) %>% 
  arrange(desc(pct))

#' [4] rest of boroughs
(property %>%
    st_drop_geometry() %>%
    filter(!is.na(ltr_ID)) %>%
    filter(active >= "2020-01-01", !active >= "2020-07-01") %>% 
    count(borough) %>% 
    mutate(pct = n/sum(n)) %>% 
    arrange(desc(pct)))[4:16,] %>% 
  summarize(sum(n))

(property %>%
    st_drop_geometry() %>%
    filter(!is.na(ltr_ID)) %>%
    filter(active >= "2020-01-01", !active >= "2020-07-01") %>% 
    count(borough) %>% 
    mutate(pct = n/sum(n)) %>% 
    arrange(desc(pct)))[4:16,] %>% 
  nrow()

#' [5] number of hosts 
property %>%
    st_drop_geometry() %>%
    filter(!is.na(ltr_ID)) %>%
    filter(active >= "2020-01-01", !active >= "2020-07-01") %>% 
  count(host_ID) %>% 
  nrow()
 
#' [6] median revenue of these hosts
revenue_2019 %>% 
  st_drop_geometry() %>% 
  filter(host_ID %in% (property %>%
                         st_drop_geometry() %>%
                         filter(!is.na(ltr_ID)) %>%
                         filter(active >= "2020-01-01", !active >= "2020-07-01"))$host_ID) %>%
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

#' [7] average host revenue of these listings
revenue_2019 %>% 
  st_drop_geometry() %>% 
  filter(host_ID %in% (property %>%
                         st_drop_geometry() %>%
                         filter(!is.na(ltr_ID)) %>%
                         filter(active >= "2020-01-01", !active >= "2020-07-01"))$host_ID) %>%
  group_by(host_ID) %>% 
  summarize("host_rev" = sum(revenue_LTM)) %>% 
  summarize(mean(host_rev))
