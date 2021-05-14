#### 50 CHAPTER 5 ANALYSIS #####################################################

#' This script produces the tables and facts for chapter 5. It runs quickly.
#' 
#' Output:
#' - None
#' 
#' Script dependencies:
#' - `02_geometry_import.R`
#' - `05_cmhc_data_import.R`
#' - `07_ltr_listing_match.R`
#' - `09_str_processing.R`
#' - `11_FREH_model.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")

qload("output/str_processed.qsm", nthreads = availableCores())
qload("output/geometry.qsm", nthreads = availableCores())
ltr <- qread("output/ltr_processed.qs", nthreads = availableCores())
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

# Possible private rooms or housing swaps in the LTR market scrape
ltr_PR <- 
  ltr_unique %>% 
  mutate(title = tolower(title)) %>% 
  filter(str_detect(title, paste0("(^| |/)(room|chambre|swap|exchange|1 ",
                                  "bedroom for rent|1 bdrm|1 fully furnished ",
                                  "bedroom|échange|lease transfer|spacious ","
                                  bedroom)( |$|/)")))


# How many STR listings have returned to the long-term market? ------------

#' Our image matching algorithm recognized 2,792 [1] unique Airbnb listings 
#' which matched with 4,842 [2] different LTR listings (as some units are posted 
#' multiple times) in the City of Montreal. The matching LTR listings were 
#' evenly split between Kijiji (3,230 [3] listings, or 53.1%) and Craigslist 
#' (2,850 [3] listings, or 46.9%). Out of the 2,792 matching Airbnb listings,
#' 53.4% (1,492 [4] listings) were active STRs in 2020, which establishes a 
#' lower bound for the number of unique housing units that went from the STR 
#' market to the LTR market due to the COVID-19 pandemic.

#' [1] Unique STR matches
property %>% 
  filter(!is.na(ltr_ID)) %>% 
  nrow() %>% 
  scales::comma()

#' [2] Unique LTR matches
ltr %>% 
  st_drop_geometry() %>% 
  filter(!is.na(property_ID)) %>% 
  unnest(property_ID) %>% 
  filter(!is.na(property_ID)) %>%
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  nrow() %>% 
  scales::comma()

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
  mutate(pct = n / sum(n)) %>% 
  mutate(n = scales::comma(n), pct = scales::percent(pct, 0.1))

#' [4] Unique STR matches active in 2020
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  count(active = active >= "2020-01-01" | created >= "2020-01-01") %>% 
  mutate(pct = n / sum(n)) %>% 
  mutate(n = scales::comma(n), pct = scales::percent(pct, 0.1))

#' Out of the 1,883 [1] Airbnb listings which we matched to Kijiji, 74.1% [2]
#' were identified by their hosts as “long-term rentals” and 25.8% [2] were 
#' identified as “short-term rentals”. Among these listings, 48.6% [3] specified 
#' lease lengths of one year, 21.6% [3] specified month-to-month, and 29.8% [3]
#' did not specify.

#' [1] KJ or CL
ltr_unique_property_ID %>% 
  group_by(kj) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(pct = n / sum(n)) %>% 
  mutate(n = scales::comma(n), pct = scales::percent(pct, 0.1))

#' [2] Long-term or short-term
ltr_unique_property_ID %>% 
  filter(kj) %>% 
  count(short_long) %>% 
  mutate(pct = n / sum(n)) %>% 
  mutate(n = scales::comma(n), pct = scales::percent(pct, 0.1))

#' [3] Agreement length
ltr_unique_property_ID %>% 
  filter(kj) %>% 
  count(type) %>% 
  mutate(pct = n / sum(n)) %>% 
  mutate(n = scales::comma(n), pct = scales::percent(pct, 0.1))


# When did STR listings move to the long-term market? ---------------------

#' By the end of the March, more than 50 [1] listings were being transferred 
#' each day. Daily numbers remained high through April, but even from May 
#' through July an average of 5.2 [2] new Airbnb listings were transferred to 
#' Craigslist or Kijiji each day. From August to December 2020, the figure was 
#' 2.5 [3].

#' [1] Peak dailiy listings transfer
ltr %>% 
  st_drop_geometry() %>% 
  unnest(property_ID) %>% 
  filter(!is.na(property_ID)) %>% 
  arrange(created) %>% 
  distinct(property_ID, .keep_all = TRUE) %>% 
  count(created, kj, sort = TRUE) %>% 
  slice(1:10)

#' [2] Average daily listings transfer, May - July
ltr %>% 
  st_drop_geometry() %>% 
  unnest(property_ID) %>% 
  filter(!is.na(property_ID)) %>% 
  arrange(created) %>% 
  distinct(property_ID, .keep_all = TRUE) %>% 
  count(created, kj) %>% 
  filter(created >= "2020-05-01", created <= "2020-07-31") %>% 
  summarize(avg = round(mean(n), 1))
  
# Average daily listings transfer, Aug - Dec
ltr %>% 
  st_drop_geometry() %>% 
  unnest(property_ID) %>% 
  filter(!is.na(property_ID)) %>% 
  arrange(created) %>% 
  distinct(property_ID, .keep_all = TRUE) %>% 
  count(created, kj) %>% 
  filter(created > "2020-07-31", created <= "2020-12-31") %>% 
  summarize(avg = round(mean(n), 1))


# Spatial distribution of matched listings --------------------------------

#' Out of the 2,792 [1] unique STR listings matched to LTR listings in the City 
#' of Montreal, nearly half (42.5% [2]) were located in the Ville-Marie borough 
#' and 29.7% [2] in Le Plateau-Mont-Royal, with Le Sud-Ouest (8.3% [2]), 
#' Côte-des-Neiges-Notre-Dame-de-Grâce (4.9% [2]) and Rosemont-La-Petite-Patrie 
#' (4.4% [2]) accounting for most of the remaining matches.... In fact, 
#' the number of STR listings matched to LTR listings in Ville-Marie is 
#' equivalent to nearly half (42.0% [3]) of all the STR listings active in the 
#' borough on March 1, 2020, and 20.2% [4] of all the listings active in the 
#' borough in 2020.

#' [1] Unique STR matches
property %>% 
  filter(!is.na(ltr_ID)) %>% 
  nrow() %>% 
  scales::comma()

#' [2] Total number of unique matches by borough
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  count(borough) %>% 
  mutate(pct = n / sum(n)) %>% 
  arrange(-pct) %>% 
  mutate(pct = scales::percent(pct, 0.1))
  slice(1:6)

#' [3] Ville-Marie active March 1 percentage
{{property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  count(borough) %>% 
  mutate(pct = n / sum(n)) %>% 
  slice(17) %>% 
  pull(n)} /
  {daily %>% 
      filter(housing, borough == "Ville-Marie", status != "B", 
             date == "2020-03-01") %>% 
      nrow}} %>% 
  scales::percent(0.1)

#' [4] Ville-Marie active 2020 percentage
{{property %>% 
    st_drop_geometry() %>% 
    filter(!is.na(ltr_ID)) %>% 
    count(borough) %>% 
    mutate(pct = n / sum(n)) %>% 
    slice(17) %>% 
    pull(n)} /
  {daily %>% 
      filter(housing, borough == "Ville-Marie", status != "B", 
             date >= "2020-01-01") %>% 
      pull(property_ID) %>% 
      unique() %>% 
      length()}} %>% 
    scales::percent(0.1)
  

# Asking rents ------------------------------------------------------------

asking_rents <- 
  ltr_unique %>% 
  filter(price > 425, price < 8000) %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(matched, created) %>%
  summarize(avg_price = mean(price), .groups = "drop") %>% 
  mutate(status = if_else(matched, "Matched to STR", "Not matched"), 
         .before = created) %>% 
  select(-matched)

asking_rents <- 
  ltr_unique %>% 
  filter(price > 425, price < 8000) %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(created) %>%
  summarize(avg_price = mean(price), .groups = "drop") %>% 
  mutate(status = "All listings", .before = created) %>% 
  bind_rows(asking_rents) %>% 
  mutate(geography = "City of Montreal")

asking_rents_vm <- 
  ltr_unique %>% 
  filter(price > 425, price < 8000, borough == "Ville-Marie") %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(matched, created) %>%
  summarize(avg_price = mean(price), .groups = "drop") %>% 
  mutate(status = if_else(matched, "Matched to STR", "Not matched"), 
         .before = created) %>% 
  select(-matched)

asking_rents <- 
  ltr_unique %>% 
  filter(price > 425, price < 8000, borough == "Ville-Marie") %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(created) %>%
  summarize(avg_price = mean(price), .groups = "drop") %>% 
  mutate(status = "All listings", .before = created) %>% 
  bind_rows(asking_rents_vm) %>% 
  mutate(geography = "Ville-Marie") %>% 
  bind_rows(asking_rents)

#' Over the week of March 15-21, 2020, when the average asking rent on LTR 
#' platforms in the City of Montreal was $1,424 [1], the average asking rent 
#' among listings which we matched to Airbnb was $1,953 [1]—37.1% [1] higher. 
#' Over the course of March, average asking rents for LTR listings matched to 
#' Airbnb declined rapidly, and since April they have declined a further 
#' 8.0% [2] (an average of 1.0% [2] per month), from $1,800 [2] in April to 
#' $1,656 [2] in December. Overall asking rents in the City peaked in May at 
#' $1,456 [2], and then fell 8.0% to $1,340 in December. During this time 
#' period, asking rents for matched listings were on average 20.1% [3] higher 
#' than rents for unmatched listings.

#' [1] Mid-March difference between matched and city-wide
asking_rents %>% 
  filter(geography == "City of Montreal", created >= "2020-03-15",
         created <= "2020-03-21") %>% 
  summarize(city = mean(avg_price[status == "All listings"]),
            match = mean(avg_price[status == "Matched to STR"]),
            dif = match - city,
            dif_pct = match / city - 1) %>% 
  mutate(across(city:dif, scales::dollar, 1)) %>% 
  mutate(dif_pct = scales::percent(dif_pct, 0.1))

#' [2] April-December averages
asking_rents %>% 
  filter(geography == "City of Montreal", year(created) == 2020,
         month(created) %in% c(4, 5, 12), created <= "2020-12-15") %>%
  group_by(status, month = month(created)) %>% 
  summarize(avg_price = mean(avg_price)) %>% 
  summarize(april = avg_price[month == 4],
            may = avg_price[month == 5],
            dec = avg_price[month == 12],
            monthly_change_apr = (dec / april) ^ (1/8) - 1,
            total_change_apr = (dec - april) / april,
            monthly_change_may = (dec / may) ^ (1/7) - 1,
            total_change_may = (dec - may) / may) %>% 
  mutate(across(april:dec, scales::dollar, 1)) %>% 
  mutate(across(monthly_change_apr:total_change_may, scales::percent, 0.1))

#' [3] Average rent differences
asking_rents %>% 
  filter(geography == "City of Montreal", year(created) == 2020,
         month(created) >= 5, created <= "2020-12-15") %>%
  group_by(status) %>% 
  summarize(avg_price = mean(avg_price)) %>% 
  summarize(pct = (avg_price[status == "Matched to STR"] - 
                     avg_price[status == "Not matched"]) / 
              avg_price[status == "Not matched"]) %>% 
  pull(pct) %>% 
  scales::percent(0.1)

#' Even in Ville-Marie, however, LTR listings matched to Airbnb have been on 
#' average 15.3% [1] higher than listings not matched, and the same pattern of 
#' initially extremely high asking rents in March yielding to lower (although 
#' still high) rents from April through July is visible.

#' [1] Ville-Marie overall average rent differences
asking_rents %>% 
  filter(geography == "Ville-Marie", year(created) == 2020,
         month(created) >= 4, created <= "2020-12-15", 
         status != "All listings") %>% 
  group_by(status) %>% 
  summarize(avg_price = mean(avg_price)) %>% 
  summarize(pct = max(avg_price) / min(avg_price) - 1) %>% 
  pull(pct) %>% 
  scales::percent(0.1)

#' Overall, STRs returning to the long-term market are correlated with a 
#' substantial decline in Montreal asking rents. From May to December, average 
#' City-wide asking rents fell an average 1.2% [1] each month—from $1,456 [1] 
#' to $1,340 [1]. The decline was even larger in Ville-Marie, where STRs are 
#' disproportionately located and where asking rents dropped 9.9% [1] (1.5% [1] 
#' each month) from $1,652 [1] to $1,504 [1]. These facts lend substantial 
#' weight to the possibility that returning STRs have exerted meaningful 
#' downward pressure on rents, although a more definitive answer to this 
#' question would require a longer-term period of measurement.

#' [1] April-December averages
asking_rents %>% 
  filter(year(created) == 2020, month(created) %in% c(4, 5, 12), 
         created <= "2020-12-15") %>%
  group_by(geography, status, month = month(created)) %>% 
  summarize(avg_price = mean(avg_price)) %>% 
  summarize(april = avg_price[month == 4],
            may = avg_price[month == 5],
            dec = avg_price[month == 12],
            monthly_change_apr = (dec / april) ^ (1/8) - 1,
            total_change_apr = (dec - april) / april,
            monthly_change_may = (dec / may) ^ (1/7) - 1,
            total_change_may = (dec - may) / may) %>% 
  mutate(across(april:dec, scales::dollar, 1)) %>% 
  mutate(across(monthly_change_apr:total_change_may, scales::percent, 0.1))


# Listing amenities -------------------------------------------------------

#' Studios were overrepresented among LTR listings which matched to Airbnb 
#' (16.3% [1]) compared with LTR listings which did not match (10.1% [1]), 
#' STR listings (in 2020, 11.5% [2]) and Montreal’s rental stock (9.9% [3]). 
#' Units with three bedrooms or more were overrepresented in LTR listings, 
#' at 23.9% [1] for matches and 20.7% [2] for non-matches, compared to the 
#' City (10.3% [4]) and the STR market (12.5% [3]). One-bedrooms, which were 
#' considerably overrepresented in STR listings (54.9% [3]) compared to the 
#' overall rental housing stock (27.2% [4]), constituted 35.9% [1] of LTR 
#' listings that matched with STR listings, and a similar proportion among 
#' non-matched listings (36.2% [2]).

#' [1] Bedrooms 
ltr %>% 
  st_drop_geometry() %>% 
  group_by(match = !is.na(property_ID), id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(match, bedrooms) %>% 
  filter(!is.na(bedrooms)) %>% 
  rowwise() %>% 
  mutate(group = min(bedrooms, 3, na.rm = TRUE)) %>% 
  group_by(match, group) %>% 
  summarize(n = sum(n)) %>% 
  rename(bedrooms = group) %>% 
  mutate(n = scales::percent(n / sum(n), 0.1))

#' [2] Bedrooms in 2019 STRs
property %>% 
  st_drop_geometry() %>% 
  filter(housing, active >= "2020-01-01", created <= "2020-12-31") %>% 
  count(bedrooms) %>% 
  filter(!is.na(bedrooms)) %>% 
  rowwise() %>% 
  mutate(group = min(bedrooms, 3, na.rm = TRUE)) %>% 
  group_by(group) %>% 
  summarize(n = sum(n)) %>% 
  rename(bedrooms = group) %>% 
  mutate(n = scales::percent(n / sum(n), 0.1))

#' [3] Bedrooms in City of Montreal rentals
city_units %>% 
  filter(date == 2019) %>% 
  slice(1:4) %>% 
  mutate(pct = scales::percent(units / sum(units), 0.1))

#' Table 5.1 
bedroom_match_table <-
  city_units %>% 
  filter(date == 2019) %>% 
  slice(1:4) %>% 
  mutate(pct = scales::percent(units / sum(units), 0.1)) %>% 
  mutate(bedrooms = c("Studio", "1-bedroom", "2-bedroom", "3+-bedroom")) %>% 
  select(bedrooms, pct) %>% 
  mutate(Category = "City of Montreal rental stock", .before = bedrooms)

bedroom_match_table <-
  property %>% 
  st_drop_geometry() %>% 
  filter(housing, active >= "2020-01-01", created <= "2020-12-31") %>% 
  count(bedrooms) %>% 
  filter(!is.na(bedrooms)) %>% 
  rowwise() %>% 
  mutate(group = min(bedrooms, 3, na.rm = TRUE)) %>% 
  group_by(group) %>% 
  summarize(pct = sum(n)) %>% 
  rename(bedrooms = group) %>% 
  mutate(pct = scales::percent(pct / sum(pct), 0.1)) %>% 
  mutate(bedrooms = c("Studio", "1-bedroom", "2-bedroom", "3+-bedroom")) %>% 
  mutate(Category = "STR market (2020)", .before = bedrooms) %>% 
  bind_rows(bedroom_match_table, .)

bedroom_match_table <-
  ltr %>% 
  st_drop_geometry() %>% 
  group_by(match = !is.na(property_ID), id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(match, bedrooms) %>% 
  filter(!is.na(bedrooms)) %>% 
  rowwise() %>% 
  mutate(group = min(bedrooms, 3, na.rm = TRUE)) %>% 
  group_by(match, group) %>% 
  summarize(n = sum(n)) %>% 
  rename(bedrooms = group) %>% 
  mutate(pct = scales::percent(n / sum(n), 0.1)) %>% 
  mutate(bedrooms = c("Studio", "1-bedroom", "2-bedroom", "3+-bedroom")) %>% 
  ungroup() %>% 
  mutate(Category = if_else(match, "LTRs matched to STR", "LTRs not matched"), 
         .before = bedrooms) %>% 
  select(-match, -n) %>% 
  bind_rows(bedroom_match_table, .)

bedroom_match_table <- 
  bedroom_match_table %>% 
  pivot_wider(values_from = pct, names_from = bedrooms)

bedroom_match_table %>% 
  gt() %>% 
  tab_header(title = "STR and LTR listings by bedroom count") %>%
  opt_row_striping()

#' Of the 141,399 [1] LTR listings we analyzed, 28.5% [2] were listed as 
#' furnished, 60.9% [2] as unfurnished, and 10.5% [2] did not provide this 
#' information. Listings which matched with STRs had a significantly higher 
#' proportion classified as furnished: 75.0% [3] furnished and 22.7% [3] 
#' unfurnished, with only 2.4% [3] not providing this information.

#' [1] All scraped LTR listings
ltr_unique %>% nrow() %>% scales::comma()

#' [2] Furnished proportion for all LTRs
ltr_unique %>% 
  count(furnished) %>% 
  mutate(pct = scales::percent(n / sum(n), 0.1))

#' [3] Furnished proportion for matches
ltr_unique_property_ID %>% 
  count(furnished) %>% 
  mutate(pct = scales::percent(n / sum(n), 0.1))


# Are matched listings commercial operations? -----------------------------

#' Of the 2,792 [1] unique STR listings that matched with the LTR market, 
#' 2,389 [2] (85.6% [2]) are entire-home listings and 368 [2] (13.2% [2]) are 
#' private-room listings. Examining the entire-home listings, 57.5% [3] of them 
#' were identified as frequently rented entire-home (FREH) listings at some 
#' point, which means they were almost certainly operated commercially. 
#' Moreover, 57.1% [3] of entire-home STR listings which matched to LTR listings 
#' were multilistings at some point, which means they were operated by hosts 
#' controlling multiple listings simultaneously. In total, 76.5% [3] of 
#' entire-home listings had one of these two strong indicators of commercial 
#' activity.

#' [1] Unique STR matches
property %>% filter(!is.na(ltr_ID)) %>% nrow() %>% scales::comma()

#' [2] listing_type breakdown among matches
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% ltr_unique_property_ID$property_ID) %>% 
  count(listing_type) %>% 
  mutate(pct = scales::percent(n / sum(n), 0.1))

#' [3] Commercial status among EH matches
daily %>% 
  filter(property_ID %in% ltr_unique_property_ID$property_ID,
         listing_type == "Entire home/apt") %>% 
  group_by(property_ID) %>% 
  summarize(FREH = as.logical(sum(FREH_3 > 0.5)),
            ML = as.logical(sum(multi))) %>% 
  summarize(total = 
              property %>% 
              st_drop_geometry() %>% 
              filter(property_ID %in% ltr_unique_property_ID$property_ID, 
                     listing_type == "Entire home/apt") %>% 
              nrow(),
            FREH_pct = sum(FREH) / total,
            ML_pct = sum(ML) / total,
            either_pct = sum(ceiling((FREH + ML) / 2)) / total) %>% 
  mutate(across(!where(is.integer), scales::percent, 0.1)) %>% 
  mutate(total = scales::comma(total))

#' The 368 [1] private-room listings require some further analysis, because each 
#' of these listings matched to a Craigslist or Kijiji listing advertised as an 
#' entire housing unit. Our analysis suggests that these listings break down 
#' into three categories. The first is miscategorizations. 73 [2] (19.8% [2]) of 
#' the LTR listings that matched to STR private-room listings had titles such as 
#' “1 fully furnished bedroom” or “swap”.

#' [1] PR matches
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% ltr_unique_property_ID$property_ID) %>% 
  count(listing_type) %>% 
  mutate(pct = scales::percent(n / sum(n), 0.1)) %>% 
  filter(listing_type == "Private room")

#' [2] Miscategorizations
property %>% 
  filter(listing_type == "Private room",
         property_ID %in% unlist(ltr_PR$property_ID)) %>% 
  nrow() %>% 
  {c(., . / {property %>% 
      st_drop_geometry() %>% 
      filter(property_ID %in% ltr_unique_property_ID$property_ID) %>% 
      count(listing_type) %>% 
      mutate(pct = n / sum(n)) %>% 
      filter(listing_type == "Private room") %>% 
      pull(n)})} %>% 
  round(3)

#' 149 [1] (40.5%) [1] of the 368 [2] private-room listings which matched to 
#' Craiglist or Kijiji were listings identified as belonging to ghost hostels in 
#' Montreal. 

#' [1] GH matches
property %>% 
  filter(listing_type == "Private room") %>% 
  filter(property_ID %in% ltr_unique_property_ID$property_ID) %>% 
  filter(property_ID %in% unlist(GH$property_IDs)) %>% 
  nrow() %>% 
  {c(., . / {property %>% 
      st_drop_geometry() %>% 
      filter(property_ID %in% ltr_unique_property_ID$property_ID) %>% 
      count(listing_type) %>% 
      filter(listing_type == "Private room") %>% 
      pull(n)})} %>% 
  round(3)

#' [2] PR matches
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% ltr_unique_property_ID$property_ID) %>% 
  count(listing_type) %>% 
  filter(listing_type == "Private room") %>% 
  pull(n)

#' The remaining 146 [1] private-room Airbnb listings which matched to 
#' Craigslist or Kijiji are likely to be ghost hostels which our algorithms 
#' failed to identify, or smaller housing units similarly subdivided into 
#' private rooms.

#' [1] Leftover PR matches
{property %>% 
    st_drop_geometry() %>% 
    filter(property_ID %in% ltr_unique_property_ID$property_ID) %>% 
    count(listing_type) %>% 
    filter(listing_type == "Private room") %>% 
    pull(n)} - {property %>% 
  filter(listing_type == "Private room",
         property_ID %in% unlist(ltr_PR$property_ID)) %>% 
  nrow()} - {property %>% 
      filter(listing_type == "Private room") %>% 
      filter(property_ID %in% ltr_unique_property_ID$property_ID) %>% 
      filter(property_ID %in% unlist(GH$property_IDs)) %>% 
      nrow()}

#' Focusing on the unambiguous case of the entire-home listings which matched 
#' between STR and LTR platforms, 24.1% [1] of the commercial listings active 
#' in 2020 have been transferred to Craiglist or Kijiji since March.... 
#' Expressed as a percentage of the commercial listings active on March 1, 2020, 
#' at the onset of the pandemic, the matches represent 45.5% [2] of these 
#' listings.

#' [1] EH matches as % of commercial operations on March 1
{property %>% 
    filter(property_ID %in% ltr_unique_property_ID$property_ID, 
           listing_type == "Entire home/apt") %>% 
    nrow() /
    daily %>% 
    filter(listing_type == "Entire home/apt", status != "B", 
           date >= "2020-01-01", (FREH_3 > 0.5 | multi == TRUE)) %>% 
    count(property_ID) %>% 
    nrow()} %>% 
  scales::percent(0.1)

#' [2] EH matches as % of commercial operations on March 1
{property %>% 
  filter(property_ID %in% ltr_unique_property_ID$property_ID, 
         listing_type == "Entire home/apt") %>% 
  nrow() /
  daily %>% 
  filter(listing_type == "Entire home/apt", status != "B", date == "2020-03-01", 
         (FREH_3 > 0.5 | multi == TRUE)) %>% 
  nrow()} %>% 
  scales::percent(0.1)


# Which STR hosts transferred their listings to Craigslist and Kij --------

#' In Montreal, 1,338 [1] unique Airbnb host IDs were linked to the 2,792 [2] 
#' LTR matches. 327 [3] of these hosts posted more than one of their STR units 
#' on Craigslist or Kijiji. For example, the top host network in Montreal 
#' (discussed in section 2) posted 195 [3] of its STR units on the LTR market. 
#' Half (45.1% [4]) of the active properties of these 1,338 [1] hosts were found
#'  on either Kijiji, Craigslist, or both. 

#' [1] number of hosts found on a LTR platform
property %>%
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID), !is.na(host_ID)) %>% 
  count(host_ID, sort = TRUE) %>% 
  nrow() %>% 
  scales::comma()

#' [2] Unique STR matches
property %>% filter(!is.na(ltr_ID)) %>% nrow() %>% scales::comma()

#' [3] Number of hosts with > 1 property on LTR market
property %>%
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID), !is.na(host_ID)) %>% 
  count(host_ID, sort = TRUE) %>% 
  filter(n > 1) %>%
  summarize(total = n(), top = max(n))

#' [4] Percentage of hosts' properties which made the switch from STR to LTR
{nrow(ltr_unique_property_ID) / 
    property %>% 
    st_drop_geometry() %>% 
    filter(host_ID %in% (filter(property, !is.na(ltr_ID)))$host_ID, 
           scraped >= "2020-01-01") %>% 
    nrow()} %>% 
  scales::percent(0.1)

#' The median STR host revenue was $4,300 [1] in the entire City of Montreal in 
#' 2019. The annual median revenue of hosts who transferred listings to the LTR 
#' market was $14,300 [2], while the median revenue of hosts who did not 
#' transfer listings was only $4,000 [2]. Moreover, many of Montreal’s highest 
#' earning STR hosts turned to LTR platforms during the COVID-19 pandemic. For 
#' example, 27 [3] of the 38 [3] hosts that made more than $500,000 in the past 
#' year listed at least one of their STR units on an LTR platform. On average 
#' these top earning hosts listed 28.9 [4] units on LTR platforms, compared to 
#' 1.6 units [4] for all other hosts). 

revenue_2019 <- 
  daily %>%
  filter(housing,
         date <= "2019-12-31", date >= "2019-01-01",
         status == "R") %>%
  group_by(property_ID) %>%
  summarize(revenue_LTM = sum(price)) %>% 
  inner_join(property, .) %>% 
  st_drop_geometry() %>% 
  select(property_ID, host_ID, listing_type:active, revenue_LTM)

half_mil_ltr <- 
  revenue_2019 %>% 
  group_by(host_ID) %>% 
  summarize(host_rev = sum(revenue_LTM)) %>% 
  filter(host_rev > 500000)

#' [1] Median host revenue
round(median(revenue_2019$revenue_LTM), -2)

#' [2] Median host revenue by host match status
revenue_2019 %>% 
  group_by(host_match = host_ID %in% (filter(property, 
                                             !is.na(ltr_ID)))$host_ID,
           host_ID) %>%
  summarize(host_rev = sum(revenue_LTM)) %>% 
  summarize("median_rev" = round(median(host_rev), -2))

#' [3] Hosts that matched and made more than 500k
half_mil_ltr %>% 
  summarize(
    total = n(),
    host_match = sum(
      host_ID %in% (filter(property, property_ID %in% 
                             ltr_unique_property_ID$property_ID)$host_ID)))

#' [4] Average number of listings by host match status
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>%
  count(host_ID) %>% 
  group_by(host_match = host_ID %in% half_mil_ltr$host_ID) %>% 
  summarize(round(mean(n), 1))

#' Out of all hosts with active STR listings in 2020 that shifted their listings 
#' to the LTR market in Montreal, 21.8% [1] had Superhost status. This is almost 
#' twice [1] as high as the general 11.7% [1] prevalence of Superhost status 
#' among Montreal hosts, which is consistent with the idea that the listings 
#' moving from STR to LTR platforms in Montreal are dominated by commercial 
#' operations. 

#' [1] Superhost status
property %>%
  st_drop_geometry() %>%
  filter(!is.na(host_ID), !is.na(superhost), scraped >= "2020-01-01") %>% 
  group_by(match = !is.na(ltr_ID), superhost) %>% 
  count(host_ID) %>% 
  group_by(match, host_ID) %>% 
  summarize(superhost = as.logical(sum(superhost))) %>% 
  summarize(pct = sum(superhost) / n()) %>% 
  mutate(dif = pct[2] / pct[1])


# Are matched listings successfully rented, or still active on Air --------

#' STR matches were listed an average of 23.3 [1] days on LTR platforms, while 
#' non-matches were listed only half as long—12.9 [1] days on average.

#' [1] length of availability on LTR platforms
ltr_unique %>% 
  group_by(matched = !is.na(property_ID)) %>% 
  summarize(round(mean(scraped - created, na.rm = TRUE), 1))

#' Out of the total 2,792 [1] Airbnb listings which we identified on LTR 
#' platforms, 1,680 [2] (60.2% [2]) were still present on Airbnb at the
#' beginning of 2020. Out of this number, 873 [2] (52.0% [2]) were still present
#' by the end of December 2020, while the other 807 [2] (48.0% [2]) had been 
#' deactivated. Extrapolating this proportion across the entire set of matched 
#' listings we identified, we estimate that 1,341 [2] matched listings have been 
#' deactivated from Airbnb during the pandemic, while 1,451 [2] remain on the 
#' platform.... However, 80.1% [3] of these listings were rented as furnished 
#' rentals on Craigslist or Kijiji. 

#' [1] Unique STR matches
property %>% filter(!is.na(ltr_ID)) %>% nrow() %>% scales::comma()

#' [2] Number of matches still listed on Airbnb
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  filter(scraped >= "2020-01-01") %>% 
  summarize(total = n(),
            total_pct = total / nrow(filter(property, !is.na(ltr_ID))),
            n_scraped = sum(scraped >= "2020-12-31"),
            pct_scraped = mean(scraped >= "2020-12-31"),
            not_scraped = total - n_scraped,
            pct_not_scraped = 1 - pct_scraped,
            n_gone = pct_not_scraped * nrow(filter(property, !is.na(ltr_ID))),
            n_active = pct_scraped * nrow(filter(property, !is.na(ltr_ID))))

#' [3] Furnished rentals with deactivated Airbnb listings
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  filter(scraped >= "2020-01-01", scraped < "2020-12-31") %>% 
  left_join(select(ltr_unique_property_ID, property_ID, furnished)) %>% 
  filter(!is.na(furnished)) %>% 
  summarize(furnished = scales::percent(mean(furnished), 0.1))

#'  Of the 873 [1] matched listings which were present on Airbnb at the 
#'  beginning of 2020 and still present by the end of December, 464 [1] 
#'  (53.2% [1]) were blocked for the entirety of the month of December.

#' [1] Inactive in December
property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID), scraped >= "2020-12-31") %>% 
  summarize(total = n(),
            n_inactive = total - sum(active >= "2020-12-01", na.rm = TRUE) +
              sum(is.na(active)),
            pct_inactive = n_inactive / total)
  
#' In total, taking into account the matched listings which have continued to 
#' see activity on Airbnb, we estimate that, of the total 2,792 [1] STR listings 
#' which were advertised on Craigslist or Kijiji, 1,341 [1] (48.0% [1]) have 
#' been permanently deactivated from Airbnb and have likely transitioned back to 
#' long-term housing, 733 [1] (26.3% [1]) have been temporarily blocked on 
#' Airbnb and have likely been rented in the long-term market but may return to 
#' being STRs in the future, and 718 [1] (25.7% [1]) failed to be rented on 
#' LTR platforms and instead remain active on Airbnb.

#' [1] Total active/deactivated/inactive estimates
deactivated_pct <- 
  property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  filter(scraped >= "2020-01-01") %>% 
  summarize(pct_not_scraped = 1 - mean(scraped >= "2020-12-31")) %>% 
  pull(pct_not_scraped)

blocked_pct <-
  property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  filter(scraped >= "2020-01-01") %>% 
  summarize(pct_blocked = (sum(active < "2020-12-01" & scraped >= "2020-12-31", 
                               na.rm = TRUE) + sum(is.na(active) & scraped >= 
                                                     "2020-12-31")) / n()) %>% 
  pull(pct_blocked)

property %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ltr_ID)) %>% 
  summarize(total = n(),
            deactivated = total * deactivated_pct,
            blocked = total * blocked_pct,
            active = total - deactivated - blocked) %>% 
  mutate(across(where(is.numeric), round, 0)) %>% 
  pivot_longer(-total) %>% 
  mutate(pct = scales::percent(value / sum(value), 0.1))


# Clean up ----------------------------------------------------------------

rm(annual_avg_rent, annual_units, annual_vacancy, asking_rents, asking_rents_vm,
   bedroom_match_table, boroughs, boroughs_raw, city, city_avg_rent, city_units,
   city_vacancy, cmhc, DA, half_mil_ltr, ltr, ltr_PR, ltr_unique,
   ltr_unique_property_ID, province, revenue_2019, streets, streets_downtown,
   blocked_pct, deactivated_pct)