#### 20 CHAPTER 2 ANALYSIS ####################################################0

#' This script produces the tables and facts for chapter 2. It runs quickly.
#' 
#' Output:
#' - None
#' 
#' Script dependencies:
#' - `02_geometry_import.R`
#' - `09_str_processing.R`
#' - `10_national_comparison.R`
#' - `13_raffle_condo.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")


# Load previous data ------------------------------------------------------

load("output/str_processed.Rdata")
load("output/geometry.Rdata")


# Prepare new objects -----------------------------------------------------

# 2019 active
active_2019 <- 
  daily %>%
  filter(housing, status %in% c("R", "A"), date <= LTM_end_date, 
         date >= LTM_start_date) %>%
  pull(property_ID) %>% 
  unique()
  
# 2019 revenue
revenue_2019 <-
  daily %>%
  filter(housing, status == "R", date <= LTM_end_date, 
         date >= LTM_start_date) %>%
  group_by(property_ID) %>%
  summarize(revenue_LTM = sum(price)) %>% 
  inner_join(property, .)


# Active daily listings ---------------------------------------------------

#'  In 2019 there was an average of 9,010 [1] active daily listings (Figure 1) 
#'  operated by an average of 5,330 [2] hosts. These hosts collectively earned 
#'  $222.7 million [3] in 2019—an average of $24,700 [4] per daily active 
#'  listing or $41,800 [5] per active host. There was also a daily average of 
#'  12,460 [1] listings which were visible on the Airbnb and VRBO websites but 
#'  were blocked by the host from receiving reservations. The presence of these 
#'  listings can erroneously suggest that a city’s STR market is larger than it 
#'  is; in the case of Montreal, blocked listings outnumber listings which are 
#'  actually active. When these blocked but inactive listings are included, the 
#'  average listing earned [6] $9,400 last year, and the average host earned 
#'  $16,800 [7]. Finally, there was a daily average of 240 [8] listings that 
#'  were not located in private housing units #'  (B&Bs, hotels, etc.), which 
#'  have been excluded from the analysis in this report. 
#'  
#'  Active daily listings peaked in August 2018 [9] at 11,8100 [9], and have 
#'  since declined. There were 5.6% [10] fewer listings active on average in 
#'  2019 than in 2018. However, host revenue followed the opposite pattern, 
#'  increasing by 14.9% [10] between 2018 and 2019. These facts point to an 
#'  increasingly commercializing STR market, where the number of listings is 
#'  relatively stable but a rising proportion of these listings are operated on 
#'  a full-time basis.

#' [1] Average active and blocked daily listings in 2019
daily %>% 
  filter(housing, date >= LTM_start_date, date <= LTM_end_date) %>% 
  count(date, B = status == "B") %>% 
  group_by(B) %>% 
  summarize(round(mean(n), digit = -1))

#' [2] Average number of hosts (taking out blocked 365 days)
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date, 
         date <= LTM_end_date) %>%
  count(date, host_ID) %>% 
  count(date) %>% 
  summarize(hosts = round(mean(n), digit = -1))

#' [3] Total annual revenue
prettyNum(round(sum(revenue_2019$revenue_LTM), digit = -5), ",")

#' [4] Average revenue per active listing
(sum(revenue_2019$revenue_LTM) /
    daily %>% 
    filter(housing, status != "B", date >= LTM_start_date, 
           date <= LTM_end_date) %>% 
    count(date) %>% 
    summarize(avg_rev_per_active = round(mean(n)))) %>% 
  round(digit = -2) %>% 
  prettyNum(",")

#' [5] Average revenue per active host
(sum(revenue_2019$revenue_LTM) /
    daily %>% 
    filter(housing, status != "B", date >= LTM_start_date, 
           date <= LTM_end_date) %>%
    group_by(date) %>% 
    summarize(n_hosts = length(unique(host_ID))) %>% 
    summarize(avg_n_hosts = round(mean(n_hosts)))) %>% 
  round(digit = -2) %>% 
  prettyNum(",")

#' [6] Average revenue per all listings
prettyNum(round(mean(revenue_2019$revenue_LTM), digit = -2), ",")

#' [7] Average revenue per all hosts
revenue_2019 %>% 
  st_drop_geometry() %>% 
  filter(!is.na(host_ID)) %>%
  group_by(host_ID) %>% 
  summarize("host_rev" = sum(revenue_LTM)) %>% 
  summarize("avg_host_rev" = prettyNum(round(mean(host_rev), digit = -2), ","))

#' [8] Non-housing active listings
daily %>% 
  filter(!housing, status != "B", date >= LTM_start_date, 
         date <= LTM_end_date) %>%
  count(date) %>% 
  summarize(non_housing = round(mean(n), digit = -1))

#' [9] Date and amount of highest activity
daily %>% 
  filter(housing, status != "B") %>% 
  count(date) %>% 
  filter(n == max(n)) %>% 
  group_by(date) %>% 
  summarize(daily_max = round((n), digit = -1))

#' [10] Active listing YOY change
daily %>% 
  filter(housing, status != "B", date >= "2018-01-01", date <= "2019-12-31") %>% 
  group_by(year_2019 = date >= "2019-01-01") %>% 
  summarize(active_listings = n() / 365,
            revenue = sum(price[status == "R"])) %>% 
  summarize(across(c(active_listings, revenue), ~{(.x[2] - .x[1]) / .x[1]}))


# Montreal in comparison with other major Canadian cities -----------------

load("output/national_comparison.Rdata")

#' In 2019, Montreal had the second largest STR market in the country by both 
#' active listing numbers (9,100 [1]) and host revenue ($222.7 million [2]), 
#' falling in both cases behind Toronto (Table 2.1). However, in relative terms 
#' Vancouver stands considerably ahead of both Montreal and Toronto. Vancouver 
#' had the most active listings per 1000 households (13.4 [3] compared to 
#' 10.7 [3] in Montreal) and the most revenue per listing ($38,500 [4] compared 
#' to $24,700 [4] in Montreal).

#' [1] Daily active listings
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date, 
         date <= LTM_end_date) %>% 
  count(date) %>% 
  summarize(active_listings = round(mean(n), digit = -1))

#' [2] Annual host revenue
prettyNum(round(sum(revenue_2019$revenue_LTM), digit = -5), ",")

#' [3] Vancouver and Montreal listings per 1000 households
national_comparison %>% 
  filter(city %in% c("Montreal", "Vancouver")) %>% 
  select(city, listings_per_1000)

#' [4] Vancouver and Montreal revenue per listing
national_comparison %>% 
  filter(city %in% c("Montreal", "Vancouver")) %>% 
  select(city, revenue_per_listing)

#' Table 2.1
national_comparison %>% 
  mutate(active_daily_listings = prettyNum(round(active_daily_listings, -1), 
                                           ","),
         listings_per_1000 = round(listings_per_1000, 1),
         revenue = prettyNum(round(revenue, -5), ","),
         revenue_per_listing = prettyNum(round(revenue_per_listing, -2), ","))


# Location of STR listings and revenue ------------------------------------

boroughs_breakdown <- 
  daily %>% 
  filter(housing, status != "B", date >= LTM_start_date, 
         date <= LTM_end_date) %>% 
  group_by(date, borough) %>% 
  summarize(n = n(),
            revenue = sum(price[status == "R"])) %>% 
  left_join(st_drop_geometry(boroughs)) %>% 
  group_by(borough, dwellings) %>% 
  summarize(active_listings = mean(n),
            annual_rev = sum(revenue), 
            .groups = "drop") %>% 
  mutate(listings_pct = active_listings / sum(active_listings),
         rev_pct = annual_rev / sum(annual_rev),
         listings_pct_dwellings = active_listings / dwellings) %>% 
  select(-dwellings)

#' STR activity in Montreal is highly concentrated in the central-city boroughs 
#' of Ville-Marie and Le Plateau-Mont-Royal (Table 2.2). These two boroughs 
#' accounted for 32.6% [1] and 25.9% [1] of all listings in 2019 respectively, 
#' and even higher shares of host revenue (41.2% [1] and 29.6% [1]). The borough 
#' with the next highest percentage of average number of daily active listings 
#' is Rosemont-La-Petite-Patrie (8.1% [2]), followed by Le Sud-Ouest (6.9% [2]). 
#' Each accounts for around 6% [2] of annual STR revenue in the city.
#' 
#' Ville-Marie and Le Plateau-Mont-Royal have by far the most STR activity when 
#' measured in per-capita terms. In Ville-Marie, active STR listings account for 
#' 4.8% [1] of all the borough’s housing units, while the equivalent figure for 
#' Le Plateau-Mont-Royal is 3.7% [1] of total dwellings (Figure 2.2).

#' [1] Figures for VM and LPMR
boroughs_breakdown %>% 
  slice(c(7, 18)) %>% 
  select(borough, listings_pct:listings_pct_dwellings)

#' [2] Figures for RLPP and LSO
boroughs_breakdown %>% 
  slice(c(8, 14)) %>% 
  select(borough, listings_pct:rev_pct)

#' Table 2.2
boroughs_breakdown %>% 
  set_names(c("Borough",
              "Daily active listings (average)",
              "Annual revenue (CAD)",
              "% of all listings",
              "% of annual revenue",
              "Active listings as % of dwellings")) %>% 
  filter(`Daily active listings (average)` > 100) %>% 
  arrange(desc(`Daily active listings (average)`)) %>%
  mutate(`Daily active listings (average)` = 
           round(`Daily active listings (average)`, digit = -1),
         `Annual revenue (CAD)` = round(`Annual revenue (CAD)`),
         `Annual revenue (CAD)` = 
           paste0("$", str_sub(`Annual revenue (CAD)`, 1, -7), ".",
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

# Need to add Montreal row, with % listings per dwelling
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date, 
         date <= LTM_end_date) %>% 
  count(date) %>% 
  summarize(active_listings = round(mean(n), digit = -1)) %>% 
  pull(active_listings) %>% 
  {. / sum(boroughs$dwellings)}


# Listing types and sizes -------------------------------------------------

listing_type_breakdown <- 
  daily %>% 
  filter(housing, status != "B", date >= LTM_start_date,
         date <= LTM_end_date) %>% 
  group_by(listing_type) %>% 
  summarize(
    active_listings = n() / 365,
    revenue = sum(price[status == "R"]),
    .groups = "drop") %>% 
  mutate(pct_of_listings = active_listings / sum(active_listings),
         pct_of_revenue = revenue / sum(revenue))

listing_type_breakdown <- 
  daily %>% 
  filter(housing, status != "B", date >= "2018-01-01", date <= "2018-12-31") %>% 
  group_by(listing_type) %>% 
  summarize(active_2018 = n() / 365) %>% 
  left_join(listing_type_breakdown, .) %>% 
  mutate(pct_listing_growth = (active_listings - active_2018) / active_2018) %>% 
  select(-active_2018)

#' The vast majority of STRs in Montreal are entire homes, a category which 
#' includes single-family homes, townhouses, apartments and condominiums. 
#' Nearly half of these (43.6% [1]) were one-bedroom housing units, with the 
#' remainder relatively evenly split between studio apartments (12.6% [1]), 
#' two-bedroom units (27.6% [1]), and three-or-more-bedroom units (16.1% [1]). 
#' In 2019 entire-home listings accounted for 75.6% [2] of all daily active 
#' listings, and 91.0% [3] of total host revenue. Private rooms accounted for 
#' nearly all of the remainder.

#' [1] Bedroom counts
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% active_2019, listing_type == "Entire home/apt") %>% 
  mutate(bedrooms = if_else(bedrooms >= 3, "3+", as.character(bedrooms))) %>% 
  count(bedrooms) %>% 
  mutate(percentage = n / sum(n))

#' [2] EH listings and revenue
listing_type_breakdown %>% 
  filter(listing_type == "Entire home/apt") %>% 
  select(-active_listings, -revenue)

#' Table 2.3
listing_type_breakdown %>%
  mutate(active_listings = round(active_listings, digits = -1),
         revenue = round(revenue, digits = -5),
         pct_of_listings = round(pct_of_listings, digits = 3),
         pct_of_revenue = round(pct_of_revenue, digits = 3),
         pct_listing_growth = round(pct_listing_growth, digits = 3)) %>% 
  rename(`Listing type` = listing_type,
         `Daily active listings (average)` = active_listings,
         `Annual revenue (CAD)` = revenue,
         `% of active listings` = pct_of_listings,
         `% of annual revenue` = pct_of_revenue,
         `% average daily listing growth (YOY 2018-2019)` = pct_listing_growth
         ) %>% 
  gt() %>% 
  tab_header(
    title = "Listing type breakdown",
    subtitle = "2019"
  ) %>%
  opt_row_striping() %>% 
  fmt_percent(columns = 4:6, decimals = 1) %>% 
  fmt_number(columns = 2,
             decimals = 0)


# STRs and housing tenure -------------------------------------------------

load("output/raffle_condo.Rdata")

active_condos_2017 <- 
  daily %>% 
  filter(housing, date >= "2017-01-01", date <= "2017-12-31", status != "B") %>% 
  left_join(listing_probabilities_2017) %>% 
  group_by(date, borough) %>% 
  summarize(n_listings = n(),
            n_condo = sum(p_condo, na.rm = TRUE)) %>% 
  group_by(borough) %>% 
  summarize(n_listings_2017 = mean(n_listings),
            n_condo_listings_2017 = mean(n_condo))

active_condos_2019 <- 
  daily %>% 
  filter(housing, date >= "2019-01-01", date <= "2019-12-31", status != "B") %>% 
  left_join(listing_probabilities_2019) %>% 
  group_by(date, borough) %>% 
  summarize(n_listings = n(),
            n_condo = sum(p_condo, na.rm = TRUE)) %>% 
  group_by(borough) %>% 
  summarize(n_listings_2019 = mean(n_listings),
            n_condo_listings_2019 = mean(n_condo))

borough_condos <- 
  DA_probabilities_2019 %>% 
  mutate(across(c(p_condo, p_owner, p_renter), ~{.x * dwellings})) %>% 
  mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, as.numeric(.x)))) %>% 
  select(dwellings:p_renter, geometry) %>% 
  st_interpolate_aw(boroughs, extensive = TRUE) %>% 
  st_drop_geometry() %>% 
  select(-Group.1) %>% 
  rename(dwellings_agg = dwellings,
         n_condo = p_condo,
         n_owner = p_owner,
         n_renter = p_renter) %>% 
  cbind(boroughs, .) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  mutate(across(c(n_condo, n_owner, n_renter), 
                ~{.x * dwellings / dwellings_agg})) %>% 
  select(-dwellings_agg)

condo_breakdown <- 
  borough_condos %>% 
  left_join(active_condos_2017) %>% 
  left_join(active_condos_2019) %>% 
  relocate(geometry, .after = last_col()) %>% 
  mutate(`Number of STRs in condos` = round(n_condo_listings_2019, digits = 1),
         `% of STRs in condos (2019)` = 
           n_condo_listings_2019 / n_listings_2019,
         `% change in % of STRs in condos (2017 to 2019)` =
           (n_condo_listings_2019 / n_listings_2019) / 
           (n_condo_listings_2017 / n_listings_2017) - 1) %>% 
  select(Borough = borough, `Number of STRs in condos`, 
         `% of STRs in condos (2019)`, 
         `% change in % of STRs in condos (2017 to 2019)`)
  
condo_breakdown %>% 
  st_drop_geometry() %>% 
  arrange(desc(`Number of STRs in condos`)) %>%
  slice(1:11) %>% 
  mutate(`Number of STRs in condos` = round(`Number of STRs in condos`, 
                                            digit = -1)) %>%
  gt() %>% 
  tab_header(
    title = "Condo breakdown",
    subtitle = 
      "Number of STRs in condos and percentage of STRs in condos by borough"
    ) %>%
  opt_row_striping() %>% 
  fmt_percent(columns = 3:4, decimals = 1) %>% 
  fmt_number(columns = 2,
             decimals = 0)


### STR growth #####################################################################

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

# 2017 to 2018
(filter(daily, housing, status != "B", date >= LTM_start_date - years(1), date <= LTM_end_date - years(1)) %>% 
    count(date) %>% 
    summarize(mean(n)) - 
    filter(daily, housing, status != "B", date >= LTM_start_date - years(2), date <= LTM_end_date - years(2)) %>% 
    count(date) %>% 
    summarize(mean(n)) + 960) /
  filter(daily, housing, status != "B", date >= LTM_start_date - years(2), date <= LTM_end_date - years(2)) %>% 
  count(date) %>% 
  summarize(mean(n) + 960) # +960 to account for HA



# YOY growth of revenue
# 2019 to 2020
(daily %>% 
    filter(housing, date > LTM_end_date, date <= max(date), status == "R") %>% 
    summarize(sum(price)) -
    daily %>% 
    filter(housing, date > LTM_end_date - years(1), date <= max(date) - years(1), status == "R") %>% 
    summarize(sum(price))
)/
  daily %>% 
  filter(housing, date > LTM_end_date - years(1), date <= max(date) - years(1), status == "R") %>% 
  summarize(sum(price))

# 2018 to 2019
(daily %>% 
    filter(housing, date >= LTM_start_date, date <= LTM_end_date, status == "R") %>% 
    summarize(sum(price)) -
    daily %>% 
    filter(housing, date >= LTM_start_date - years(1), date <= LTM_end_date - years(1), status == "R") %>% 
    summarize(sum(price))
)/
  daily %>% 
  filter(housing, date >= LTM_start_date - years(1), date <= LTM_end_date - years(1), status == "R") %>% 
  summarize(sum(price))

# 2017 to 2018
(daily %>% 
    filter(housing, date >= LTM_start_date - years(1), date <= LTM_end_date - years(1), status == "R") %>% 
    summarize(sum(price)) -
    daily %>% 
    filter(housing, date >= LTM_start_date - years(2), date <= LTM_end_date - years(2), status == "R") %>% 
    summarize(sum(price))
)/
  daily %>% 
  filter(housing, date >= LTM_start_date - years(2), date <= LTM_end_date - years(2), status == "R") %>% 
  summarize(sum(price))


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

ML_table %>% # look at % of commercial operations revenue in the last month of the analysis
  filter(date >= max(date) - months(1)) %>% 
  arrange(desc(Revenue))

# Entire home multilistings
daily %>% 
  filter(listing_type == "Entire home/apt") %>% 
  group_by(date) %>% 
  summarize(Listings = sum(multi)) %>% 
  filter(date == key_date)




