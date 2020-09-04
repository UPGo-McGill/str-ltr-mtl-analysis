#### 20 CHAPTER 2 ANALYSIS #####################################################

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

load("output/str_processed.Rdata")
load("output/national_comparison.Rdata")
load("output/geometry.Rdata")
load("output/condo_analysis.Rdata")


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

#'  In 2019 there was an average of 9,040 [1] active daily listings (Figure 2.1) 
#'  operated by an average of 5,330 [2] hosts. These hosts collectively earned 
#'  $224.4 million [3] in 2019—an average of $24,900 [4] per daily active 
#'  listing or $42,100 [5] per active host. There was also a daily average of 
#'  12,420 [1] listings which were visible on the Airbnb and VRBO websites but 
#'  were blocked by the host from receiving reservations. The presence of these 
#'  listings can erroneously suggest that a city’s STR market is larger than it 
#'  is; in the case of Montreal, blocked listings outnumber listings which are 
#'  actually active. When these blocked but inactive listings are included, the 
#'  average listing earned $9,500 [6] last year, and the average host earned 
#'  $16,900 [7]. Finally, there was a daily average of 240 [8] listings that 
#'  were not located in private housing units (B&Bs, hotels, etc.), which 
#'  have been excluded from the analysis in this report. 
#'  
#'  Active daily listings peaked in August 2018 [9] at 11,810 [9], and have 
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
  filter(housing, status != "B", date >= LTM_start_date - years(1), 
         date <= LTM_end_date) %>% 
  group_by(year_2019 = date >= LTM_start_date) %>% 
  summarize(active_listings = n() / 365,
            revenue = sum(price[status == "R"])) %>% 
  summarize(across(c(active_listings, revenue), ~{(.x[2] - .x[1]) / .x[1]}))


# STR growth rates --------------------------------------------------------

#' Overall, the year-over-year change in average active listings from 2016 to 
#' 2017 (12 months) was 18.5% [1], the year-over-year change from 2017 to 2018 
#' was 0.8% [2], and the year-over-year change from 2018 to 2019 was -5.6% [3]. 
#' In the first half of 2020, active listings fell much faster thanks to the 
#' COVID-19 pandemic. The year-over-year change in active daily listings for 
#' 2020 so far (January to July) is -25.1% [4].
#' 
#' Despite there being fewer active listings in 2019 than in 2018, the number of 
#' reserved nights increased by 12.1% [5], from 1.7 million [6] reserved nights 
#' to 1.9 million [6] reserved nights, while revenue increased 14.9% [7]. In 
#' fact, with a few brief exceptions, revenue maintained a positive 
#' year-over-year growth rate consistently until the COVID-19 pandemic began. 
#' (Revenue from January to July 2020 is down 55.8% [8] compared to the same 
#' time last year).

#' [1] YOY listing growth, 2016-2017
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date - years(3),
         date <= LTM_end_date - years(2)) %>% 
  group_by(year_2017 = date >= LTM_start_date - years(2)) %>% 
  summarize(n = n() / 365) %>% 
  summarize(change = (n[2] - n[1]) / n[1])

#' [2] YOY listing growth, 2017-2018
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date - years(2),
         date <= LTM_end_date - years(1)) %>% 
  group_by(year_2018 = date >= LTM_start_date - years(1)) %>% 
  summarize(n = n() / 365) %>% 
  summarize(change = (n[2] - n[1]) / n[1])

#' [3] YOY listing growth, 2018-2019
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date - years(1),
         date <= LTM_end_date) %>% 
  group_by(year_2019 = date >= LTM_start_date) %>% 
  summarize(n = n() / 365) %>% 
  summarize(change = (n[2] - n[1]) / n[1])

#' [4] YOY listing growth, 2019-2020
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date,
         date <= LTM_end_date + years(1),
         (date <= "2019-07-31" | date >= LTM_start_date + years(1)),
         date != "2020-02-29") %>%
  group_by(year_2020 = date >= LTM_start_date + years(1)) %>% 
  summarize(n = n() / 181) %>% 
  summarize(change = (n[2] - n[1]) / n[1])

#' [5] YOY reservation change, 2018-2019
daily %>% 
  filter(housing, status == "R", date >= LTM_start_date - years(1),
         date <= LTM_end_date) %>% 
  group_by(year_2019 = date >= LTM_start_date) %>% 
  summarize(n = n()) %>% 
  summarize(change = (n[2] - n[1]) / n[1])

#' [6] Reservation counts, 2018-2019
daily %>% 
  filter(housing, status == "R", date >= LTM_start_date - years(1),
         date <= LTM_end_date) %>% 
  group_by(year_2019 = date >= LTM_start_date) %>% 
  summarize(n = n())

#' [7] YOY revenue change, 2018-2019
daily %>% 
  filter(housing, status == "R", date >= LTM_start_date - years(1),
         date <= LTM_end_date) %>% 
  group_by(year_2019 = date >= LTM_start_date) %>% 
  summarize(revenue = sum(price)) %>% 
  summarize(change = (revenue[2] - revenue[1]) / revenue[1])

#' [8] YOY revenue change, 2019-2020
daily %>% 
  filter(housing, status == "R", date >= LTM_start_date,
         date <= LTM_end_date + years(1),
         (date <= "2019-07-31" | date >= LTM_start_date + years(1)),
         date != "2020-02-29") %>%
  group_by(year_2020 = date >= LTM_start_date + years(1)) %>% 
  summarize(revenue = sum(price)) %>% 
  summarize(change = (revenue[2] - revenue[1]) / revenue[1])


# Montreal in comparison with other major Canadian cities -----------------

#' In 2019, Montreal had the second largest STR market in the country by both 
#' active listing numbers (9,010 [1]) and host revenue ($224.4 million [2]), 
#' falling in both cases behind Toronto (Table 2.2). However, in relative terms 
#' Vancouver stands considerably ahead of both Montreal and Toronto. Vancouver 
#' had the most active listings per 1000 households (12.3 [3] compared to 
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
  filter(city == "Vancouver") %>% 
  select(city, revenue_per_listing)

(sum(revenue_2019$revenue_LTM) /
    daily %>% 
    filter(housing, status != "B", date >= LTM_start_date, 
           date <= LTM_end_date) %>% 
    count(date) %>% 
    summarize(avg_rev_per_active = round(mean(n)))) %>% 
  round(digit = -2) %>% 
  prettyNum(",")

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
  filter(housing, status != "B", date >= LTM_start_date - years(1), 
         date <= LTM_end_date) %>% 
  group_by(date, borough) %>% 
  summarize(n = n(),
            revenue = sum(price[status == "R"])) %>% 
  left_join(st_drop_geometry(boroughs)) %>% 
  group_by(borough, dwellings) %>% 
  summarize(active_listings = mean(n[date >= LTM_start_date]),
            active_2018 = mean(n[date < LTM_start_date]),
            active_growth = (active_listings - active_2018) / active_2018,
            annual_rev = sum(revenue[date >= LTM_start_date]),
            rev_2018 = sum(revenue[date < LTM_start_date]),
            rev_growth = (annual_rev - rev_2018) / rev_2018,
            .groups = "drop") %>% 
  mutate(listings_pct = active_listings / sum(active_listings),
         listings_pct_dwellings = active_listings / dwellings,
         rev_pct = annual_rev / sum(annual_rev)) %>% 
  select(borough, active_listings, active_growth, listings_pct,
         listings_pct_dwellings, annual_rev, rev_pct, rev_growth)

#' STR activity in Montreal is highly concentrated in the central-city boroughs 
#' of Ville-Marie and Le Plateau-Mont-Royal (Table 2.2). These two boroughs 
#' accounted for 32.6% [1] and 25.9% [1] of all listings in 2019 respectively, 
#' and even higher shares of host revenue (41.2% [1] and 29.6% [1]). The borough 
#' with the next highest percentage of average number of daily active listings 
#' is Rosemont-La-Petite-Patrie (8.0% [2]), followed by Le Sud-Ouest (6.9% [2]). 
#' Each accounts for around 6% [2] of annual STR revenue in the city.
#' 
#' Ville-Marie and Le Plateau-Mont-Royal have by far the most STR activity when 
#' measured in per-capita terms. In Ville-Marie, active STR listings account for 
#' 4.8% [1] of all the borough’s housing units, while the equivalent figure for 
#' Le Plateau-Mont-Royal is 3.7% [1] of total dwellings (Figure 2.2).

#' [1] Figures for VM and LPMR
boroughs_breakdown %>% 
  slice(c(7, 18))

#' [2] Figures for RLPP and LSO
boroughs_breakdown %>% 
  slice(c(8, 14))

#' Table 2.2
boroughs_breakdown %>% 
  select(borough, active_listings, active_growth, listings_pct_dwellings,
         annual_rev, rev_growth) %>% 
  set_names(c("Borough",
              "Daily active listings (average)",
              "Active listing year-over-year growth rate",
              "Active listings as % of dwellings",
              "Annual revenue (CAD)",
              "Annual revenue growth")) %>% 
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
  fmt_percent(columns = c(3:4, 6), decimals = 1) %>% 
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
  filter(housing, status != "B", date >= LTM_start_date - years(1), 
         date <= LTM_end_date - years(1)) %>% 
  group_by(listing_type) %>% 
  summarize(active_2018 = n() / 365) %>% 
  left_join(listing_type_breakdown, .) %>% 
  mutate(pct_listing_growth = (active_listings - active_2018) / active_2018) %>% 
  select(-active_2018)

#' The vast majority of STRs in Montreal are entire homes, a category which 
#' includes single-family homes, townhouses, apartments and condominiums. 
#' Nearly half of these (43.6% [1]) were one-bedroom housing units, with the 
#' remainder relatively evenly split between studio apartments (12.6% [1]), 
#' two-bedroom units (27.7% [1]), and three-or-more-bedroom units (16.0% [1]). 
#' In 2019 entire-home listings accounted for 75.6% [2] of all daily active 
#' listings, and 91.1% [2] of total host revenue. Private rooms accounted for 
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
  fmt_number(columns = 2, decimals = 0) %>% 
  fmt_currency(columns = 3, decimals = 0)


# STRs and housing tenure -------------------------------------------------

active_tenure_2017 <- 
  daily %>% 
  filter(housing, date >= "2017-01-01", date <= "2017-12-31", status != "B") %>% 
  left_join(listing_probabilities_2017) %>% 
  group_by(date, borough) %>% 
  summarize(n_listings_2017 = n(),
            n_condo_2017 = as.numeric(sum(condo, na.rm = TRUE)),
            n_renter_2017 = sum(p_renter, na.rm = TRUE)) %>% 
  group_by(borough) %>% 
  summarize(across(where(is.numeric), mean))

active_tenure_2019 <- 
  daily %>% 
  filter(housing, date >= LTM_start_date, 
         date <= LTM_end_date, status != "B") %>% 
  left_join(listing_probabilities_2019) %>% 
  group_by(date, borough) %>% 
  summarize(n_listings_2019 = n(),
            n_condo_2019 = as.numeric(sum(condo, na.rm = TRUE)),
            n_renter_2019 = sum(p_renter, na.rm = TRUE)) %>% 
  group_by(borough) %>% 
  summarize(across(where(is.numeric), mean))

borough_tenure <- 
  DA_probabilities_2019 %>% 
  mutate(across(c(p_condo, p_renter), ~{.x * dwellings})) %>% 
  mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, as.numeric(.x)))) %>% 
  select(p_condo, p_renter, geometry) %>% 
  st_interpolate_aw(boroughs, extensive = TRUE) %>% 
  st_drop_geometry() %>% 
  select(-Group.1) %>% 
  rename(n_condo = p_condo, n_renter = p_renter) %>% 
  cbind(boroughs, .) %>% 
  as_tibble() %>% 
  st_as_sf()

tenure_breakdown <-
  borough_tenure %>% 
  left_join(active_tenure_2017) %>% 
  left_join(active_tenure_2019) %>% 
  relocate(geometry, .after = last_col()) %>%
  transmute(borough, n_condo_2017, n_renter_2017, n_condo_2019, n_renter_2019,
            condo_pct_2017 = n_condo_2017 / n_listings_2017,
            condo_pct_2019 = n_condo_2019 / n_listings_2019,
            renter_pct_2017 = n_renter_2017 / n_listings_2017,
            renter_pct_2019 = n_renter_2019 / n_listings_2019)

#' Of all the listings active during 2019, 11.2% [1] were identified as 
#' condominiums in this way, making condominiums the second most common 
#' property type in Montreal. The overwhelming majority (73.5% [1]) were 
#' identified as “Apartment”, and most of the rest were either “House” 
#' (5.5% [1]) or “Loft” (4.6% [1]).... There are 12 [2] dissemination areas in 
#' Montreal in which condominiums are more than 95% of the housing stock. 
#' These 12 areas contain 114 [3] active STR listings, which by definition must 
#' be nearly entirely condominiums. And yet only 44.7% [4] of these listings are 
#' described as condominiums by their hosts; 39.5% [4] are described as 
#' apartments and 6.1% [4] are described as lofts. In fact, the correlation 
#' between the proportion of condominiums in a dissemination area and the 
#' proportion of listings in the area which self-describe as condominiums is 
#' -0.01 [5]—completely random.

#' [1] Active listing property types
property %>% 
  filter(property_ID %in% active_2019) %>% 
  st_drop_geometry() %>% 
  count(property_type, sort = TRUE) %>% 
  mutate(pct = n / sum(n))

#' [2] High-condo DAs
high_condos <- 
  DA_probabilities_2019 %>% 
  filter(p_condo >= 0.95) %>% 
  pull(GeoUID)

length(high_condos)

#' [3] Listings in high-condo DAs
property %>% 
  filter(property_ID %in% active_2019, GeoUID %in% high_condos) %>% 
  st_drop_geometry() %>% 
  nrow()

#' [4] Property types in high-condo DAs
property %>% 
  filter(property_ID %in% active_2019, GeoUID %in% high_condos) %>% 
  st_drop_geometry() %>% 
  count(property_type, sort = TRUE) %>% 
  mutate(pct = n / sum(n))

#' [5] Correlation between condo % and condo property_type
property %>% 
  filter(property_ID %in% active_2019) %>% 
  st_drop_geometry() %>% 
  count(GeoUID, property_type) %>% 
  group_by(GeoUID) %>% 
  summarize(condo_pct = n[property_type == "Condominium"] / sum(n)) %>% 
  left_join(DA_probabilities_2019, .) %>% 
  st_drop_geometry() %>% 
  summarize(cor = cor(p_condo, condo_pct, use = "complete.obs"))

#' Table 2.4
tenure_breakdown %>% 
  st_drop_geometry() %>% 
  mutate(listings_2017 = n_condo_2017 / condo_pct_2017,
         listings_2019 = n_condo_2019 / condo_pct_2019) %>% 
  summarize(
    borough = "City of Montreal",
    n_condo_2017 = sum(n_condo_2017),
    n_renter_2017 = sum(n_renter_2017),
    n_condo_2019 = sum(n_condo_2019),
    n_renter_2019 = sum(n_renter_2019),
    condo_pct_2017 = n_condo_2017 / sum(listings_2017),
    condo_pct_2019 = n_condo_2019 / sum(listings_2019),
    renter_pct_2017 = n_renter_2017 / sum(listings_2017),
    renter_pct_2019 = n_renter_2019 / sum(listings_2019)) %>% 
  bind_rows(st_drop_geometry(tenure_breakdown)) %>% 
  mutate(`Number of STRs in condos` = round(n_condo_2019, digits = 1),
         `% of STRs in condos (2019)` = condo_pct_2019,
         `% change in % of STRs in condos (2017 to 2019)` =
           (condo_pct_2019 - condo_pct_2017) / condo_pct_2017,
         `Number of STRs in rental units` = round(n_renter_2019, digits = 1),
         `% of STRs in rental units (2019)` = renter_pct_2019,
         `% change in % of STRs in rental units (2017 to 2019)` =
           (renter_pct_2019 - renter_pct_2017) / renter_pct_2017) %>% 
  select(-c(n_condo_2017:renter_pct_2019)) %>% 
  rename(Borough = borough) %>% 
  arrange(desc(`Number of STRs in condos`)) %>%
  slice(1:12) %>% 
  mutate(`Number of STRs in condos` = round(`Number of STRs in condos`, 
                                            digit = -1),
         `Number of STRs in rental units` = 
           round(`Number of STRs in rental units`, digit = -1)) %>%
  gt() %>% 
  tab_header(title = "Tenure breakdown",
             subtitle = "STRs in condominiums and rental units by borough") %>%
  opt_row_striping() %>% 
  fmt_percent(columns = c(3:4, 6:7), decimals = 1) %>% 
  fmt_number(columns = c(2, 5), decimals = 0)


# Revenue distribution ----------------------------------------------------

host_rev <-
  daily %>%
  filter(housing, date >= LTM_start_date, date <= LTM_end_date, 
         status == "R", !is.na(host_ID)) %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price))
  
#' Among all the STR hosts who earned revenue in the City of Montreal last year, 
#' the median revenue was $4,300 [1], while the top host (in this case a network 
#' of numerous host accounts which we discuss below) earned $12.9 million [2] 
#' (Table 2.5). Throughout the City of Montreal, there were 38 hosts [3] that 
#' earned more than $500,000 in 2019. Figure 2.6 shows the percentage of the 
#' total $224.4 million [4] in STR revenue which accrued to each decile of 
#' hosts. The most successful 10% of hosts earned more than two-thirds 
#' (68.8% [5]) of all STR revenue. The revenue concentration is even steeper 
#' among the top 10%: the top 5% earned 58.9% [6] of revenue, while the top 1% 
#' of hosts earned 40.7% [7] of all revenue. 

#' [1] Median host revenue
median(host_rev$rev) %>% round(-2)

#' [2] Top earner
max(host_rev$rev) %>% round(-5)

#' [3] Hosts above $500,000
host_rev %>% 
  filter(rev >= 500000) %>% 
  nrow()

#' [4] Total annual revenue
prettyNum(round(sum(revenue_2019$revenue_LTM), digit = -5), ",")

#' [5] Top 10% revenue
host_rev %>% summarize(top_10 = sum(rev[rev > quantile(rev, .9)]) / sum(rev))
  
#' [6] Top 5% revenue
host_rev %>% summarize(top_10 = sum(rev[rev > quantile(rev, .95)]) / sum(rev))

#' [7] Top 1% revenue
host_rev %>% summarize(top_10 = sum(rev[rev > quantile(rev, .99)]) / sum(rev))

#' Table 2.5
host_rev %>% 
  pull(rev) %>% 
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


# Multilistings -----------------------------------------------------------

commercial_listings <- 
  daily %>% 
  filter(status != "B", date >= "2016-01-01") %>% 
  mutate(commercial = if_else(FREH_3 < 0.5 & !multi, FALSE, TRUE)) %>% 
  count(date, commercial) %>% 
  group_by(commercial) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6)) %>% 
  ungroup()


#' Since 94.7% [1] of entire-home listings have three or fewer bedrooms...

#' [1] Bedrooms
property %>% 
  filter(housing, created <= LTM_end_date, scraped >= LTM_start_date, 
         listing_type == "Entire home/apt", !is.na(bedrooms)) %>% 
  st_drop_geometry() %>% 
  summarize(bedrooms_3_or_fewer = mean(bedrooms <= 3))

#' In 2019, 52.5% [1] of active listings in Montreal were multilistings, earning 
#' 65.7% [2] of total host revenue. Multilistings have been a steadily growing 
#' share of both listings and revenue in Montreal since 2017 (Figure 2.7), and 
#' amidst generally declining STR activity during the COVID-19 pandemic,
#' multilistings briefly earned nearly 3 out of every 4 dollars [3] on STR 
#' platforms in Montreal.

#' [1] 2019 ML listings
daily %>% 
  filter(housing, status != "B", date >= LTM_start_date, 
         date <= LTM_end_date) %>% 
  count(multi) %>% 
  summarize(multi_listings = n[2] / sum(n))

#' [2] 2019 ML revenue
daily %>% 
  filter(housing, status == "R", date >= LTM_start_date, 
       date <= LTM_end_date) %>% 
  group_by(multi) %>% 
  tally(price) %>% 
  summarize(multi_rev = n[2] / sum(n))

#' [3] June 2020 ML revenue
daily %>% 
  filter(housing, status == "R", date >= "2020-06-01", date <= "2020-06-30") %>% 
  group_by(multi) %>% 
  tally(price) %>% 
  summarize(multi_rev = n[2] / sum(n))

#' On January 1, 2017, there were 5,310 [1] non-commercial listings and 
#' 4,190 [1] commercial listings active in Montreal. By January 1, 2020, three 
#' years later, these numbers had flipped; while the total number of active 
#' listings was roughly the same (9,660 in 2020 and 9,500 in 2017 [2]) the 
#' number of commercial listings had increased by more than 50% to 6,720 [1], 
#' while the number of non-commercial listings had nearly halved to 2,950 [1].

#' [1] Commercial and non-commercial listings
commercial_listings %>% 
  filter(date == "2017-01-01" | date == "2020-01-01") %>% 
  mutate(n = round(n, -1))

#' [2] Total listings
commercial_listings %>% 
  filter(date == "2017-01-01" | date == "2020-01-01") %>% 
  group_by(date) %>% 
  summarize(n = sum(n)) %>% 
  mutate(n = round(n, -1))


# Clean up ----------------------------------------------------------------

rm(active_tenure_2017, active_tenure_2019, borough_tenure, boroughs,
   boroughs_breakdown, boroughs_raw, city, commercial_listings, DA,
   DA_probabilities_2017, DA_probabilities_2019, host_rev, 
   listing_probabilities_2017, listing_probabilities_2019, 
   listing_type_breakdown, national_comparison, province, revenue_2019,
   streets, streets_downtown, tenure_breakdown, active_2019, high_condos)
