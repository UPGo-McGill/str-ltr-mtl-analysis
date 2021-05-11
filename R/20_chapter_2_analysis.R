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

qload("output/str_processed.qsm", nthreads = availableCores())
load("output/national_comparison.Rdata")
qload("output/geometry.qsm", nthreads = availableCores())
qload("output/condo_analysis.qsm", nthreads = availableCores())


# Prepare new objects -----------------------------------------------------

# 2019 active
active_2019 <-
  daily %>%
  filter(housing, status %in% c("R", "A"), year(date) == 2019) %>%
  pull(property_ID) %>%
  unique()

# 2020 active
active_2020 <- 
  daily %>%
  filter(housing, status %in% c("R", "A"), year(date) == 2020) %>%
  pull(property_ID) %>% 
  unique()
  
# 2019 revenue
revenue_2019 <-
  daily %>%
  filter(housing, status == "R", year(date) == 2019) %>%
  group_by(property_ID) %>%
  summarize(revenue_LTM = sum(price)) %>%
  inner_join(property, .)

# 2020 revenue
revenue_2020 <-
  daily %>%
  filter(housing, status == "R", year(date) == 2020) %>%
  group_by(property_ID) %>%
  summarize(revenue_LTM = sum(price)) %>%
  inner_join(property, .)


# Active daily listings ---------------------------------------------------

#' In 2019 there was an average of 9,210 [1] active daily listings (Figure 2.1) 
#' operated by an average of 5,300 [2] hosts. These hosts collectively earned 
#' $231.0 million [3] in 2019—an average of $25,100 [4] per daily active 
#' listing or $43,600 [5] per active host. As a result of the COVID-19 pandemic, 
#' in 2020 active daily listings decreased by 33.3% [6] and revenue by 66.3% [7]. 
#' The 6,130 [1] listings still active in 2020 were operated by an average of 
#' 3,470 [2] hosts, for an average revenue of $12,700 [8] per active listing or 
#' $22,500 [9] per active host. Montreal STR host revenue in 2020 totalled 
#' $77.8 [10] million.

#' There was also a daily average of 12,750 [1] listings in 2020 which were 
#' visible on the Airbnb and Vrbo websites but were blocked by the host from 
#' receiving reservations, a slight increase from the 2019 average of 
#' 12,360 [1]. The presence of these listings can erroneously suggest that a 
#' city’s STR market is larger than it is; in the case of Montreal, blocked 
#' listings outnumber listings which are actually active. When these blocked but 
#' inactive listings are included, the average listing earned $2,700 [11] last 
#' year, and the average host earned $4,600 [12]. Finally, there was a daily 
#' average of 460 [13] listings that were not located in private housing units 
#' (B&Bs, hotels, etc.), which have been excluded from the analysis in this 
#' report. 

#' [1] Average active and blocked daily listings in 2019 and 2020
daily %>% 
  filter(housing, year(date) >= 2019) %>% 
  count(date, B = status == "B", Y2019 = year(date) == 2019) %>% 
  group_by(B, Y2019) %>% 
  summarize(listings = scales::comma(mean(n), 10), .groups = "drop")

#' [2] Average number of hosts (taking out blocked 365 days)
daily %>% 
  filter(housing, status != "B", year(date) >= 2019) %>%
  count(date, host_ID, Y2019 = year(date) == 2019) %>% 
  count(date, Y2019) %>% 
  group_by(Y2019) %>% 
  summarize(hosts = scales::comma(mean(n), 10))

#' [3] Total annual revenue, 2019
scales::dollar(sum(revenue_2019$revenue_LTM), 100000)

#' [4] Average revenue per active listing, 2019
(sum(revenue_2019$revenue_LTM) /
    daily %>% 
    filter(housing, status != "B", year(date) == 2019) %>% 
    count(date) %>% 
    summarize(avg_rev_per_active = round(mean(n)))) %>% 
  pull(avg_rev_per_active) %>% 
  scales::dollar(100)

#' [5] Average revenue per active host, 2019
(sum(revenue_2019$revenue_LTM) /
    daily %>% 
    filter(housing, status != "B", year(date) == 2019) %>%
    group_by(date) %>% 
    summarize(n_hosts = length(unique(host_ID))) %>% 
    summarize(avg_n_hosts = round(mean(n_hosts)))) %>% 
  pull(avg_n_hosts) %>% 
  scales::dollar(100)

#' [6] Listing decline
daily %>% 
  filter(housing, status != "B", year(date) >= 2019) %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarize(n = n() / 365) %>% 
  mutate(change = slider::slide_dbl(n, ~{(.x[2] - .x[1]) / .x[1]}, 
                                    .before = 1)) %>% 
  mutate(change = scales::percent(change, 0.1)) %>% 
  slice(2) %>% 
  pull(change)

# [7] Revenue decline
{(sum(revenue_2019$revenue_LTM) - sum(revenue_2020$revenue_LTM)) / 
    sum(revenue_2019$revenue_LTM)} %>% 
  scales::percent(0.1)

#' [8] Average revenue per active listing, 2020
(sum(revenue_2020$revenue_LTM) /
    daily %>% 
    filter(housing, status != "B", year(date) == 2020) %>% 
    count(date) %>% 
    summarize(avg_rev_per_active = round(mean(n)))) %>% 
  pull(avg_rev_per_active) %>% 
  scales::dollar(100)

#' [9] Average revenue per active host, 2020
(sum(revenue_2020$revenue_LTM) /
    daily %>% 
    filter(housing, status != "B", year(date) == 2020) %>%
    group_by(date) %>% 
    summarize(n_hosts = length(unique(host_ID))) %>% 
    summarize(avg_n_hosts = round(mean(n_hosts)))) %>% 
  pull(avg_n_hosts) %>% 
  scales::dollar(100)

#' [10] Total annual revenue, 2020
scales::dollar(sum(revenue_2020$revenue_LTM), 100000)

#' [11] Average revenue per all listings
{sum(revenue_2020$revenue_LTM) / {
  property %>% 
    filter(year(scraped) == 2020 | 
             (year(created) <= 2020 & year(scraped) == 2021)) %>% 
    nrow()
  }} %>% 
  scales::dollar(100)

#' [12] Average revenue per all hosts
{sum(revenue_2020$revenue_LTM) / {
  property %>% 
    st_drop_geometry() %>% 
    filter(year(scraped) == 2020 | 
             (year(created) <= 2020 & year(scraped) == 2021)) %>% 
    filter(!is.na(host_ID)) %>% 
    distinct(host_ID) %>% 
    nrow()
}} %>% 
  scales::dollar(100)

#' [13] Non-housing active listings
daily %>% 
  filter(!housing, status != "B", year(date) == 2020) %>%
  count(date) %>% 
  summarize(non_housing = mean(n)) %>% 
  pull(non_housing) %>% 
  scales::comma(10)

#' Active daily listings peaked in August 2018 [14] at 12,290 [14], and began to 
#' decline well before the COVID-19 pandemic sent Montreal’s STR market into 
#' collapse.

#' [14] Date and amount of highest activity
daily %>% 
  filter(housing, status != "B") %>% 
  count(date) %>% 
  filter(n == max(n)) %>% 
  group_by(date) %>% 
  summarize(daily_max = scales::comma(n, 10)) %>% 
  pull(daily_max)


# STR growth rates --------------------------------------------------------

#' Overall, the year-over-year change in average active listings from 2016 to 
#' 2017 (12 months) was 21.8% [1], the year-over-year change from 2017 to 2018 
#' was 3.0% [1], and the year-over-year change from 2018 to 2019 was -8.1% [1]. 
#' The year-over-year change for 2020, meanwhile, was 33.3% [1]: an 
#' unprecedented drop. The 2020 drop in revenue—66.3% [2]—is even more dramatic,
#' and stands in contrast to a previous trend of rising revenue and reservations 
#' per listing. Despite there having been fewer active listings in 2019 than in 
#' 2018, the number of reserved nights increased by 9.3% [3], from 
#' 1.76 million [3] reserved nights to 1.93 million [3] reserved nights, while 
#' revenue increased 10.2% [4]. From 2019 to 2020, the number of reserved nights 
#' dropped 57.9% [3] to 810,000. In general, with a few brief exceptions, 
#' despite stagnating or falling listing numbers, revenue maintained a positive 
#' year-over-year growth rate consistently until the COVID-19 pandemic began.

#' [1] YOY listing growth
daily %>% 
  filter(housing, status != "B", year(date) >= 2016) %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarize(n = n() / 365) %>% 
  mutate(change = slider::slide_dbl(n, ~{(.x[2] - .x[1]) / .x[1]}, 
                                    .before = 1)) %>% 
  mutate(change = scales::percent(change, 0.1))

# [2] Revenue decline
{(sum(revenue_2019$revenue_LTM) - sum(revenue_2020$revenue_LTM)) / 
    sum(revenue_2019$revenue_LTM)} %>% 
  scales::percent(0.1)

#' [3] YOY reservation counts and change
daily %>% 
  filter(housing, status == "R", year(date) %in% 2018:2020) %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarize(n = n()) %>% 
  mutate(change = slider::slide_dbl(n, ~{(.x[2] - .x[1]) / .x[1]}, 
                                    .before = 1)) %>% 
  mutate(n = scales::comma(n, 10000),
         change = scales::percent(change, 0.1))

#' [4] YOY revenue change, 2018-2019
daily %>% 
  filter(housing, status == "R", year(date) %in% 2018:2019) %>% 
  group_by(year_2019 = year(date) == 2019) %>% 
  summarize(revenue = sum(price)) %>% 
  summarize(change = (revenue[2] - revenue[1]) / revenue[1]) %>% 
  pull(change) %>% 
  scales::percent(0.1)


# Montreal in comparison with other major Canadian cities -----------------

#' In 2020, Montreal had the second largest STR market in the country by both 
#' active listings (6,130 [1]) and host revenue ($77.8 million [2]), falling in 
#' both cases behind Toronto (Table 2.1). However, in relative terms Vancouver 
#' stands considerably ahead of both Montreal and Toronto. Vancouver had the 
#' most active listings per 1000 households (8.7 compared to 7.9 in Montreal) 
#' and the most revenue per listing ($22,800 compared to $12,700 in Montreal) 
#' in 2020. All major STR markets in Canada saw sharp declines in STR activity 
#' compared to 2019. 

#' [1] Daily active listings
daily %>% 
  filter(housing, status != "B", year(date) == 2020) %>% 
  nrow() %>% 
  {. / 366} %>% 
  scales::comma(10)

#' [2] Annual host revenue
scales::dollar(sum(revenue_2020$revenue_LTM), 100000)

national_comparison %>% 
  # slice(6) %>% 
  mutate(x = active_daily_listings / listings_per_1000)

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
  filter(housing, status != "B", year(date) >= 2019) %>% 
  group_by(date, borough) %>% 
  summarize(n = n(), revenue = sum(price[status == "R"])) %>% 
  left_join(st_drop_geometry(boroughs)) %>% 
  group_by(borough, dwellings) %>% 
  summarize(active_listings = mean(n[date >= LTM_start_date]),
            active_2019 = mean(n[date < LTM_start_date]),
            active_growth = (active_listings - active_2019) / active_2019,
            annual_rev = sum(revenue[date >= LTM_start_date]),
            rev_2019 = sum(revenue[date < LTM_start_date]),
            rev_growth = (annual_rev - rev_2019) / rev_2019,
            .groups = "drop") %>% 
  mutate(listings_pct = active_listings / sum(active_listings),
         listings_pct_dw = active_listings / dwellings,
         listings_pct_dw_2019 = active_2019 / dwellings,
         rev_pct = annual_rev / sum(annual_rev)) %>% 
  select(borough, active_listings, active_growth, listings_pct,
         listings_pct_dw, listings_pct_dw_2019, annual_rev, rev_pct, rev_growth)

#' STR activity in Montreal is highly concentrated in the central-city boroughs 
#' of Ville-Marie and Le Plateau-Mont-Royal (Table 2.2). These two boroughs 
#' accounted for 33.3% [1] and 23.7% [1] of all listings in 2020 respectively, 
#' and even higher shares of host revenue (39.3% [1] and 27.3% [1]). The borough 
#' with the next highest percentage of average number of daily active listings 
#' is Côte-des-Neiges-Notre-Dame-de-Grâce (8.0% [2]), followed by 
#' Rosemont-La-Petite-Patrie (7.6% [2]). Each accounts for around 6% [2] of 
#' annual STR revenue in the city.
#' 
#' Ville-Marie and Le Plateau-Mont-Royal have by far the most STR activity when 
#' measured in per-capita terms. In 2020, active STR listings accounted for 
#' 3.3% [1] of all of Ville-Marie’s housing units, while the equivalent figure 
#' for Le Plateau-Mont-Royal was 2.4% [1] (Figure 2.3). The percentage of active 
#' listings as a share of dwellings has declined significantly since 2019, when 
#' 4.9% and 3.7% [1] of all housing units were active STRs in Ville-Marie and Le 
#' Plateau Mont-Royal.

#' [1] Figures for VM and LPMR
boroughs_breakdown %>% 
  slice(c(7, 18))

#' [2] Figures for RLPP and LSO
boroughs_breakdown %>% 
  slice(c(3, 14))

#' Table 2.2
boroughs_breakdown %>% 
  select(borough, active_listings, active_growth, listings_pct_dw,
         annual_rev, rev_growth) %>% 
  set_names(c("Borough",
              "Daily active listings (average)",
              "Active listing year-over-year growth rate",
              "Active listings as % of dwellings",
              "Annual revenue (CAD)",
              "Annual revenue growth")) %>% 
  filter(`Daily active listings (average)` > 75) %>% 
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
    subtitle = "Boroughs with at least 75 daily active listings, 2020"
  ) %>%
  opt_row_striping() %>% 
  fmt_percent(columns = c(3:4, 6), decimals = 1) %>% 
  fmt_number(columns = 2,
             decimals = 0)

# Need to add Montreal row, with % listings per dwelling
daily %>% 
  filter(housing, status != "B", year(date) == 2020) %>% 
  count(date) %>% 
  summarize(active_listings = round(mean(n), digit = -1)) %>% 
  pull(active_listings) %>% 
  {. / sum(boroughs$dwellings)} %>% 
  scales::percent(0.1)


# Listing types and sizes -------------------------------------------------

listing_type_breakdown <- 
  daily %>% 
  filter(housing, status != "B", year(date) == 2020) %>% 
  group_by(listing_type) %>% 
  summarize(
    active_listings = n() / 365,
    revenue = sum(price[status == "R"]),
    .groups = "drop") %>% 
  mutate(pct_of_listings = active_listings / sum(active_listings),
         pct_of_revenue = revenue / sum(revenue))

listing_type_breakdown <- 
  daily %>% 
  filter(housing, status != "B", year(date) == 2019) %>% 
  group_by(listing_type) %>% 
  summarize(active_2019 = n() / 365) %>% 
  left_join(listing_type_breakdown, .) %>% 
  mutate(pct_listing_growth = (active_listings - active_2019) / active_2019) %>% 
  select(-active_2019)

#' The vast majority of STRs in Montreal are entire homes, a category which 
#' includes single-family homes, townhouses, apartments and condominiums. 
#' Nearly half of these (43.8% [1]) were one-bedroom housing units, with the 
#' remainder relatively evenly split between studio apartments (14.1% [1]), 
#' two-bedroom units (26.2% [1]), and three-or-more-bedroom units (15.9% [1]). 

#' In 2020 entire-home listings accounted for 75.2% [2] of all daily active 
#' listings, and 90.9% [2] of total host revenue. Private rooms accounted for 
#' nearly all of the remainder.

#' [1] Bedroom counts
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% active_2020, listing_type == "Entire home/apt") %>% 
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
         `% average daily listing growth (YOY 2019-2020)` = pct_listing_growth
         ) %>% 
  gt() %>% 
  tab_header(title = "Listing type breakdown", subtitle = "2020") %>%
  opt_row_striping() %>% 
  fmt_percent(columns = 4:6, decimals = 1) %>% 
  fmt_number(columns = 2, decimals = 0) %>% 
  fmt_currency(columns = 3, decimals = 0)


# STRs and housing tenure -------------------------------------------------

active_tenure_2017 <- 
  daily %>% 
  filter(housing, year(date) == 2017, status != "B") %>% 
  left_join(listing_probabilities_2017) %>% 
  group_by(date, borough) %>% 
  summarize(n_listings_2017 = n(),
            n_condo_2017 = as.numeric(sum(condo, na.rm = TRUE)),
            n_renter_2017 = sum(p_renter, na.rm = TRUE)) %>% 
  group_by(borough) %>% 
  summarize(across(where(is.numeric), mean))

active_tenure_2019 <- 
  daily %>% 
  filter(housing, year(date) == 2019, status != "B") %>% 
  inner_join(listing_probabilities_2019) %>% 
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

#' Of all the listings active during 2020, 12.5% [1] were identified as 
#' condominiums in this way, making condominiums the second most common 
#' property type in Montreal. The overwhelming majority (70.1% [1]) were 
#' identified as “Apartment”, and most of the rest were either “House” 
#' (6.4% [1]) or “Loft” (4.8% [1]).... There are 12 [2] dissemination areas in 
#' Montreal in which condominiums are more than 95% of the housing stock. 
#' These 12 areas contain 92 [3] active STR listings, which by definition must 
#' be nearly entirely condominiums. And yet only 57.6% [4] of these listings are 
#' described as condominiums by their hosts; 27.2% [4] are described as 
#' apartments and 6.5% [4] are described as lofts. In fact, the correlation 
#' between the proportion of condominiums in a dissemination area and the 
#' proportion of listings in the area which self-describe as condominiums is 
#' 0.01 [5]—completely random.

#' [1] Active listing property types
property %>% 
  filter(property_ID %in% active_2020) %>% 
  st_drop_geometry() %>% 
  count(property_type, sort = TRUE) %>% 
  mutate(pct = n / sum(n),
         pct = scales::percent(pct, 0.1))

#' [2] High-condo DAs
high_condos <- 
  DA_probabilities_2019 %>% 
  filter(p_condo >= 0.95) %>% 
  pull(GeoUID)

length(high_condos)

#' [3] Listings in high-condo DAs
property %>% 
  filter(property_ID %in% active_2020, GeoUID %in% high_condos) %>% 
  st_drop_geometry() %>% 
  nrow()

#' [4] Property types in high-condo DAs
property %>% 
  filter(property_ID %in% active_2020, GeoUID %in% high_condos) %>% 
  st_drop_geometry() %>% 
  count(property_type, sort = TRUE) %>% 
  mutate(pct = n / sum(n),
         pct = scales::percent(pct, 0.1))

#' [5] Correlation between condo % and condo property_type
property %>% 
  filter(property_ID %in% active_2020) %>% 
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
  filter(!is.na(listings_2019)) %>% 
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
  filter(housing, year(date) == 2020, status == "R", !is.na(host_ID)) %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price))
  
#' We identified one particular network of hosts whose 500 [1] listings earned 
#' approximately $5.2 million [2] in 2020.

#' [1] Max host listings
host_rev %>% 
  filter(rev == max(rev)) %>% 
  pull(host_ID) %>% 
  {filter(property, host_ID %in% .)} %>% 
  filter(housing) %>% 
  filter(year(scraped) == 2020 | 
           (year(scraped) == 2021 & year(created) < 2021)) %>% 
  nrow() %>% 
  scales::comma(10)

#' [2] Max host rev
max(host_rev$rev) %>% scales::dollar(100000)

#' Among all the STR hosts who earned revenue in the City of Montreal last year, 
#' the median revenue was $2,600 [1], while the top host (in this case a network 
#' of numerous host accounts which we discuss below) earned $5.2 million [2] 
#' (Table 2.5). Throughout the City of Montreal, there were 14 hosts [3] that 
#' earned more than $500,000 in 2020 (a steep decline from 40 [4] in 2019). 
#' Figure 2.6 shows the percentage of the total $77.8 million [5] in STR revenue 
#' which accrued to each decile of hosts. The most successful 10% of hosts 
#' earned more than two-thirds (68.0% [6]) of all STR revenue. The revenue 
#' concentration is even steeper among the top 10%: the top 5% earned 57.9% [7] 
#' of revenue, while the top 1% of hosts earned 39.3% [8] of all revenue. 

#' [1] Median host revenue
median(host_rev$rev) %>% scales::dollar(100)

#' [2] Top earner
max(host_rev$rev) %>% scales::dollar(100000)

#' [3] Hosts above $500,000
host_rev %>% 
  filter(rev >= 500000) %>% 
  nrow()

#' [4] Hosts above $500,000 in 2019
daily %>% 
  filter(housing, status == "R", year(date) == 2019) %>% 
  group_by(host_ID) %>% 
  summarize(rev = sum(price)) %>% 
  filter(rev > 500000) %>% 
  nrow()

#' [5] Total annual revenue
revenue_2020$revenue_LTM %>% 
  sum() %>% 
  scales::dollar(100000)

#' [6] Top 10% revenue
host_rev %>% 
  summarize(top_10 = sum(rev[rev > quantile(rev, .9)]) / sum(rev)) %>% 
  pull(top_10) %>% 
  scales::percent(0.1)
  
#' [7] Top 5% revenue
host_rev %>% 
  summarize(top_5 = sum(rev[rev > quantile(rev, .95)]) / sum(rev)) %>% 
  pull(top_5) %>% 
  scales::percent(0.1)

#' [8] Top 1% revenue
host_rev %>% 
  summarize(top_1 = sum(rev[rev > quantile(rev, .99)]) / sum(rev)) %>% 
  pull(top_1) %>% 
  scales::percent(0.1)

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
  tab_header(title = "Host income") %>%
  opt_row_striping() 


# Multilistings -----------------------------------------------------------

commercial_listings <- 
  daily %>% 
  filter(housing, status != "B", date >= "2016-01-01") %>% 
  mutate(commercial = if_else(FREH_3 < 0.5 & !multi, FALSE, TRUE)) %>% 
  count(date, commercial) %>% 
  group_by(commercial) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6)) %>% 
  ungroup()

#' Since 95.0% [1] of entire-home listings have three or fewer bedrooms...

#' [1] Bedrooms
property %>% 
  filter(housing, created <= LTM_end_date, scraped >= LTM_start_date, 
         listing_type == "Entire home/apt", !is.na(bedrooms)) %>% 
  st_drop_geometry() %>% 
  summarize(bed_3_or_fewer = mean(bedrooms <= 3)) %>% 
  pull(bed_3_or_fewer) %>% 
  scales::percent(0.1)

#' In 2020, 39.8% [1] of active listings in Montreal were multilistings, earning 
#' 45.9% [2] of total host revenue. Multilistings have been a steadily growing 
#' share of both listings and revenue in Montreal since 2017 (Figure 2.7), and 
#' amidst generally declining STR activity during the COVID-19 pandemic,
#' multilistings now earn a majority of all revenue [3] on STR platforms in 
#' Montreal.

#' [1] 2020 ML listings
daily %>% 
  filter(housing, status != "B", year(date) == 2020) %>% 
  count(multi) %>% 
  summarize(multi_listings = n[2] / sum(n)) %>% 
  pull(multi_listings) %>% 
  scales::percent(0.1)

#' [2] 2020 ML revenue
daily %>% 
  filter(housing, status == "R", year(date) == 2020) %>% 
  group_by(multi) %>% 
  tally(price) %>% 
  summarize(multi_rev = n[2] / sum(n)) %>% 
  pull(multi_rev) %>% 
  scales::percent(0.1)

#' [3] Dec 2020 ML revenue
daily %>% 
  filter(housing, status == "R", year(date) == 2020, month(date) == 12) %>% 
  group_by(multi) %>% 
  tally(price) %>% 
  summarize(multi_rev = n[2] / sum(n)) %>% 
  pull(multi_rev) %>% 
  scales::percent(0.1)

#' On January 1, 2017, there were 5,760 [1] non-commercial listings and 
#' 3,620 [1] commercial listings active in Montreal. By January 1, 2020, three 
#' years later, these numbers had flipped; while the total number of active 
#' listings was roughly the same (9,620 in 2020 and 9,390 in 2017 [2]) the 
#' number of commercial listings had increased by two thirds to 5,980 [1], 
#' while the number of non-commercial listings had decreased by the same
#' proportion to 3,640 [1].

#' [1] Commercial and non-commercial listings
commercial_listings %>% 
  filter(date == "2017-01-01" | date == "2020-01-01") %>% 
  mutate(n = scales::comma(n, 10))

#' [2] Total listings
commercial_listings %>% 
  filter(date == "2017-01-01" | date == "2020-01-01") %>% 
  group_by(date) %>% 
  summarize(n = sum(n)) %>% 
  mutate(n = scales::comma(n, 10))


# Clean up ----------------------------------------------------------------

rm(active_tenure_2017, active_tenure_2019, borough_tenure, boroughs,
   boroughs_breakdown, boroughs_raw, city, commercial_listings, DA,
   DA_probabilities_2017, DA_probabilities_2019, host_rev, 
   listing_probabilities_2017, listing_probabilities_2019, 
   listing_type_breakdown, national_comparison, province, revenue_2019,
   streets, streets_downtown, tenure_breakdown, active_2019, high_condos)
