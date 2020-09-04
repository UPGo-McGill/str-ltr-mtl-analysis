#### 40 CHAPTER 4 ANALYSIS #####################################################

#' This script produces the facts for chapter 4. It runs quickly.
#' 
#' Output:
#' - None
#' 
#' Script dependencies:
#' - `09_str_processing.R`
#' - `11_FREH_model.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")
library(feasts)
library(fabletools)

load("output/str_processed.Rdata")


# Prepare objects ---------------------------------------------------------

active_by_status <- 
  daily %>% 
  filter(housing, date >= "2016-01-01", status != "B") %>% 
  count(date, status)

# Create and decompose reservations time series
reservations <- 
  active_by_status %>% 
  filter(status == "R") %>% 
  tsibble::as_tsibble() %>% 
  tsibble::index_by(yearmon = tsibble::yearmonth(date)) %>% 
  summarize(n = sum(n)) %>% 
  filter(yearmon <= tsibble::yearmonth("2020-02")) %>% 
  model(x11 = feasts:::X11(n, type = "additive")) %>% 
  components()

# Get March-August seasonal
mar_aug_seasonal <- 
  reservations %>% 
  slice(39:44) %>% 
  pull(seasonal)

# Get Feb trend
feb_trend <- 
  reservations %>% 
  slice(50) %>% 
  pull(trend)

# Apply March-Aug seasonal component to Feb trend
trends <-
  tibble(
    date = as.Date(c("2020-03-16", "2020-04-16", "2020-05-16", "2020-06-16",
                     "2020-07-16", "2020-07-31")),
    trend = (feb_trend + mar_aug_seasonal) / c(31, 30, 31, 30, 31, 31))

# Set July 31 value to average of July and August
trends[6,]$trend <- mean(trends[5:6,]$trend)

reservations <- 
  active_by_status %>% 
  filter(status == "R") %>% 
  mutate(n = slide_dbl(n, mean, .before = 6)) %>% 
  filter(date >= "2019-01-01") %>% 
  left_join(trends) %>% 
  select(-status) %>% 
  mutate(trend = if_else(date == "2020-03-01", n, trend)) %>% 
  filter(date >= "2020-03-01") %>% 
  mutate(trend = zoo::na.approx(trend))

reservations <- 
  active_by_status %>% 
  filter(status == "R") %>% 
  mutate(n = slide_dbl(n, mean, .before = 6)) %>% 
  filter(date >= "2019-01-01", date <= "2020-02-29") %>% 
  select(-status) %>% 
  bind_rows(reservations) 

# Get average nightly prices
average_prices <- 
  daily %>% 
  filter(housing, status == "R", date >= "2016-01-01",
         listing_type == "Entire home/apt") %>% 
  group_by(date) %>% 
  summarize(price = mean(price))

# Create monthly price time series
monthly_prices <- 
  average_prices %>% 
  tsibble::as_tsibble() %>% 
  tsibble::index_by(yearmon = tsibble::yearmonth(date)) %>% 
  summarize(price = mean(price))

# Get March-August seasonal
mar_jul_price_seasonal <- 
  monthly_prices %>% 
  filter(yearmon <= tsibble::yearmonth("2020-02")) %>% 
  model(x11 = feasts:::X11(price, type = "additive")) %>% 
  components() %>%
  slice(39:43) %>% 
  pull(seasonal)

# Get Feb trend
feb_price_trend <- 
  monthly_prices %>% 
  filter(yearmon <= tsibble::yearmonth("2020-02")) %>% 
  model(x11 = feasts:::X11(price, type = "additive")) %>% 
  components() %>% 
  slice(50) %>% 
  pull(trend)

# Apply March-Aug seasonal component to Feb trend
mar_jul_price_trend <- 
  tibble(yearmon = tsibble::yearmonth(c("2020-03", "2020-04", "2020-05", 
                                        "2020-06", "2020-07")),
         trend = feb_price_trend + mar_jul_price_seasonal)

# Apply to daily averages to get trend
average_prices <- 
  average_prices %>% 
  mutate(yearmon = tsibble::yearmonth(date)) %>% 
  inner_join(mar_jul_price_trend) %>% 
  group_by(yearmon) %>% 
  mutate(trend = price * trend / mean(price)) %>% 
  ungroup() %>% 
  select(date, trend) %>% 
  left_join(average_prices, .)


# Reservations and prices collapsed during COVID-19 -----------------------

#' There were 12.1% [1] more nights reserved in Montreal STRs in 2019 than 
#' there were in 2018—peaking at nearly 9,500 [2] nightly STR reservations in 
#' the summer of 2019.... In March 2020, however, when reserved nights should 
#' have been steadily increasing en route to the summer peak, they instead 
#' collapsed in the face of COVID-19. While total reserved nights from January 
#' to February 2020 increased at a rapid 45.3% [3] compared to 2019, reserved 
#' nights from March to August 2020 decreased 70.7% [4] compared to the previous 
#' year.

#' [1] YOY growth in reservations, 2018-2019
daily %>% 
  filter(housing, status == "R", date >= LTM_start_date - years(1), 
         date <= LTM_end_date) %>% 
  count(date_2019 = date >= LTM_start_date) %>% 
  summarize((n[2] - n[1]) / n[1])
  
#' [2] Peak 2019 nightly reservations
daily %>% 
  filter(housing, status == "R", date >= LTM_start_date) %>% 
  count(date, sort = TRUE) %>% 
  slice(1) %>% 
  pull(n) %>% 
  round(-2)

#' [3] YOY reservation growth, Jan-Feb 2019-2020
daily %>% 
  filter(housing, status == "R", date >= LTM_start_date, 
         substr(date, 6, 7) %in% c("01", "02")) %>% 
  count(date_2020 = date >= LTM_start_date + years(1)) %>% 
  summarize((n[2] - n[1]) / n[1])

#' [4] YOY reservation growth, Mar-Aug 2019-2020
daily %>% 
  filter(housing, status == "R", date >= LTM_start_date, 
         substr(date, 6, 7) %in% c("03", "04", "05", "06", "07", "08")) %>% 
  count(date_2020 = date >= LTM_start_date + years(1)) %>% 
  summarize((n[2] - n[1]) / n[1])

#' On July 31, 2020, fewer than 1,900 [1] STRs were reserved in Montreal. But 
#' the trajectory of STR activity established prior to the pandemic, combined 
#' with the fact that bookings normally increase rapidly through the spring and 
#' summer, suggests that, in the absence of the pandemic, Montreal would have 
#' been expected to receive 8,700 [1] reservations instead. The COVID-19 
#' pandemic, therefore, depressed STR activity by 78.3% [1], or 6,810 
#' [1] reservations, on that date. In total, from March through July 2020, we 
#' estimate that there have been 680,600 [2] fewer STR nights reserved than 
#' would normally have been expected to occur. The 317,500 [2] total nights 
#' reserved in this time period is only 31.8% [2] of the 1.0 million [2] total 
#' that would represent the previous growth trend.

#' [1] Actual and expected reservations on 2020-07-31
reservations %>% 
  filter(date == "2020-07-31") %>% 
  mutate(dif = trend - n, pct_change = 1 - n / trend)

#' [2] Total actual and expected reservations, Mar-Aug 2020
reservations %>% 
  filter(date >= "2020-03-01") %>% 
  summarize(across(c(n, trend), sum)) %>% 
  mutate(dif = trend - n, pct = n / trend)
  
#' Throughout the May-August period of 2020, nightly prices have been an average 
#' of 22.2% [1] lower than expected. Spread across the 320,700 nights reserved 
#' during this period, this means that STR operators collectively earned 
#' $9.0 million [2] less than they would have on their bookings in the absence 
#' of the pandemic.

#' [1] Average price difference
average_prices %>% 
  filter(date >= "2020-03-01") %>% 
  summarize(1 - sum(price) / sum(trend))

#' [2] Total revenue difference
average_prices %>%
  rename(price_trend = trend) %>% 
  filter(date >= "2020-03-01") %>% 
  left_join(reservations) %>% 
  mutate(rev_dif = n * (price_trend - price)) %>% 
  tally(rev_dif)

#' When the lower prices on reservations which did occur is combined with the 
#' reservations which did not occur, our estimate is that Montreal’s STR hosts 
#' lost a total of $106.0 million [1] in revenue between March and August 2020 
#' because of the COVID-19 pandemic.

#' [1] Total lost revenue
average_prices %>%
  rename(price_trend = trend) %>% 
  filter(date >= "2020-03-01") %>% 
  left_join(reservations) %>% 
  mutate(total_rev_dif = n * (price_trend - price) + 
           (trend - n) * price_trend) %>% 
  tally(total_rev_dif)


# Compliance with the Provincial STR ban ----------------------------------

#' Between March 29 and June 25, there were a total of 34,980 [1] reservations 
#' in the City, compared to 129,020 [2] for the same period in 2019 (a decrease 
#' of 72.9% [3]). Only 380 [4] of these reservations were for longer than 30 
#' days, which means that the remaining 34,600 [5] reservations in Montreal were 
#' illegal. Additionally, these illegal reservations were distributed widely 
#' among Montreal’s STR hosts. There were 6,120 [6] hosts with active listings 
#' in the March 29 - June 25 period, and 4,360 [7] of these hosts received at 
#' least one reservation. 4,330 [8] hosts received reservations of 30 days or 
#' fewer, which means that 70.7% [9] of Montreal’s active STR hosts appear to 
#' have violated the Provincial order.

#' [1] Total reservations 2020: add unique res_ID count to compressed no-ID rows
daily %>% 
  filter(housing, status == "R", date >= "2020-03-29", date <= "2020-06-25") %>% 
  filter(is.na(res_ID)) %>% 
  select(1:12) %>% 
  strr_compress(quiet = TRUE) %>% 
  nrow() %>% 
  `+`(daily %>% 
        filter(housing, status == "R", date >= "2020-03-29", 
               date <= "2020-06-25", !is.na(res_ID)) %>% 
        count(res_ID) %>% 
        nrow()) %>% 
  round(-1)

#' [2] Total reservations 2019
daily %>% 
  filter(housing, status == "R", date >= "2019-03-29", date <= "2019-06-25") %>% 
  filter(is.na(res_ID)) %>% 
  select(1:12) %>% 
  strr_compress(quiet = TRUE) %>% 
  nrow() %>% 
  `+`(daily %>% 
        filter(housing, status == "R", date >= "2019-03-29", 
               date <= "2019-06-25", !is.na(res_ID)) %>% 
        count(res_ID) %>% 
        nrow()) %>% 
  round(-1)

#' [3] Change in reservations
{1 - {daily %>% 
    filter(housing, status == "R", date >= "2020-03-29", 
           date <= "2020-06-25") %>% 
    filter(is.na(res_ID)) %>% 
    select(1:12) %>% 
    strr_compress(quiet = TRUE) %>% 
    nrow() %>% 
    `+`(daily %>% 
          filter(housing, status == "R", date >= "2020-03-29", 
                 date <= "2020-06-25", !is.na(res_ID)) %>% 
          count(res_ID) %>% 
          nrow())} / 
  {daily %>% 
      filter(housing, status == "R", date >= "2019-03-29", 
             date <= "2019-06-25") %>% 
      filter(is.na(res_ID)) %>% 
      select(1:12) %>% 
      strr_compress(quiet = TRUE) %>% 
      nrow() %>% 
      `+`(daily %>% 
            filter(housing, status == "R", date >= "2019-03-29", 
                   date <= "2019-06-25", !is.na(res_ID)) %>% 
            count(res_ID) %>% 
            nrow())
  }} %>% 
  round(3)

#' [4] Long reservations
daily %>% 
  filter(housing, status == "R", date >= "2020-03-01") %>% 
  count(res_ID, sort = TRUE) %>% 
  filter(n >= 31, !is.na(res_ID)) %>% 
  nrow() %>% 
  round(-1)

#' [5] Short reservations
daily %>% 
  filter(housing, status == "R", date >= "2020-03-29", date <= "2020-06-25") %>% 
  filter(is.na(res_ID)) %>% 
  select(1:12) %>% 
  strr_compress() %>% 
  nrow() %>% 
  `+`(daily %>% 
        filter(housing, status == "R", date >= "2020-03-29", 
               date <= "2020-06-25", !is.na(res_ID)) %>% 
        count(res_ID) %>% 
        nrow()) %>% 
  `-`(daily %>% 
        filter(housing, status == "R", date >= "2020-03-01") %>% 
        count(res_ID, sort = TRUE) %>% 
        filter(n >= 31, !is.na(res_ID)) %>% 
        nrow()) %>% 
  round(-1)

#' [6] All active hosts
daily %>% 
  filter(housing, status %in% c("R", "A"), date >= "2020-03-29", 
         date <= "2020-06-25", !is.na(host_ID)) %>% 
  count(host_ID) %>% 
  nrow() %>% 
  round(-1)

#' [7] Hosts with reservations
daily %>% 
  filter(housing, status == "R", date >= "2020-03-29", 
         date <= "2020-06-25", !is.na(host_ID)) %>% 
  count(host_ID) %>% 
  nrow() %>% 
  round(-1)

#' [8] Hosts with short reservations
daily %>% 
  filter(housing, status == "R", date >= "2020-03-29", 
         date <= "2020-06-25", !is.na(host_ID), 
         (is.na(res_ID) | !res_ID %in% {
           daily %>% 
             filter(housing, status == "R", date >= "2020-03-01") %>% 
             count(res_ID, sort = TRUE) %>% 
             filter(n >= 31, !is.na(res_ID)) %>% 
             pull(res_ID)})) %>% 
  count(host_ID) %>% 
  nrow() %>% 
  round(-1)

#' [9] Percentage of hosts with short reservations
{{daily %>% 
    filter(housing, status == "R", date >= "2020-03-29", 
           date <= "2020-06-25", !is.na(host_ID), 
           (is.na(res_ID) | !res_ID %in% {
             daily %>% 
               filter(housing, status == "R", date >= "2020-03-01") %>% 
               count(res_ID, sort = TRUE) %>% 
               filter(n >= 31, !is.na(res_ID)) %>% 
               pull(res_ID)})) %>% 
    count(host_ID) %>% 
    nrow()} / {daily %>% 
        filter(housing, status %in% c("R", "A"), date >= "2020-03-29", 
               date <= "2020-06-25", !is.na(host_ID)) %>% 
        count(host_ID) %>% 
        nrow()}} %>% 
  round(3)


# COVID’s impact on frequently rented entire-home listings ----------------

#' According to the model, the number of housing units in Montreal lost due to 
#' commercial STRs reached its all-time peak (5,860 [1]) at the beginning of 
#' 2020. Most of these (5,580 [1]) were FREH listings, with the remainder
#' (290 [1]) being ghost hostels—clusters of private-room listings operated 
#' out of a single housing unit. As of July 2020, the number of FREH listings 
#' had dropped to its lowest amount since we began tracking it in 2016, with 
#' just 1,750 [2] listings displaying availability and reservations consistent 
#' with historical patterns of full-time STR activity in Montreal.

#' [1] Peak housing loss
GH_total <- 
  GH %>% 
  st_drop_geometry() %>% 
  filter(status != "B") %>% 
  group_by(date) %>% 
  summarize(GH_units = sum(housing_units))

daily %>% 
  group_by(date) %>% 
  summarize(FREH = sum(FREH_3)) %>% 
  left_join(GH_total) %>% 
  mutate(housing_loss = FREH + GH_units) %>% 
  filter(housing_loss == max(housing_loss, na.rm = TRUE)) %>% 
  mutate(across(-date, round, -1))

#' [2] Minimum housing loss
daily %>% 
  group_by(date) %>% 
  summarize(FREH = sum(FREH_3)) %>% 
  left_join(GH_total) %>% 
  mutate(housing_loss = FREH + GH_units) %>% 
  filter(date == "2020-07-01") %>% 
  mutate(across(-date, round, -1))

#' There were 5,900 [1] listings which we consider likely to have been FREH in 
#' either or both of January and February 2020. Of these listings, 1,940 [2] 
#' were no longer listed on Airbnb or VRBO as of July 31, 2020. This is 
#' 32.8% [2] of these listings—twice as high as the 16.6% [3] of listings which 
#' were FREH in either January or February 2019 and were no longer listed on the 
#' STR platforms by the end of July 2019. In total, 36.1% [4] of non-FREH 
#' listings active in January or February 2020 were deactivated by the end of 
#' July 2020, while the corresponding figure last year was 32.7% [5].

#' [1] FREH in either Jan or Feb 2020
FREH_in_jan_feb <- 
  daily %>% 
  filter(housing, date >= "2020-01-01", date <= "2020-02-29", FREH_3 > 0.5) %>% 
  pull(property_ID) %>% 
  unique()

length(FREH_in_jan_feb) %>% round(-1)

#' [2] Jan-Feb FREH units deleted by end of July 2020
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% FREH_in_jan_feb) %>% 
  summarize(total = round(sum(scraped <= "2020-07-31"), -1),
            pct = round(mean(scraped <= "2020-07-31"), 3))

#' [3] Percentage of 2019 Jan-Feb FREH deleted by end of July 2019
FREH_in_jan_feb_2019 <- 
  daily %>% 
  filter(housing, date >= "2019-01-01", date <= "2019-02-28", FREH_3 > 0.5) %>% 
  pull(property_ID) %>% 
  unique()

property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% FREH_in_jan_feb_2019) %>% 
  summarize(mean(scraped <= "2019-07-31"))

#' [4] Percentage of non-FREH Jan-Feb listings deleted by end of July 2020
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% {daily %>% 
      filter(housing, date >= "2020-01-01", date <= "2020-02-29", 
             status != "B") %>% 
      pull(property_ID) %>% 
      unique()}, !property_ID %in% FREH_in_jan_feb) %>% 
  summarize(mean(scraped <= "2020-07-31"))

#' [5] Percentage of non-FREH Jan-Feb listings deleted by end of July 2019
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% {daily %>% 
      filter(housing, date >= "2019-01-01", date <= "2019-02-28", 
             status != "B") %>% 
      pull(property_ID) %>% 
      unique()}, !property_ID %in% FREH_in_jan_feb_2019) %>% 
  summarize(mean(scraped <= "2019-07-31"))

#' Of the 3,970 [1] FREH listings which remained listed throughout March - July,
#' 1,340 [2] (33.8% [3]) were blocked (i.e. not available for reservations) for 
#' all of the month of July, and 2,060 [4] (51.8% [5]) were blocked for a 
#' majority of the month. This is extremely rare behaviour for a dedicated STR 
#' listing, since the summer is usually the busiest season for tourist
#' accommodations in Montreal. In 2019, only 11.9% [6] of listings which were 
#' FREH in January or February were blocked for all of July, and only 20.3% [7] 
#' were blocked for a majority of the month.

#' [1] Jan-Feb FREH units still active at end of July 2020
property %>% 
  filter(property_ID %in% FREH_in_jan_feb, scraped > "2020-07-31") %>% 
  nrow() %>% 
  round(-1)

#' [2] Jan-Feb FREH blocked all July
daily %>% 
  filter(housing, date >= "2020-07-01", date <= "2020-07-31") %>% 
  group_by(property_ID) %>% 
  filter(mean(status == "B") == 1) %>% 
  pull(property_ID) %>% 
  unique() %>% 
  {filter(property, property_ID %in% ., property_ID %in% FREH_in_jan_feb)} %>% 
  nrow() %>% 
  round(-1)

#' [3] Percentage
{daily %>% 
  filter(housing, date >= "2020-07-01", date <= "2020-07-31") %>% 
  group_by(property_ID) %>% 
  filter(mean(status == "B") == 1) %>% 
  pull(property_ID) %>% 
  unique() %>% 
  {filter(property, property_ID %in% ., property_ID %in% FREH_in_jan_feb)} %>% 
  nrow() %>% 
  `/`(property %>% 
        filter(property_ID %in% FREH_in_jan_feb, scraped > "2020-07-31") %>% 
        nrow())} %>% 
  round(3)

#' [4] Jan-Feb FREH blocked most of July
daily %>% 
  filter(housing, date >= "2020-07-01", date <= "2020-07-31") %>% 
  group_by(property_ID) %>% 
  filter(mean(status == "B") > 0.5) %>% 
  pull(property_ID) %>% 
  unique() %>% 
  {filter(property, property_ID %in% ., property_ID %in% FREH_in_jan_feb)} %>% 
  nrow() %>% 
  round(-1)

#' [5] Percentage
{daily %>% 
    filter(housing, date >= "2020-07-01", date <= "2020-07-31") %>% 
    group_by(property_ID) %>% 
    filter(mean(status == "B") > 0.5) %>% 
    pull(property_ID) %>% 
    unique() %>% 
    {filter(property, property_ID %in% ., property_ID %in% FREH_in_jan_feb)} %>% 
    nrow() %>% 
    `/`(property %>% 
          filter(property_ID %in% FREH_in_jan_feb, scraped > "2020-07-31") %>% 
          nrow())} %>% 
  round(3)

#' [6] Jan-Feb FREH 2019 percentage blocked all July 2019
{daily %>% 
    filter(housing, date >= "2019-07-01", date <= "2019-07-31") %>% 
    group_by(property_ID) %>% 
    filter(mean(status == "B") == 1) %>% 
    pull(property_ID) %>% 
    unique() %>% 
    {filter(property, property_ID %in% ., 
            property_ID %in% FREH_in_jan_feb_2019)} %>% 
    nrow() %>% 
    `/`(property %>% 
          filter(property_ID %in% FREH_in_jan_feb_2019, 
                 scraped > "2019-07-31") %>% 
          nrow())} %>% 
  round(3)

#' [7] Jan-Feb FREH 2019 percentage blocked most of July 2019
{daily %>% 
    filter(housing, date >= "2019-07-01", date <= "2019-07-31") %>% 
    group_by(property_ID) %>% 
    filter(mean(status == "B") > 0.5) %>% 
    pull(property_ID) %>% 
    unique() %>% 
    {filter(property, property_ID %in% ., 
            property_ID %in% FREH_in_jan_feb_2019)} %>% 
    nrow() %>% 
    `/`(property %>% 
          filter(property_ID %in% FREH_in_jan_feb_2019, 
                 scraped > "2019-07-31") %>% 
          nrow())} %>% 
  round(3)

#' For example, in the month of February 2020, 63.7% [1] of all reserved nights
#' were booked in these FREH properties. 

#' [1] Percentage of reserved nights in FREH properties in Feb 2020
daily %>% 
  filter(housing, date >= "2020-02-01", date <= "2020-02-29", status == "R") %>% 
  summarize(mean(property_ID %in% FREH_in_jan_feb))


# Clean up ----------------------------------------------------------------

rm(active_by_status, average_prices, GH_total, mar_jul_price_trend,
   monthly_prices, reservations, trends, feb_price_trend, feb_trend,
   FREH_in_jan_feb, FREH_in_jan_feb_2019, mar_aug_seasonal, 
   mar_jul_price_seasonal)
