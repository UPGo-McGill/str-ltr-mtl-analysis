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
library(tsibble)
library(feasts)
library(fable)

qload("output/str_processed.qsm", nthreads = availableCores())


# Forecast reservations and prices ----------------------------------------

# Get daily reservations and prices
reservations_and_prices <- 
  daily %>% 
  filter(housing, date >= "2016-01-01", status == "R") %>% 
  group_by(date) %>% 
  summarize(res = n(), price = mean(price))

# Create monthly time series
monthly_series <- 
  reservations_and_prices %>% 
  tsibble::as_tsibble(index = date) %>% 
  tsibble::index_by(yearmon = yearmonth(date)) %>% 
  summarize(price = sum(res * price) / sum(res),
            res = sum(res)) %>% 
  relocate(price, .after = res)

# Create reservations model
reservations_model <- 
  monthly_series %>%
  filter(yearmon <= yearmonth("2020-02")) %>% 
  model(res = decomposition_model(
    STL(res, robust = TRUE), NAIVE(season_adjust)))

# Create price model
price_model <- 
  monthly_series %>% 
  filter(yearmon <= yearmonth("2020-02")) %>% 
  model(price = decomposition_model(
    STL(price, robust = TRUE), NAIVE(season_adjust)))

# Create reservations forecast
reservations_forecast <-
  reservations_model %>% 
  forecast(h = "10 months") %>% 
  as_tibble() %>% 
  select(yearmon, res_trend_month = .mean)

# Create price forecast
price_forecast <- 
  price_model %>% 
  forecast(h = "10 months") %>% 
  as_tibble() %>% 
  select(yearmon, price_trend_month = .mean)

# Integrate forecasts into monthly data
monthly_series <- 
  monthly_series %>% 
  left_join(reservations_forecast, by = "yearmon") %>% 
  left_join(price_forecast, by = "yearmon")

# Integrate forecasts into daily data
reservations_and_prices <- 
  reservations_and_prices %>% 
  mutate(across(c(res, price), slider::slide_dbl, ~.x[1], .before = 366, 
                .complete = TRUE, .names = "{.col}_trend")) %>%
  mutate(across(c(res_trend, price_trend), slider::slide_dbl, mean, 
                .before = 6, .complete = TRUE)) %>%
  mutate(across(c(res_trend, price_trend), 
                ~if_else(date >= "2020-03-01", .x, NA_real_))) %>%
  mutate(yearmon = yearmonth(date)) %>%
  left_join(select(monthly_series, -res, -price), by = "yearmon") %>%
  group_by(yearmon) %>%
  mutate(res_trend = res_trend * res_trend_month / sum(res_trend),
         price_trend = price_trend * price_trend_month / mean(price_trend)) %>%
  ungroup() %>%
  select(-c(yearmon:price_trend_month))


# Reservations and prices collapsed during COVID-19 -----------------------

#' There were 9.3% [1] more nights reserved in Montreal STRs in 2019 than 
#' there were in 2018—peaking at more than 9,600 [2] nightly STR reservations in 
#' the summer of 2019.... In March 2020, however, when reserved nights should 
#' have been steadily increasing en route to the summer peak, they instead 
#' collapsed in the face of COVID-19. While total reserved nights from January 
#' to February 2020 increased at a rapid 46.6% [3] compared to 2019, reserved 
#' nights from March to August 2020 decreased 69.9% [4] compared to the previous 
#' year.

#' [1] YOY growth in reservations, 2018-2019
daily %>% 
  filter(housing, status == "R", year(date) %in% 2018:2019) %>% 
  count(year = year(date)) %>% 
  summarize(change = (n[2] - n[1]) / n[1]) %>% 
  pull(change) %>% 
  scales::percent(0.1)
  
#' [2] Peak 2019 nightly reservations
daily %>% 
  filter(housing, status == "R", year(date) == 2019) %>% 
  count(date, sort = TRUE) %>% 
  slice(1) %>% 
  pull(n) %>% 
  scales::comma(10)

#' [3] YOY reservation growth, Jan-Feb 2019-2020
daily %>% 
  filter(housing, status == "R", year(date) %in% 2019:2020, 
         month(date) %in% 1:2) %>% 
  count(year = year(date)) %>% 
  summarize(change = (n[2] - n[1]) / n[1]) %>% 
  pull(change) %>% 
  scales::percent(0.1)

#' [4] YOY reservation growth, Mar-Dec 2019-2020
daily %>% 
  filter(housing, status == "R", year(date) %in% 2019:2020, 
         month(date) >= 3) %>% 
  count(year = year(date)) %>% 
  summarize(change = (n[2] - n[1]) / n[1]) %>% 
  pull(change) %>% 
  scales::percent(0.1)

#' On December 31, 2020, just over 1,500 [1] STRs were reserved in Montreal. 
#' But the trajectory of STR activity established prior to the pandemic, 
#' combined with the fact that bookings normally increase rapidly during the
#' winter holidays, suggests that, in the absence of the pandemic, Montreal 
#' would have been expected to receive 7,150 [1] reservations instead. The 
#' COVID-19 pandemic, therefore, depressed STR activity by 78.8% [1], or 
#' 5,630 [1] reservations, on that date. In total, from March through December 
#' 2020, we estimate that there have been 1.46 million [2] fewer STR nights 
#' reserved than would normally have been expected to occur. The 522,000 [2] 
  #' total nights reserved in this time period is only 26.3% [2] of the 1.98 
#' million [2] total that would represent the previous growth trend.

#' [1] Actual and expected reservations on 2020-12-31
reservations_and_prices %>% 
  select(-price, -price_trend) %>% 
  filter(date == "2020-12-31") %>% 
  mutate(dif = res_trend - res, pct_change = 1 - res / res_trend) %>% 
  mutate(across(res:dif, scales::comma, 10)) %>% 
  mutate(pct_change = scales::percent(pct_change, 0.1))

#' [2] Total actual and expected reservations, Mar-Dec 2020
reservations_and_prices %>% 
  filter(date >= "2020-03-01") %>% 
  summarize(across(c(res, res_trend), sum)) %>% 
  mutate(dif = res_trend - res, pct = res / res_trend) %>% 
  mutate(across(res:dif, scales::comma, 1000)) %>% 
  mutate(pct = scales::percent(pct, 0.1))

#' Throughout the May-December period of 2020, nightly prices have been an 
#' average of 21.1% [1] lower than expected. Spread across the 521,600 [2] 
#' nights reserved during this period, this means that STR operators 
#' collectively earned $12.5 million [3] less than they would have on their 
#' bookings in the absence of the pandemic.

#' [1] Average price difference
reservations_and_prices %>% 
  filter(date >= "2020-03-01") %>% 
  summarize(dif = 1 - sum(price) / sum(price_trend)) %>% 
  pull(dif) %>% 
  scales::percent(0.1)

#' [2] Total reservations
daily %>% 
  filter(housing, date >= "2020-03-01", status == "R") %>% 
  nrow() %>% 
  scales::comma(100)

#' [3] Total revenue difference
reservations_and_prices %>% 
  filter(date >= "2020-03-01") %>% 
  mutate(rev_dif = res * (price_trend - price)) %>% 
  tally(rev_dif) %>% 
  pull(n) %>% 
  scales::dollar(100000)

average_prices %>%
  rename(price_trend = trend) %>% 
  filter(date >= "2020-03-01") %>% 
  left_join(reservations) %>% 
  mutate(rev_dif = n * (price_trend - price)) %>% 
  tally(rev_dif) %>% 
  pull(n) %>% 
  scales::dollar(100000)

#' When the lower prices on reservations which did occur is combined with the 
#' reservations which did not occur, our estimate is that Montreal’s STR hosts 
#' lost a total of $196.7 million [1] in revenue between March and December 2020 
#' because of the COVID-19 pandemic.

#' [1] Total lost revenue
reservations_and_prices %>% 
  filter(date >= "2020-03-01") %>% 
  summarize(total_rev = sum(res * price),
            total_trend = sum(res_trend * price_trend),
            dif = total_trend - total_rev) %>% 
  pull(dif) %>% 
  scales::dollar(100000)


# Compliance with the Provincial STR ban ----------------------------------

#' Between March 29 and June 25, there were a total of 36,240 [1] reservations 
#' in the City, compared to 127,750 [2] for the same period in 2019 (a decrease 
#' of 71.6% [3]). Only 630 [4] of these reservations were for longer than 30 
#' days, which means that the remaining 35,610 [5] reservations in Montreal were 
#' illegal. Additionally, these illegal reservations were distributed widely 
#' among Montreal’s STR hosts. There were 6,120 [6] hosts with active listings 
#' in the March 29 - June 25 period, and 4,350 [7] of these hosts received at 
#' least one reservation. 4,320 [8] hosts received reservations of 30 days or 
#' fewer, which means that 70.6% [9] of Montreal’s active STR hosts appear to 
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
  scales::comma(10)

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
  scales::comma(10)

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
  scales::percent(0.1)

#' [4] Long reservations
daily %>% 
  filter(housing, status == "R", date >= "2020-03-01") %>% 
  count(res_ID, sort = TRUE) %>% 
  filter(n >= 31, !is.na(res_ID)) %>% 
  nrow() %>% 
  scales::comma(10)

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
  scales::comma(10)

#' [6] All active hosts
daily %>% 
  filter(housing, status %in% c("R", "A"), date >= "2020-03-29", 
         date <= "2020-06-25", !is.na(host_ID)) %>% 
  count(host_ID) %>% 
  nrow() %>% 
  scales::comma(10)

#' [7] Hosts with reservations
daily %>% 
  filter(housing, status == "R", date >= "2020-03-29", 
         date <= "2020-06-25", !is.na(host_ID)) %>% 
  count(host_ID) %>% 
  nrow() %>% 
  scales::comma(10)

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
  scales::comma(10)

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
  scales::percent(0.1)


# COVID’s impact on frequently rented entire-home listings ----------------

#' According to the model, the number of housing units in Montreal lost due to 
#' commercial STRs reached its all-time peak (5,550 [1]) at the beginning of 
#' 2020. Most of these (5,250 [1]) were FREH listings, with the remainder
#' (300 [1]) being ghost hostels—clusters of private-room listings operated 
#' out of a single housing unit. In October 2020, the number of FREH listings 
#' had dropped to its lowest amount since we began tracking it in 2016, with 
#' just 1,740 [2] listings displaying availability and reservations consistent 
#' with historical patterns of full-time STR activity in Montreal. By December
#' 2020, this number had increased somewhat to 2,040 [3].

#' [1] Peak housing loss
GH_total <- 
  GH %>% 
  st_drop_geometry() %>% 
  filter(status != "B") %>% 
  group_by(date) %>% 
  summarize(GH_units = sum(housing_units))

daily %>% 
  filter(housing) %>% 
  group_by(date) %>% 
  summarize(FREH = sum(FREH_3)) %>% 
  left_join(GH_total) %>% 
  mutate(housing_loss = FREH + GH_units) %>% 
  filter(housing_loss == max(housing_loss, na.rm = TRUE)) %>% 
  mutate(across(-date, scales::comma, 10))

#' [2] Minimum housing loss
daily %>% 
  filter(housing) %>% 
  group_by(date) %>% 
  summarize(FREH = sum(FREH_3)) %>% 
  left_join(GH_total) %>% 
  mutate(housing_loss = FREH + GH_units,
         date = as.Date(date)) %>%
  filter(date >= "2016-01-01", day(date) == 1) %>%
  filter(FREH == min(FREH, na.rm = TRUE)) %>% 
  mutate(across(-date, scales::comma, 10))

#' [3] Housing loss in December 2020
daily %>% 
  filter(housing) %>% 
  group_by(date) %>% 
  summarize(FREH = sum(FREH_3)) %>% 
  left_join(GH_total) %>% 
  mutate(housing_loss = FREH + GH_units,
         date = as.Date(date)) %>%
  filter(date == "2020-12-01") %>%
  mutate(across(-date, scales::comma, 10))

#' There were 5,630 [1] listings which we consider likely to have been FREH in 
#' either or both of January and February 2020. Of these listings, 2,610 [2] 
#' were no longer listed on Airbnb or VRBO as of July 31, 2020. This is 
#' 46.4% [2] of these listings—almost 50% higher than the 32.2% [3] of listings 
#' which were FREH in either January or February 2019 and were no longer listed 
#' on the STR platforms by the end of December 2019. By contrast, 47.3% [4] of 
#' non-FREH listings active in January or February 2020 were deactivated by the 
#' end of December 2020, while the corresponding figure last year was 44.3% [5].

#' [1] FREH in either Jan or Feb 2020
FREH_in_jan_feb <- 
  daily %>% 
  filter(housing, date >= "2020-01-01", date <= "2020-02-29", FREH_3 > 0.5) %>% 
  pull(property_ID) %>% 
  unique()

length(FREH_in_jan_feb) %>% scales::comma(10)

#' [2] Jan-Feb FREH units deleted by end of July 2020
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% FREH_in_jan_feb) %>% 
  summarize(total = scales::comma(sum(scraped <= "2020-12-31"), 10),
            pct = scales::percent(mean(scraped <= "2020-12-31"), 0.1))

#' [3] Percentage of 2019 Jan-Feb FREH deleted by end of July 2019
FREH_in_jan_feb_2019 <- 
  daily %>% 
  filter(housing, date >= "2019-01-01", date <= "2019-02-28", FREH_3 > 0.5) %>% 
  pull(property_ID) %>% 
  unique()

property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% FREH_in_jan_feb_2019) %>% 
  summarize(pct = scales::percent(mean(scraped <= "2019-12-31"), 0.1))

#' [4] Percentage of non-FREH Jan-Feb listings deleted by end of July 2020
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% {daily %>% 
      filter(housing, date >= "2020-01-01", date <= "2020-02-29", 
             status != "B") %>% 
      pull(property_ID) %>% 
      unique()}, !property_ID %in% FREH_in_jan_feb) %>% 
  summarize(pct = scales::percent(mean(scraped <= "2020-12-31"), 0.1))

#' [5] Percentage of non-FREH Jan-Feb listings deleted by end of July 2019
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% {daily %>% 
      filter(housing, date >= "2019-01-01", date <= "2019-02-28", 
             status != "B") %>% 
      pull(property_ID) %>% 
      unique()}, !property_ID %in% FREH_in_jan_feb_2019) %>% 
  summarize(pct = scales::percent(mean(scraped <= "2019-12-31"), 0.1))

#' Of the 3,020 [1] FREH listings which remained listed throughout March - 
#' December, 1,220 [2] (40.5% [3]) were blocked (i.e. not available for 
#' reservations) for all of the month of July, and 1,780 [4] (58.9% [5]) were 
#' blocked for a majority of the month.... In 2019, only 16.2% [6] of listings 
#' which were FREH in January or February were blocked for all of December, and 
#' only 20.4% [7] were blocked for a majority of the month.

#' [1] Jan-Feb FREH units still active at end of Dec 2020
property %>% 
  filter(property_ID %in% FREH_in_jan_feb, scraped > "2020-12-31") %>% 
  nrow() %>% 
  scales::comma(10)

#' [2] Jan-Feb FREH blocked all December
daily %>% 
  filter(housing, date >= "2020-12-01", date <= "2020-12-31") %>% 
  group_by(property_ID) %>% 
  filter(mean(status == "B") == 1) %>% 
  pull(property_ID) %>% 
  unique() %>% 
  {filter(property, property_ID %in% ., property_ID %in% FREH_in_jan_feb)} %>% 
  nrow() %>% 
  scales::comma(10)

#' [3] Percentage
{daily %>% 
  filter(housing, date >= "2020-12-01", date <= "2020-12-31") %>% 
  group_by(property_ID) %>% 
  filter(mean(status == "B") == 1) %>% 
  pull(property_ID) %>% 
  unique() %>% 
  {filter(property, property_ID %in% ., property_ID %in% FREH_in_jan_feb)} %>% 
  nrow() %>% 
  `/`(property %>% 
        filter(property_ID %in% FREH_in_jan_feb, scraped > "2020-12-31") %>% 
        nrow())} %>% 
  scales::percent(0.1)

#' [4] Jan-Feb FREH blocked most of December
daily %>% 
  filter(housing, date >= "2020-12-01", date <= "2020-12-31") %>% 
  group_by(property_ID) %>% 
  filter(mean(status == "B") > 0.5) %>% 
  pull(property_ID) %>% 
  unique() %>% 
  {filter(property, property_ID %in% ., property_ID %in% FREH_in_jan_feb)} %>% 
  nrow() %>% 
  scales::comma(10)

#' [5] Percentage
{daily %>% 
    filter(housing, date >= "2020-12-01", date <= "2020-12-31") %>% 
    group_by(property_ID) %>% 
    filter(mean(status == "B") > 0.5) %>% 
    pull(property_ID) %>% 
    unique() %>% 
    {filter(property, property_ID %in% ., property_ID %in% FREH_in_jan_feb)} %>% 
    nrow() %>% 
    `/`(property %>% 
          filter(property_ID %in% FREH_in_jan_feb, scraped > "2020-12-31") %>% 
          nrow())} %>% 
  scales::percent(0.1)

#' [6] Jan-Feb FREH 2019 percentage blocked all December 2019
{daily %>% 
    filter(housing, date >= "2019-12-01", date <= "2019-12-31") %>% 
    group_by(property_ID) %>% 
    filter(mean(status == "B") == 1) %>% 
    pull(property_ID) %>% 
    unique() %>% 
    {filter(property, property_ID %in% ., 
            property_ID %in% FREH_in_jan_feb_2019)} %>% 
    nrow() %>% 
    `/`(property %>% 
          filter(property_ID %in% FREH_in_jan_feb_2019, 
                 scraped > "2019-12-31") %>% 
          nrow())} %>% 
  scales::percent(0.1)

#' [7] Jan-Feb FREH 2019 percentage blocked most of July 2019
{daily %>% 
    filter(housing, date >= "2019-12-01", date <= "2019-12-31") %>% 
    group_by(property_ID) %>% 
    filter(mean(status == "B") > 0.5) %>% 
    pull(property_ID) %>% 
    unique() %>% 
    {filter(property, property_ID %in% ., 
            property_ID %in% FREH_in_jan_feb_2019)} %>% 
    nrow() %>% 
    `/`(property %>% 
          filter(property_ID %in% FREH_in_jan_feb_2019, 
                 scraped > "2019-12-31") %>% 
          nrow())} %>% 
  scales::percent(0.1)

#' For example, in the month of February 2020, 62.0% [1] of all reserved nights
#' were booked in these FREH properties. 

#' [1] Percentage of reserved nights in FREH properties in Feb 2020
daily %>% 
  filter(housing, date >= "2020-02-01", date <= "2020-02-29", status == "R") %>% 
  summarize(pct = mean(property_ID %in% FREH_in_jan_feb)) %>% 
  mutate(pct = scales::percent(pct, 0.1))


# Clean up ----------------------------------------------------------------

rm(active_by_status, average_prices, GH_total, mar_jul_price_trend,
   monthly_prices, reservations, trends, feb_price_trend, feb_trend,
   FREH_in_jan_feb, FREH_in_jan_feb_2019, mar_aug_seasonal, 
   mar_jul_price_seasonal)
