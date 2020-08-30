#### 40 CHAPTER 4 ANALYSIS ######################################################

#' This script produces the facts for chapter 4. It runs quickly.
#' 
#' Output:
#' - None
#' 
#' Script dependencies:
#' - `09_str_processing.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")

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

#' There were 12.3% [1] more nights reserved in Montreal STRs in 2019 than 
#' there were in 2018—peaking at nearly 9,500 [2] nightly STR reservations in 
#' the summer of 2019.... In March 2020, however, when reserved nights should 
#' have been steadily increasing en route to the summer peak, they instead 
#' collapsed in the face of COVID-19. While total reserved nights from January 
#' to February 2020 increased at a rapid 45.6% [3] compared to 2019, reserved 
#' nights from March to August 2020 decreased 70.5% [4] compared to the previous 
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
  count(date, sort = TRUE)

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

#' On July 31, 2020, fewer than 2,000 [1] STRs were reserved in Montreal. But 
#' the trajectory of STR activity established prior to the pandemic, combined 
#' with the fact that bookings normally increase rapidly through the spring and 
#' summer, suggests that, in the absence of the pandemic, Montreal would have 
#' been expected to receive 8,700 [1] reservations instead. The COVID-19 
#' pandemic, therefore, depressed STR activity by 77.2% [1], or 6,730 
#' [1] reservations, on that date. In total, from March through July 2020, we 
#' estimate that there have been 679,700 [2] fewer STR nights reserved than 
#' would normally have been expected to occur. The 320,700 [2] total nights 
#' reserved in this time period is only 32.1% [2] of the 1,000,000 [2] total 
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
#' of 21.3% [1] lower than expected. Spread across the 320,700 nights reserved 
#' during this period, this means that STR operators collectively earned 
#' $8.8 million [2] less than they would have on their bookings in the absence 
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
#' lost a total of $105.6 million [1] in revenue between March and August 2020 
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

#' Between March 28 and June 25, there were a total of 36,820 [1] reservations 
#' in the City, compared to 129,940 [2] for the same period in 2019 (a decrease 
#' of 71.7% [3]). Only 390 [4] of these reservations were for longer than 30 
#' days, which means that the remaining 36,440 [5] reservations in Montreal were 
#' illegal. Additionally, these illegal reservations were distributed widely 
#' among Montreal’s STR hosts. There were 6,130 [6] hosts with active listings 
#' in the March 28 - June 25 period, and 4,440 [7] of these hosts received at 
#' least one reservation. 4,420 [8] hosts received reservations of 30 days or 
#' fewer, which means that 72.1% [9] of Montreal’s active STR hosts appear to 
#' have violated the Provincial order.

#' [1] Total reservations 2020: add unique res_ID count to compressed no-ID rows
daily %>% 
  filter(housing, status == "R", date >= "2020-03-28", date <= "2020-06-25") %>% 
  filter(is.na(res_ID)) %>% 
  select(1:12) %>% 
  strr_compress() %>% 
  nrow() %>% 
  `+`(daily %>% 
        filter(housing, status == "R", date >= "2020-03-28", 
               date <= "2020-06-25", !is.na(res_ID)) %>% 
        count(res_ID) %>% 
        nrow()) %>% 
  round(-1)

#' [2] Total reservations 2019
daily %>% 
  filter(housing, status == "R", date >= "2019-03-28", date <= "2019-06-25") %>% 
  filter(is.na(res_ID)) %>% 
  select(1:12) %>% 
  strr_compress() %>% 
  nrow() %>% 
  `+`(daily %>% 
        filter(housing, status == "R", date >= "2019-03-28", 
               date <= "2019-06-25", !is.na(res_ID)) %>% 
        count(res_ID) %>% 
        nrow()) %>% 
  round(-1)

#' [3] Change in reservations
1 - {daily %>% 
    filter(housing, status == "R", date >= "2020-03-28", 
           date <= "2020-06-25") %>% 
    filter(is.na(res_ID)) %>% 
    select(1:12) %>% 
    strr_compress() %>% 
    nrow() %>% 
    `+`(daily %>% 
          filter(housing, status == "R", date >= "2020-03-28", 
                 date <= "2020-06-25", !is.na(res_ID)) %>% 
          count(res_ID) %>% 
          nrow())} / 
  {daily %>% 
      filter(housing, status == "R", date >= "2019-03-28", 
             date <= "2019-06-25") %>% 
      filter(is.na(res_ID)) %>% 
      select(1:12) %>% 
      strr_compress() %>% 
      nrow() %>% 
      `+`(daily %>% 
            filter(housing, status == "R", date >= "2019-03-28", 
                   date <= "2019-06-25", !is.na(res_ID)) %>% 
            count(res_ID) %>% 
            nrow())
  } %>% 
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
  filter(housing, status == "R", date >= "2020-03-28", date <= "2020-06-25") %>% 
  filter(is.na(res_ID)) %>% 
  select(1:12) %>% 
  strr_compress() %>% 
  nrow() %>% 
  `+`(daily %>% 
        filter(housing, status == "R", date >= "2020-03-28", 
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
  filter(housing, status %in% c("R", "A"), date >= "2020-03-28", 
         date <= "2020-06-25", !is.na(host_ID)) %>% 
  count(host_ID) %>% 
  nrow() %>% 
  round(-1)

#' [7] Hosts with reservations
daily %>% 
  filter(housing, status == "R", date >= "2020-03-28", 
         date <= "2020-06-25", !is.na(host_ID)) %>% 
  count(host_ID) %>% 
  nrow() %>% 
  round(-1)

#' [8] Hosts with short reservations
daily %>% 
  filter(housing, status == "R", date >= "2020-03-28", 
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
    filter(housing, status == "R", date >= "2020-03-28", 
           date <= "2020-06-25", !is.na(host_ID), 
           (is.na(res_ID) | !res_ID %in% {
             daily %>% 
               filter(housing, status == "R", date >= "2020-03-01") %>% 
               count(res_ID, sort = TRUE) %>% 
               filter(n >= 31, !is.na(res_ID)) %>% 
               pull(res_ID)})) %>% 
    count(host_ID) %>% 
    nrow()} / {daily %>% 
        filter(housing, status %in% c("R", "A"), date >= "2020-03-28", 
               date <= "2020-06-25", !is.na(host_ID)) %>% 
        count(host_ID) %>% 
        nrow()}} %>% 
  round(3)

# All reserved nights
daily %>% 
  filter(housing, status == "R", date >= "2020-03-28", date <= "2020-06-25") %>% 
  nrow()

daily %>% 
  filter(housing, status == "R", date >= "2020-03-28", date <= "2020-06-25") %>% 
  count(res_ID, sort = TRUE)


# COVID’s impact on frequently rented entire-home listings ----------------











# Prepare new objects -----------------------------------------------------

FREH <- 
  daily %>% 
  filter(date >= "2016-01-01") %>% 
  group_by(date) %>% 
  summarize(across(c(FREH_3), sum)) %>%
  filter(substr(date, 9, 10) == "01")

GH_total <-
  GH %>%
  st_drop_geometry() %>%
  filter(status != "B") %>% 
  group_by(date) %>%
  summarize(GH_units = sum(housing_units)) %>%
  mutate(GH_average = frollmean(GH_units, 30, align = "right", fill = 198))

housing_loss <-
  FREH %>%
  select(date, FREH_3) %>% 
  rename(`Entire home/apt` = FREH_3) %>%
  left_join(GH_total, by = "date") %>%
  select(-GH_units) %>% 
  rename(`Private room` = GH_average) %>%
  gather(`Entire home/apt`, `Private room`, 
         key = `Listing type`, value = `Housing units`) 


# Look at year-over-year growth of Reserved and Available listings over certain time periods ------------------------------------------------------

#' [1] Year-over-year growth of Reserved listings
(daily %>% 
   filter(status == "R", date >= LTM_start_date, date <= LTM_end_date) %>% 
   nrow() -
   daily %>% 
   filter(status == "R", date >= LTM_start_date - years(1), date <= LTM_end_date - years(1)) %>% 
   nrow()
) /
  daily %>% 
  filter(status == "R", date >= LTM_start_date - years(1), date <= LTM_end_date - years(1)) %>% 
  nrow()

#' [2] growth of Reserved listings for the first half of 2020
(daily %>% 
    filter(status == "R", date >= "2020-01-01", date <= max(date)) %>% 
    nrow() -
    daily %>% 
    filter(status == "R", date >= "2019-01-01", date <= max(date) - years(1)) %>% 
    nrow()
) /
  daily %>% 
  filter(status == "R", date >= "2019-01-01", date <= max(date) - years(1)) %>% 
  nrow()

#' [3] Lowest point of available listings
daily %>% 
  filter(status == "A", date >= "2018-01-01") %>% 
  count(date) %>% 
  arrange(n)


# Compare the number of reservations during the ban, compared to the same period the year before ------------------------------------------------------

#' [1] Reservations between 28 march and 25 june 2020
daily_comp_ban <-
  strr_compress(filter(daily, date >= "2020-03-28", date <= "2020-06-25"))

daily_comp_ban %>%
  filter(booked_date >= "2020-03-28", booked_date <= "2020-06-25")

#' [2] Reservations between 28 march and 25 june 2019
daily_comp_2019 <-
  strr_compress(filter(daily, date >= "2019-03-28", date <= "2019-06-25"))

daily_comp_2019 %>%
  filter(booked_date >= "2019-03-28", booked_date <= "2019-06-25")

#' [3] Difference between the two years
(
  (daily_comp_ban %>%
  filter(booked_date >= "2020-03-28", booked_date <= "2020-06-25") %>% 
  nrow())
  -
  (daily_comp_2019 %>%
  filter(booked_date >= "2019-03-28", booked_date <= "2019-06-25") %>% 
  nrow())
  )/(daily_comp_2019 %>%
     filter(booked_date >= "2019-03-28", booked_date <= "2019-06-25") %>% 
     nrow()
   )

#' [4] Reservations of more than 30 days during the ban
daily_comp_ban %>%
  filter(booked_date >= "2020-03-28", booked_date <= "2020-06-25") %>% 
  mutate(duration = end_date-start_date+1) %>% 
  filter(duration >= 30)


# Impact of COVID on FREH listings  ------------------------------------------------------

#' [1] Get the lowest point in housing loss 
(housing_loss %>% 
    group_by(date) %>% 
    summarize(housing_loss = round(sum(`Housing units`), digit = -1)) %>% 
    arrange(housing_loss))[1,]

#' [2] Calculate the FREH units on 2020-01-01 that were deleted by end of June 2020
property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, 
                                 housing, 
                                 date == "2020-01-01", 
                                 FREH_3 > 0.5)$property_ID,
         scraped < max(scraped) - months(1),
         scraped != "2020-05-21") %>% 
  nrow() %>% round(digit = -1)

#' [3] Calculate the FREH units on 2019-01-01 that were deleted by end of June 2019
property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, 
                                 housing,
                                 date == "2019-01-01",
                                 FREH_3 > 0.5)$property_ID,
         scraped < max(scraped) - months(1) - years(1)) %>% 
  nrow() %>% round(digit = -1)


#' [4] Percentage of FREH listings which were deleted by end of June 2020
property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, 
                                 housing,
                                 date == "2020-01-01", 
                                 FREH_3 > 0.5)$property_ID,
         scraped < max(scraped) - months(1),
         scraped != "2020-05-21") %>% 
  nrow() /
  filter(daily,  
         housing,
         date == "2020-01-01", 
         FREH_3 > 0.5) %>% 
  nrow()

#' [5] Percentage of FREH listings which were deleted by end of June 2019
property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, housing,
                                 date == "2019-01-01", 
                                 FREH_3 > 0.5)$property_ID,
         scraped < max(scraped) - months(1) - years(1)) %>% 
  nrow() /
  filter(daily, 
         housing, 
         date == "2019-01-01", 
         FREH_3 > 0.5) %>% 
  nrow()

#' [6] FREH listings on January blocked ALL MONTH of June
property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, housing, date == "2020-01-01", FREH_3 > 0.5)$property_ID,
         property_ID %in% (daily %>% 
                             filter(housing,
                                    date >= "2020-06-01", 
                                    status == "B") %>% 
                             count(property_ID) %>% 
                             filter(n == 30))$property_ID) %>% 
  nrow() %>% round(digit = -1)

#' [7] FREH listings on January blocked A MAJORITY of June
property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, housing, date == "2020-01-01", FREH_3 > 0.5)$property_ID,
         property_ID %in% (daily %>% 
                             filter(housing,
                                    date >= "2020-06-01", 
                                    status == "B") %>% 
                             count(property_ID) %>% 
                             filter(n > 15))$property_ID) %>% 
  nrow() %>% round(digit = -1)

#' [8] FREH listings on January blocked ALL MONTH of June in 2019
property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, date == "2019-01-01", FREH_3 > 0.5)$property_ID,
         property_ID %in% (daily %>% 
                             filter(housing,
                                    date >= "2019-06-01", date <= "2019-06-30",
                                    status == "B") %>% 
                             count(property_ID) %>% 
                             filter(n == 30))$property_ID) %>% 
  nrow() %>% round(digit = -1)

#' [9] FREH listings on January blocked A MAJORITY of June in 2019
property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, housing, date == "2019-01-01", FREH_3 > 0.5)$property_ID,
         property_ID %in% (daily %>% 
                             filter(housing,
                                    date >= "2019-06-01", date <= "2019-06-30", 
                                    status == "B") %>% 
                             count(property_ID) %>% 
                             filter(n > 15))$property_ID) %>% 
  nrow() %>% round(digit = -1)

#' [10] Get the FREH listings blocked for the last days of June (after ban)
property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, housing, date == "2020-01-01", FREH_3 > 0.5)$property_ID,
         property_ID %in% (daily %>% 
                             filter(housing,
                                    date >= "2020-06-25", date <= "2020-06-30",
                                    status == "B") %>% 
                             count(property_ID) %>% 
                             filter(n == 6))$property_ID) %>% 
  nrow() %>% round(digit = -1)

# behavior of non-commercial listings during ban ------------------------------------------------------
# numbers not in the text, but talked about at the end of chap.4 ------------------------------------------------------

#' [1] proportion of non-commercial listings which were deleted by end of June in 2020
property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, housing, date == "2020-01-01", FREH_3 < 0.5, multi == F)$property_ID,
         scraped < max(scraped) - months(1),
         scraped != "2020-05-21") %>% 
  nrow() /
  filter(daily, housing, date == "2020-01-01", FREH_3 < 0.5, multi == F) %>% 
  nrow()

#' [2] proportion of non-commercial listings which were deleted by end of June in 2019
property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, housing, date == "2019-01-01", FREH_3 < 0.5, multi == F)$property_ID,
         scraped < max(scraped) - months(1) - years(1)) %>% 
  nrow()/
  filter(daily, housing, date == "2019-01-01", FREH_3 < 0.5, multi == F) %>% 
  nrow()

#' [3] number of non-commercial listings blocked in June 2020
property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, 
                                 housing, 
                                 date == "2020-01-01", 
                                 FREH_3 < 0.5, multi == F)$property_ID,
         property_ID %in% (daily %>% 
                             filter(housing,
                                    date >= "2020-06-01", 
                                    status == "B") %>% 
                             count(property_ID) %>% 
                             filter(n == 30))$property_ID) %>% 
  nrow() /
  filter(daily, housing, date == "2020-01-01", FREH_3 < 0.5, multi == F) %>% 
  nrow()

#' [4] number of non-commercial listings blocked in June 2019
property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, 
                                 housing, 
                                 date == "2019-01-01", 
                                 FREH_3 < 0.5, multi == F)$property_ID,
         property_ID %in% (daily %>% 
                             filter(housing,
                                    date >= "2019-06-01", date <= "2019-06-30",
                                    status == "B") %>% 
                             count(property_ID) %>% 
                             filter(n == 30))$property_ID) %>% 
  nrow() /
  filter(daily, housing, date == "2019-01-01", FREH_3 < 0.5, multi == F) %>% 
  nrow()
