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

# Load previous data ------------------------------------------------------

load("output/str_processed.Rdata")

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
  filter(booked_date >= "2020-03-28", booked_date <= "2020-06-25") %>% 
  mutate(duration=end_date-start_date+1) %>% 
  filter(duration >= 28)

#' [2] Reservations between 28 march and 25 june 2019
daily_comp_2019 <-
  strr_compress(filter(daily, date >= "2019-03-28", date <= "2019-06-25"))

daily_comp_2019 %>%
  filter(booked_date >= "2019-03-28", booked_date <= "2019-06-25")

