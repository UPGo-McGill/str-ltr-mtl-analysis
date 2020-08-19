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
  mutate(duration=end_date-start_date+1) %>% 
  filter(duration >= 30)


# Impact of COVID on FREH listings  ----------------------------------

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

# lowest point in housing loss 
(housing_loss %>% 
    group_by(date) %>% 
    summarize(housing_loss = round(sum(`Housing units`), digit = -1)) %>% 
    arrange(housing_loss))[1,]

# units FREH on 2020-01-01 and deleted by end of june 2020. Same for 2019
property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, 
                                 housing, 
                                 date == "2020-01-01", 
                                 FREH_3 > 0.5)$property_ID,
         scraped < max(scraped) - months(1),
         scraped != "2020-05-21") %>% 
  nrow() %>% round(digit = -1)

property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, 
                                 housing,
                                 date == "2019-01-01",
                                 FREH_3 > 0.5)$property_ID,
         scraped < max(scraped) - months(1) - years(1)) %>% 
  nrow() %>% round(digit = -1)



# percentage of FREH listings which were deleted by end of june.
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

property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, housing,
                                 date == "2019-01-01", 
                                 FREH_3 > 0.5)$property_ID,
         scraped < max(scraped) - months(1) - years(1)) %>% 
  nrow()/
  filter(daily, 
         housing, 
         date == "2019-01-01", 
         FREH_3 > 0.5) %>% 
  nrow()


# FREH listings on january blocked all month of june, or a majority
property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, housing, date == "2020-01-01", FREH_3 > 0.5)$property_ID,
         property_ID %in% (daily %>% 
                             filter(housing,
                                    date >= "2020-06-01", 
                                    status == "B") %>% 
                             count(property_ID) %>% 
                             filter(n == 30))$property_ID) %>% 
  nrow() %>% round(digit=-1)

property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, housing, date == "2020-01-01", FREH_3 > 0.5)$property_ID,
         property_ID %in% (daily %>% 
                             filter(housing,
                                    date >= "2020-06-01", 
                                    status == "B") %>% 
                             count(property_ID) %>% 
                             filter(n > 15))$property_ID) %>% 
  nrow() %>% round(digit=-1)


property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, date == "2019-01-01", FREH_3 > 0.5)$property_ID,
         property_ID %in% (daily %>% 
                             filter(housing,
                                    date >= "2019-06-01", date <= "2019-06-30",
                                    status == "B") %>% 
                             count(property_ID) %>% 
                             filter(n == 30))$property_ID) %>% 
  nrow() %>% round(digit=-1)

property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, housing, date == "2019-01-01", FREH_3 > 0.5)$property_ID,
         property_ID %in% (daily %>% 
                             filter(housing,
                                    date >= "2019-06-01", date <= "2019-06-30", 
                                    status == "B") %>% 
                             count(property_ID) %>% 
                             filter(n > 15))$property_ID) %>% 
  nrow() %>% round(digit=-1)

# blocked for the last days of June (after ban)
property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, housing, date == "2020-01-01", FREH_3 > 0.5)$property_ID,
         property_ID %in% (daily %>% 
                             filter(housing,
                                    date >= "2020-06-25", date <= "2020-06-30",
                                    status == "B") %>% 
                             count(property_ID) %>% 
                             filter(n == 6))$property_ID) %>% 
  nrow() %>% round(digit=-1)

# behavior of non-commercial listings during ban ------------------------------
# numbers not in the text, but talked about at the end of chap.4

# proportion of non-commercial listings which were deleted by end of June.
property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, housing, date == "2020-01-01", FREH_3 < 0.5, multi == F)$property_ID,
         scraped < max(scraped) - months(1),
         scraped != "2020-05-21") %>% 
  nrow() /
  filter(daily, housing, date == "2020-01-01", FREH_3 < 0.5, multi == F) %>% 
  nrow()

property %>% 
  st_drop_geometry %>% 
  filter(property_ID %in% filter(daily, housing, date == "2019-01-01", FREH_3 < 0.5, multi == F)$property_ID,
         scraped < max(scraped) - months(1) - years(1)) %>% 
  nrow()/
  filter(daily, housing, date == "2019-01-01", FREH_3 < 0.5, multi == F) %>% 
  nrow()

# number of non-commercial listings blocked in June
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
