#### 30 CHAPTER 3 ANALYSIS ####################################################

#' This script produces the tables and facts for chapter 2. It runs quickly.
#' 
#' Output:
#' - None
#' 
#' Script dependencies:
#' - `02_geometry_import.R`
#' - `09_str_processing.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")


# Load previous data ------------------------------------------------------

load("output/str_processed.Rdata")
load("output/geometry.Rdata")


# Prepare new objects -----------------------------------------------------

# STR-induced housing loss - FREH LISTINGS
FREH <- 
daily %>% 
  filter(date >= "2016-01-01") %>% 
  group_by(date) %>% 
  summarize(across(c(FREH, FREH_3), sum)) %>%
  filter(substr(date, 9, 10) == "01")

#' [1] FREH on the first of January
FREH %>% 
  filter(date == "2020-01-01") %>% 
  pull(FREH_3) %>% 
  round(digit=-2)

#' [2] FREH increase between 2018 and 2019
(FREH %>% 
  filter(date == "2020-01-01") %>% 
  pull(FREH_3) -
  FREH %>% 
  filter(date == "2019-01-01") %>% 
  pull(FREH_3)
)/
  FREH %>% 
  filter(date == "2019-01-01") %>% 
  pull(FREH_3)


# Variation of FREH by boroughs -----------------------------------------------------

FREH_borough <- 
  daily %>% 
  filter(date >= "2016-01-01") %>% 
  group_by(date, borough) %>% 
  summarize(across(c(FREH, FREH_3), sum)) %>%
  filter(substr(date, 9, 10) == "01")

#' Table 3.1
FREH_borough %>% 
  filter(date == "2019-01-01") %>% 
  rename(`2018 FREH` = FREH_3) %>% 
  ungroup() %>% 
  select(-date, -FREH) %>% 
  left_join(
    FREH_borough %>% 
      filter(date == "2020-01-01") %>% 
      rename(`2019 FREH` = FREH_3) %>% 
      ungroup() %>% 
      select(-date, -FREH)
  ) %>% 
  add_row(borough = "City of Montreal", 
          `2018 FREH` = !! FREH %>% 
            filter(date == "2019-01-01") %>% 
            pull(FREH_3), 
          `2019 FREH` = !! FREH %>% 
            filter(date == "2020-01-01") %>% 
            pull(FREH_3)) %>% 
  filter(`2019 FREH` > 100) %>% 
  rename(Borough = borough) %>% 
  arrange(desc(`2019 FREH`)) %>%
  mutate(Variation = (`2019 FREH` - `2018 FREH`) / `2018 FREH`,
         `2019 FREH` = round(`2019 FREH`, digit = -1),
         `2018 FREH` = round(`2018 FREH`, digit = -1)) %>%
  gt() %>% 
  tab_header(
    title = "Borough breakdown",
    subtitle = "YOY FREH growth"
  ) %>%
  opt_row_striping() %>% 
  fmt_percent(columns = 4, decimals = 1) %>% 
  fmt_number(columns = 2:3,
             decimals = 0)


# STR-induced housing loss - GH LISTINGS ----------------------------------------------------- 

#' [1] average of active GH_units daily
GH %>% 
  st_drop_geometry() %>% 
  filter(date >= LTM_start_date, date <= LTM_end_date, status != "B") %>% 
  group_by(date) %>% 
  summarize(GH_units = sum(housing_units)) %>% 
  summarize(round(mean(GH_units), digit=-1))

#' [2] total ghost hostel listings
GH_total <-
  GH %>%
  st_drop_geometry() %>%
  filter(status != "B") %>% 
  group_by(date) %>%
  summarize(GH_units = sum(housing_units)) %>%
  mutate(GH_average = frollmean(GH_units, 30, align = "right", fill = 130))


# STR-induced housing loss - COMBINED HOUSING LOSS ----------------------------------------------------- 

housing_loss <-
  FREH %>%
  select(date, FREH_3) %>% 
  rename(`Entire home/apt` = FREH_3) %>%
  left_join(GH_total, by = "date") %>%
  select(-GH_units) %>% 
  rename(`Private room` = GH_average) %>%
  gather(`Entire home/apt`, `Private room`, 
         key = `Listing type`, value = `Housing units`) 

#' [1] housing loss in end 2019
sum(filter(housing_loss, date == "2020-01-01")$`Housing units`) %>% 
  round()

#' [2] housing loss variation
(housing_loss %>% 
    filter(date == "2020-01-01") %>% 
    summarize(sum(`Housing units`)) - 
    housing_loss %>% 
    filter(date == "2019-01-01") %>% 
    summarize(sum(`Housing units`))
) /
  housing_loss %>% 
  filter(date == "2019-01-01") %>% 
  summarize(sum(`Housing units`))

#' [3] Housing loss figure for a given year (the next day of the end of the year will give information on the previous year)
sum(filter(housing_loss, date == "2019-01-01")$`Housing units`) %>% 
  round(digits = -2)

#' [4] housing loss of family size units in 2019
(property %>% 
  st_drop_geometry() %>% 
  filter(bedrooms >= 2) %>% 
  select(property_ID, bedrooms) %>% 
  inner_join(daily) %>% 
  filter(date == "2020-01-01") %>% 
  summarize(sum(FREH_3)) + 
  GH %>% 
  st_drop_geometry() %>% 
  filter(date >= LTM_start_date, date <= LTM_end_date, status != "B") %>% 
  group_by(date) %>% 
  summarize(GH_units = sum(housing_units)) %>% 
  summarize(round(mean(GH_units)))) %>% 
  round(digit =-2)

#' [5] how many 2 bedrooms unit in housing loss
property %>% 
  st_drop_geometry() %>% 
  filter(bedrooms == 2) %>% 
  select(property_ID, bedrooms) %>% 
  inner_join(daily) %>% 
  filter(date == "2020-01-01") %>% 
  summarize(sum(FREH_3)) %>% 
  round(digits = -2)


# STRs and Montreal’s housing market indicators - Vacancy rates ----------------------------------------------------- 

#











#

# STRs and Montreal’s housing market indicators - Average rent -----------------------------------------------------

#













#

# STR-related increase in rent in Montreal, 2017-2019 -----------------------------------------------------






