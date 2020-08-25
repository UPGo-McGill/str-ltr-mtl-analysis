#### 30 CHAPTER 3 ANALYSIS ####################################################

#' This script produces the tables and facts for chapter 3. It runs quickly.
#' 
#' Output:
#' - None
#' 
#' Script dependencies:
#' - `02_geometry_import.R`
#' - `05_cmhc_data_import.R`
#' - `09_str_processing.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")


# Load previous data ------------------------------------------------------

load("output/str_processed.Rdata")
load("output/geometry.Rdata")
load("output/cmhc.Rdata")


# Prepare new objects -----------------------------------------------------

FREH <- 
  daily %>% 
  filter(date >= "2016-01-01") %>% 
  group_by(date) %>% 
  summarize(across(c(FREH, FREH_3), sum)) %>%
  filter(substr(date, 9, 10) == "01")

FREH_borough <- 
  daily %>% 
  filter(date %in% as.Date(c("2018-12-01", "2019-12-01"))) %>% 
  group_by(borough, date) %>% 
  summarize(FREH = sum(FREH_3)) %>% 
  mutate(date = if_else(date >= "2019-01-01", 2019, 2018))

GH_borough <- 
  GH %>% 
  filter(date %in% as.Date(c("2018-12-31", "2019-12-31"))) %>% 
  mutate(geometry = st_centroid(geometry)) %>% 
  st_join(boroughs) %>% 
  st_drop_geometry() %>% 
  group_by(borough, date) %>% 
  summarize(GH = sum(housing_units, na.rm = TRUE)) %>% 
  as_tibble() %>% 
  mutate(date = if_else(date >= "2019-01-01", 2019, 2018))


# STR-induced housing loss ------------------------------------------------

#' At the end of 2019, there were 5,150 [1] FREH listings in the City of 
#' Montreal, and 370 [2] more housing units which were operating as ghost 
#' hostels. In total, therefore, short-term rentals removed 5,520 [3] housing 
#' units from Montreal’s long-term market last year (Figure 3.1). Notably, while 
#' the number of active daily listings declined by 5.6% over 2019, the number of 
#' housing units which STRs took off of Montreal’s housing market increased by 
#' 16.6% [4] in that same time period, from 4,740 [5] to 5,520.

#' [1] FREH in 2019-1
FREH %>% filter(date == "2019-12-01") %>% pull(FREH_3) %>% round(digit = -1)

#' [2] GH in 2019-12
GH %>% filter(date == "2019-12-31") %>% pull(housing_units) %>% sum() %>% 
  round(digit = -1)

#' [3] Total housing loss for 2019
{{FREH %>% filter(date == "2019-12-01") %>% pull(FREH_3)} +
    {GH %>% filter(date == "2019-12-31") %>% pull(housing_units) %>% sum()}} %>% 
  round(digit = -1)

#' [4] YOY increase in housing loss
{{FREH %>% filter(date == "2019-12-01") %>% pull(FREH_3)} +
    {GH %>% filter(date == "2019-12-31") %>% pull(housing_units) %>% sum()}} /
  {{FREH %>% filter(date == "2018-12-01") %>% pull(FREH_3)} +
      {GH %>% filter(date == "2018-12-31") %>% pull(housing_units) %>% sum()}}

#' [5] Total housing loss for 2018
{{FREH %>% filter(date == "2018-12-01") %>% pull(FREH_3)} +
  {GH %>% filter(date == "2018-12-31") %>% pull(housing_units) %>% sum()}} %>% 
  round(digit = -1)

#' At the end of 2019 more than six in ten (62.0% [1]) entire-home listings 
#' and one in four (26.1% [1]) private-room listings were taking housing off 
#' the market in Montreal (Figure 3.2). Three years earlier, the proportions 
#' were only 34.2% [1] and 12.7% [1] respectively.

#' [1] Housing loss shares
daily %>% 
  filter(housing, status != "B", listing_type %in% c("Entire home/apt",
                                                     "Private room")) %>% 
  group_by(date) %>% 
  summarize(eh = mean(FREH_3[listing_type == "Entire home/apt"] > 0.5),
            pr = mean(GH[listing_type == "Private room"])) %>% 
  mutate(across(c(eh, pr), slide_dbl, mean, .before = 30)) %>% 
  filter(date %in% as.Date(c("2016-12-31", "2019-12-31")))

#' The 5,520 housing units taken off of Montreal’s housing market in 2019 is 
#' only 0.7% [1] of the total amount of housing in the city, but this housing 
#' loss has been concentrated in a small part of the city.... In the borough 
#' of Ville-Marie, 3.2% [2] of all housing units have been converted to 
#' dedicated STRs, while the figure is 2.2% [2] for Le Plateau-Mont-Royal. In 
#' Ville-Marie the rental vacancy rate was 2.5% [3] in 2019, while it was 
#' 1.5% [4] in Le Plateau-Mont-Royal in 2018 (CMHC did not release a number for 
#' 2019). This means that there are more dedicated STRs in these neighbourhoods 
#' than there are vacant apartments for rent.

#' [1] Total housing loss as % of dwellings
{{{FREH %>% filter(date == "2019-12-01") %>% pull(FREH_3)} +
    {GH %>% filter(date == "2019-12-31") %>% pull(housing_units) %>% sum()}} /
  sum(boroughs$dwellings)} %>% 
  round(3)

#' [2] Housing loss in VM and LPMR
boroughs %>% 
  left_join(FREH_borough) %>% 
  left_join(GH_borough) %>% 
  filter(date == "2019") %>% 
  mutate(GH = if_else(is.na(GH), 0L, GH),
         housing_loss_per_dwelling = (FREH + GH) / dwellings,
         housing_loss_per_dwelling = round(housing_loss_per_dwelling, 3)) %>% 
  st_drop_geometry() %>% 
  filter(borough %in% c("Ville-Marie", "Le Plateau-Mont-Royal"))

#' [3] Vacancy rates in VM and LPMR
annual_vacancy %>% 
  filter(dwelling_type == "Total",
         bedroom == "Total",
         zone %in% c(1, 6),
         !is.na(vacancy)) %>% 
  group_by(zone) %>% 
  filter(date == max(date)) %>% 
  ungroup()

#' Table 3.1
borough_housing_table <- 
  boroughs %>% 
  st_drop_geometry() %>% 
  left_join(FREH_borough) %>% 
  left_join(GH_borough) %>%
  mutate(GH = if_else(is.na(GH), 0L, GH)) %>% 
  group_by(borough) %>% 
  summarize(
    dwellings = mean(dwellings),
    housing_loss_2018 = sum(FREH[date == 2018]) + sum(GH[date == 2018]),
    housing_loss_2019 = sum(FREH[date == 2019]) + sum(GH[date == 2019]),
    yoy_change = (housing_loss_2019 - housing_loss_2018) / housing_loss_2018,
    housing_loss_pct_2019 = housing_loss_2019 / dwellings)
  
borough_housing_table %>% 
  summarize(
    dwellings = sum(dwellings),
    housing_loss_2018 = sum(housing_loss_2018),
    housing_loss_2019 = sum(housing_loss_2019),
    yoy_change = (housing_loss_2019 - housing_loss_2018) / housing_loss_2018,
    housing_loss_pct_2019 = housing_loss_2019 / dwellings) %>% 
  mutate(borough = "City of Montreal") %>% 
  bind_rows(borough_housing_table) %>% 
  relocate(borough) %>% 
  select(-dwellings) %>% 
  filter(housing_loss_2019 > 50) %>% 
  mutate(across(c(housing_loss_2018, housing_loss_2019), round, -1)) %>% 
  mutate(across(c(yoy_change, housing_loss_pct_2019), round, 4)) %>% 
  arrange(desc(housing_loss_2019)) %>%
  rename(Borough = borough,
         `Housing loss (2019)` = housing_loss_2019,
         `Housing loss (2018)` = housing_loss_2018,
         `Year-over-year growth (%)` = yoy_change,
         `% of housing lost (2019)` = housing_loss_pct_2019) %>% 
  gt() %>% 
  tab_header(title = "STR-induced housing loss by borough") %>%
  opt_row_striping() %>% 
  fmt_percent(columns = c(4, 5), decimals = 1) %>% 
  fmt_number(columns = 2:3, decimals = 0)


# The impact of STRs on residential rents ---------------------------------








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






