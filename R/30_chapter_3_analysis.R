#### Chapter 3 ANALYSIS ####################################################

source("R/01_startup.R")
load("output/str_processed.Rdata")
load("output/geometry.Rdata")


### STR-induced housing loss - FREH LISTINGS  ######################################################
FREH <- 
daily %>% 
  filter(date >= "2016-01-01") %>% 
  group_by(date) %>% 
  summarize(across(c(FREH, FREH_3), sum)) %>%
  filter(substr(date, 9, 10) == "01")

FREH %>% 
  filter(date == "2020-01-01") %>% 
  pull(FREH_3) %>% 
  round(digit=-2)

# increase since last year
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


# variation of FREH number per borough

boroughs_FREH_breakdown <- tibble(Borough = character(length = length(boroughs$borough)), 
                                  `2018 FREH` = numeric(length = length(boroughs$borough)),
                                  `2019 FREH` = numeric(length = length(boroughs$borough)),
                                  `Variation` = numeric(length = length(boroughs$borough)),
)

FREH_borough <- 
  daily %>% 
  filter(date >= "2016-01-01") %>% 
  group_by(date, borough) %>% 
  summarize(across(c(FREH, FREH_3), sum)) %>%
  filter(substr(date, 9, 10) == "01")
  


for (i in 1:length(boroughs$borough)) { # testing a bigger loop, good one with try(), does work. long, since not doing remotely.
  
  boroughs_FREH_breakdown[i,1] <- boroughs$borough[[i]]
  
  boroughs_FREH_breakdown[i,2] <- 
    FREH_borough %>% 
       filter(date == "2019-01-01", borough == boroughs$borough[[i]]) %>% 
       pull(FREH_3)
  
  boroughs_FREH_breakdown[i,3] <- 
    FREH_borough %>% 
       filter(date == "2020-01-01", borough == boroughs$borough[[i]]) %>% 
       pull(FREH_3) 
  
}

as.data.frame(c("City of Montreal"), FREH %>% 
                filter(date == "2020-01-01") %>% 
                pull(FREH_3),
              FREH %>% 
                filter(date == "2019-01-01") %>% 
                pull(FREH_3) )

as.data.frame(Borough = character(c("City of Montreal")), 
       `2018 FREH` = numeric(FREH %>% 
                               filter(date == "2020-01-01") %>% 
                               pull(FREH_3)),
       `2019 FREH` = numeric(FREH %>% 
                               filter(date == "2019-01-01") %>% 
                               pull(FREH_3)),
       `Variation` = numeric(),
)
  

boroughs_FREH_breakdown %>% 
  add_row(Borough = "City of Montreal", 
          `2018 FREH` = FREH %>% 
            filter(date == "2019-01-01") %>% 
            pull(FREH_3), 
          `2019 FREH` = FREH %>% 
            filter(date == "2020-01-01") %>% 
            pull(FREH_3), `Variation` = 0) %>% 
  filter(`2019 FREH` > 100) %>% 
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


### STR-induced housing loss - GH LISTINGS  ######################################################

# average of active GH_units daily
GH %>% 
  st_drop_geometry() %>% 
  filter(date >= LTM_start_date, date <= LTM_end_date, status != "B") %>% 
  group_by(date) %>% 
  summarize(GH_units = sum(housing_units)) %>% 
  summarize(round(mean(GH_units), digit=-1))

GH_total <-
  GH %>%
  st_drop_geometry() %>%
  filter(status != "B") %>% 
  group_by(date) %>%
  summarize(GH_units = sum(housing_units)) %>%
  mutate(GH_average = frollmean(GH_units, 30, align = "right", fill = 198))

### STR-induced housing loss - Combined housing loss ################################################

housing_loss <-
  FREH %>%
  select(date, FREH_3) %>% 
  rename(`Entire home/apt` = FREH_3) %>%
  left_join(GH_total, by = "date") %>%
  select(-GH_units) %>% 
  rename(`Private room` = GH_average) %>%
  gather(`Entire home/apt`, `Private room`, 
         key = `Listing type`, value = `Housing units`) 

# housing loss in end 2019
sum(filter(housing_loss, date == "2020-01-01")$`Housing units`) %>% 
  round()

# housing loss variation
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


# Housing loss figure for a given year (the next day of the end of the year will give information on the previous year)
sum(filter(housing_loss, date == "2019-01-01")$`Housing units`) %>% 
  round(digits = -2)


# housing loss of family size units in 2019
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

#how many 2 bedrooms unit in housing loss
property %>% 
  st_drop_geometry() %>% 
  filter(bedrooms == 2) %>% 
  select(property_ID, bedrooms) %>% 
  inner_join(daily) %>% 
  filter(date == "2020-01-01") %>% 
  summarize(sum(FREH_3)) %>% 
  round(digits = -2)


### STRs and Montreal’s housing market indicators - Vacancy rates ######################################################

#











#

### STRs and Montreal’s housing market indicators - Average rent ######################################################


#













#


### STR-related increase in rent in Montreal, 2017-2019 ######################################################






