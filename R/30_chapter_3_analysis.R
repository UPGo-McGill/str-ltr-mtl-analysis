#### Chapter 3 ANALYSIS ####################################################

### load libraries ###########################################
library(tidyverse)
library(lubridate)
library(sf)
library(gt)
library(future)
library(ggplot2)
library(data.table)

### LTM start_date and end_date
LTM_start_date <- as.Date("2019-01-01")
LTM_end_date <- as.Date("2019-12-31")

### STR-induced housing loss - FREH LISTINGS  ######################################################

FREH %>% 
  filter(date == LTM_end_date) %>% 
  nrow()


# adding newer listings with FREH pattern to the number
dec2019_freh_properties <- 
  rbind( 
    daily_pred %>% 
      filter(date >= "2019-12-01", date < "2020-01-01",
             FREH_later == T) %>% 
      distinct(property_ID),
    FREH %>% 
      filter(date == LTM_end_date) %>% 
      distinct(property_ID)
  ) %>% 
  distinct(property_ID)


# look at every single listings that weren't considered as FREH, but added with the statistical model
daily_pred %>% 
  filter(date >= "2019-12-01", date < "2020-01-01",
         property_ID %in% dec2019_freh_properties$property_ID,
         !property_ID %in% filter(FREH, date == LTM_end_date)$property_ID, 
         FREH_later)


# bedrooms breakdown of FREH listings
property_2019 %>% 
  st_drop_geometry() %>%  
  filter(housing == TRUE,
         listing_type == "Entire home/apt",
         property_ID %in% dec2019_freh_properties$property_ID) %>% 
  count(bedrooms) %>% 
  mutate(percentage = n / sum(n)) %>% 
  group_by(bedrooms) %>% 
  summarize(`Percentage of listings` = sum(percentage)) %>% 
  filter(bedrooms >= 2) %>% 
  summarize(sum(`Percentage of listings`))
filter(`Percentage of listings` > 0.01) %>% 
  gt() %>% 
  tab_header(
    title = "LTM active properties, entire home/apt",
  ) %>%
  opt_row_striping() %>% 
  fmt_percent(columns = 2)


#2018-2019 YOY growth of FREH
(rbind( 
  daily_pred %>% 
    filter(date >= "2019-12-01", date < "2020-01-01",
           FREH_later == T) %>% 
    distinct(property_ID),
  FREH %>% 
    filter(date == LTM_end_date) %>% 
    distinct(property_ID)
) %>% 
    distinct(property_ID) %>% 
    nrow() -
    rbind( 
      daily_pred %>% 
        filter(date >= "2018-12-01", date < "2019-01-01",
               FREH_later == T) %>% 
        distinct(property_ID),
      FREH %>% 
        filter(date == LTM_end_date - years(1)) %>% 
        distinct(property_ID)
    ) %>% 
    distinct(property_ID) %>% 
    nrow()) /
  rbind( 
    daily_pred %>% 
      filter(date >= "2018-12-01", date < "2019-01-01",
             FREH_later == T) %>% 
      distinct(property_ID),
    FREH %>% 
      filter(date == LTM_end_date - years(1)) %>% 
      distinct(property_ID)
  ) %>% 
  distinct(property_ID) %>% 
  nrow()

# variation of FREH number per borough

boroughs_FREH_breakdown <- tibble(Borough = character(length = length(boroughs$borough)), 
                                  `2018 FREH` = numeric(length = length(boroughs$borough)),
                                  `2019 FREH` = numeric(length = length(boroughs$borough)),
                                  `Variation` = numeric(length = length(boroughs$borough)),
)


for (i in 1:length(boroughs$borough)) { # testing a bigger loop, good one with try(), does work. long, since not doing remotely.
  
  boroughs_FREH_breakdown[i,1] <- boroughs$borough[[i]]
  
  boroughs_FREH_breakdown[i,2] <- rbind( 
    daily_pred %>% 
      filter(date >= "2018-12-01", date < "2019-01-01",
             FREH_later == T) %>% 
      distinct(property_ID),
    FREH %>% 
      filter(date == LTM_end_date - years (1)) %>% 
      distinct(property_ID)
  ) %>% 
    distinct(property_ID) %>% 
    inner_join(select(distinct(daily, property_ID, .keep_all = T), property_ID, borough), by = "property_ID") %>% 
    filter(borough == boroughs$borough[[i]]) %>% 
    nrow()
  
  boroughs_FREH_breakdown[i,3] <- rbind( 
    daily_pred %>% 
      filter(date >= "2019-12-01", date < "2020-01-01",
             FREH_later == T) %>% 
      distinct(property_ID),
    FREH %>% 
      filter(date == LTM_end_date) %>% 
      distinct(property_ID)
  ) %>% 
    distinct(property_ID) %>% 
    inner_join(select(distinct(daily, property_ID, .keep_all = T), property_ID, borough), by = "property_ID") %>% 
    filter(borough == boroughs$borough[[i]]) %>% 
    nrow()
  
  boroughs_FREH_breakdown[i,4] <- (rbind( 
    daily_pred %>% 
      filter(date >= "2019-12-01", date < "2020-01-01",
             FREH_later == T) %>% 
      distinct(property_ID),
    FREH %>% 
      filter(date == LTM_end_date) %>% 
      distinct(property_ID)
  ) %>% 
    distinct(property_ID) %>% 
    inner_join(select(distinct(daily, property_ID, .keep_all = T), property_ID, borough), by = "property_ID") %>% 
    filter(borough == boroughs$borough[[i]]) %>% 
    nrow() - rbind( 
      daily_pred %>% 
        filter(date >= "2018-12-01", date < "2019-01-01",
               FREH_later == T) %>% 
        distinct(property_ID),
      FREH %>% 
        filter(date == LTM_end_date - years (1)) %>% 
        distinct(property_ID)
    ) %>% 
    distinct(property_ID) %>% 
    inner_join(select(distinct(daily, property_ID, .keep_all = T), property_ID, borough), by = "property_ID") %>% 
    filter(borough == boroughs$borough[[i]]) %>% 
    nrow()) / rbind( 
      daily_pred %>% 
        filter(date >= "2018-12-01", date < "2019-01-01",
               FREH_later == T) %>% 
        distinct(property_ID),
      FREH %>% 
        filter(date == LTM_end_date - years (1)) %>% 
        distinct(property_ID)
    ) %>% 
    distinct(property_ID) %>% 
    inner_join(select(distinct(daily, property_ID, .keep_all = T), property_ID, borough), by = "property_ID") %>% 
    filter(borough == boroughs$borough[[i]]) %>% 
    nrow() 
  
}


boroughs_FREH_breakdown %>% 
  filter(`2019 FREH` > 100) %>% 
  arrange(desc(`2019 FREH`)) %>%
  mutate(`2019 FREH` = round(`2019 FREH`, digit = -1),
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

GH %>% 
  st_drop_geometry() %>% 
  filter(date >= LTM_start_date, date <= LTM_end_date) %>% 
  group_by(date) %>% 
  summarize(GH_units = sum(housing_units)) %>% 
  summarize(round(mean(GH_units), digit=-1))


GH %>% 
  st_drop_geometry() %>% 
  group_by(date) %>% 
  summarize(GH_units = sum(housing_units)) %>%
  filter(date >= "2020-01-01") %>% 
  View()
ggplot() +
  geom_line(aes(date, GH_units), colour = "black", size = 1) +
  theme_minimal() +
  scale_y_continuous(name = NULL) +
  ggtitle("Units converted to ghost hostels in Montreal")


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
  group_by(date) %>%
  summarize(`Entire home/apt` = n()) %>%
  left_join(GH_total, by = "date") %>%
  rename(`Private room` = GH_average) %>%
  gather(`Entire home/apt`, `Private room`, 
         key = `Listing type`, value = `Housing units`) 

# housing loss variation
(housing_loss %>% 
    filter(date == LTM_end_date) %>% 
    summarize(sum(`Housing units`)) - 
    housing_loss %>% 
    filter(date == LTM_end_date - years(1)) %>% 
    summarize(sum(`Housing units`))
) /
  housing_loss %>% 
  filter(date == LTM_end_date - years(1)) %>% 
  summarize(sum(`Housing units`))


# Current housing loss figure
sum(filter(housing_loss, date == "2019-12-31")$`Housing units`)


# housing loss of family size units.
property %>% 
  filter(property_ID %in% filter(FREH, date == "2019-12-31")$property_ID,
         bedrooms == 2)


### STRs and Montreal’s housing market indicators - Vacancy rates ######################################################

#











#

### STRs and Montreal’s housing market indicators - Average rent ######################################################


#













#


### STR-related increase in rent in Montreal, 2017-2019 ######################################################






