### load libraries and data ###########################################
library(tidyverse)
library(lubridate)
library(sf)
library(gt)
library(future)
library(ggplot2)

Sys.setenv(LANG = "en")

memory.limit(size = 48000)
plan(multiprocess, workers = 4)

load("data/montreal_str_processed_a.Rdata")

# Set up data

seasonality_maker <- function(daily, end_date) { 
  daily %>% 
    filter(date <= end_date, 
           date > ymd(end_date) - years(2)) %>% 
    mutate(yearmonth = zoo::as.yearmon(date)) %>% 
    group_by(yearmonth) %>% 
    summarize(
      res = sum(status == "R"),
      ava_res = sum(status == "R", status == "A"),
    ) %>% 
    mutate(
      month = lubridate::month(yearmonth), 
      season_res = as.vector(decompose(ts(res, frequency = 12),
                                        "multiplicative")$seasonal) / 12,
      season_ava_res  = as.vector(decompose(ts(ava_res,      frequency = 12),
                                        "multiplicative")$seasonal) / 12
    ) %>% 
    group_by(month, season_res, season_ava_res) %>% 
    summarise() %>% 
    ungroup() #%>% 
    # mutate(month = factor(month, levels=unique(month)))
}

seasonality <- 
  seasonality_maker(daily, "2019-12-31") %>% 
  mutate(res_min = season_res * 90,
         ava_res_min = season_ava_res * 183) %>% 
  select(-season_res, -season_ava_res)


SAFREH <- 
left_join(left_join( 
  (
    daily %>% 
     filter(date >= "2018-01-01", status == "R") %>% 
     mutate(year = lubridate::year(date),
            month = lubridate::month(date)) %>% 
     group_by(year, month) %>% 
     count(property_ID) %>% 
     rename(res = n)
   ),
  (
    daily %>% 
     filter(date >= "2018-01-01", status == "R" | status == "A") %>% 
      mutate(year = lubridate::year(date),
             month = lubridate::month(date)) %>% 
      group_by(year, month) %>% 
      count(property_ID) %>% 
     rename(ava_res = n)
    ),
  by = c("property_ID", "year", "month")
), seasonality, by = "month") %>% 
  filter(res >= res_min,
         ava_res >= ava_res_min) %>% 
  count(year, month)



SAFREH <- 
  SAFREH %>% 
  mutate(day = 15,
         date = as.Date(paste(year, month, day, sep="-")))



housing_loss %>% 
  mutate(`Listing type` = factor(
    `Listing type`, 
    levels = c("Summer listings", "Private room", "Entire home/apt"))) %>% 
  ggplot() +
  geom_col(data = SAFREH, aes(date, n), fill = col_palette[5])+
  geom_col(aes(date, `Housing units`, fill = `Listing type`),
           lwd = 0, alpha = 0.8) +
  theme_minimal() +
  # scale_y_continuous(name = NULL, label = scales::comma, limits = c(0, 200)) +
  scale_x_date(name = NULL, limits = c(as.Date("2018-01-01"), NA)) +
  scale_fill_manual(values = col_palette[3:1]) +
  theme(legend.position = "bottom", 
  )


