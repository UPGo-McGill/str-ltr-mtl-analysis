#### 05 CMHC DATA IMPORT #######################################################

#' This script produces the `cmhc.Rdata` object. It should only be rerun when
#' CMHC data needs to be rebuilt from scratch. The necessary XLSX files can be
#' downloaded with lines 15-TK of the script on first run.

#' External dependencies:
#' - `shapefiles/CMHC_NBHD_2016-mercWGS84.shp`: CMHC neighbourhood shapefile
#' - `mtl_avg_rent.csv` & `mtl_vacancy.csv`: Tables downloaded from CMHC housing 
#'   market information portal (https://www03.cmhc-schl.gc.ca/hmip-pimh/)


source("R/01_startup.R")
library(tidyxl)
library(unpivotr)


# Download XLSX files if necessary ----------------------------------------

# # 2019 Montreal RMR data table
# download.file(paste0(
#   "https://assets.cmhc-schl.gc.ca/sites/cmhc/data-research/data-tables/", 
#   "rental-market-data/rmr-data-tables/2019/rmr-montreal-2019-en.xlsx?",
#   "rev=d8045cac-9e1a-4d95-85d8-1e239cf13747"), 
#   destfile = "data/montreal_rmr_2019.xlsx")

# # National average rents
# download.file(c(
#   paste0("https://assets.cmhc-schl.gc.ca/sf/project/cmhc/xls/data-tables/",
#          "average-apartment-rents-vacant-occupied/average-rents-vacant-",
#          "occupied-units-2015-en.xlsx?rev=e694194d-2d82-41dd-9ff5-",
#          "3fde3e8a2bbe"),
#   paste0(
#     "https://assets.cmhc-schl.gc.ca/sf/project/cmhc/xls/data-tables/",
#     "average-apartment-rents-vacant-occupied/average-rents-vacant-occupied-",
#     "units-2016-en.xlsx?rev=f7244d07-08d9-41ce-b150-08733d6ac975"),
#   paste0(
#     "https://assets.cmhc-schl.gc.ca/sf/project/cmhc/xls/data-tables/",
#     "average-apartment-rents-vacant-occupied/average-rents-vacant-occupied-",
#     "units-2017-en.xlsx?rev=4fffc203-9cf2-4c77-8c32-8c1eee9495e8"),
#   paste0(
#     "https://assets.cmhc-schl.gc.ca/sf/project/cmhc/xls/data-tables/",
#     "average-apartment-rents-vacant-occupied-urban-rental-market-survey/",
#     "average-rents-vacant-occupied-units-2018-en.xlsx?",
#     "rev=bafb8c36-28a2-4817-a629-eb4826206ebf"),
#   paste0(
#     "https://assets.cmhc-schl.gc.ca/sites/cmhc/data-research/data-tables/",
#     "average-rents-vacant-occupied-units/average-rents-vacant-occupied-units-",
#     "2019-en.xlsx?rev=8dbefa49-8770-4d89-bc1a-0e11060ed3b7")),
#   destfile = paste0("data/avg_rents_", 2015:2019, ".xlsx"))



# Helper functions to import tables ---------------------------------------

import_RMR <- function(data, var_name) {
  data %>% 
    filter(row >= 5) %>%
    select(row, col, data_type, character, numeric) %>%
    behead("up-left", bedroom) %>%
    behead("up-left", date) %>%
    behead("left", zone) %>%
    mutate(zone_name = str_remove(zone, 'Zone [:digit:]* - '),
           zone = as.numeric(str_extract(zone, '(?<=Zone )[:digit:]*')),
           date = if_else(date == "Oct-18", 2018, 2019)) %>% 
    select(zone, zone_name, date, bedroom, numeric, character) %>%
    group_by(zone, zone_name, bedroom, date) %>% 
    summarize({{var_name}} := numeric[1],
              quality = character[2],
              change = if_else(date == 2019, character[3], NA_character_),
              .groups = "drop") %>% 
    mutate(quality = if_else(is.na({{var_name}}), NA_character_, quality),
           change = case_when(change == "−" ~ "no change",
                              change == "↓" ~ "decrease",
                              change == "↑" ~ "increase",
                              is.na(change) ~ NA_character_)) %>% 
    distinct() %>% 
    {if (all(is.na(.$change))) select(., -change) else select(., everything())}
}

import_web_table <- function(data, var_name) {
  data %>% 
    rename_with(~unique(vacancy_2019$bedroom)[c(4, 1:3, 5)], 
                !starts_with("X")) %>% 
    rename(date = X1) %>%
    rename_with(~paste0(unique(vacancy_2019$bedroom)[c(4, 1:3, 5)], " - quality"), 
                c(1:5 * 2 + 1)) %>% 
    mutate(date = as.numeric(str_extract(date, "[:digit:]*"))) %>% 
    pivot_longer(c(where(is.numeric), -date), names_to = "bedroom", 
                 values_to = "var") %>% 
    pivot_longer(c(where(is.character), -bedroom), names_to = "temp", 
                 values_to = "quality") %>% 
    mutate(temp = str_remove(temp, " - quality")) %>% 
    filter(bedroom == temp) %>% 
    select(-temp) %>% 
    mutate(var = var / 100) %>% 
    rename({{var_name}} := var)
  }


# Import raw files --------------------------------------------------------

cmhc <- read_sf("data/shapefiles/CMHC_NBHD_2016-mercWGS84.shp")
vacancy_2019 <- xlsx_cells("data/montreal_rmr_2019.xlsx", "Table 3.1.1")
avg_rent_2019 <- xlsx_cells("data/montreal_rmr_2019.xlsx", "Table 3.1.2")
annual_vacancy <- read_csv("data/mtl_vacancy.csv", skip = 2, n_max = 18)
annual_avg_rent <- read_csv("data/mtl_avg_rent.csv", skip = 2, n_max = 18) %>% 
  select(1:11)


# Process shapefile -------------------------------------------------------

cmhc <- 
  cmhc %>% 
  filter(METCODE == "1060", NBHDCODE <= 360) %>% 
  select(ZONECODE, NAME_EN) %>% 
  rename(zone = ZONECODE, neighbourhood = NAME_EN) %>% 
  group_by(zone) %>% 
  summarize(name = paste0(neighbourhood, collapse = "/"), .group = "drop")


# Process 2019 vacancy rates ----------------------------------------------

vacancy_2019 <- vacancy_2019 %>% import_RMR(vacancy) %>% filter(zone <= 18)


# Process 2019 average rent -----------------------------------------------

avg_rent_2019 <- avg_rent_2019 %>% import_RMR(rent) %>% filter(zone <= 18)


# Process annual vacancy --------------------------------------------------

annual_vacancy <- annual_vacancy %>% import_web_table(vacancy)
              

# Process annual rents ----------------------------------------------------

annual_avg_rent <- annual_avg_rent %>% import_web_table(avg_rent)
  
