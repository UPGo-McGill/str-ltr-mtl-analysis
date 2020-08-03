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
# 
# # National vacancy
# download.file(c(
#   paste0(
#     "https://assets.cmhc-schl.gc.ca/sf/project/cmhc/pubsandreports/excel/",
#     "rms-3-urban-vacancy-rates-by-bedroom-type-2015-10.xlsx?",
#     "rev=4f908e16-717e-4ed8-8eef-e6ca941cdc27"),
#   paste0(
#     "https://assets.cmhc-schl.gc.ca/sf/project/cmhc/pubsandreports/excel/",
#     "rms-3-urban-vacancy-rates-by-bedroom-type-2016-10.xlsx?",
#     "rev=95ee1335-6b23-43ba-b3c4-e8b5e7433a3f"),
#   paste0(
#     "https://assets.cmhc-schl.gc.ca/sf/project/cmhc/pubsandreports/excel/",
#     "rms-3-urban-vacancy-rates-by-bedroom-type-2017-10.xlsx?",
#     "rev=6bfe023e-691e-421b-a41f-4f9ff4204661"),
#   paste0(
#     "https://assets.cmhc-schl.gc.ca/sites/cmhc/data-research/data-tables/",
#     "urban-rental-market-survey-data/2018/urban-rental-market-survey-data-",
#     "vacancy-rates-2018-10-en.xlsx?rev=7427253e-44d0-4ec6-a62c-7afc7c249855"),
#   paste0(
#     "https://assets.cmhc-schl.gc.ca/sites/cmhc/data-research/data-tables/",
#     "urban-rental-market-survey-data/2019/urban-rental-market-survey-data-",
#     "vacancy-rates-2019.xlsx?rev=15373968-504f-42e8-a277-b1633b21553d")),
#   destfile = paste0("data/annual_vacancy_", 2015:2019, ".xlsx"))
# 
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
#   destfile = paste0("data/annual_avg_rent_", 2015:2019, ".xlsx"))


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

import_annual_vacancy <- function(data, year) {
  
  province_names <- 
    cancensus::list_census_regions("CA16", quiet = TRUE) %>% 
    filter(level == "PR") %>% 
    pull(name) %>% 
    sort()
  
  data %>% 
    filter(row >= 5) %>%
    select(row, col, data_type, character, numeric) %>%
    behead("up-left", bedroom) %>% 
    behead("left", province) %>% 
    behead("left", centre) %>% 
    behead("left", zone) %>% 
    behead("left", neighbourhood) %>% 
    behead("left", dwelling_type) %>% 
    mutate(date = year) %>% 
    select(province, centre, zone, neighbourhood, dwelling_type, date, bedroom, 
           character) %>% 
    mutate(province = 
             case_when(province == "Alta/Alb."          ~ province_names[[1]],
                       province == "B.C./C.-B."         ~ province_names[[2]],
                       province == "Man./Man."          ~ province_names[[3]],
                       province == "N.B./N.-B."         ~ province_names[[4]],
                       province == "Nfld.Lab./T.-N.-L." ~ province_names[[5]],
                       province == "N.S./N.-É."         ~ province_names[[7]],
                       province == "Ont./Ont."          ~ province_names[[9]],
                       province == "Que/Qc"             ~ province_names[[11]],
                       province == "Sask./Sask."        ~ province_names[[12]])
    ) %>% 
    filter(!is.na(province)) %>% 
    mutate(dwelling_type = case_when(
      dwelling_type == "Row / En\r\nbande" ~ "Row",
      dwelling_type %in% c("Apt &\r\nOther /\r\nApp. &\r\nautres",
                           "Apt & Other /\r\nApp. & autres",
                           "Apt & Other /\r\nApp. &\r\nautres") ~ 
        "Apartment and other",
      TRUE ~ dwelling_type
    )) %>% 
    mutate(bedroom = case_when(
      str_detect(bedroom, "Bachelor") ~ "Bachelor",
      str_detect(bedroom, "1") ~ "1 Bedroom",
      str_detect(bedroom, "2") ~ "2 Bedroom",
      str_detect(bedroom, "3") ~ "3 Bedroom +",
      str_detect(bedroom, "Total") ~ "Total"
    )) %>% 
    group_by(date, province, centre, zone, neighbourhood, dwelling_type, 
             bedroom) %>% 
    summarize(
      vacancy = character[1],
      quality = character[2],
      .groups = "drop_last") %>% 
    ungroup() %>% 
    mutate(
      vacancy = if_else(str_detect(vacancy, "%"),
                        as.numeric(str_remove(vacancy, "%")) / 100,
                        NA_real_)) %>% 
    mutate(quality = if_else(quality %in% c("a", "b", "c", "d"), quality,
                             NA_character_))
}

import_annual_avg_rent <- function(data) {
  
  data <-
    data %>% 
    filter(row >= 9) %>%
    select(row, col, data_type, character, numeric) %>%
    behead("up-left", bedroom) %>% 
    behead("up-left", occupied_status) %>% 
    behead("left", zone) %>% 
    behead("left", date) %>% 
    filter(!is.na(zone), zone != "", 
           !str_starts(zone, "(\\*)|(a - E)|(The f)")) %>%
    mutate(heading = if_else(!str_starts(zone, "Zone ") & str_ends(zone, " CMA"), 
                             TRUE, FALSE)) %>% 
    arrange(row, col)
  
  indices <- 
    test %>% 
    filter(heading == TRUE) %>% 
    pull(row) %>% 
    unique()
  
  data %>% 
    rowwise() %>% 
    mutate(index_group = min(indices[indices >= row])) %>% 
    ungroup() %>% 
    group_by(index_group) %>% 
    mutate(CMA = first(zone[row == index_group])) %>% 
    ungroup() %>% 
    select(CMA, zone, bedroom, occupied_status, date, character, numeric) %>% 
    group_by(CMA, zone, bedroom, occupied_status, date) %>% 
    summarize(avg_rent = first(numeric), quality = last(character), 
              .groups = "drop") %>% 
    group_by(CMA, zone, bedroom) %>% 
    summarize(occupied_status = occupied_status[1:2],
              date = date[1:2],
              avg_rent = avg_rent[1:2],
              occ_rent_higher = rep(if_else(quality[3] == "Y", TRUE, FALSE), 2),
              quality = quality[1:2],
              .groups = "drop") %>% 
    relocate(occ_rent_higher, .after = last_col()) %>% 
    mutate(zone_name = str_remove(zone, 'Zone [:digit:]* - '),
           zone = as.numeric(str_extract(zone, '(?<=Zone )[:digit:]*'))) %>% 
    relocate(zone_name, .after = zone) %>% 
    mutate(quality = if_else(quality %in% c("a", "b", "c", "d"), quality,
                             NA_character_))
}


# Import raw files --------------------------------------------------------

cmhc <- read_sf("data/shapefiles/CMHC_NBHD_2016-mercWGS84.shp")
vacancy_2019 <- xlsx_cells("data/montreal_rmr_2019.xlsx", "Table 3.1.1")
avg_rent_2019 <- xlsx_cells("data/montreal_rmr_2019.xlsx", "Table 3.1.2")
city_vacancy <- read_csv("data/mtl_vacancy.csv", skip = 2, n_max = 18)
city_avg_rent <- read_csv("data/mtl_avg_rent.csv", skip = 2, n_max = 18) %>% 
  select(1:11)
annual_vacancy <- paste0("data/annual_vacancy_", 2015:2019, ".xlsx") %>% 
  map(xlsx_cells, "Neighbourhood - Quartier")
annual_avg_rent <- paste0("data/annual_avg_rent_", 2015:2019, ".xlsx") %>% 
  map(xlsx_cells)


# Process shapefile -------------------------------------------------------

cmhc <- 
  cmhc %>% 
  filter(METCODE == "1060", NBHDCODE <= 360) %>% 
  select(ZONECODE, NAME_EN) %>% 
  rename(zone = ZONECODE, neighbourhood = NAME_EN) %>% 
  group_by(zone) %>% 
  summarize(name = paste0(neighbourhood, collapse = "/"), .group = "drop")


# Process 2019 vacancy and average rent -----------------------------------

vacancy_2019 <- vacancy_2019 %>% import_RMR(vacancy) %>% filter(zone <= 18)
avg_rent_2019 <- avg_rent_2019 %>% import_RMR(rent) %>% filter(zone <= 18)


# Process annual city vacancy and average rent ----------------------------

city_vacancy <- city_vacancy %>% import_web_table(vacancy)
city_avg_rent <- city_avg_rent %>% import_web_table(avg_rent)
  

# Process annual zone vacancy and average rent ----------------------------

annual_vacancy <-
  annual_vacancy %>% 
  map2_dfr(2015:2019, import_annual_vacancy) %>% 
  filter(centre == "Montréal", neighbourhood == "Total") %>% 
  select(-neighbourhood)

annual_avg_rent <- annual_avg_rent %>% map_dfr(import_annual_avg_rent) %>% 
  filter(CMA == "Montréal CMA", zone <= 18)

