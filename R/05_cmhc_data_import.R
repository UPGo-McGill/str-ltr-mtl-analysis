#### 05 CMHC DATA IMPORT #######################################################

#' This script should only be rerun when CMHC data needs to be rebuilt from 
#' scratch. The necessary XLSX files can be downloaded with lines 24-100 of the 
#' script on first run.
#' 
#' Output:
#' - `cmhc.Rdata`
#' 
#' Script dependencies:
#' - None
#' 
#' External dependencies:
#' - `CMHC_NBHD_2016-mercWGS84.shp`: CMHC neighbourhood shapefile
#' - `mtl_units.csv`, `mtl_avg_rent.csv` & `mtl_vacancy.csv`: Tables downloaded 
#'   from CMHC housing market information portal 
#'   (https://www03.cmhc-schl.gc.ca/hmip-pimh/)

source("R/01_startup.R")
library(tidyxl)
library(unpivotr)


# Download XLSX files if necessary ----------------------------------------

# # National units
# download.file(c(
#   paste0(
#     "https://assets.cmhc-schl.gc.ca/sf/project/cmhc/pubsandreports/excel/", 
#     "rms-1-urban-units-rental-universe-by-bedroom-type-2015-10.xlsx?", 
#     "rev=953481b3-42bd-4ecc-98d8-9747624f94ff"),
#   paste0(
#     "https://assets.cmhc-schl.gc.ca/sf/project/cmhc/pubsandreports/excel/", 
#     "rms-1-urban-units-rental-universe-by-bedroom-type-2016-10.xlsx?", 
#     "rev=c74ecdb7-2d3b-4da5-a4eb-428579041dc5"),
#   paste0(
#     "https://assets.cmhc-schl.gc.ca/sf/project/cmhc/pubsandreports/excel/", 
#     "rms-1-urban-units-rental-universe-by-bedroom-type-2017-10.xlsx?", 
#     "rev=1eb7624c-c220-4194-89e8-dc4883fb3da2"),
#   paste0(
#     "https://assets.cmhc-schl.gc.ca/sites/cmhc/data-research/data-tables/", 
#     "urban-rental-market-survey-data/2018/urban-rental-market-survey-data-", 
#     "number-units-2018-10-en.xlsx?rev=62f1b6b0-a004-41b0-bfe4-39fadf86f49b"),
#   paste0(
#     "https://assets.cmhc-schl.gc.ca/sites/cmhc/data-research/data-tables/",
#     "urban-rental-market-survey-data-number-units/urban-rental-market-", 
#     "survey-data-number-units-2019.xlsx?rev=c86bcd61-72d3-42b7-a782-", 
#     "f076e9edbc7d")),
#   destfile = paste0("data/cmhc/annual_units_", 2015:2019, ".xlsx"))
#
#
# # National average rents
# download.file(c(
#   paste0(
#     "https://assets.cmhc-schl.gc.ca/sf/project/cmhc/xls/data-tables/",
#     "average-apartment-rents-vacant-occupied/average-rents-vacant-occupied-", 
#     "units-2015-en.xlsx?rev=e694194d-2d82-41dd-9ff5-3fde3e8a2bbe"),
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
#   destfile = paste0("data/cmhc/annual_avg_rent_", 2015:2019, ".xlsx"))
#
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
#   destfile = paste0("data/cmhc/annual_vacancy_", 2015:2019, ".xlsx"))


# Helper functions to import tables ---------------------------------------

import_web_table <- function(data, var_name, quality = TRUE) {
  
  data <- 
    data %>% 
    rename(date = X1) %>%
    mutate(date = as.numeric(str_extract(date, "[:digit:]*")))
  
  if (!quality) {
    data %>% 
      select(1:6) %>% 
      pivot_longer(-date, names_to = "bedroom", values_to = "var") %>% 
      rename({{var_name}} := var)
    
  } else {
    data %>% 
      select(1:11) %>% 
      rename_with(~paste0(names(data)[1:5 * 2], " - quality"), 
                  c(1:5 * 2 + 1)) %>% 
      pivot_longer(c(where(is.numeric), -date), names_to = "bedroom", 
                   values_to = "var") %>% 
      pivot_longer(c(where(is.character), -bedroom), names_to = "temp", 
                   values_to = "quality") %>% 
      mutate(temp = str_remove(temp, " - quality")) %>% 
      filter(bedroom == temp) %>% 
      select(-temp) %>% 
      rename({{var_name}} := var)
  }
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
    mutate(heading = if_else(!str_starts(zone, "Zone ") & 
                               str_ends(zone, " CMA"), TRUE, FALSE)) %>% 
    arrange(row, col)
  
  indices <- 
    data %>% 
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
    select(date, CMA, zone, bedroom, occupied_status, character, numeric) %>% 
    group_by(date, CMA, zone, bedroom, occupied_status) %>% 
    summarize(avg_rent = first(numeric), quality = last(character), 
              .groups = "drop") %>% 
    group_by(date, CMA, zone, bedroom) %>% 
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

import_annual_units <- function(data, year) {
  
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
    mutate(date = year, 
           character = parse_number(character),
           numeric = if_else(is.na(numeric), character, numeric)) %>% 
    select(date, province, centre, zone, neighbourhood, dwelling_type, bedroom, 
           units = numeric) %>% 
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
    ))
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


# Process annual city tables ----------------------------------------------

city_units <- read_csv("data/cmhc/mtl_units.csv", skip = 2, n_max = 18)
city_avg_rent <- read_csv("data/cmhc/mtl_avg_rent.csv", skip = 2, n_max = 18)
city_vacancy <- read_csv("data/cmhc/mtl_vacancy.csv", skip = 2, n_max = 18)

city_avg_rent <- city_avg_rent %>% import_web_table(avg_rent)
city_units <- city_units %>% import_web_table(units, quality = FALSE)
city_vacancy <- city_vacancy %>% import_web_table(vacancy) %>% 
  mutate(vacancy = vacancy / 100)
  

# Process annual zone tables ----------------------------------------------

annual_avg_rent <- paste0("data/cmhc/annual_avg_rent_", 2015:2019, ".xlsx") %>% 
  map(xlsx_cells)
annual_units <- paste0("data/cmhc/annual_units_", 2015:2019, ".xlsx") %>% 
  map(xlsx_cells, "Neighbourhood - Quartier")
annual_vacancy <- paste0("data/cmhc/annual_vacancy_", 2015:2019, ".xlsx") %>% 
  map(xlsx_cells, "Neighbourhood - Quartier")

annual_avg_rent <- 
  annual_avg_rent %>% 
  map_dfr(import_annual_avg_rent) %>% 
  filter(CMA == "Montréal CMA", zone <= 18) %>% 
  mutate(zone_name = if_else(zone == 6, "Plateau-Mont-Royal", zone_name)) %>% 
  select(-CMA)

zones <- 
  annual_avg_rent %>% 
  group_by(zone, zone_name) %>% 
  summarize()

annual_units <- 
  annual_units %>% 
  map2_dfr(2015:2019, import_annual_units) %>% 
  filter(centre == "Montréal", neighbourhood == "Total") %>% 
  select(-province, -centre, -neighbourhood) %>% 
  mutate(zone = case_when(
    str_detect(zone, "Downtown")      ~ 1,
    str_detect(zone, "Verdun")        ~ 2,
    str_detect(zone, "LaSalle")       ~ 3,
    str_detect(zone, "ND-de-G")       ~ 4,
    str_detect(zone, "Ct-des")        ~ 5,
    str_detect(zone, "Plateau")       ~ 6,
    str_detect(zone, "Villeray")      ~ 7,
    str_detect(zone, "Hochelag")      ~ 8,
    str_detect(zone, "Rosemont")      ~ 9,
    str_detect(zone, "Anjou")         ~ 10,
    str_detect(zone, "-Nord")         ~ 11,
    str_detect(zone, "Ahuntsic")      ~ 12,
    str_detect(zone, "Saint-Laurent") ~ 13,
    str_detect(zone, "Dorval")        ~ 14,
    str_detect(zone, "Baie-d")        ~ 15,
    str_detect(zone, "Sainte-G")      ~ 16,
    str_detect(zone, "Mercier")       ~ 17,
    str_detect(zone, "Pte-aux")       ~ 18,
    TRUE ~ NA_real_
  )) %>% 
  left_join(zones, by = "zone") %>% 
  relocate(zone_name, .after = zone) %>% 
  filter(zone <= 18) %>% 
  arrange(date, zone, dwelling_type, bedroom)

annual_vacancy <-
  annual_vacancy %>% 
  map2_dfr(2015:2019, import_annual_vacancy) %>% 
  filter(centre == "Montréal", neighbourhood == "Total") %>% 
  select(-province, -centre, -neighbourhood) %>% 
  mutate(zone = case_when(
    str_detect(zone, "Downtown")      ~ 1,
    str_detect(zone, "Verdun")        ~ 2,
    str_detect(zone, "LaSalle")       ~ 3,
    str_detect(zone, "ND-de-G")       ~ 4,
    str_detect(zone, "Ct-des")        ~ 5,
    str_detect(zone, "Plateau")       ~ 6,
    str_detect(zone, "Villeray")      ~ 7,
    str_detect(zone, "Hochelag")      ~ 8,
    str_detect(zone, "Rosemont")      ~ 9,
    str_detect(zone, "Anjou")         ~ 10,
    str_detect(zone, "-Nord")         ~ 11,
    str_detect(zone, "Ahuntsic")      ~ 12,
    str_detect(zone, "Saint-Laurent") ~ 13,
    str_detect(zone, "Dorval")        ~ 14,
    str_detect(zone, "Baie-d")        ~ 15,
    str_detect(zone, "Sainte-G")      ~ 16,
    str_detect(zone, "Mercier")       ~ 17,
    str_detect(zone, "Pte-aux")       ~ 18,
    TRUE ~ NA_real_
  )) %>% 
  left_join(zones, by = "zone") %>% 
  relocate(zone_name, .after = zone) %>% 
  filter(zone <= 18) %>% 
  arrange(date, zone, dwelling_type, bedroom)


# Process shapefile -------------------------------------------------------

cmhc <- read_sf("data/shapefiles/CMHC_NBHD_2016-mercWGS84.shp")

cmhc <- 
  cmhc %>% 
  filter(METCODE == "1060", NBHDCODE <= 360) %>% 
  select(ZONECODE, NAME_EN) %>% 
  rename(zone = ZONECODE, neighbourhood = NAME_EN) %>% 
  group_by(zone) %>% 
  summarize(name = paste0(neighbourhood, collapse = "/"), .groups = "drop") %>% 
  mutate(zone = as.numeric(zone)) %>% 
  left_join(zones, by = "zone") %>% 
  select(zone, zone_name, geometry) %>% 
  st_transform(32618)

DA_CMA <-
  cancensus::get_census(
    dataset = "CA16", regions = list(CMA = "24462"), level = "DA",
    geo_format = "sf", vectors = c("v_CA16_4836", "v_CA16_4838")) %>% 
  st_transform(32618) %>% 
  select(5, 4, 13:15) %>% 
  set_names(c("GeoUID", "dwellings", "total_households", "renter_households", 
              "geometry")) %>% 
  st_set_agr("constant")

cmhc <- 
  DA_CMA %>% 
  filter(!is.na(renter_households), !is.na(total_households)) %>% 
  select(renter_households, total_households) %>% 
  st_interpolate_aw(cmhc, extensive = TRUE) %>% 
  st_drop_geometry() %>% 
  select(renter_households, total_households) %>% 
  cbind(cmhc, .) %>% 
  as_tibble() %>% 
  st_as_sf()


# Save output -------------------------------------------------------------

rm(DA_CMA, import_annual_avg_rent, import_annual_units, import_annual_vacancy,
   import_web_table, zones)

save(annual_avg_rent, annual_units, annual_vacancy, 
     city_avg_rent, city_units, city_vacancy, cmhc, file = "output/cmhc.Rdata")
