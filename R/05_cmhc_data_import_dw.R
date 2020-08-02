#### 05 CMHC DATA IMPORT #######################################################

#' This script produces the `cmhc.Rdata` object. It should only be rerun when
#' CMHC data needs to be rebuilt from scratch. The necessary XLSX files can be
#' downloaded with lines 15-TK of the script on first run.

#' External dependencies:
#' - `shapefiles/CMHC_NBHD_2016-mercWGS84.shp`: CMHC neighbourhood shapefile
#' - `vac_rate_mtl_ville.csv`: Table downloaded from CMHC housing market 
#'   information portal (https://www03.cmhc-schl.gc.ca/hmip-pimh/)


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


# Helper function to import tables ----------------------------------------

import_CMHC <- function(data, var_name) {
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
    mutate(change = case_when(
      change == "−" ~ "no change",
      change == "↓" ~ "decrease",
      change == "↑" ~ "increase",
      is.na(change) ~ NA_character_))
  
}

# Import raw files --------------------------------------------------------

cmhc <- read_sf("data/shapefiles/CMHC_NBHD_2016-mercWGS84.shp")
vacancy_2019 <- xlsx_cells("data/montreal_rmr_2019.xlsx", "Table 3.1.1")
avg_rent_2019 <- xlsx_cells("data/montreal_rmr_2019.xlsx", "Table 3.1.2")
annual_vacancy <- read_csv("data/vac_rate_mtl_ville.csv", skip = 2, n_max = 18)


# Process shapefile -------------------------------------------------------

cmhc <- 
  cmhc %>% 
  filter(METCODE == "1060", NBHDCODE <= 360) %>% 
  select(ZONECODE, NAME_EN) %>% 
  rename(zone = ZONECODE, neighbourhood = NAME_EN) %>% 
  group_by(zone) %>% 
  summarize(name = paste0(neighbourhood, collapse = "/"), .group = "drop")


# Process 2019 vacancy rates ----------------------------------------------

vacancy_2019 <- vacancy_2019 %>% import_CMHC(vacancy) %>% filter(zone <= 18)


# Process 2019 average rent -----------------------------------------------

avg_rent_2019 <- avg_rent_2019 %>% import_CMHC(rent) %>% filter(zone <= 18)


# Process annual vacancy --------------------------------------------------

annual_vacancy <-
  annual_vacancy %>% 
  rename_with(~unique(vacancy_2019$bedroom)[c(4, 1:3, 5)], 
              !starts_with("X")) %>% 
  rename(date = X1) %>%
  rename_with(~paste0(unique(vacancy_2019$bedroom)[c(4, 1:3, 5)], " - quality"), 
              c(1:5 * 2 + 1)) %>% 
  mutate(date = as.numeric(str_extract(date, "[:digit:]*"))) %>% 
  pivot_longer(c(where(is.numeric), -date), names_to = "bedroom", 
               values_to = "vacancy") %>% 
  pivot_longer(c(where(is.character), -bedroom), names_to = "temp", 
               values_to = "quality") %>% 
  mutate(temp = str_remove(temp, " - quality")) %>% 
  filter(bedroom == temp) %>% 
  select(-temp) %>% 
  mutate(vacancy = vacancy / 100)
              




#########average rents YOY file cleaning wooooooooo ############# 

#####2019#####

###import data 

rent_2019 <- read_xlsx("data/average-rents-vacant-occupied-units-2019-en.xlsx")

# Turn rows 1 and 2 into column names

names(rent_2019) <- 
  paste("column", 1:length(rent_2019), sep = "_")

names(rent_2019)[[1]] <- 
  "Zone"

# Remove useless rows and columns
rent_2019 <- 
  rent_2019 %>% 
  slice(-1, -2, -3, -4, -5, -6, -7) %>% 
  select(-7, -12, -17, -22, -27)

# rename again 
names(rent_2019) <- 
  paste("column", 1:length(rent_2019), sep = "_")

## get what's needed from the file (totals)
rent_2019 <-
  rent_2019 %>% 
  select(1, 2, 21, 22)


##rename columns 
names(rent_2019)[[1]] <- 
  "Zone"

names(rent_2019)[[2]] <- 
  "Year"

names(rent_2019)[[3]] <- 
  "Occupied_Units"

names(rent_2019)[[4]] <- 
  "DQ"

# Remove first two rows
rent_2019 <- 
  rent_2019 %>% 
  slice(-1, -2)


##filter for montreal rows 

rent_2019 <-
  rent_2019 %>% 
  slice(110:149)

### island is only 1:18 

rent_2019 <-
  rent_2019 %>% 
  slice(1:18)

# clean up zone column 

rent_2019 <- 
  separate(data = rent_2019, col = Zone, into = c("Zone_Number", "Zone_Name"), 
           sep = " - ")

rent_2019 <- 
  separate(data = rent_2019, col = "Zone_Number", 
           into = c("Zone", "Zone_Number"), sep = " ") 

rent_2019 <- 
  rent_2019 %>% 
  select(-Zone)


#####2018#####

###import data 

rent_2018 <- read_xlsx("data/average-rents-vacant-occupied-units-2018-en.xlsx")

# Turn rows into column names

names(rent_2018) <- 
  paste("column", 1:length(rent_2018), sep = "_")

# Remove useless rows and columns
rent_2018 <- 
  rent_2018 %>% 
  slice(-1, -2, -3, -4, -5, -6, -7) %>% 
  select(-7, -12, -17, -22, -27)

# rename again 
names(rent_2018) <- 
  paste("column", 1:length(rent_2018), sep = "_")

## get what's needed from the file (totals)
rent_2018 <-
  rent_2018 %>% 
  select(1, 2, 21, 22)


##rename columns 
names(rent_2018)[[1]] <- 
  "Zone"

names(rent_2018)[[2]] <- 
  "Year"

names(rent_2018)[[3]] <- 
  "Occupied_Units"

names(rent_2018)[[4]] <- 
  "DQ"

# Remove first two rows
rent_2018 <- 
  rent_2018 %>% 
  slice(-1, -2)


##filter for montreal rows 

rent_2018 <-
  rent_2018 %>% 
  slice(110:149)

### island is only 1:18 

rent_2018 <-
  rent_2018 %>% 
  slice(1:18)

# clean up zone column 

rent_2018 <- 
  separate(data = rent_2018, col = Zone, into = c("Zone_Number", "Zone_Name"), 
           sep = " - ")

rent_2018 <- 
  separate(data = rent_2018, col = "Zone_Number", 
           into = c("Zone", "Zone_Number"), sep = " ") 

rent_2018 <- 
  rent_2018 %>% 
  select(-Zone)

#####2017#####

###import data 

rent_2017 <- read_xlsx("data/average-rents-vacant-occupied-units-2017-en.xlsx")

# Turn rows into column names

names(rent_2017) <- 
  paste("column", 1:length(rent_2017), sep = "_")

# Remove useless rows and columns
rent_2017 <- 
  rent_2017 %>% 
  slice(-1, -2, -3, -4, -5, -6, -7) %>% 
  select(-7, -12, -17, -22, -27)

# rename again 
names(rent_2017) <- 
  paste("column", 1:length(rent_2017), sep = "_")

## get what's needed from the file (totals)
rent_2017 <-
  rent_2017 %>% 
  select(1, 2, 21, 22)


##rename columns 
names(rent_2017)[[1]] <- 
  "Zone"

names(rent_2017)[[2]] <- 
  "Year"

names(rent_2017)[[3]] <- 
  "Occupied_Units"

names(rent_2017)[[4]] <- 
  "DQ"

# Remove first two rows
rent_2017 <- 
  rent_2017 %>% 
  slice(-1, -2)


##filter for montreal rows 

rent_2017 <-
  rent_2017 %>% 
  slice(110:149)

### island is only 1:18 

rent_2017 <-
  rent_2017 %>% 
  slice(1:18)

# clean up zone column 

rent_2017 <- separate(data = rent_2017, col = Zone, into = c("Zone_Number", "Zone_Name"), sep = " - ")

rent_2017 <- 
  separate(data = rent_2017, col = "Zone_Number", into = c("Zone", "Zone_Number"), sep = " ") 

rent_2017 <- 
  rent_2017 %>% 
  select(-Zone)


#####2016#####

###import data 

rent_2016 <- read_xlsx("data/average-rents-vacant-occupied-units-2016-en.xlsx")

# Turn rows into column names

names(rent_2016) <- 
  paste("column", 1:length(rent_2016), sep = "_")

# Remove useless rows and columns
rent_2016 <- 
  rent_2016 %>% 
  slice(-1, -2, -3, -4, -5, -6, -7) %>% 
  select(-7, -12, -17, -22, -27)

# rename again 
names(rent_2016) <- 
  paste("column", 1:length(rent_2016), sep = "_")

## get what's needed from the file (totals)
rent_2016 <-
  rent_2016 %>% 
  select(1, 2, 21, 22)


##rename columns 
names(rent_2016)[[1]] <- 
  "Zone"

names(rent_2016)[[2]] <- 
  "Year"

names(rent_2016)[[3]] <- 
  "Occupied_Units"

names(rent_2016)[[4]] <- 
  "DQ"

# Remove first two rows
rent_2016 <- 
  rent_2016 %>% 
  slice(-1, -2)


##filter for montreal rows 

rent_2016 <-
  rent_2016 %>% 
  slice(105:140)

### island is only 1:18 

rent_2016 <-
  rent_2016 %>% 
  slice(1:18)

# clean up zone column 

rent_2016 <- separate(data = rent_2016, col = Zone, into = c("Zone_Number", "Zone_Name"), sep = " - ")

rent_2016 <- 
  separate(data = rent_2016, col = "Zone_Number", into = c("Zone", "Zone_Number"), sep = " ") 

rent_2016 <- 
  rent_2016 %>% 
  select(-Zone)


#####2015#####

###import data 

rent_2015 <- read_xlsx("data/average-rents-vacant-occupied-units-2015-en.xlsx")

# Turn rows into column names

names(rent_2015) <- 
  paste("column", 1:length(rent_2015), sep = "_")

# Remove useless rows and columns
rent_2015 <- 
  rent_2015 %>% 
  slice(-1, -2, -3, -4, -5, -6, -7) %>% 
  select(-7, -12, -17, -22, -27)

# rename again 
names(rent_2015) <- 
  paste("column", 1:length(rent_2015), sep = "_")

## get what's needed from the file (totals)
rent_2015 <-
  rent_2015 %>% 
  select(1, 2, 21, 22)


##rename columns 
names(rent_2015)[[1]] <- 
  "Zone"

names(rent_2015)[[2]] <- 
  "Year"

names(rent_2015)[[3]] <- 
  "Occupied_Units"

names(rent_2015)[[4]] <- 
  "DQ"

# Remove first two rows
rent_2015 <- 
  rent_2015 %>% 
  slice(-1, -2)


##filter for montreal rows 

rent_2015 <-
  rent_2015 %>% 
  slice(105:140)

### island is only 1:18 

rent_2015 <-
  rent_2015 %>% 
  slice(1:18)

# clean up zone column 

rent_2015 <- separate(data = rent_2015, col = Zone, into = c("Zone_Number", "Zone_Name"), sep = " - ")

rent_2015 <- 
  separate(data = rent_2015, col = "Zone_Number", into = c("Zone", "Zone_Number"), sep = " ") 

rent_2015 <- 
  rent_2015 %>% 
  select(-Zone)


##### bind avg rent from 2015 - 2019 ####

rent_total <- bind_rows(rent_2015, rent_2016, rent_2017, rent_2018, rent_2019)

names(rent_total)[[4]] <- "Avg_Rent"


####city of montreal avg rents #### 

#https://www03.cmhc-schl.gc.ca/hmip-pimh/#TableMapChart/2466023/4/Montr%C3%A9al%20(V)%20(Quebec)

rent_city <- read_xls("data/VDM_AVG_RENT.xls")

###remove excess info & rename

rent_city <- 
  rent_city %>% 
  slice(-21:-28)

rent_city <- 
  rent_city %>% 
  select(1, 10, 11)


names(rent_city) <- 
  paste("column", 1:length(rent_city), sep = "_")

names(rent_city)[[1]] <- "Year"
names(rent_city)[[2]] <- "Avg_Rent"
names(rent_city)[[3]] <- "DQ"

rent_city <- 
  rent_city %>% 
  slice(16:20)

#clean up year column 

rent_city <- separate(data = rent_city, col = Year, into = c("Year", "Month"), sep = " ")

rent_city <- 
  rent_city %>% 
  select(-Month)


rent_city <- 
  rent_city %>% 
  mutate(Zone_Name = "City of Montreal", Zone_Number = "N/A")

#####BIND TO EXISTING FILE

rent_total <- bind_rows(rent_total, rent_city)


save(rent_total, file = "output/rent_total.Rdata")


