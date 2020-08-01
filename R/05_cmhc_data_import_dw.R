#### 05 CMHC DATA IMPORT #######################################################

#' This script produces the `cmhc.Rdata` object. It should only be rerun when
#' CMHC data needs to be rebuilt from scratch. The necessary XLSX files can be
#' downloaded with lines 15-TK of the script on first run.

#' External dependencies:
#' - `shapefiles/CMHC_NBHD_2016-mercWGS84.shp`: CMHC neighbourhood shapefile


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


# Import raw files --------------------------------------------------------

cmhc <- read_sf("data/shapefiles/CMHC_NBHD_2016-mercWGS84.shp")
vacancy_2019 <- tidyxl::xlsx_cells("data/montreal_rmr_2019.xlsx", "Table 1.1.1")


# Process shapefile -------------------------------------------------------

cmhc <- 
  cmhc %>% 
  filter(METCODE == "1060", NBHDCODE <= 360) %>% 
  select(ZONECODE, NAME_EN) %>% 
  rename(zone = ZONECODE, neighbourhood = NAME_EN) %>% 
  group_by(zone) %>% 
  summarize(name = paste0(neighbourhood, collapse = "/"))


# Process 2019 vacancy rates ----------------------------------------------

vacancy_2019 <- 
  vacancy_2019 %>% 
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
  summarize(vacancy = numeric[1],
            quality = character[2],
            change = if_else(date == 2019, character[3], NA_character_)) %>% 
  mutate(change = case_when(
    change == "−" ~ "no change",
    change == "↓" ~ "decrease",
    change == "↑" ~ "increase",
    is.na(change) ~ NA_character_))


# Process 2019 average rent -----------------------------------------------

# TKTK







#####TOTAL (apt and townhouse) rental avg rent 2018 2019 ####### 

####import second sheet 

avg_rent_2018_2019 <- 
  read_xlsx("~/Desktop/RA/montreal-2020/data/rmr-montreal-2019-en.xlsx", 20)


# Remove useless rows and columns
avg_rent_2018_2019 <- 
  avg_rent_2018_2019 %>% 
  slice(-1, -2, -3)

# Turn rows 1 and 2 into column names

names(avg_rent_2018_2019) <- 
  paste("column", 1:length(avg_rent_2018_2019), sep = "_")

names(avg_rent_2018_2019)[[1]] <- 
  "Zone"

# Use a for loop to rename the four columns with the first-year values
for (i in 1:5) {
  
  names(avg_rent_2018_2019)[(i - 1) * 4 + 2] <- 
    paste(avg_rent_2018_2019[1, (i - 1) *  4 + 2], 
          avg_rent_2018_2019[2, (i - 1) * 4 + 2],
          sep = ", ")  
}


# Use a for loop to rename the four columns with first-year data quality

for (i in 1:5) {
  
  names(avg_rent_2018_2019)[(i - 1) * 4 + 3] <- 
    paste("Data quality", names(avg_rent_2018_2019)[(i - 1) * 4 + 2],
          sep = ", ")
  
}


# Use a for loop to rename the four columns with the second-year values

for (i in 1:5) {
  
  names(avg_rent_2018_2019)[(i - 1) * 4 + 4] <- 
    paste(avg_rent_2018_2019[1, (i - 1) * 4 + 2], 
          (avg_rent_2018_2019[2, (i - 1) * 4 + 4]),
          sep = ", ")  
}


# Use a for loop to rename the four columns with second-year data quality

for (i in 1:5) {
  
  names(avg_rent_2018_2019)[(i - 1) * 4 + 5] <- 
    paste("Data quality", names(avg_rent_2018_2019)[(i - 1) * 4 + 4],
          sep = ", ")
  
}

# Remove first two rows
avg_rent_2018_2019 <- 
  avg_rent_2018_2019 %>% 
  slice(-1, -2)


#split first column 

avg_rent_2018_2019 <- separate(data = avg_rent_2018_2019, col = Zone, 
                               into = c("Zone_Number", "Zone_Name"), sep = " - ")

# delete extra rows (totals for island, laval, etc )

avg_rent_2018_2019 <- 
  avg_rent_2018_2019 %>% 
  slice(-19, -26, -33, -34, -35, -44, -45, -47, -48)

### note that: mtl island is zone 1-18!!!!!!!!!! ##

#split zone number column 

avg_rent_2018_2019 <- 
  separate(data = avg_rent_2018_2019, col = "Zone_Number", 
           into = c("Zone", "Zone_Number"), sep = " ")


##change to numeric and add 0 to single digit numbers 

avg_rent_2018_2019$Zone_Number[avg_rent_2018_2019$Zone_Number == "1"] <- "01"


avg_rent_2018_2019$Zone_Number[avg_rent_2018_2019$Zone_Number == "2"] <- "02"


avg_rent_2018_2019$Zone_Number[avg_rent_2018_2019$Zone_Number == "3"] <- "03"


avg_rent_2018_2019$Zone_Number[avg_rent_2018_2019$Zone_Number == "4"] <- "04"


avg_rent_2018_2019$Zone_Number[avg_rent_2018_2019$Zone_Number == "5"] <- "05"


avg_rent_2018_2019$Zone_Number[avg_rent_2018_2019$Zone_Number == "6"] <- "06"


avg_rent_2018_2019$Zone_Number[avg_rent_2018_2019$Zone_Number == "7"] <- "07"


avg_rent_2018_2019$Zone_Number[avg_rent_2018_2019$Zone_Number == "8"] <- "08"


avg_rent_2018_2019$Zone_Number[avg_rent_2018_2019$Zone_Number == "9"] <- "09"


avg_rent_2018_2019 <- 
  rename(avg_rent_2018_2019, BDBACH_OCT_18 = 'Bachelor, Oct-18', 
         BDBACH_OCT_19 = 'Bachelor, Oct-19', 
         DQ_BDBACH_OCT_18 = 'Data quality, Bachelor, Oct-18',  
         DQ_BDBACH_OCT_19 = 'Data quality, Bachelor, Oct-19', 
         BD1_OCT_18 = '1 Bedroom, Oct-18', 
         DQ_BD1_OCT_18 = 'Data quality, 1 Bedroom, Oct-18', 
         BD1_OCT_19 = '1 Bedroom, Oct-19', 
         DQ_BD1_OCT_19 = 'Data quality, 1 Bedroom, Oct-19', 
         BD2_OCT_18 = '2 Bedroom, Oct-18', 
         DQ_BD2_OCT_18 = 'Data quality, 2 Bedroom, Oct-18', 
         BD2_OCT_19 = '2 Bedroom, Oct-19', 
         DQ_BD2_OCT_19 = 'Data quality, 2 Bedroom, Oct-19', 
         BD3_OCT_18 = '3 Bedroom +, Oct-18', 
         DQ_BD3_OCT_18 = 'Data quality, 3 Bedroom +, Oct-18',
         BD3_OCT_19 = '3 Bedroom +, Oct-19', 
         DQ_BD3_OCT_19 = 'Data quality, 3 Bedroom +, Oct-19', 
         BDTOT_OCT_18 = 'Total, Oct-18', 
         DQ_BDTOT_OCT_18 = 'Data quality, Total, Oct-18',
         BDTOT_OCT_19 = 'Total, Oct-19', 
         DQ_BDTOT_OCT_19 = 'Data quality, Total, Oct-19') 

## pivot once for bedroom and % 
avg_rent_2018_2019 <-
  avg_rent_2018_2019 %>% 
  pivot_longer(cols = starts_with("BD"), 
               names_to = "unit_type",
               values_to = "percent"
  )


### pivot again for bedroom and data quality

avg_rent_2018_2019 <- 
  avg_rent_2018_2019 %>% 
  pivot_longer(cols = starts_with("DQ"),
               names_to = "data_quality",
               values_to = "data_quality_grade")

### remove DQ for matching 

avg_rent_2018_2019 <- 
  avg_rent_2018_2019 %>% 
  mutate(data_quality = str_remove(data_quality, "DQ_"))


## matching dq and ut columns to find the accurate numbers... 

avg_rent_2018_2019 <- 
  avg_rent_2018_2019 %>% 
  filter(unit_type == data_quality)

#delete duplicate data quaity row 

avg_rent_2018_2019 <- avg_rent_2018_2019 %>% select(-data_quality) 

#split unit column to make year 

avg_rent_2018_2019 <- separate(data = avg_rent_2018_2019, col = "unit_type", 
                               into = c("bedroom_type", "year"), sep = "_OCT_")

## renaming the year column 

avg_rent_2018_2019$year[avg_rent_2018_2019$year == "18"] <- "2018"


avg_rent_2018_2019$year[avg_rent_2018_2019$year == "19"] <- "2019"



## as.numeric number columns 

avg_rent_2018_2019$percent <- as.numeric(avg_rent_2018_2019$percent)

avg_rent_2018_2019$Zone_Number <- as.numeric(avg_rent_2018_2019$Zone_Number)

avg_rent_2018_2019$year <- as.numeric(avg_rent_2018_2019$year)

###join with mtl zones 

avg_rent_df <- 
  avg_rent_2018_2019 %>% 
  left_join(mtl_zones, .)

avg_rent_df <- 
  avg_rent_df %>% 
  select(-Zone)

save(avg_rent_df, file = "output/total_primary_market_average_rent.Rdata")



#####TOTAL (apt and townhouse) rental vacancy rate 2018 2019 ####### 

##import sheet

tot_vacancy_2018_2019 <- 
  read_xlsx("~/Desktop/RA/montreal-2020/data/rmr-montreal-2019-en.xlsx", 19)


# Remove useless rows and columns
tot_vacancy_2018_2019 <- 
  tot_vacancy_2018_2019 %>% 
  slice(-1, -2, -3) %>% 
  select(-6, -11, -16, -21, -26)

# Turn rows 1 and 2 into column names

names(tot_vacancy_2018_2019) <- 
  paste("column", 1:length(tot_vacancy_2018_2019), sep = "_")

names(tot_vacancy_2018_2019)[[1]] <- 
  "Zone"

# Use a for loop to rename the four columns with the first-year values
for (i in 1:5) {
  
  names(tot_vacancy_2018_2019)[(i - 1) * 4 + 2] <- 
    paste(tot_vacancy_2018_2019[1, (i - 1) *  4 + 2], 
          tot_vacancy_2018_2019[2, (i - 1) * 4 + 2],
          sep = ", ")  
}


# Use a for loop to rename the four columns with first-year data quality

for (i in 1:5) {
  
  names(tot_vacancy_2018_2019)[(i - 1) * 4 + 3] <- 
    paste("Data quality", names(tot_vacancy_2018_2019)[(i - 1) * 4 + 2],
          sep = ", ")
  
}


# Use a for loop to rename the four columns with the second-year values

for (i in 1:5) {
  
  names(tot_vacancy_2018_2019)[(i - 1) * 4 + 4] <- 
    paste(tot_vacancy_2018_2019[1, (i - 1) * 4 + 2], 
          (tot_vacancy_2018_2019[2, (i - 1) * 4 + 4]),
          sep = ", ")  
}


# Use a for loop to rename the four columns with second-year data quality

for (i in 1:5) {
  
  names(tot_vacancy_2018_2019)[(i - 1) * 4 + 5] <- 
    paste("Data quality", names(tot_vacancy_2018_2019)[(i - 1) * 4 + 4],
          sep = ", ")
  
}

# Remove first two rows
tot_vacancy_2018_2019 <- 
  tot_vacancy_2018_2019 %>% 
  slice(-1, -2)


#split first column 

tot_vacancy_2018_2019 <- separate(data = tot_vacancy_2018_2019, col = Zone, 
                                  into = c("Zone_Number", "Zone_Name"), 
                                  sep = " - ")

# delete extra rows (totals for island, laval, etc )

tot_vacancy_2018_2019 <- 
  tot_vacancy_2018_2019 %>% 
  slice(-19, -26, -33, -34, -35, -44, -45, -47, -48)

### note that: mtl island is zone 1-18!!!!!!!!!! ##

#split zone number column 

tot_vacancy_2018_2019 <- 
  separate(data = tot_vacancy_2018_2019, col = "Zone_Number", 
           into = c("Zone", "Zone_Number"), sep = " ")


##change to numeric and add 0 to single digit numbers 

tot_vacancy_2018_2019$Zone_Number[tot_vacancy_2018_2019$Zone_Number == "1"] <- "01"


tot_vacancy_2018_2019$Zone_Number[tot_vacancy_2018_2019$Zone_Number == "2"] <- "02"


tot_vacancy_2018_2019$Zone_Number[tot_vacancy_2018_2019$Zone_Number == "3"] <- "03"


tot_vacancy_2018_2019$Zone_Number[tot_vacancy_2018_2019$Zone_Number == "4"] <- "04"


tot_vacancy_2018_2019$Zone_Number[tot_vacancy_2018_2019$Zone_Number == "5"] <- "05"


tot_vacancy_2018_2019$Zone_Number[tot_vacancy_2018_2019$Zone_Number == "6"] <- "06"


tot_vacancy_2018_2019$Zone_Number[tot_vacancy_2018_2019$Zone_Number == "7"] <- "07"


tot_vacancy_2018_2019$Zone_Number[tot_vacancy_2018_2019$Zone_Number == "8"] <- "08"


tot_vacancy_2018_2019$Zone_Number[tot_vacancy_2018_2019$Zone_Number == "9"] <- "09"


#rename 

tot_vacancy_2018_2019 <- 
  rename(tot_vacancy_2018_2019, BDBACH_OCT_18 = 'Bachelor, Oct-18', 
         BDBACH_OCT_19 = 'Bachelor, Oct-19', 
         DQ_BDBACH_OCT_18 = 'Data quality, Bachelor, Oct-18',  
         DQ_BDBACH_OCT_19 = 'Data quality, Bachelor, Oct-19', 
         BD1_OCT_18 = '1 Bedroom, Oct-18', 
         DQ_BD1_OCT_18 = 'Data quality, 1 Bedroom, Oct-18', 
         BD1_OCT_19 = '1 Bedroom, Oct-19', 
         DQ_BD1_OCT_19 = 'Data quality, 1 Bedroom, Oct-19', 
         BD2_OCT_18 = '2 Bedroom, Oct-18', 
         DQ_BD2_OCT_18 = 'Data quality, 2 Bedroom, Oct-18', 
         BD2_OCT_19 = '2 Bedroom, Oct-19', 
         DQ_BD2_OCT_19 = 'Data quality, 2 Bedroom, Oct-19', 
         BD3_OCT_18 = '3 Bedroom +, Oct-18', 
         DQ_BD3_OCT_18 = 'Data quality, 3 Bedroom +, Oct-18',
         BD3_OCT_19 = '3 Bedroom +, Oct-19', 
         DQ_BD3_OCT_19 = 'Data quality, 3 Bedroom +, Oct-19', 
         BDTOT_OCT_18 = 'Total, Oct-18', 
         DQ_BDTOT_OCT_18 = 'Data quality, Total, Oct-18',
         BDTOT_OCT_19 = 'Total, Oct-19', 
         DQ_BDTOT_OCT_19 = 'Data quality, Total, Oct-19') 

## pivot once for bedroom and % 
tot_vacancy_2018_2019 <-
  tot_vacancy_2018_2019 %>% 
  pivot_longer(cols = starts_with("BD"), 
               names_to = "unit_type",
               values_to = "percent"
  )


### pivot again for bedroom and data quality

tot_vacancy_2018_2019 <- 
  tot_vacancy_2018_2019 %>% 
  pivot_longer(cols = starts_with("DQ"),
               names_to = "data_quality",
               values_to = "data_quality_grade")

### remove DQ for matching 

tot_vacancy_2018_2019 <- 
  tot_vacancy_2018_2019 %>% 
  mutate(data_quality = str_remove(data_quality, "DQ_"))


## matching dq and ut columns to find the accurate numbers... 

tot_vacancy_2018_2019 <- 
  tot_vacancy_2018_2019 %>% 
  filter(unit_type == data_quality)

#delete duplicate data quaity row 

tot_vacancy_2018_2019 <- tot_vacancy_2018_2019 %>% select(-data_quality) 

#split unit column to make year 

tot_vacancy_2018_2019 <- 
  separate(data = tot_vacancy_2018_2019, col = "unit_type", 
           into = c("bedroom_type", "year"), sep = "_OCT_")

## renaming the year column 

tot_vacancy_2018_2019$year[tot_vacancy_2018_2019$year == "18"] <- "2018"


tot_vacancy_2018_2019$year[tot_vacancy_2018_2019$year == "19"] <- "2019"



## as.numeric number columns 

tot_vacancy_2018_2019$percent <- as.numeric(tot_vacancy_2018_2019$percent)

tot_vacancy_2018_2019$Zone_Number <- as.numeric(tot_vacancy_2018_2019$Zone_Number)

tot_vacancy_2018_2019$year <- as.numeric(tot_vacancy_2018_2019$year)

###join with mtl zones 

tot_vac_df <- 
  tot_vacancy_2018_2019 %>% 
  left_join(mtl_zones, .)

tot_vac_df <- 
  tot_vac_df %>% 
  select(-Zone)

save(tot_vac_df, file = "output/total_primary_market_vacancy_rate.Rdata")


#----------------------- cleaning cmhc ville de mtl level data from the housing portal ####

#https://www03.cmhc-schl.gc.ca/hmip-pimh/#TableMapChart/2466023/4/Montr%C3%A9al%20(V)%20(Quebec)

city_vac_rate <- read_csv("~/Desktop/RA/montreal-2020/data/vac_rate_mtl_ville.csv")


# Remove useless rows and columns
city_vac_rate <- 
  city_vac_rate %>% 
  slice(-21:-28)

# Turn rows 1 and 2 into column names

names(city_vac_rate) <- 
  paste("column", 1:length(city_vac_rate), sep = "_")

names(city_vac_rate)[[1]] <- 
  "Year"

# Use a for loop to rename the columns with the first-year values
for (i in 1:5) {
  
  names(city_vac_rate)[(i - 1) * 2 + 2] <- 
    paste(#city_vac_rate[1, (i - 1) *  4 + 2], 
      city_vac_rate[2, (i - 1) * 2 + 2],
      sep = ", ")  
}


# Use a for loop to rename the columns with first-year data quality

for (i in 1:5) {
  
  names(city_vac_rate)[(i - 1) * 2 + 3] <- 
    paste("Data quality", names(city_vac_rate)[(i - 1) * 2 + 2],
          sep = ", ")
  
}


# Remove first two rows
city_vac_rate <- 
  city_vac_rate %>% 
  slice(-1, -2)


#split first column 

city_vac_rate <- 
  separate(data = city_vac_rate, col = Year, into = c("Year", "Month"), sep = " ")


#delete month column 

city_vac_rate <- 
  city_vac_rate %>% 
  select(-Month)

#rename 

city_vac_rate <- 
  rename(city_vac_rate, BDBACH = 'Bachelor', 
         DQ_BDBACH = 'Data quality, Bachelor',  
         BD1 = '1 Bedroom', DQ_BD1 = 'Data quality, 1 Bedroom', 
         BD2 = '2 Bedroom', DQ_BD2 = 'Data quality, 2 Bedroom', 
         BD3 = '3 Bedroom +', DQ_BD3 = 'Data quality, 3 Bedroom +',
         BDTOT = 'Total', DQ_BDTOT = 'Data quality, Total') 


## pivot once for bedroom and % 
city_vac_rate <-
  city_vac_rate %>% 
  pivot_longer(cols = starts_with("BD"), 
               names_to = "unit_type",
               values_to = "percent"
  )

### pivot again for bedroom and data quality

city_vac_rate <- 
  city_vac_rate %>% 
  pivot_longer(cols = starts_with("DQ"),
               names_to = "data_quality",
               values_to = "data_quality_grade")

### remove DQ for matching 

city_vac_rate <- 
  city_vac_rate %>% 
  mutate(data_quality = str_remove(data_quality, "DQ_"))


## matching 

city_vac_rate <- 
  city_vac_rate %>% 
  filter(unit_type == data_quality)

#delete duplicate data quaity row 

city_vac_rate <- city_vac_rate %>% select(-data_quality) 


## as.numeric number columns 

city_vac_rate$percent <- as.numeric(city_vac_rate$percent)

city_vac_rate$Year <- as.numeric(city_vac_rate$Year)


save(city_vac_rate, file = "output/montreal_yoy_vac_rate.Rdata")


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


