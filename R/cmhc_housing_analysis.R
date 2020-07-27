#-----------------------------------# cmhc private apartment data 2018 / 2019 -------------------------------

library(tidyverse)
library(readxl)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)


# Import data -------------------------------------------------------------

vacancy_rates_2018_2019 <- 
  read_xlsx("~/Desktop/RA/montreal-2020/data/rmr-montreal-2019-en.xlsx")


# Create example tibble ---------------------------------------------------

tibble(
  geography = character(),
  geography_type = character(),
  category = character(),
  date = as.Date(numeric(), origin = "1970-01-01"),
  value = numeric(),
  quality = character()
)


# Process APARTMENT vacancy_rates_2018_2019 --------------------------------------------------------

# Remove useless rows and columns
vacancy_rates_2018_2019 <- 
  vacancy_rates_2018_2019 %>% 
  slice(-1, -2, -3) %>% 
  select(-6, -11, -16, -21, -26)

# Turn rows 1 and 2 into column names

names(vacancy_rates_2018_2019) <- 
  paste("column", 1:length(vacancy_rates_2018_2019), sep = "_")

names(vacancy_rates_2018_2019)[[1]] <- 
  "Zone"

# Use a for loop to rename the four columns with the first-year values
for (i in 1:5) {
  
  names(vacancy_rates_2018_2019)[(i - 1) * 4 + 2] <- 
    paste(vacancy_rates_2018_2019[1, (i - 1) *  4 + 2], 
          vacancy_rates_2018_2019[2, (i - 1) * 4 + 2],
          sep = ", ")  
}


# Use a for loop to rename the four columns with first-year data quality

for (i in 1:5) {
  
  names(vacancy_rates_2018_2019)[(i - 1) *4 + 3] <- 
    paste("Data quality", names(vacancy_rates_2018_2019)[(i - 1) * 4 + 2],
          sep = ", ")
  
}


# Use a for loop to rename the four columns with the second-year values

for (i in 1:5) {
  
  names(vacancy_rates_2018_2019)[(i - 1) * 4 + 4] <- 
    paste(vacancy_rates_2018_2019[1, (i - 1) * 4 + 2], 
          (vacancy_rates_2018_2019[2, (i - 1) * 4 + 4]),
          sep = ", ")  
}


# Use a for loop to rename the four columns with second-year data quality

for (i in 1:5) {
  
  names(vacancy_rates_2018_2019)[(i - 1) * 4 + 5] <- 
    paste("Data quality", names(vacancy_rates_2018_2019)[(i - 1) * 4 + 4],
          sep = ", ")
  
}

# Remove first two rows
vacancy_rates_2018_2019 <- 
  vacancy_rates_2018_2019 %>% 
  slice(-1, -2)


#split first column 

vacancy_rates_2018_2019 <- separate(data = vacancy_rates_2018_2019, col = Zone, into = c("Zone_Number", "Zone_Name"), sep = " - ")


# delete extra rows (totals for island, laval, etc )

vacancy_rates_2018_2019 <- 
  vacancy_rates_2018_2019 %>% 
  slice(-19, -26, -33, -34, -35, -44, -45, -47, -48)

### note that: mtl island is zone 1-18!!!!!!!!!! ##

#split zone number column 

vacancy_rates_2018_2019 <- 
  separate(data = vacancy_rates_2018_2019, col = "Zone_Number", into = c("Zone", "Zone_Number"), sep = " ") 

vacancy_rates_2018_2019 <- 
  vacancy_rates_2018_2019 %>% 
  select(-Zone)


##change to numeric and add 0 to single digit numbers 

vacancy_rates_2018_2019$Zone_Number[vacancy_rates_2018_2019$Zone_Number == "1"] <- "01"


vacancy_rates_2018_2019$Zone_Number[vacancy_rates_2018_2019$Zone_Number == "2"] <- "02"


vacancy_rates_2018_2019$Zone_Number[vacancy_rates_2018_2019$Zone_Number == "3"] <- "03"


vacancy_rates_2018_2019$Zone_Number[vacancy_rates_2018_2019$Zone_Number == "4"] <- "04"


vacancy_rates_2018_2019$Zone_Number[vacancy_rates_2018_2019$Zone_Number == "5"] <- "05"


vacancy_rates_2018_2019$Zone_Number[vacancy_rates_2018_2019$Zone_Number == "6"] <- "06"


vacancy_rates_2018_2019$Zone_Number[vacancy_rates_2018_2019$Zone_Number == "7"] <- "07"


vacancy_rates_2018_2019$Zone_Number[vacancy_rates_2018_2019$Zone_Number == "8"] <- "08"


vacancy_rates_2018_2019$Zone_Number[vacancy_rates_2018_2019$Zone_Number == "9"] <- "09"


#rename 

vacancy_rates_2018_2019 <- 
  rename(vacancy_rates_2018_2019, BDBACH_OCT_18 = 'Bachelor, Oct-18', BDBACH_OCT_19 = 'Bachelor, Oct-19', 
         DQ_BDBACH_OCT_18 = 'Data quality, Bachelor, Oct-18',  DQ_BDBACH_OCT_19 = 'Data quality, Bachelor, Oct-19', 
         BD1_OCT_18 = '1 Bedroom, Oct-18', DQ_BD1_OCT_18 = 'Data quality, 1 Bedroom, Oct-18', 
         BD1_OCT_19 = '1 Bedroom, Oct-19', DQ_BD1_OCT_19 = 'Data quality, 1 Bedroom, Oct-19', 
         BD2_OCT_18 = '2 Bedroom, Oct-18', DQ_BD2_OCT_18 = 'Data quality, 2 Bedroom, Oct-18', 
         BD2_OCT_19 = '2 Bedroom, Oct-19', DQ_BD2_OCT_19 = 'Data quality, 2 Bedroom, Oct-19', 
         BD3_OCT_18 = '3 Bedroom +, Oct-18', DQ_BD3_OCT_18 = 'Data quality, 3 Bedroom +, Oct-18',
         BD3_OCT_19 = '3 Bedroom +, Oct-19', DQ_BD3_OCT_19 = 'Data quality, 3 Bedroom +, Oct-19', 
         BDTOT_OCT_18 = 'Total, Oct-18', DQ_BDTOT_OCT_18 = 'Data quality, Total, Oct-18',
         BDTOT_OCT_19 = 'Total, Oct-19', DQ_BDTOT_OCT_19 = 'Data quality, Total, Oct-19') 


## pivot once for bedroom and % 
vacancy_rates_2018_2019pv <-
  vacancy_rates_2018_2019 %>% 
  pivot_longer(cols = starts_with("BD"), 
               names_to = "unit_type",
               values_to = "percent"
  )

### pivot again for bedroom and data quality

vacancy_rates_2018_2019pv <- 
  vacancy_rates_2018_2019pv %>% 
  pivot_longer(cols = starts_with("DQ"),
               names_to = "data_quality",
               values_to = "data_quality_grade")

### remove DQ for matching 

vacancy_rates_2018_2019pv <- 
  vacancy_rates_2018_2019pv %>% 
  mutate(data_quality = str_remove(data_quality, "DQ_"))


## matching 

vacancy_rates_2018_2019pv <- 
  vacancy_rates_2018_2019pv %>% 
  filter(unit_type == data_quality)

#delete duplicate data quaity row 

vacancy_rates_2018_2019pv <- vacancy_rates_2018_2019pv %>% select(-data_quality) 

#split unit column to make year 

vacancy_rates_2018_2019pv <- separate(data = vacancy_rates_2018_2019pv, col = "unit_type", into = c("bedroom_type", "year"), sep = "_OCT_")

## renaming the year column 

vacancy_rates_2018_2019pv$year[vacancy_rates_2018_2019pv$year == "18"] <- "2018"


vacancy_rates_2018_2019pv$year[vacancy_rates_2018_2019pv$year == "19"] <- "2019"



## as.numeric number columns 

vacancy_rates_2018_2019pv$percent <- as.numeric(vacancy_rates_2018_2019pv$percent)

vacancy_rates_2018_2019pv$Zone_Number <- as.numeric(vacancy_rates_2018_2019pv$Zone_Number)

vacancy_rates_2018_2019pv$year <- as.numeric(vacancy_rates_2018_2019pv$year)


###### download CMHC shapefile #####################


mtl_neighbourhoods <-
  read_sf("~/Desktop/RA/montreal-2020/data/CMHC_NBHD_2016-mercWGS84/CMHC_NBHD_2016-mercWGS84.shp") #%>% 
#select(objectID = OBJECTID, Zone_Number = ZONECODE, neighbourhood_code = NBHDCODE, 
#       neighbourhood_name_en = NBHDNAME_L, neighbourhood_name_fr = NBHDNAME_1, 
#     MET_neighbourhood = METNBHD, MET_code = METCODE, geometry = geometry)#%>% 
#st_set_crs(2954) %>% 
#st_transform(2950)

##filter for montreal cma at zone level  ##filter for montreal city level at zone boundaries 

mtl_zones <- 
  mtl_neighbourhoods %>% 
  filter(METCODE == "1060",
         NBHDCODE <= "360") %>% 
  group_by(ZONECODE) %>% 
  summarize() %>% 
  plot()

##prep shapefile for joining 

mtl_zones <-
  mtl_zones %>% 
  rename(Zone_Number = ZONECODE)

mtl_zones$Zone_Number <- as.numeric(mtl_zones$Zone_Number)

#ggplot(mtl_neighbourhoods) + geom_sf()


###filter for montreal island zones 

#vacancy_rates_2018_2019pv <- 
# vacancy_rates_2018_2019pv %>% 
#filter(Zone_Number <= 18)


#### join table ### ################################### 

#df <- 
# vacancy_rates_2018_2019 %>% 
# left_join(mtl_neighbourhoods, .) 



dfpivoted <- 
  vacancy_rates_2018_2019pv %>% 
  left_join(mtl_zones, .)

### color palette 

col_palette <- 
  c("#33CCCC", "#FF3333", "#006699", "#FFCC00")


##make character into numeric 

#df$BDBACH_OCT_18 <- as.numeric(df$BDBACH_OCT_18)
#df$BDBACH_OCT_19 <- as.numeric(df$BDBACH_OCT_19)
#df$BD1_OCT_18 <- as.numeric(df$BD1_OCT_18)
#df$BD1_OCT_19 <- as.numeric(df$BD1_OCT_19)
#df$BD2_OCT_18 <- as.numeric(df$BD2_OCT_18)
#df$BD2_OCT_19 <- as.numeric(df$BD2_OCT_19)
#df$BD3_OCT_18 <- as.numeric(df$BD3_OCT_18)
#df$BD3_OCT_19 <- as.numeric(df$BD3_OCT_19)
#df$BDTOTAL_OCT_18 <- as.numeric(df$BDTOTAL_OCT_18)
#df$BDTOTAL_OCT_19 <- as.numeric(df$BDTOTAL_OCT_19)


#mapping ####

####test map with new pivoted data!!!!!!! #### 


map <-
  dfpivoted %>% 
  filter(bedroom_type == "BDBACH", 
         year == "2018") %>% 
  st_as_sf() %>% 
  ggplot() + 
  geom_sf(aes(fill = percent), lwd = 1, colour = "white") +
  # geom_sf_label(aes(label = BDBACH_OCT_18), 
  #   size = 1.8,
  #   alpha = 0.5,
  #   fill = alpha("white", 0.6)) +
  scale_fill_gradientn(colors = col_palette[c(3,1)],
                       na.value = "grey80", 
                       limits = c(1.1, 5.6),
                       oob = scales:: squish) +
  # labels = scales::percent) +
  guides(fill = guide_colorbar(title = "Percentage (%)")) +
  theme_void() + 
  labs(title = "Bachelor Apartment Vacancy Rates (%), by CMHC Zone, 2018") +
  theme(legend.position = "right",
        text = element_text(face = "plain"), 
        legend.title = element_text(face = "bold", 
                                    size = 10),
        legend.text = element_text(size = 10))


###bachelor apartment ####

bachelor_vac_2018 <-
  df %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = BDBACH_OCT_18), lwd = 1, colour = "white") +
  # geom_sf_label(aes(label = BDBACH_OCT_18), 
  #   size = 1.8,
  #   alpha = 0.5,
  #   fill = alpha("white", 0.6)) +
  scale_fill_gradientn(colors = col_palette[c(3,1)],
                       na.value = "grey80", 
                       limits = c(1.1, 5.6),
                       oob = scales:: squish) +
  # labels = scales::percent) +
  guides(fill = guide_colorbar(title = "Percentage (%)")) +
  theme_void() + 
  labs(title = "Bachelor Apartment Vacancy Rates (%), by CMHC Zone, 2018") +
  theme(legend.position = "right",
        text = element_text(face = "plain"), 
        legend.title = element_text(face = "bold", 
                                    size = 10),
        legend.text = element_text(size = 10))

ggsave("output/bachelor_vac_2018.pdf", plot = bachelor_vac_2018, width = 8,
       height = 4, units = "in", useDingbats = FALSE)

bachelor_vac_2019 <-
  df %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = BDBACH_OCT_19), lwd = 1, colour = "white") +
  # geom_sf_label(aes(label = BDBACH_OCT_18), 
  #   size = 1.8,
  #   alpha = 0.5,
  #   fill = alpha("white", 0.6)) +
  scale_fill_gradientn(colors = col_palette[c(3,1)],
                       na.value = "grey80", 
                       limits = c(0.2, 3.1),
                       oob = scales:: squish) +
  # labels = scales::percent) +
  guides(fill = guide_colorbar(title = "Percentage (%)")) +
  theme_void() + 
  labs(title = "Bachelor Apartment Vacancy Rates (%), by CMHC Zone, 2019") +
  theme(legend.position = "right",
        text = element_text(face = "plain"), 
        legend.title = element_text(face = "bold", 
                                    size = 10),
        legend.text = element_text(size = 10))

ggsave("output/bachelor_vac_2019.pdf", plot = bachelor_vac_2019, width = 8,
       height = 4, units = "in", useDingbats = FALSE)

###1bed #####

bd1_vac_2018 <-
  df %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = BD1_OCT_18), lwd = 1, colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(3,1)],
                       na.value = "grey80", 
                       limits = c(0.6, 3.9),
                       oob = scales:: squish) +
  #labels = scales::percent) +
  guides(fill = guide_colorbar(title = "Percentage (%)")) +
  theme_void() + 
  labs(title = "1 Bedroom Apartment Vacancy Rates (%), by CMHC Zone, 2018") +
  theme(legend.position = "right",
        text = element_text(face = "plain"), 
        legend.title = element_text(face = "bold", 
                                    size = 10),
        legend.text = element_text(size = 10))

ggsave("output/bd1_vac_2018.pdf", plot = bd1_vac_2018, width = 8,
       height = 4, units = "in", useDingbats = FALSE)

bd1_vac_2019 <-
  df %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = BD1_OCT_19), lwd = 1, colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(3,1)],
                       na.value = "grey80", 
                       limits = c(0.3, 4.4),
                       oob = scales:: squish) +
  #labels = scales::percent) +
  guides(fill = guide_colorbar(title = "Percentage (%)")) +
  theme_void() + 
  labs(title = "1 Bedroom Apartment Vacancy Rates (%), by CMHC Zone, 2019") +
  theme(legend.position = "right",
        text = element_text(face = "plain"), 
        legend.title = element_text(face = "bold", 
                                    size = 10),
        legend.text = element_text(size = 10))

ggsave("output/bd1_vac_2019.pdf", plot = bd1_vac_2019, width = 8,
       height = 4, units = "in", useDingbats = FALSE)

###2bed #####

bd2_vac_2018 <-
  df %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = BD2_OCT_18), lwd = 1, colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(3,1)],
                       na.value = "grey80", 
                       limits = c(0.3, 4.3),
                       oob = scales:: squish) +
  #labels = scales::percent) +
  guides(fill = guide_colorbar(title = "Percentage (%)")) +
  theme_void() + 
  labs(title = "2 Bedroom Apartment Vacancy Rates (%), by CMHC Zone, 2018") +
  theme(legend.position = "right",
        text = element_text(face = "plain"), 
        legend.title = element_text(face = "bold", 
                                    size = 10),
        legend.text = element_text(size = 10))

ggsave("output/bd2_vac_2018.pdf", plot = bd2_vac_2018, width = 8,
       height = 4, units = "in", useDingbats = FALSE)

bd2_vac_2019 <-
  df %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = BD2_OCT_19), lwd = 1, colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(3,1)],
                       na.value = "grey80", 
                       limits = c(0.2, 3.7),
                       oob = scales:: squish) +
  #labels = scales::percent) +
  guides(fill = guide_colorbar(title = "Percentage (%)")) +
  theme_void() + 
  labs(title = "2 Bedroom Apartment Vacancy Rates (%), by CMHC Zone, 2019") +
  theme(legend.position = "right",
        text = element_text(face = "plain"), 
        legend.title = element_text(face = "bold", 
                                    size = 10),
        legend.text = element_text(size = 10))

ggsave("output/bd2_vac_2019.pdf", plot = bd2_vac_2019, width = 8,
       height = 4, units = "in", useDingbats = FALSE)

###3bed #####

bd3_vac_2018 <-
  df %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = BD3_OCT_18), lwd = 1, colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(3,1)],
                       na.value = "grey80", 
                       limits = c(0.6, 3.9),
                       oob = scales:: squish) +
  #labels = scales::percent) +
  guides(fill = guide_colorbar(title = "Percentage (%)")) +
  theme_void() + 
  labs(title = "3 or More Bedroom Apartment Vacancy Rates (%), by CMHC Zone, 2018") +
  theme(legend.position = "right",
        text = element_text(face = "plain"), 
        legend.title = element_text(face = "bold", 
                                    size = 10),
        legend.text = element_text(size = 10))

ggsave("output/bd3_vac_2018.pdf", plot = bd1_vac_2018, width = 8,
       height = 4, units = "in", useDingbats = FALSE)

bd3_vac_2019 <-
  df %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = BD3_OCT_19), lwd = 1, colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(3,1)],
                       na.value = "grey80", 
                       limits = c(0.3, 4.4),
                       oob = scales:: squish) +
  #labels = scales::percent) +
  guides(fill = guide_colorbar(title = "Percentage (%)")) +
  theme_void() + 
  labs(title = "3 or More Bedroom Apartment Vacancy Rates (%), by CMHC Zone, 2019") +
  theme(legend.position = "right",
        text = element_text(face = "plain"), 
        legend.title = element_text(face = "bold", 
                                    size = 10),
        legend.text = element_text(size = 10))

ggsave("output/bd3_vac_2019.pdf", plot = bd3_vac_2019, width = 8,
       height = 4, units = "in", useDingbats = FALSE)

###total vac apt#####

total_vac_2018 <-
  df %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = BDTOTAL_OCT_18), lwd = 1, colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(3,1)],
                       na.value = "grey80", 
                       limits = c(0.7, 4.0),
                       oob = scales:: squish) +
  #labels = scales::percent) +
  guides(fill = guide_colorbar(title = "Percentage (%)")) +
  theme_void() + 
  labs(title = "Total Private Apartment Vacancy Rates (%), by CMHC Zone, 2018") +
  theme(legend.position = "right",
        text = element_text(face = "plain"), 
        legend.title = element_text(face = "bold", 
                                    size = 10),
        legend.text = element_text(size = 10))

ggsave("output/tot_vac_2018.pdf", plot = total_vac_2018, width = 8,
       height = 4, units = "in", useDingbats = FALSE)

tot_vac_2019 <-
  df %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = BDTOTAL_OCT_19), lwd = 1, colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(3,1)],
                       na.value = "grey80", 
                       limits = c(0.3, 3.1),
                       oob = scales:: squish) +
  #labels = scales::percent) +
  guides(fill = guide_colorbar(title = "Percentage (%)")) +
  theme_void() + 
  labs(title = "Total Private Apartment Vacancy Rates (%), by CMHC Zone, 2019") +
  theme(legend.position = "right",
        text = element_text(face = "plain"), 
        legend.title = element_text(face = "bold", 
                                    size = 10),
        legend.text = element_text(size = 10))

ggsave("output/tot_vac_2019.pdf", plot = bd1_vac_2019, width = 8,
       height = 4, units = "in", useDingbats = FALSE)


save(tot_vac_2019, file = "output/tot_vac_2019.Rdata")
save(total_vac_2018, file = "output/tot_vac_2018.Rdata")

#####TOTAL (apt and townhouse) rental avg rent 2018 2019 ####### 

####import second sheet 

avg_rent_2018_2019 <- read_xlsx("~/Desktop/RA/montreal-2020/data/rmr-montreal-2019-en.xlsx", 20)


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

avg_rent_2018_2019 <- separate(data = avg_rent_2018_2019, col = Zone, into = c("Zone_Number", "Zone_Name"), sep = " - ")

# delete extra rows (totals for island, laval, etc )

avg_rent_2018_2019 <- 
  avg_rent_2018_2019 %>% 
  slice(-19, -26, -33, -34, -35, -44, -45, -47, -48)

### note that: mtl island is zone 1-18!!!!!!!!!! ##

#split zone number column 

avg_rent_2018_2019 <- 
  separate(data = avg_rent_2018_2019, col = "Zone_Number", into = c("Zone", "Zone_Number"), sep = " ")


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
  rename(avg_rent_2018_2019, BDBACH_OCT_18 = 'Bachelor, Oct-18', BDBACH_OCT_19 = 'Bachelor, Oct-19', 
         DQ_BDBACH_OCT_18 = 'Data quality, Bachelor, Oct-18',  DQ_BDBACH_OCT_19 = 'Data quality, Bachelor, Oct-19', 
         BD1_OCT_18 = '1 Bedroom, Oct-18', DQ_BD1_OCT_18 = 'Data quality, 1 Bedroom, Oct-18', 
         BD1_OCT_19 = '1 Bedroom, Oct-19', DQ_BD1_OCT_19 = 'Data quality, 1 Bedroom, Oct-19', 
         BD2_OCT_18 = '2 Bedroom, Oct-18', DQ_BD2_OCT_18 = 'Data quality, 2 Bedroom, Oct-18', 
         BD2_OCT_19 = '2 Bedroom, Oct-19', DQ_BD2_OCT_19 = 'Data quality, 2 Bedroom, Oct-19', 
         BD3_OCT_18 = '3 Bedroom +, Oct-18', DQ_BD3_OCT_18 = 'Data quality, 3 Bedroom +, Oct-18',
         BD3_OCT_19 = '3 Bedroom +, Oct-19', DQ_BD3_OCT_19 = 'Data quality, 3 Bedroom +, Oct-19', 
         BDTOT_OCT_18 = 'Total, Oct-18', DQ_BDTOT_OCT_18 = 'Data quality, Total, Oct-18',
         BDTOT_OCT_19 = 'Total, Oct-19', DQ_BDTOT_OCT_19 = 'Data quality, Total, Oct-19') 

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

avg_rent_2018_2019 <- separate(data = avg_rent_2018_2019, col = "unit_type", into = c("bedroom_type", "year"), sep = "_OCT_")

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




### color palette 

col_palette <- 
  c("#33CCCC", "#FF3333", "#006699", "#FFCC00")

##map

total_avg_rent_2018 <-
  avg_rent_df %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = BDTOTAL_OCT_18), lwd = 1, colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(3,1)],
                       na.value = "grey80", 
                       limits = c(965, 1076),
                       oob = scales:: squish) +
  guides(fill = guide_colorbar(title = "Dollars ($)")) +
  theme_void() + 
  labs(title = "Total Private Row (Townhouse) and Apartment Average Rents ($), by CMHC Zone, 2018") +
  theme(legend.position = "right",
        text = element_text(face = "plain"), 
        legend.title = element_text(face = "bold", 
                                    size = 10),
        legend.text = element_text(size = 10))

ggsave("output/tot_avg_rent_2018.pdf", plot = total_avg_rent_2018, width = 8,
       height = 4, units = "in", useDingbats = FALSE)

total_avg_rent_2019 <-
  avg_rent_df %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = BDTOTAL_OCT_19), lwd = 1, colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(3,1)],
                       na.value = "grey80", 
                       limits = c(695, 	1228),
                       oob = scales:: squish) +
  guides(fill = guide_colorbar(title = "Dollars ($)")) +
  theme_void() + 
  labs(title = "Total Private Row (Townhouse) and Apartment Average Rents ($), by CMHC Zone, 2019") +
  theme(legend.position = "right",
        text = element_text(face = "plain"), 
        legend.title = element_text(face = "bold", 
                                    size = 10),
        legend.text = element_text(size = 10))

ggsave("output/tot_avg_rent_2019.pdf", plot = total_avg_rent_2019, width = 8,
       height = 4, units = "in", useDingbats = FALSE)


save(total_avg_rent_2019, file = "output/tot_avgrent_2019.Rdata")
save(total_avg_rent_2018, file = "output/tot_avgrent_2018.Rdata")



#####TOTAL (apt and townhouse) rental vacancy rate 2018 2019 ####### 

##import sheet

tot_vacancy_2018_2019 <- read_xlsx("~/Desktop/RA/montreal-2020/data/rmr-montreal-2019-en.xlsx", 19)


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

tot_vacancy_2018_2019 <- separate(data = tot_vacancy_2018_2019, col = Zone, into = c("Zone_Number", "Zone_Name"), sep = " - ")

# delete extra rows (totals for island, laval, etc )

tot_vacancy_2018_2019 <- 
  tot_vacancy_2018_2019 %>% 
  slice(-19, -26, -33, -34, -35, -44, -45, -47, -48)

### note that: mtl island is zone 1-18!!!!!!!!!! ##

#split zone number column 

tot_vacancy_2018_2019 <- 
  separate(data = tot_vacancy_2018_2019, col = "Zone_Number", into = c("Zone", "Zone_Number"), sep = " ")


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
  rename(tot_vacancy_2018_2019, BDBACH_OCT_18 = 'Bachelor, Oct-18', BDBACH_OCT_19 = 'Bachelor, Oct-19', 
         DQ_BDBACH_OCT_18 = 'Data quality, Bachelor, Oct-18',  DQ_BDBACH_OCT_19 = 'Data quality, Bachelor, Oct-19', 
         BD1_OCT_18 = '1 Bedroom, Oct-18', DQ_BD1_OCT_18 = 'Data quality, 1 Bedroom, Oct-18', 
         BD1_OCT_19 = '1 Bedroom, Oct-19', DQ_BD1_OCT_19 = 'Data quality, 1 Bedroom, Oct-19', 
         BD2_OCT_18 = '2 Bedroom, Oct-18', DQ_BD2_OCT_18 = 'Data quality, 2 Bedroom, Oct-18', 
         BD2_OCT_19 = '2 Bedroom, Oct-19', DQ_BD2_OCT_19 = 'Data quality, 2 Bedroom, Oct-19', 
         BD3_OCT_18 = '3 Bedroom +, Oct-18', DQ_BD3_OCT_18 = 'Data quality, 3 Bedroom +, Oct-18',
         BD3_OCT_19 = '3 Bedroom +, Oct-19', DQ_BD3_OCT_19 = 'Data quality, 3 Bedroom +, Oct-19', 
         BDTOT_OCT_18 = 'Total, Oct-18', DQ_BDTOT_OCT_18 = 'Data quality, Total, Oct-18',
         BDTOT_OCT_19 = 'Total, Oct-19', DQ_BDTOT_OCT_19 = 'Data quality, Total, Oct-19') 

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

tot_vacancy_2018_2019 <- separate(data = tot_vacancy_2018_2019, col = "unit_type", into = c("bedroom_type", "year"), sep = "_OCT_")

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


### color palette 

col_palette <- 
  c("#33CCCC", "#FF3333", "#006699", "#FFCC00")

##map

total_vac_rate_2018 <-
  tot_vac_df %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = BDTOTAL_OCT_18), lwd = 1, colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(3,1)],
                       na.value = "grey80", 
                       limits = c(0.7, 4.0),
                       oob = scales:: squish) +
  guides(fill = guide_colorbar(title = "Percentage (%)")) +
  theme_void() + 
  labs(title = "Total Private Row (Townhouse) and Apartment Vacancy Rate (%), by CMHC Zone, 2018") +
  theme(legend.position = "right",
        text = element_text(face = "plain"), 
        legend.title = element_text(face = "bold", 
                                    size = 10),
        legend.text = element_text(size = 10))

ggsave("output/tot_vac_rate_2018.pdf", plot = total_vac_rate_2018, width = 8,
       height = 4, units = "in", useDingbats = FALSE)

total_vac_rate_2019 <-
  tot_vac_df %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = BDTOTAL_OCT_19), lwd = 1, colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(3,1)],
                       na.value = "grey80", 
                       limits = c(0.3, 	3.1),
                       oob = scales:: squish) +
  guides(fill = guide_colorbar(title = "Percentage (%)")) +
  theme_void() + 
  labs(title = "Total Private Row (Townhouse) and Apartment Vacancy Rate (%), by CMHC Zone, 2019") +
  theme(legend.position = "right",
        text = element_text(face = "plain"), 
        legend.title = element_text(face = "bold", 
                                    size = 10),
        legend.text = element_text(size = 10))

ggsave("output/tot_vac_rate_2019.pdf", plot = total_vac_rate_2019, width = 8,
       height = 4, units = "in", useDingbats = FALSE)


save(total_vac_rate_2019, file = "output/tot_vacrate_2019.Rdata")
save(total_vac_rate_2018, file = "output/tot_vacrate_2018.Rdata")


#----------------------- cleaning cmhc ville de mtl level data from the housing portal ####

city_vac_rate <- read_csv("~/Desktop/RA/montreal-2020/data/vac_rate_mtl_ville.csv")


# Process APARTMENT vacancy_rates_2018_2019 --------------------------------------------------------

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

city_vac_rate <- separate(data = city_vac_rate, col = Year, into = c("Year", "Month"), sep = " ")


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


save(vacancy_rates_2018_2019, mtl_neighbourhoods, avg_rent_2018_2019, tot_vacancy_2018_2019, city_vac_rate, file = "output/housing_impacts_all.Rdata")
