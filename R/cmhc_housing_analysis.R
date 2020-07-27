#-----------------------------------# mapping & tables of cmhc data -------------------------------

library(tidyverse)
library(readxl)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(gridExtra)
#-----------------------


### color palette 

col_palette <- 
  c("#33CCCC", "#FF3333", "#006699", "#FFCC00")


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

##########mapping apartment vac and avg rents ########## 

###bachelor apartment

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

###1bed

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

###2bed 

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

###3bed

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

###total vac apt
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


### mapping total avg rent (townhouse & apt)####

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


##mapping total vacancy rate of primary market (including townhome and apartment) ####

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



########making tables for the report ##### 

#vac_rate_table <- tibble(`Listing Type` = character(length = length(tot_vacancy_2018_2019$bedroom_type)))
 #                        , 
 #                                `Daily active listings (average)` = numeric(length = length(unique_listing_type)),
 #                                `Annual revenue (CAD)` = numeric(length = length(unique_listing_type)),
  #                               `% of all listings` = numeric(length = length(unique_listing_type)),
   #                              `% of annual revenue` = numeric(length = length(unique_listing_type)),
  #                               `% average daily listing growth` = numeric(length = length(unique_listing_type))
#)

vac_rate_bd_breakdown <- matrix(c("3.3 %", "1.6 %", "1.5 %", "0.8 %", 
                              "1.4 %", "2.2 %", "2.8 %", "N/A", 
                              "N/A", "1.3 %", "0.7 %", "0.3 %"),ncol=4,byrow=TRUE)
colnames(vac_rate_bd_breakdown) <- c("Studio and Bachelors","1-Bedroom","2-Bedrooms", "3-Bedrooms and more")
rownames(vac_rate_bd_breakdown) <- c("City of Montreal","Downtown/Iles-des-soeurs","Plateau-Mont-Royal")
grid.table(vac_rate_bd_breakdown)


ggsave("output/vac_rate_bd_breakdown.pdf", plot = vac_rate_bd_breakdown) #, width = 8,
     #  height = 5, units = "in", useDingbats = FALSE)


avg_rent_bd_breakdown <- matrix(c("3.3 %", "1.6 %", "1.5 %", "0.8 %", 
                                  "1.4 %", "2.2 %", "2.8 %", "N/A", 
                                  "N/A", "1.3 %", "0.7 %", "0.3 %"),ncol=4,byrow=TRUE)
colnames(vac_rate_bd_breakdown) <- c("Studio and Bachelors","1-Bedroom","2-Bedrooms", "3-Bedrooms and more")
rownames(vac_rate_bd_breakdown) <- c("City of Montreal","Downtown/Iles-des-soeurs","Plateau-Mont-Royal")
grid.table(vac_rate_bd_breakdown)


ggsave("output/vac_rate_bd_breakdown.pdf", plot = vac_rate_bd_breakdown, width = 8,
       height = 5, units = "in", useDingbats = FALSE)
