#### Chapter 3 FIGURES ####################################################

### load libraries ###########################################
library(tidyverse)
library(ggplot2)
library(patchwork)
library(data.table)

### Colour palette #############################################################

col_palette <- 
  c("#FF3333", "#FFCC00", "#FF6600", "#66FFFF", "#00CC66")

scales::show_col(col_palette)

### FIGURE 8 - Housing units converted to dedicated STRs in Montreal ###########################################

housing_graph <-
  housing_loss %>% 
  mutate(`Listing type` = factor(
    `Listing type`, 
    levels = c("Summer listings", "Private room", "Entire home/apt"))) %>% 
  ggplot(aes(`Listing type`)) +
  geom_col(aes(date, `Housing units`, fill = `Listing type`),
           lwd = 0) +
  theme_minimal() +
  #scale_y_continuous(name = NULL, label = scales::comma, limits = c(0, 200)) +
  #scale_x_date(name = NULL, limits = c(as.Date("2016-10-01"), NA)) +
  scale_fill_manual(values = col_palette[3:1]) +
  theme(legend.position = "bottom", 
        text = element_text(face = "plain"), #family = "Futura", 
        legend.title = element_text(face = "bold", #family = "Futura", 
                                    size = 10),
        legend.text = element_text( size = 10) #family = "Futura",
  )
