#### Chapter 3 FIGURES ####################################################

source("R/01_startup.R")


### FIGURE 3.1 - Housing units converted to dedicated STRs in Montreal ###########################################

housing_graph <- housing_loss %>% 
  mutate(`Listing type` = factor(
    `Listing type`, 
    levels = c("Private room", "Entire home/apt"))) %>% 
  ggplot(aes(`Listing type`)) +
  geom_col(aes(date, `Housing units`, 
               fill = `Listing type`), lwd = 0) +
  theme_minimal() +
  scale_fill_manual(values = col_palette[c(1, 3, 2)]) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-10-01"), NA)) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  annotate("rect", xmin = as.Date("2020-03-14"), xmax = as.Date("2020-06-25"), ymin = 0, ymax = 5200, alpha = .2)+
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(face = "plain"), #family = "Futura", 
        legend.title = element_text(face = "bold", #family = "Futura", 
                                    size = 10),
        legend.text = element_text( size = 10) #family = "Futura",
  )
