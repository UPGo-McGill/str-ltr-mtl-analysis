#### Chapter 3 FIGURES ####################################################

source("R/01_startup.R")


### FIGURE 3.1 - Housing units converted to dedicated STRs in Montreal ###########################################

# Reuse GH_total from analysis to produce housing loss graph
GH_total <-
  GH %>%
  st_drop_geometry() %>%
  filter(status != "B") %>% 
  group_by(date) %>%
  summarize(GH_units = sum(housing_units)) %>%
  mutate(GH_average = frollmean(GH_units, 30, align = "right", fill = 198))
  
# Housing loss graph
FREH %>%
  group_by(date) %>%
  summarize(`Entire home/apt` = n()) %>%
  left_join(GH_total, by = "date") %>%
  rename(`Private room` = GH_average) %>%
  gather(`Entire home/apt`, `Private room`, 
         key = `Listing type`, value = `Housing units`) %>% 
  mutate(`Listing type` = factor(
    `Listing type`, 
    levels = c("Private room", "Entire home/apt"))
    ) %>% 
  ggplot(aes(`Listing type`)) +
  geom_col(aes(date, `Housing units`, 
               fill = `Listing type`), 
           lwd = 0) +
  scale_fill_manual(values = col_palette[c(1, 3, 2)]) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-10-01"), NA)) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  annotate("rect", xmin = as.Date("2020-03-14"), xmax = as.Date("2020-06-25"), ymin = -Inf, ymax = Inf, alpha = .2)+
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(face = "plain"), #family = "Futura", 
        legend.title = element_text(face = "bold", #family = "Futura", 
                                    size = 10),
        legend.text = element_text( size = 10) #family = "Futura",
  )
