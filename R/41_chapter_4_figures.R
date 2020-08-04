#### Chapter 4 FIGURES ####################################################

source("R/01_startup.R")

### Figure 9 - Active and reserved listings since 2018 ####################################################

ggplot()+
  geom_line(data = daily %>% 
              filter(housing, date >= "2017-12-29", status == "R") %>% 
              count(date, status) %>% 
              mutate(n = data.table::frollmean(n, 7)),
            aes(date, n, color = status), alpha = 0.4)+
  geom_smooth(data = daily %>% 
                filter(housing, date >= "2017-12-29", status == "R") %>% 
                count(date, status) %>% 
                mutate(n = data.table::frollmean(n, 7)),
              aes(date, n, color = status), se = F, lwd=0.9)+
  geom_line(data = daily %>% 
              filter(housing, date >= "2017-12-29", status == "A") %>% 
              count(date, status) %>% 
              mutate(n = data.table::frollmean(n, 7)),
            aes(date, n, color = status), alpha = 0.4)+
  geom_smooth(data = daily %>% 
                filter(housing, date >= "2017-12-29", status == "A") %>% 
                count(date, status) %>% 
                mutate(n = data.table::frollmean(n, 7)),
              aes(date, n, color = status), se = F, lwd=0.9)+
  scale_color_manual(name="Status", labels = c("Available", "Reserved"),values = col_palette[c(1, 3)]) +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = "Number of listings") +
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



### Figure 10 - Active and reserved listings in 2020 ####################################################

ggplot()+
  geom_line(data = daily %>% 
              filter(housing, date >= "2019-12-29", status == "R") %>% 
              count(date, status) %>% 
              mutate(n = data.table::frollmean(n, 7)),
            aes(date, n, color = status), alpha = 0.4)+
  stat_smooth(geom='line', data = daily %>% 
                filter(housing, date >= "2019-12-29", status == "R") %>% 
                count(date, status) %>% 
                mutate(n = data.table::frollmean(n, 7)),
              aes(date, n, color = status), se = F, lwd=0.9)+
  geom_line(data = daily %>% 
              filter(housing, date >= "2019-12-29", status == "A") %>% 
              count(date, status) %>% 
              mutate(n = data.table::frollmean(n, 7)),
            aes(date, n, color = status), alpha = 0.4)+
  stat_smooth(geom='line', data = daily %>% 
                filter(housing, date >= "2019-12-29", status == "A") %>% 
                count(date, status) %>% 
                mutate(n = data.table::frollmean(n, 7)),
              aes(date, n, color = status), se = F, lwd = 0.9)+
  scale_color_manual(name="Status", labels = c("Available", "Reserved"),values = col_palette[c(1, 3)]) +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = "Number of listings") +
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

?scale_color_manual

