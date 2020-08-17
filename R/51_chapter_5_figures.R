#### Chapter 5 FIGURES ####################################################

source("R/01_startup.R")

load("output/str_processed.Rdata")
load("output/geometry.Rdata")
load("output/ltr_processed.Rdata")

### Unique matches #################################################################

# distinct LTR listings
unique_ltr <- 
  ltr %>% 
  st_drop_geometry() %>% 
  arrange(desc(scraped)) %>% 
  distinct(id, .keep_all = T)

#unique matching property_ID locations using street address
ltr_unique_property_ID <- 
  ltr %>% 
  st_drop_geometry() %>% 
  filter(!is.na(property_ID)) %>% 
  unnest(property_ID) %>% 
  filter(property_ID %in% filter(property, scraped >= "2020-01-01")$property_ID) %>% 
  arrange(desc(scraped)) %>% 
  distinct(property_ID) %>% 
  inner_join(unnest(ltr, property_ID), by = "property_ID") %>% 
  arrange(desc(scraped)) %>% 
  distinct(property_ID, .keep_all = T)


### FIGURE 11. Concentration of STR listings matched with LTR listings by borough ########################################

ltr_unique_property_ID %>% 
  select(-geometry) %>% 
  count(borough) %>% 
  left_join(boroughs, .) %>% 
  ggplot()+
  geom_sf(aes(fill = n),
          lwd = NA, 
          colour = "white")+
  guides(fill = guide_colorbar(title = "STR to LTR Matches")) +
  scale_fill_gradientn(colors = col_palette[c(3, 4, 1)],
                       na.value = "grey80",
                       limits = c(0, 400),
                       oob = scales::squish
  ) +
  theme_void() +
  theme(legend.position = "right",
        # text = element_text(family = "Futura", face = "plain"),
        # legend.title = element_text(family = "Futura", face = "bold", 
        #                             size = 10),
        # legend.text = element_text(family = "Futura", size = 10)
  )


### Figure 12. Median asking rents for matches and non-matches through time #############################################

unique_ltr %>% 
  filter(price >425, price <8000) %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  group_by(matched, created) %>%
  summarize(avg_price = mean(price)) %>% 
  ggplot()+
  geom_line(aes(created, avg_price, color = matched), alpha = 0.2)+
  geom_smooth(aes(created, avg_price, color = matched), se = F)+
  geom_smooth(data = (unique_ltr %>% 
                        filter(price >425, price <8000) %>% 
                        group_by(created) %>%
                        summarize(avg_price = mean(price))), aes(created, avg_price),  se = F,
              color = "grey80")+
  scale_x_date(name = "Date created") +
  scale_y_continuous(name = "Average daily price ($)", label = scales::comma) +
  scale_color_manual(name = "Group",
                     values = col_palette[c(1, 3)],
                     labels = c("Did not match", "Matched")) +
  annotate("rect", xmin = as.Date("2020-03-14"), xmax = as.Date("2020-06-25"), ymin = -Inf, ymax = Inf, alpha = .1)+
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(#family = "Futura", face = "bold", 
          size = 10),
        legend.text = element_text(#family = "Futura", 
          size = 10)
  )


#ggtitle("Daily price of new LTR listings")+

### Figure 13. Distribution of matches and non-matches by date created on STR platforms #################################

property %>% 
  st_drop_geometry() %>% 
  filter(scraped >= "2020-01-01",
         property_ID %in% filter(daily, housing, status != "B", date >= "2020-01-01")$property_ID) %>% 
  mutate(how_long_they_stay = round((scraped-created) / 30) /12) %>% 
  arrange(desc(how_long_they_stay)) %>% 
  mutate(matched = if_else(!is.na(ltr_ID), TRUE, FALSE)) %>% 
  count(how_long_they_stay, matched) %>% 
  group_by(matched) %>% 
  mutate(perc = n/sum(n)) %>% 
  ggplot()+
  geom_line(aes(how_long_they_stay, perc, color = matched), alpha = 0.3)+
  geom_smooth(aes(how_long_they_stay, perc, color = matched), se = F)+
  xlab("Years of activity")+
  ylab("Percentage of all listings within the group")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_color_manual(name = "Group",
                     values = col_palette[c(1, 3)],
                     labels = c("Did not match", "Matched")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(#family = "Futura", face = "bold", 
          size = 10),
        legend.text = element_text(#family = "Futura", 
          size = 10)
  )

### Figure 14. Distribution of matches by revenue earned between 14 March 2019 to 14 March 2020 ##########################

daily %>%
  filter(housing,
         date <= LTM_end_date, date >= LTM_start_date,
         status == "R") %>%
  group_by(property_ID) %>%
  summarize(revenue_LTM = sum(price) ) %>% 
  inner_join(property, .) %>% # still a lot of more work to do on that!!!!!
  st_drop_geometry() %>% 
  filter(revenue_LTM > 0) %>%
  mutate(matched = if_else(host_ID %in% (property %>%
                                           st_drop_geometry() %>%
                                           filter(property_ID %in% ltr_unique_property_ID$property_ID) %>%
                                           pull(host_ID)), TRUE, FALSE)) %>%
  group_by(host_ID, matched) %>% 
  summarize(host_rev = sum(revenue_LTM)) %>% 
  group_by(matched) %>% 
  ggplot()+
  geom_density(aes(host_rev, fill = matched), colour=NA, alpha = 0.7)+
  scale_x_continuous(name="Hosts revenue", limits = c(0, 100000),
                     labels =scales::dollar)+
  scale_y_continuous(name="Density of all hosts within the group",
                     labels =scales::comma)+
  scale_fill_manual(name = "Group",
                     values = col_palette[c(1, 3)],
                     labels = c("Did not match", "Matched"))+
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(face = "plain"), #family = "Futura", 
        legend.title = element_text(#family = "Futura", face = "bold", 
          size = 10),
        legend.text = element_text(#family = "Futura", 
          size = 10)
  )


### Figure 15. Length of stay of matches and non-matches on LTR platforms ##########################

unique_ltr %>% 
  mutate(how_long_they_stay = scraped-created) %>% 
  arrange(desc(how_long_they_stay)) %>% 
  mutate(matched = if_else(!is.na(property_ID), TRUE, FALSE)) %>% 
  count(how_long_they_stay, matched) %>% 
  group_by(matched) %>% 
  mutate(perc = n/sum(n)) %>% 
  ggplot()+
  # geom_line(aes(how_long_they_stay, perc, color = matched), alpha = 0.3)+
  geom_smooth(aes(how_long_they_stay, perc, color = matched), se = F)+
  xlab("Days online")+
  ylab("Percentage of all listings within the group")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_color_manual(name = "Group",
                     values = col_palette[c(1, 3)],
                     labels = c("Did not match", "Matched")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(#family = "Futura", face = "bold", 
          size = 10),
        legend.text = element_text(#family = "Futura", 
          size = 10)
        )


