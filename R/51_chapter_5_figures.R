#### Chapter 5 FIGURES ####################################################

### load libraries and data ###########################################
library(tidyverse)
library(lubridate)
library(sf)
library(gt)
library(future)
library(ggplot2)

load("data/str_montreal.Rdata")
load("data/ltr_matches.Rdata")

### FIGURE 11. Concentration of STR listings matched with LTR listings by borough ########################################

ltr_unique_ab_id %>% 
  select(-geometry) %>% 
  count(borough) %>% 
  left_join(boroughs, .) %>% 
  ggplot()+
  geom_sf(aes(fill = n),
          lwd = NA, 
          colour = "white")+
  scale_fill_gradientn(
    colors = col_palette[c(5,2,3)],
    na.value = "grey80",
    limits = c(0, 400),
    oob = scales::squish,
  ) +
  guides(fill = guide_colorbar(title = "STR to LTR Matches")) +
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
  mutate(matched = if_else(!is.na(ab_id), TRUE, FALSE)) %>% 
  group_by(matched, created) %>%
  summarize(avg_price = mean(price)) %>% 
  ggplot()+
  geom_line(aes(created, avg_price, color = matched), alpha = 0.2)+
  geom_smooth(aes(created, avg_price, color = matched), se = F)+
  ggtitle("Daily price of new LTR listings")+
  geom_smooth(data = (unique_ltr %>% 
                        filter(price >425, price <8000) %>% 
                        group_by(created) %>%
                        summarize(avg_price = mean(price))), aes(created, avg_price),  se = F,
              color = "grey80")+
  scale_color_manual(name = "Group",
                     values = c("#00CC66", "#FF3333"),
                     labels = c("Did not match", "Matched"))


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
  ylab("% of all listings within the group")+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(name = "Group",
                     values = c("#00CC66", "#FF3333"),
                     labels = c("Did not match", "Matched")) +
  ggtitle("How old are STR listings (active in 2020)")


### Figure 14. Distribution of matches by revenue earned between 14 March 2019 to 14 March 2020 ##########################

revenue_2019 %>% # still a lot of more work to do on that!!!!!
  st_drop_geometry() %>% 
  filter(revenue_LTM > 0) %>%
  mutate(matched = if_else(host_ID %in% (property %>%
                                           st_drop_geometry() %>%
                                           filter(property_ID %in% ltr_unique_ab_id$ab_id) %>%
                                           pull(host_ID)), TRUE, FALSE)) %>%
  group_by(host_ID, matched) %>% 
  summarize("host_rev" = round(sum(revenue_LTM), digit = -3)) %>% 
  count(host_rev, matched) %>% 
  ungroup() %>% 
  mutate(perc = host_rev/sum(host_rev) )%>%
  group_by(matched) %>% 
  ggplot()+
  geom_line(aes(host_rev, perc, color = matched), alpha = 0.3)+
  geom_smooth(aes(host_rev, perc, color = matched), se = F)+
  xlab("Hosts revenue")+
  ylab("% of all hosts within the group")+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(name = "Group",
                     values = c("#00CC66", "#FF3333"),
                     labels = c("Did not match", "Matched")) +
  ggtitle("Revenue distribution of hosts")


### Figure 15. Length of stay of matches and non-matches on LTR platforms ##########################

unique_ltr %>% 
  mutate(how_long_they_stay = scraped-created) %>% 
  arrange(desc(how_long_they_stay)) %>% 
  mutate(matched = if_else(!is.na(ab_id), TRUE, FALSE)) %>% 
  count(how_long_they_stay, matched) %>% 
  group_by(matched) %>% 
  mutate(perc = n/sum(n)) %>% 
  ggplot()+
  # geom_line(aes(how_long_they_stay, perc, color = matched), alpha = 0.3)+
  geom_smooth(aes(how_long_they_stay, perc, color = matched), se = F)+
  xlab("Days online")+
  ylab("% of all listings within the group")+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(name = "Group",
                     values = c("#00CC66", "#FF3333"),
                     labels = c("Did not match", "Matched")) +
  ggtitle("How old are LTR listings")














