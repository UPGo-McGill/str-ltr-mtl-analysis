### load libraries and data ###########################################
library(tidyverse)
library(lubridate)
library(sf)
library(gt)
library(future)
library(ggplot2)


load("data/str_montreal.Rdata")
load("data/ltr_matches.Rdata")

### Colour palette #############################################################

col_palette <- 
  c("#FF3333", "#FFCC00", "#FF6600", "#66FFFF", "#00CC66")

scales::show_col(col_palette)


### matches location #################################################

# Per borough
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

  
### Asking rent #########################################################

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




### Portrait of listings that went on LTR ##########################

#time STR listings were active
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


# host revenue distribution match vs did not match
revenue_2019 %>% # still a lot of more work to do on that!!!!!
  st_drop_geometry() %>% 
  filter(revenue_LTM > 0) %>%
  mutate(matched = if_else(host_ID %in% (property %>%
                                        st_drop_geometry() %>%
                                        filter(property_ID %in% ltr_unique_ab_id$ab_id) %>%
                                        pull(host_ID)), TRUE, FALSE)) %>%
  group_by(host_ID, matched) %>% 
  summarize("host_rev" = sum(revenue_LTM)) %>% 
  group_by(matched) %>% 
  ggplot()+
  geom_density(aes(host_rev, fill = matched), alpha = 0.7)+
  scale_x_continuous(limits = c(0, 100000))+
  xlab("Hosts revenue")+
  scale_color_manual(name = "Group",
                     values = c("#00CC66", "#FF3333"),
                     labels = c("Did not match", "Matched")) +
  ggtitle("Smoothed density estimate of hosts revenue")


# time LTR listings were active
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






#
unique_ltr_unnest <- 
unique_ltr %>% 
  unnest(ab_id)

unique_ltr_unnest %>% 
  select(ab_id, furnished)

inner_join(select(st_drop_geometry(property), scraped, property_ID),
           unique_ltr_unnest %>% 
             filter(!is.na(ab_id)) %>% 
             select(ab_id, furnished) %>% 
             rename(property_ID = ab_id)
             , by = "property_ID"
) %>% 
  distinct() %>% 
  count(scraped, furnished) %>%
  ggplot()+
  geom_point(aes(scraped, n, color = furnished), se = F)
