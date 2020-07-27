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
ltr_mtl %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ab_id)) %>% 
  distinct(ab_id, .keep_all = T) %>% 
  left_join(boroughs, .) %>% # this left join is a bit problematic : It duplicates 2 rows.
  count(borough) %>% 
  mutate(perc = n / sum(n)) %>% 
  View()
  ggplot()+
  geom_sf(aes(fill = perc),
          lwd = 1, 
          colour = "white")+
  scale_fill_gradientn(
    colors = col_palette[c(5,2,3)],
    na.value = "grey80",
    # limits = c(0, 0.1),
    # oob = scales::squish,
  labels = scales::percent) +
  guides(fill = guide_colorbar(title = "Matches/All matches")) +
  theme_void() +
  theme(legend.position = "right",
        # text = element_text(family = "Futura", face = "plain"),
        # legend.title = element_text(family = "Futura", face = "bold", 
        #                             size = 10),
        # legend.text = element_text(family = "Futura", size = 10)
  )



# Per DAs
ltr_mtl_cts %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ab_id)) %>% 
  distinct(ab_id, .keep_all = T) %>% 
  count(GeoUID) %>% 
  left_join(filter(DAs, CMA_UID == "Montreal"), .) %>% 
  mutate(perc = n / sum(n, na.rm = T)) %>% 
  ggplot()+
  geom_sf(aes(fill = perc),
          lwd = NA, 
          colour = "white")+
  scale_fill_gradientn(
    colors = col_palette[c(5,2,3)],
    na.value = "grey80",
    # limits = c(0, 0.1),
    # oob = scales::squish,
    labels = scales::percent) +
  guides(fill = guide_colorbar(title = "Matches/All matches")) +
  theme_void() +
  theme(legend.position = "right",
        # text = element_text(family = "Futura", face = "plain"),
        # legend.title = element_text(family = "Futura", face = "bold", 
        #                             size = 10),
        # legend.text = element_text(family = "Futura", size = 10)
  )


  
### Asking rent #########################################################
ltr_mtl %>% 
  st_drop_geometry() %>% 
  filter(price >425, price <8000) %>% 
  arrange(created) %>% 
  distinct(ab_id, id, .keep_all = T) %>% 
  mutate(matched = if_else(!is.na(ab_id), TRUE, FALSE)) %>% 
ggplot()+
  geom_smooth(aes(created, price, color = matched), se = F)+
  scale_color_manual(name = "Group",
                     values = c("#00CC66", "#FF3333"),
                     labels = c("Did not match", "Matched"))+
  ggtitle("Daily price of new listings")


### Portrait of hosts that put listings on LTR #########################
#host_revenue
daily %>%
  filter(housing,
         date <= key_date, date > key_date - years(1),
         status == "R") %>%
  group_by(property_ID) %>%
  summarize(revenue_LTM = sum(price) * exchange_rate) %>% 
  left_join(property, .) %>% 
  distinct(.keep_all = T) %>% 
  st_drop_geometry() %>% 
  group_by(host_ID) %>% 
  summarize(host_rev = sum(revenue_LTM)*exchange_rate) %>% 
  filter(host_rev>0) %>% 
  arrange(-host_rev) %>% 
  drop_na() %>% 
  ggplot()+
  geom_histogram(aes(host_rev),fill = "red", alpha = 0.2)+
  geom_histogram(data = daily %>%
                   filter(housing,
                          date <= key_date, date > key_date - years(1),
                          status == "R", host_ID %in% ltr_hosts_id$host_ID) %>%
                   group_by(property_ID) %>%
                   summarize(revenue_LTM = sum(price) * exchange_rate) %>% 
                   left_join(property, .) %>% 
                   distinct(.keep_all = T) %>% 
                   st_drop_geometry() %>% 
                   group_by(host_ID) %>% 
                   summarize(host_rev = sum(revenue_LTM)*exchange_rate) %>% 
                   filter(host_rev>0) %>% 
                   arrange(-host_rev) %>% 
                   drop_na(), 
                 aes(x = host_rev, y = ..count.. * 35),fill = "blue", alpha = 0.2)+
  xlim(c(0,100000)) +
  scale_y_continuous(sec.axis = sec_axis(~ ./35, name = "Count (Match)"))+
  theme(axis.text.y.right = element_text(color = "blue"),
  axis.line.y.right = element_line(color = "blue"),
  axis.text.y.left = element_text(color = "red"),
  axis.line.y.left = element_line(color = "red"))+
  ylab("Count (Not match)")+
  ggtitle("Host revenue histogram")+
  xlab("Host revenue")


### Portrait of listings that went on LTR ##########################
#last scrape date
property %>% 
  st_drop_geometry() %>% 
  filter(ltr_id %in% ltr_mtl$id) %>% 
  distinct(property_ID, .keep_all = T) %>% 
  count(scraped) %>% 
  arrange(scraped) %>% 
  filter(n < "2020-01-01") %>% 
  nrow()
  ggplot()+
  geom_line(aes(scraped, n))+
  ylab("Number of listings") +
  ggtitle("Number of listings that matched per last scraped date")


#time listings were active
property %>% 
  st_drop_geometry() %>% 
  filter(is.na(ltr_id),
         scraped >= "2020-01-01") %>%
  distinct(property_ID, .keep_all = T) %>% 
  mutate(how_long_they_stay = (scraped-created) / 30) %>% 
  ggplot()+
  geom_histogram(aes(how_long_they_stay), fill = "red", alpha = 0.2)+
  geom_histogram(data = property %>% 
                   st_drop_geometry() %>% 
                   filter(!is.na(ltr_id),
                          scraped >= "2020-01-01") %>%
                   distinct(property_ID, .keep_all = T) %>% 
                   mutate(how_long_they_stay = (scraped-created) / 30),
                 aes(x = how_long_they_stay, y = ..count.. * 15),fill = "blue", alpha = 0.2)+
  xlim(c(0,60)) +
  scale_y_continuous(sec.axis = sec_axis(~ ./15, name = "Count (Match)"))+
  theme(axis.text.y.right = element_text(color = "blue"),
        axis.line.y.right = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "red"),
        axis.line.y.left = element_line(color = "red"))+
  ylab("Count (Not match)")+
  ggtitle("Since how long (months) are listings on AirBNB (scraped - created)")+
  xlab("Months")

#time FREH_2020 listings were active
property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% FREH$property_ID,
         is.na(ltr_id)) %>% 
  distinct(property_ID, .keep_all = T) %>% 
  mutate(how_long_they_stay = (scraped-created) / 30) %>% 
  ggplot()+
  geom_histogram(aes(how_long_they_stay), fill = "red", alpha = 0.2)+
  geom_histogram(data = property %>% 
                   st_drop_geometry() %>% 
                   filter(property_ID %in% FREH$property_ID,
                          !is.na(ltr_id)) %>% 
                   distinct(property_ID, .keep_all = T) %>% 
                   mutate(how_long_they_stay = (scraped-created) / 30),
                 aes(x = how_long_they_stay, y = ..count.. * 15),fill = "blue", alpha = 0.2)+
  xlim(c(0,60)) +
  scale_y_continuous(sec.axis = sec_axis(~ ./15, name = "Count (Match)"))+
  theme(axis.text.y.right = element_text(color = "blue"),
        axis.line.y.right = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "red"),
        axis.line.y.left = element_line(color = "red"))+
  ylab("Count (Not matched)")+
  ggtitle("Since how long (months) are FREH_2020 listings on AirBNB (scraped - created)")+
  xlab("Months")



ggplot()+
  geom_line(data= ltr_mtl %>% 
              st_drop_geometry() %>% 
              # filter(!is.na(ab_id)) %>% 
              group_by(created) %>% 
              arrange(scraped) %>% 
              distinct(id, .keep_all = TRUE) %>% 
              group_by(day=floor_date(created, "day")) %>% 
              summarize(n = n(), `Median price of listings` = median(price, na.rm=TRUE)), 
            aes(day, `Median price of listings`, color="Median price on date created"))+
  geom_line(data= ltr_mtl %>% 
              st_drop_geometry() %>%
              # filter(!is.na(ab_id)) %>% 
              group_by(created) %>% 
              arrange(desc(scraped)) %>% 
              distinct(id, .keep_all = TRUE) %>% 
              group_by(day=floor_date(scraped, "day")) %>% 
              summarize(n = n(), `Median price of listings` = median(price, na.rm=TRUE)), 
            aes(day, `Median price of listings`, color="Median price on date last scraped"))+
  labs(x="Date",
       y="Median price",
       title="Median price on date created versus on last date scraped",
       color="Legend")+
  scale_colour_manual(name = "Legend",
                      values = c("Median price on date created" = "blue", "Median price on date last scraped" = "red"),
                      labels = c("Median price on date created", "Median price on date last scraped"))+
  theme_minimal()


#time LTR listings were active, match vs no match
ltr_mtl %>% 
  st_drop_geometry() %>% 
  filter(is.na(ab_id)) %>%
  arrange(desc(scraped)) %>% 
  distinct(id, .keep_all = T) %>% 
  mutate(how_long_they_stay = (scraped-created)) %>% 
  filter(how_long_they_stay > 0) %>% 
  ggplot()+
  geom_histogram(aes(how_long_they_stay), fill = "red", alpha = 0.2)+
  geom_histogram(data = ltr_mtl %>%
                   st_drop_geometry() %>%
                   filter(!is.na(ab_id)) %>%
                   arrange(desc(scraped)) %>% 
                   distinct(id, .keep_all = T) %>%
                   mutate(how_long_they_stay = (scraped-created)),
                 aes(x = how_long_they_stay, y = ..count.. * 15),fill = "blue", alpha = 0.2)+
  xlim(c(0,60)) +
  scale_y_continuous(sec.axis = sec_axis(~ ./15, name = "Count (Match)"))+
  theme(axis.text.y.right = element_text(color = "blue"),
        axis.line.y.right = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "red"),
        axis.line.y.left = element_line(color = "red"))+
  ylab("Count (Not match)")+
  ggtitle("How long LTR listings stayed online (scraped - created)")+
  xlab("Days")
