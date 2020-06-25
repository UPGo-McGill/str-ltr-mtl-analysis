### load libraries and data ###########################################
library(tidyverse)
library(lubridate)
library(sf)
library(gt)
library(future)
library(ggplot2)

memory.limit(size = 48000)
plan(multiprocess, workers = 4)

load("data/montreal_str_processed_b.Rdata")
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



# Per CTs
ltr_mtl_cts %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ab_id)) %>% 
  distinct(ab_id, .keep_all = T) %>% 
  count(GeoUID) %>% 
  left_join(filter(CTs, CMA_UID == "Montreal"), .) %>% 
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
                     labels = c("Matched", "Did not match"))+
  ggtitle("Daily price of new listings")


### Portrait of hosts that put listings on LTR #########################
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
                 aes(x = host_rev, y = ..count.. * 30),fill = "blue", alpha = 0.2)+
  xlim(c(0,100000)) +
  scale_y_continuous(sec.axis = sec_axis(~ ./30, name = "Count (Match)"))+
  theme(axis.text.y.right = element_text(color = "blue"),
  axis.line.y.right = element_line(color = "blue"),
  axis.text.y.left = element_text(color = "red"),
  axis.line.y.left = element_line(color = "red"))+
  ylab("Count (Not match)")+
  ggtitle("Host revenue histogram")+
  xlab("Host revenue")



### portrait of STR units going on LTR #################################
## GH, FREH, Multi
# 
# GH_hosts <- 
#   GH %>% 
#   count(host_ID) %>% 
#   st_drop_geometry()
# 
# rbind(property %>% 
#         filter(property_ID %in% matches$ab_id,
#                host_ID %in% GH_hosts$host_ID) %>% 
#         st_drop_geometry() %>% 
#         select(property_ID),
#       daily %>% 
#         filter(date >= "2019-03-14",
#                multi == T) %>% 
#         count(property_ID) %>% 
#         filter(property_ID %in% matches$ab_id) %>% 
#         select(property_ID),
#       FREH %>% 
#         filter(property_ID %in% matches$ab_id) %>% 
#         count(property_ID) %>% 
#         select(property_ID)
# ) %>% distinct() %>% nrow() /
#   rbind((kj_matches),
#         (cl_matches)) %>% select(x_name) %>%  distinct() %>%  nrow()
# nrow(distinct(kj_matches, x_name))
# 
# distinct(rbind(property %>% 
#                  filter(property_ID %in% matches$ab_id,
#                         host_ID %in% count(st_drop_geometry(GH), host_ID)$host_ID) %>% 
#                  st_drop_geometry() %>% 
#                  select(property_ID),
#                daily %>% 
#                  filter(date >= "2019-03-14",
#                         multi == T) %>% 
#                  count(property_ID) %>% 
#                  filter(property_ID %in% matches$ab_id) %>% 
#                  select(property_ID),
#                FREH %>% 
#                  filter(property_ID %in% matches$ab_id) %>% 
#                  count(property_ID) %>% 
#                  select(property_ID))) %>% nrow() /
#   distinct(matches, ab_id) %>% nrow()

