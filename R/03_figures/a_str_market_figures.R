#### STR MONTREAL FIGURES #####################################################
library(tidyverse)
library(ggplot2)
library(patchwork)

memory.limit(size = 48000)
plan(multiprocess, workers = 4)

load("data/montreal_str_processed_c.Rdata")


### Colour palette #############################################################

col_palette <- 
  c("#FF3333", "#FFCC00", "#FF6600", "#66FFFF", "#00CC66")

scales::show_col(col_palette)


### Active listings ##############################################################

active_listings_type <- 
  daily %>% 
  filter(housing) %>% 
  count(date, listing_type)

daily %>% 
  filter(housing) %>% 
  count(date) %>% 
  ggplot() +
  geom_line(aes(date, n), colour = "black", size = 1.5) +
  geom_line(data = active_listings_type, aes(date, n, colour = listing_type),
            size = 0.75) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_date(name = NULL, limits = c(as.Date("2017-01-01"), NA)) +
  scale_colour_manual(name = "", values = col_palette[1:3]) +
  theme_minimal() +
  theme(legend.position = "bottom",
#        text = element_text(family = "Futura"),
  )




### Montreal maps ###############################################################

ggplot(CTs)+
  geom_sf(aes(fill = dwellings))

property %>%
  filter(housing, created <= key_date, scraped >= key_date) %>%
  st_join(boroughs) %>% 
  st_drop_geometry() %>%
  count(borough, dwellings) %>%
  left_join(boroughs, .) %>% 
  ggplot() +
  geom_sf(
    aes(fill = n / dwellings), 
    lwd = 1, 
    colour = "white") +
  # geom_sf_label(
  #   aes(label = borough), 
  #   size = 1.8, 
  #   # family = "Futura",
  #   alpha = 0.5,
  #   fill = alpha("white", 0.6)) +
  scale_fill_gradientn(
    colors = col_palette[c(5,3)],
    na.value = "grey80",
    limits = c(0, 0.1),
    oob = scales::squish,
    labels = scales::percent) +
  guides(fill = guide_colorbar(title = "STRs/dwelling")) +
  theme_void() +
  theme(legend.position = "right",
        # text = element_text(family = "Futura", face = "plain"),
        # legend.title = element_text(family = "Futura", face = "bold", 
        #                             size = 10),
        # legend.text = element_text(family = "Futura", size = 10)
  )

property %>%
  filter(housing, created <= key_date, scraped >= key_date) %>%
  st_join(boroughs) %>% 
  st_drop_geometry() %>%
  count(borough, dwellings) %>%
  left_join(boroughs, .) %>% 
  mutate(percentage = n/dwellings) %>% 
  rename(listings = n) %>% 
  st_drop_geometry %>% 
  arrange(desc(percentage)) %>% 
  gt()





property %>%
  filter(housing, created <= key_date, scraped >= key_date) %>%
  st_drop_geometry() %>%
  count(GeoUID) %>%
  left_join(CTs, .) %>%
  st_intersection(borough_geometries) %>% 
  ggplot() +
  geom_sf(aes(fill = n / dwellings), lwd = NA, colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(5, 3)],
                       na.value = "grey80",
                       limits = c(0, 0.1),
                       oob = scales::squish,
                       labels = scales::percent) +
  guides(fill = guide_colorbar(title = "STRs/dwelling")) +
  theme_void() +
  theme(legend.position = "right",
        # text = element_text(family = "Futura", face = "plain"),
        # legend.title = element_text(family = "Futura", face = "bold", 
        #                             size = 10),
        # legend.text = element_text(family = "Futura", size = 10)
  )

# listings_map <- 
#   boroughs_map + CT_map + plot_layout(guides = "collect")




### Host revenue percentiles graph ###########################################

daily %>%
  filter(housing == TRUE, date > key_date - years(1), date < key_date, status == "R") %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price)*exchange_rate) %>%
  filter(rev > 0) %>%
  summarize(
    `Top 1%`  = sum(rev[rev > quantile(rev, c(0.99))] / sum(rev)),
    `Top 5%`  = sum(rev[rev > quantile(rev, c(0.95))] / sum(rev)),
    `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)),
    `Top 20%` = sum(rev[rev > quantile(rev, c(0.80))] / sum(rev))) %>% 
  gather(`Top 1%`, `Top 5%`, `Top 10%`, `Top 20%`, key = "percentile", 
         value = "value") %>% 
  mutate(percentile = factor(percentile, 
                             levels = c('Top 1%', 'Top 5%', 'Top 10%', 
                                        'Top 20%'))
  ) %>% 
  ggplot() +
  geom_bar(aes(percentile, value, fill = percentile), stat = "identity") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = col_palette[1:4]) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        # text = element_text(family = "Futura", face = "plain"),
        # legend.title = element_text(family = "Futura", face = "bold",
                                    # size = 10)
        # legend.text = element_text(family = "Futura", size = 10),
        # legend.position = "none"
        )




### Housing loss ####################################################

housing_loss %>% 
  mutate(`Listing type` = factor(
    `Listing type`, 
    levels = c("Summer listings", "Private room", "Entire home/apt"))) %>% 
  ggplot(aes(`Listing type`)) +
  geom_col(aes(date, `Housing units`, fill = `Listing type`),
           lwd = 0) +
  theme_minimal() +
  # scale_y_continuous(name = NULL, label = scales::comma, limits = c(0, 200)) +
  scale_x_date(name = NULL, limits = c(as.Date("2017-01-01"), NA)) +
  scale_fill_manual(values = col_palette[3:1]) +
  theme(legend.position = "bottom", 
        # text = element_text(family = "Futura", face = "plain"),
        # legend.title = element_text(family = "Futura", face = "bold",
        #                             size = 10),
        # legend.text = element_text(family = "Futura", size = 10)
  )



### multilistings graph #############################################

ML_table %>% 
  gather(Listings, Revenue, key = `Multilisting percentage`, value = Value) %>% 
  ggplot() +
  geom_line(aes(date, Value, colour = `Multilisting percentage`), alpha = 0.2) +
  geom_smooth(aes(date, Value, colour = `Multilisting percentage`), se = FALSE,
              method = "loess", span = 0.25) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::percent) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-06-01"), NA)) +
  scale_colour_manual(values = col_palette[c(1, 3)]) +
  theme(legend.position = "bottom", 
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold",
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10)
  )



### principal residence map ############################################

property_pr %>% 
  st_filter(city) %>% 
  filter(housing, created <= key_date, scraped > key_date - years(1)) %>% 
  ggplot() +
  geom_sf(data = boroughs, fill = "white", colour = "black", lwd = 0) +
  # geom_sf(data = streets, colour = "grey50", lwd = 0.2) +
  geom_sf(aes(colour = principal_res), size = 0.7, alpha = 0.6) +
  scale_colour_manual(name = "Principal residence", 
                      values = col_palette[c(5,3)]) +
  guides(colour = 
           guide_legend(override.aes = list(fill = col_palette[c(1,3)],
                                            alpha = 1))
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    # text = element_text(family = "Futura", face = "plain"),
    # legend.title = element_text(family = "Futura", face = "bold", size = 10),
    # legend.text = element_text(family = "Futura", size = 10)
  )
