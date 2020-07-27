#### STR MONTREAL FIGURES #####################################################
library(tidyverse)
library(ggplot2)
library(patchwork)
library(data.table)


load("data/str_montreal_overview.Rdata")


### Colour palette #############################################################

col_palette <- 
  c("#FF3333", "#FFCC00", "#FF6600", "#66FFFF", "#00CC66")

scales::show_col(col_palette)


### Active listings ##############################################################

active_listings_type <- 
  daily %>% 
  filter(housing, status != "B") %>% 
  count(date, listing_type) %>% 
  mutate(n = if_else(date <= "2017-05-31" & listing_type == "Entire home/apt",
                     # Adjusted to account for addition of HA on 2017-06-01
                     n + 800, as.numeric(n)))

daily %>% 
  filter(housing, status != "B") %>% 
  count(date) %>% 
  mutate(n = if_else(date <= "2017-05-31", n + 960, as.numeric(n)),
         n = data.table::frollmean(n, 7)) %>%
  ggplot() +
  geom_line(aes(date, n), colour = "black", size = 1.5) +
  geom_line(data = active_listings_type, aes(date, n, colour = listing_type),
            size = 0.75) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-01-01"), NA)) +
  scale_colour_manual(name = "", values = col_palette[1:4]) +
  theme_minimal() +
  theme(legend.position = "bottom",
#        text = element_text(family = "Futura"),
  )


daily %>% 
  filter(housing, date >= "2020-01-01", status == "A") %>% 
  count(date) %>% 
  ggplot()+
  geom_line(aes(date, n))

### Montreal maps ###############################################################


daily %>%
  filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>%
  group_by(borough) %>% 
  count(date) %>%
  summarize(`Daily active listings (average)` = mean(n, na.rm = T)) %>%
  left_join(select(boroughs, borough, dwellings), .) %>% 
  mutate(percentage = `Daily active listings (average)` / dwellings,
         `Daily active listings (average)` = round(`Daily active listings (average)`, digit=-1)) %>% 
  arrange(desc(`Daily active listings (average)`)) %>% 
  select(borough, `Daily active listings (average)`, dwellings, percentage) %>% 
  ggplot() +
  geom_sf(
    aes(fill = percentage), 
    lwd = 1, 
    colour = "white") +
  # geom_sf_label(
  #   aes(label = borough), 
  #   size = 1.8, 
  #   # family = "Futura",
  #   alpha = 0.5,
  #   fill = alpha("white", 0.6)) +
  scale_fill_gradientn(
    colors = col_palette[c(5,2,3)],
    na.value = "grey80",
    labels = scales::percent) +
  guides(fill = guide_colorbar(title = "Daily active listing (average)/dwelling")) +
  theme_void() +
  theme(legend.position = "right",
        # text = element_text(family = "Futura", face = "plain"),
        # legend.title = element_text(family = "Futura", face = "bold", 
        #                             size = 10),
        # legend.text = element_text(family = "Futura", size = 10)
  )


daily %>%
  filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>%
  left_join(select(st_drop_geometry(property), GeoUID, property_ID), .) %>% 
  group_by(GeoUID) %>% 
  count(date, GeoUID) %>% 
  group_by(GeoUID) %>% 
  summarize(`Daily active listings (average)` = mean(n, na.rm = T)) %>%
  left_join(select(DAs, GeoUID, dwellings), .) %>% 
  mutate(percentage = `Daily active listings (average)` / dwellings) %>% 
  ggplot() +
  geom_sf(aes(fill = percentage), lwd = NA, colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(5, 2, 3)],
                       na.value = "grey80",
                       limits = c(0, 0.03),
                       oob = scales::squish,
                       labels = scales::percent
                       ) +
  guides(fill = guide_colorbar(title = "Daily active listing (average)/dwelling")) +
  theme_void() +
  theme(legend.position = "right",
        # text = element_text(family = "Futura", face = "plain"),
        # legend.title = element_text(family = "Futura", face = "bold", 
        #                             size = 10),
        # legend.text = element_text(family = "Futura", size = 10)
  )


property %>%
  filter(housing, created <= key_date, scraped >= key_date) %>%
  st_drop_geometry() %>%
  count(GeoUID) %>%
  left_join(DAs, .) %>%
  st_intersection(borough_geometries) %>% 
  ggplot() +
  geom_sf(aes(fill = n / dwellings), lwd = NA, colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(5, 2, 3)],
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





### Host revenue percentiles graph ###########################################

daily %>%
  filter(housing == TRUE, date >= LTM_start_date, date <= LTM_end_date, status == "R") %>%
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
  scale_x_date(name = NULL, limits = c(as.Date("2020-01-01"), NA)) +
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


### daily variation ###################################################
daily_variation <- 
daily %>% 
  filter(status != "B", date >= "2017-01-01")

daily_variation_grouped <- 
  daily_variation %>% 
  filter(date >= "2017-01-01",
         date <= "2017-12-31") %>% 
  count(date) %>% 
  mutate(n = if_else(date <= "2017-05-31", n + 960, as.numeric(n))) %>% #addition of HA
  rename(n2017 = n,
         date2017 = date) %>% 
  full_join(rename(mutate(count(filter(daily_variation, date >= "2018-01-01", date <= "2018-12-31"), date), 
                          date2017 = date - years(1)), n2018 = n, date2018 = date), by = c("date2017")) %>% 
  full_join(rename(mutate(count(filter(daily_variation, date >= "2019-01-01", date <= "2019-12-31"), date), 
                          date2018 = date - years(1)), n2019 = n, date2019 = date), by = c("date2018")) %>% 
  full_join(rename(mutate(count(filter(daily_variation, date >= "2020-01-01", date <= "2020-12-31"), date), 
                          date2019 = date - years(1)), n2020 = n, date2020 = date), by = c("date2019")) %>% 
  mutate(`2017-2018` = (n2018-n2017)/n2017,
         `2018-2019` = (n2019-n2018)/n2018,
         `2019-2020` = (n2020-n2019)/n2019)

daily_variation_grouped <- 
  rbind(rename(select(daily_variation_grouped, date2018, `2017-2018`), date = date2018, variation = `2017-2018`), 
        rename(select(daily_variation_grouped, date2019, `2018-2019`), date = date2019,  variation = `2018-2019`), 
        rename(select(daily_variation_grouped, date2020, `2019-2020`), date = date2020,  variation = `2019-2020`))

daily_variation_grouped %>% 
  mutate(variation = variation*100) %>% 
  ggplot()+
  # geom_rect(aes(ymin = 0, ymax = Inf, xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2020-01-01", "%Y-%m-%d")), 
  #           alpha = 0.002, fill = "green")+
  # geom_rect(aes(ymin = -Inf, ymax = 0, xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2020-01-01", "%Y-%m-%d")),
  #           alpha = 0.002, fill = "red")+
  geom_line(aes(date, variation), se = F)+
  ggtitle("Daily variation compared to same date one year before")+
  xlab("Date (daily)")+
  ylab("Variation compared to a year before (%)") +
  scale_x_date(limits = as.Date(c("2018-01-01","2020-01-01")),
               date_labels = "%Y (%b)")+
  ylim(c(-20,15))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")



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


### borough FREH YOY growth neighborhood breakdown ####################

boroughs_FREH_breakdown %>% 
  left_join(rename(boroughs, Borough = borough), .) %>% 
  ggplot()+
  geom_sf(aes(fill = Variation),
          lwd = 1, 
          colour = "white") +
  # geom_sf_label(
  #   aes(label = borough), 
  #   size = 1.8, 
  #   # family = "Futura",
  #   alpha = 0.5,
  #   fill = alpha("white", 0.6)) +
  scale_fill_gradientn(
    colors = col_palette[c(5,2,3)],
    na.value = "grey80",
    labels = scales::percent) +
  guides(fill = guide_colorbar(title = "2018-2019 FREH YOY growth")) +
  theme_void() +
  theme(legend.position = "right",
        # text = element_text(family = "Futura", face = "plain"),
        # legend.title = element_text(family = "Futura", face = "bold", 
        #                             size = 10),
        # legend.text = element_text(family = "Futura", size = 10)
  )


