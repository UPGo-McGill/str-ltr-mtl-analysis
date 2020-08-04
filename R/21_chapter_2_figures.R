#### Chapter 2 FIGURES ####################################################

source("R/01_startup.R")
library(patchwork)


### FIGURE 2.1 - Active listings ##############################################################

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
  geom_line(aes(date, n), colour = col_palette[5], size = 1.5) +
  geom_line(data = active_listings_type, aes(date, n, colour = listing_type),
            size = 0.75) +
  annotate("rect", xmin = as.Date("2020-03-14"), xmax = as.Date("2020-06-25"), ymin = 0, ymax = 12500, alpha = .2)+
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-01-01"), NA)) +
  scale_colour_manual(name = "", values = col_palette[1:4]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()
        #        text = element_text(family = "Futura"),
  )


### FIGURE 2.2 - Active listings density by dissemination areas and borough #####################################################

active_borough <- daily %>%
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
  geom_sf(aes(fill = percentage), 
          lwd= 0, 
          colour = NA) +
  scale_fill_gradientn(colors = col_palette[c(3, 4, 1)],
                       na.value = "grey80",
                       limits = c(0, 0.05),
                       oob = scales::squish,
                       labels = scales::percent
  )  +
  guides(fill = guide_colorbar(title = "Percentage of daily active listings\n(average) out of total dwellings")) +
  theme_void() +
  theme(legend.position = "right",
        # text = element_text(family = "Futura", face = "plain"),
        # legend.title = element_text(family = "Futura", face = "bold", 
        #                             size = 10),
        # legend.text = element_text(family = "Futura", size = 10)
  )


active_DA <- daily %>%
  filter(housing, status != "B", date >= LTM_start_date, date <= LTM_end_date) %>%
  left_join(select(st_drop_geometry(property), GeoUID, property_ID), .) %>% 
  group_by(GeoUID) %>% 
  count(date, GeoUID) %>% 
  group_by(GeoUID) %>% 
  summarize(`Daily active listings (average)` = mean(n, na.rm = T)) %>%
  left_join(select(DAs, GeoUID, dwellings), .) %>% 
  mutate(percentage = `Daily active listings (average)` / dwellings) %>% 
  ggplot() +
  geom_sf(aes(fill = percentage), 
          lwd = NA, 
          colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(3, 4, 1)],
                       na.value = "grey80",
                       limits = c(0, 0.05),
                       oob = scales::squish,
                       labels = scales::percent
  ) +
  guides(fill = guide_colorbar(title = "Percentage of daily active listings\n(average) out of total dwellings")) +
  theme_void() +
  theme(legend.position = "right",
        # text = element_text(family = "Futura", face = "plain"),
        # legend.title = element_text(family = "Futura", face = "bold", 
        #                             size = 10),
        # legend.text = element_text(family = "Futura", size = 10)
  )
  #+
  #coord_sf( ##### Raffle brang me outside the city of Mtl. 
  #  xlim = sf::st_bbox(city)[c(1,3)],
  #  ylim = sf::st_bbox(city)[c(2,4)],
  #  expand = FALSE)

active_DA + active_borough + plot_layout(ncol=1) + plot_layout(guides = 'collect') & theme(legend.position="right")


### FIGURE 2.3 - Estimated percentage of listings located in condos #####################################################

tenure_probabilities_sf_2019 %>% 
  group_by(GeoUID) %>% 
  summarize(`Percentage of condo STRs`=sum(prob_condo)/n()) %>% 
  ggplot()+
  geom_sf(aes(fill=`Percentage of condo STRs`), colour=NA)+
  scale_fill_gradientn(colors = col_palette[c(3, 4, 1)],
                       labels = scales::percent)+ #limits = c(0,1000)
  theme_void()+
  theme(legend.position = "bottom",
        #text = element_text(family = "Futura"),
  )

### FIGURE 2.4 - Relationship between the percentage of condos and STR concentration by borough #####################################################

DAs_raffle_p_condo <- DAs_raffle %>% 
  select(GeoUID, p_condo) %>% 
  st_drop_geometry()

tenure_probabilities_sf_2019 %>%
  left_join(., DAs_raffle_p_condo, by="GeoUID") %>% 
  st_join(., boroughs) %>%
  st_drop_geometry() %>% 
  group_by(GeoUID, borough) %>% 
  summarize(`Percentage of STRs by dwellings`= n()/(sum(dwellings)/n()),
            `Percentage of DA that is condos` = sum(p_condo)/n(),
            `Percentage of condo in STRs`=sum(prob_condo)/n()) %>% 
  filter(`Percentage of STRs by dwellings` <0.5) %>% 
  ggplot(aes(x=`Percentage of DA that is condos`, 
             y=`Percentage of STRs by dwellings`, 
             colour = case_when(borough == "Ville-Marie" ~ "Ville-Marie",
                                borough == "Le Plateau-Mont-Royal" ~ "Le Plateau-Mont-Royal",
                                TRUE ~ "Other")))+
  geom_point()+  
  geom_smooth(method=lm, se=FALSE)+
  scale_colour_manual(name = "Borough", values=c(col_palette[3], "grey", col_palette[1]))+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_minimal()+
  theme(aspect.ratio=1,
        legend.position = "right",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())


### FIGURE 2.5 - Year-over-year rate of growth of active daily listings #####################################################
daily_variation <- 
  left_join(
    (daily %>% 
       filter(housing, status == "R", date >= "2017-01-01") %>% 
       group_by(date) %>% 
       summarize(daily_rev = sum(price)) %>% 
       mutate(rev_var = as.numeric(NA)) %>% 
       filter(date != "2020-02-29")),
    (daily %>% 
       filter(status != "B", date >= "2017-01-01") %>% 
       count(date) %>% 
       mutate(n = if_else(date <= "2017-05-31", n + 960, as.numeric(n)),
              n_var = as.numeric(NA)) %>% 
       filter(date != "2020-02-29")), by = "date")


for(i in 366:length(daily_variation$date)) {
  year1_n <- 
    daily_variation %>% 
    filter(date == daily_variation$date[[i]] - years(1)) %>% 
    pull(n)
  
  year2_n <- 
    daily_variation %>% 
    filter(date == daily_variation$date[[i]]) %>% 
    pull(n)
  
  daily_variation$n_var[[i]] <- as.numeric((year2_n - year1_n) / year1_n)
  
  
  
  
  year1_rev <- 
    daily_variation %>% 
    filter(date == daily_variation$date[[i]] - years(1)) %>% 
    pull(daily_rev)
  
  year2_rev <- 
    daily_variation %>% 
    filter(date == daily_variation$date[[i]]) %>% 
    pull(daily_rev)
  
  daily_variation$rev_var[[i]] <- as.numeric((year2_rev - year1_rev) / year1_rev)
  
}

daily_variation <- 
  rbind(
    daily_variation %>% 
      rename(variation = rev_var) %>% 
      mutate(group = "Revenue") %>% 
      select(date, variation, group),
    
    daily_variation %>% 
      rename(variation = n_var) %>% 
      mutate(group = "Active Listings") %>% 
      select(date, variation, group)
  ) %>% 
  filter(date >= "2018-01-01")

daily_variation %>% 
  mutate(variation = data.table::frollmean(variation, 14)) %>% 
  ggplot()+
  # geom_rect(aes(ymin = 0, ymax = Inf, xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2020-01-01", "%Y-%m-%d")), 
  #           alpha = 0.002, fill = "green")+
  # geom_rect(aes(ymin = -Inf, ymax = 0, xmin = as.Date("2018-01-01", "%Y-%m-%d"), xmax = as.Date("2020-01-01", "%Y-%m-%d")),
  #           alpha = 0.002, fill = "red")+
  geom_line(aes(date, variation, color = group))+
  # geom_smooth(aes(date, variation, color = group), se = F)+
  ggtitle("Bi-weekly variation compared to same date one year before (Frollmean 14)")+
  xlab("Date (Bi-weekly)")+
  ylab("Variation compared to a year before (%)") +
  scale_x_date(limits = as.Date(c("2018-01-12","2020-05-31")),
               date_labels = "%Y (%b)")+
  # ylim(c(-20,15))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  scale_y_continuous(labels = scales::percent)
# scale_color_manual(name = "Group",
#                    values = c("#00CC66", "#FF3333"),
#                    labels = c("Active listings", "Revenue"))


### FIGURE 2.6 - STR host revenue distribution in Montreal #####################################################

color <- colorRampPalette(brewer.pal(name="RdYlBu", n = 9))(10)

weighted_bar_graph <- daily %>%
  filter(housing == TRUE, date >= LTM_start_date, date <= LTM_end_date, status == "R") %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price)) %>% #*exchange_rate
  filter(rev > 0) %>%
  summarize(
    `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)),
    `Top 20%` = sum(rev[rev > quantile(rev, c(0.80))] / sum(rev)) - sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)),
    `Top 30%` = sum(rev[rev > quantile(rev, c(0.70))] / sum(rev)) - sum(rev[rev > quantile(rev, c(0.80))] / sum(rev)),
    `Top 40%` = sum(rev[rev > quantile(rev, c(0.60))] / sum(rev)) - sum(rev[rev > quantile(rev, c(0.70))] / sum(rev)),
    `Top 50%` = sum(rev[rev > quantile(rev, c(0.50))] / sum(rev)) - sum(rev[rev > quantile(rev, c(0.60))] / sum(rev)),
    `Top 60%` = sum(rev[rev > quantile(rev, c(0.40))] / sum(rev)) - sum(rev[rev > quantile(rev, c(0.50))] / sum(rev)),
    `Top 70%` = sum(rev[rev > quantile(rev, c(0.30))] / sum(rev)) - sum(rev[rev > quantile(rev, c(0.40))] / sum(rev)),
    `Top 80%` = sum(rev[rev > quantile(rev, c(0.20))] / sum(rev)) - sum(rev[rev > quantile(rev, c(0.30))] / sum(rev)),
    `Top 90%` = sum(rev[rev > quantile(rev, c(0.10))] / sum(rev)) - sum(rev[rev > quantile(rev, c(0.20))] / sum(rev)),
    `Top 100%` = sum(rev[rev > quantile(rev, c(0.00))] / sum(rev))- sum(rev[rev > quantile(rev, c(0.10))] / sum(rev))
  )%>% 
  gather(`Top 10%`, `Top 20%`, `Top 30%`, `Top 40%`, `Top 50%`, 
         `Top 60%`, `Top 70%`, `Top 80%`, `Top 90%`, `Top 100%`, 
         key = "percentile", value = "value") %>% 
  mutate(percentile = factor(percentile, 
                             levels = c('Top 10%', 'Top 20%', 'Top 30%', 'Top 40%', 'Top 50%', 
                                        'Top 60%', 'Top 70%', 'Top 80%', 'Top 90%', 'Top 100%'))
  ) 

weighted_bar_graph$perfect_distribution <- 0.1
weighted_bar_graph$ventile <- 1:10
weighted_bar_graph$dummy1 <- weighted_bar_graph$perfect_distribution
weighted_bar_graph$dummy2 <- weighted_bar_graph$value


stacked_area_graph <- weighted_bar_graph %>%  
  rename("0"=perfect_distribution, "1"=value, "0.25"=dummy1, "0.75"=dummy2) %>%
  pivot_longer(c("0","0.25", "0.75", "1")) %>% 
  ggplot()+
  geom_area(aes(x=(as.numeric(name)), y=value, group=ventile, fill=ventile), colour="white", lwd=1.5)+
  scale_y_continuous(position = "left", 
                     breaks = seq(0, 1, by = 0.05), 
                     label = c(" ", "Top 100%", " ","Top 90%", " ", "Top 80%"," ", "Top 70%", " ", "Top 60%", 
                               " ", "Top 50%", " ", "Top 40%", " ", "Top 30%", " ", "Top 20%", " ", "Top 10%", " "))+
  scale_fill_gradientn(colours=color)+
  theme_void()+
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 20, hjust = 1),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank())

# use ggsave to change height of graph

ggsave("Downloads/stacked_area_graph.pdf", plot = stacked_area_graph, width =7, 
       height = 8, units = "in")

### FIGURE 2.7 - Multilistings #####################################################

ML_table %>% 
  gather(Listings, Revenue, key = `Multilisting percentage`, value = Value) %>% 
  ggplot() +
  geom_line(aes(date, Value, colour = `Multilisting percentage`), alpha = 0.2) +
  geom_smooth(aes(date, Value, colour = `Multilisting percentage`), se = FALSE,
              method = "loess", span = 0.25) +
  scale_x_date(name = NULL, limits = c(as.Date("2017-06-01"), NA)) +
  scale_y_continuous(name = NULL, label = scales::percent) +
  scale_colour_manual(values = col_palette[c(1, 3)]) +
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



