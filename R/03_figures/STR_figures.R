#### STR MONTREAL FIGURES #####################################################

library(patchwork)


### Colour palette #############################################################

col_palette <- 
  c("#FF3333", "#FFCC00", "#FF6600", "#66FFFF", "#00CC66")



### FIGURE 1 - active listings #################################################

active_listings_type <- 
  daily %>% 
  filter(housing, status != "U") %>% 
  count(date, listing_type) %>% 
  mutate(n = if_else(date <= "2017-05-31" & listing_type == "Entire home/apt", 
                     # Adjusted to account for addition of HA on 2017-06-01
                     n + 40, as.numeric(n)))

active_listings_graph <-
  daily %>% 
  filter(housing, status != "U") %>% 
  count(date) %>% 
  # Numbers adjusted to account for addition of Homeaway on 2017-06-01
  mutate(n = if_else(date <= "2017-05-31", n + 48, as.numeric(n)),
         n = data.table::frollmean(n, 7)) %>% 
  ggplot() +
  geom_line(aes(date, n), colour = "black", size = 1.5) +
  geom_line(data = active_listings_type, aes(date, n, colour = listing_type),
            size = 0.75) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_date(name = NULL, limits = c(as.Date("2017-01-01"), NA)) +
  scale_colour_manual(name = "", values = col_palette[1:3]) +
  theme_minimal() +
  labs(title = "Active listings from 2017 to March 2020")+ 
  theme(legend.position = "bottom",
        #text = element_text(family = "Futura"),
  )

ggsave("output/figure_1.pdf", plot = active_listings_graph, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figure_1.pdf") ### not working for Mtl atm


### FIGURE 1.1 - active listings in the past year #################################################

active_listings_type_2019_2020 <- 
  daily %>% 
  filter(housing, status != "U") %>% 
  count(date, listing_type) %>% 
  filter(date >= "2019-03-01") 

active_listings_graph_2019_2020 <-
  daily %>% 
  filter(housing, status != "U") %>% 
  count(date) %>% 
  # Numbers adjusted to account for addition of Homeaway on 2017-06-01
  filter(date >= "2019-03-01") %>% 
  ggplot() +
  geom_line(aes(date, n), colour = "black", size = 1.5) +
  geom_line(data = active_listings_type, aes(date, n, colour = listing_type),
            size = 0.75) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_date(name = NULL, limits = c(as.Date("2019-03-01"), NA)) +
  scale_colour_manual(name = "", values = col_palette[1:3]) +
  theme_minimal() +
  labs(title = "Active listings from March 2019 to March 2020")+ 
  theme(legend.position = "bottom",
        #text = element_text(family = "Futura"),
  )

ggsave("output/figure_1.1.pdf", plot = active_listings_graph_2019_2020, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

#extrafont::embed_fonts("output/figure_1.pdf") ### not working for Mtl atm



### FIGURE 2 - Montreal maps ##############################################

mtl <- st_read("limadmin-shp", "LIMADMIN")

mtl <- mtl %>% 
  st_transform(32618)

mtl_borough_census <- 
  st_intersection(mtl, DAs) %>% 
  group_by(NOM) %>% 
  summarize(dwellings=sum(dwellings))

borough_map <-
  property %>%
  filter(housing, created <= key_date, scraped >= key_date) %>%
  st_intersection(mtl_borough_census, .) %>% 
  group_by(NOM) %>% 
  summarize(listings_borough=n(), dwellings_count=sum(dwellings)/n()) %>% 
  mutate(listing_density=listings_borough/dwellings_count)

borough_map <- borough_map %>% 
  st_drop_geometry() %>% 
  full_join(mtl_borough_census, ., by="NOM") %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(
    aes(fill = listing_density), 
    lwd = 1, 
    colour = "white") +
  geom_sf_label(
    aes(label = NOM), 
    size = 1.8, 
    #family = "Futura",
    alpha = 0.5,
    fill = alpha("white", 0.6)) +
  scale_fill_gradientn(
    colors = col_palette[c(2, 1)],
    na.value = "grey80",
    limits = c(0.0005, 0.10),
    oob = scales::squish,
    labels = scales::percent) +
  guides(fill = guide_colorbar(title = "STRs/dwelling")) +
  theme_void() +
  labs(title = "STR listings per total dwellings by borough")+ 
  theme(legend.position = "right",
        text = element_text(face = "plain"), #family = "Futura", 
        legend.title = element_text(face = "bold", #family = "Futura", 
                                    size = 10),
        legend.text = element_text(size = 10) #family = "Futura", 
  )


DA_map <- 
  property %>%
  filter(housing, created <= key_date, scraped >= key_date) %>%
  st_drop_geometry() %>%
  count(GeoUID) %>%
  left_join(DAs, .) %>%
  st_intersection(city) %>% 
  ggplot() +
  geom_sf(aes(fill = n / dwellings), lwd = 0, colour = NA) +
  scale_fill_gradientn(colors = col_palette[c(2, 1)],
                       na.value = "grey80",
                       limits = c(0, 0.1),
                       oob = scales::squish,
                       labels = scales::percent) +
  guides(fill = guide_colorbar(title = "STRs/dwelling")) +
  theme_void() +
  labs(title = "STR listings per total dwellings by dissemination area")+ 
  theme(legend.position = "right",
        text = element_text(face = "plain"),    #family = "Futura", 
        legend.title = element_text(face = "bold", #family = "Futura", 
                                    size = 10),
        legend.text = element_text(size = 10) #family = "Futura",
  )

listings_map <- 
  DA_map + plot_layout(guides = "collect")

ggsave("output/figure_2.pdf", plot = listings_map, width = 8,
       height = 4, units = "in", useDingbats = FALSE)

ggsave("output/figure_2.1.pdf", plot = borough_map, width = 8,
       height = 4, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figure_2.pdf") ### not working for Mtl at the moment


### FIGURE 3 - seasonality graph ###############################################

#seasonality_graph <- 
#  seasonality_Canada %>% 
#  filter(city != 15) %>%
#  ggplot() +
#  geom_line(
#    aes(month, season_book, group = city), 
#    lwd = 1.5, 
#    colour = alpha("grey10", 0.2)) +
#  geom_line(
#    aes(month, season_book, group = 1), 
#    data = seasonality, 
#    lwd = 2,
#    colour = col_palette[3]) +
#  scale_y_continuous(name = "", labels = scales::percent, limits = c(0, NA)) +
#  scale_x_discrete(name = "") +
#  theme_minimal() +
#  theme(text = element_text(family = "Futura"))

# ggsave("output/figure_3.pdf", plot = seasonality_graph, width = 8, 
#       height = 5.5, units = "in", useDingbats = FALSE)

# extrafont::embed_fonts("output/figure_3.pdf")


### FIGURE 4 - host revenue percentiles graph ##################################

exchange_rate <- 1

revenue_graph <-
  daily %>%
  filter(housing == TRUE, date > end_date - years(1), status == "R") %>%
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
        text = element_text(face = "plain"), #family = "Futura", 
        legend.title = element_text(face = "bold", #family = "Futura", 
                                    size = 10),
        legend.text = element_text(size = 10), #family = "Futura", 
        legend.position = "none")

ggsave("output/figure_4.pdf", plot = revenue_graph, width = 8, height = 5, 
       units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figure_4.pdf")


### FIGURE 5 - housing loss ####################################################


housing_graph <-
  housing_loss %>% 
  mutate(`Listing type` = factor(
    `Listing type`, 
    levels = c("Private room", "Entire home/apt"))) %>%  #"Summer listings", 
  ggplot(aes(`Listing type`)) +
  geom_col(aes(date, `Housing units`, fill = `Listing type`),
           lwd = 0) +
  theme_minimal() +
  #scale_y_continuous(name = NULL, label = scales::comma, limits = c(0, 200)) +
  #scale_x_date(name = NULL, limits = c(as.Date("2016-10-01"), NA)) +
  scale_fill_manual(values = col_palette[3:1]) +
  theme(legend.position = "bottom", 
        text = element_text(face = "plain"), #family = "Futura", 
        legend.title = element_text(face = "bold", #family = "Futura", 
                                    size = 10),
        legend.text = element_text( size = 10) #family = "Futura",
  )

HHH <- housing_loss %>% 
  mutate(`Listing type` = factor(
    `Listing type`, 
    levels = c("Summer listings", "Private room", "Entire home/apt"))) %>% 
  filter(date >= "2019-01-01") %>% 
  group_by(`Listing type`)

### 671 summer listings were taken out of the market in the 2019 season

ggsave("output/figure_5.pdf", plot = housing_graph, width = 8, height = 7, 
       units = "in", useDingbats = FALSE)

#extrafont::embed_fonts("output/figure_5.pdf")


### FIGURE 6 - vacancy rate change #############################################

#vacancy_rate <- 
#  tibble(Year = c(2017, 2018, 2019, 2019, 2020, 2021, 2022,
#                  2017, 2018, 2019, 2020, 2021, 2022),
#         `Vacancy rate` = c(rep("Vacancy rate", 3),
#                            rep("Vacancy rate (projected)", 4),
#                            rep("Vacancy rate with no dedicated STRs", 6)
#         ),
#         Value = c(0.010, 0.002, 0.012, 0.012, 0.006, 0.011, 0.020,
#                   0.017, 0.018, 0.029, 0.023, 0.031, 0.041))

#vacancy_graph <- 
#  vacancy_rate %>% 
#  ggplot(aes(Year, Value)) +
#  geom_line(aes(colour = `Vacancy rate`, linetype = `Vacancy rate`),
#            lwd = 2) +
#  scale_x_continuous(name = "") +
#  scale_y_continuous(name = "", limits = c(0, 0.045), 
#                     labels = scales::percent) +
#  scale_colour_manual(values = col_palette[c(1, 1, 3)]) +
#  theme_minimal() +
#  theme(legend.position = "bottom",
#        legend.title = element_blank(),
#        panel.grid.major.x = element_blank(),
#        panel.grid.minor.x = element_blank(),
#        text = element_text(family = "Futura", face = "plain"),
#        legend.text = element_text(family = "Futura", size = 10))

#ggsave("output/figure_6.pdf", plot = vacancy_graph, width = 8, height = 4, 
#       units = "in", useDingbats = FALSE)

#extrafont::embed_fonts("output/figure_6.pdf")


### FIGURE 7 - multilistings graph #############################################

ML_summary <- 
  daily %>% 
  group_by(date) %>% 
  summarize(Listings = mean(multi),
            Revenue = sum(price * (status == "R") * multi * exchange_rate, 
                          na.rm = TRUE) / 
              sum(price * (status == "R") * exchange_rate, na.rm = TRUE))

ML_graph <-
  ML_summary %>% 
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
        text = element_text(face = "plain"), #family = "Futura", 
        legend.title = element_text(face = "bold", #family = "Futura", 
                                    size = 10),
        legend.text = element_text(size = 10) #family = "Futura", 
  )
ML_graph_2019_2020 <-
  ML_summary %>% 
  filter(date >="2019-03-01") %>% 
  gather(Listings, Revenue, key = `Multilisting percentage`, value = Value) %>% 
  ggplot() +
  geom_line(aes(date, Value, colour = `Multilisting percentage`), alpha = 0.2) +
  geom_smooth(aes(date, Value, colour = `Multilisting percentage`), se = FALSE,
              method = "loess", span = 0.25) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::percent) +
  scale_x_date(name = NULL, limits = c(as.Date("2019-03-01"), NA)) +
  scale_colour_manual(values = col_palette[c(1, 3)]) +
  theme(legend.position = "bottom", 
        text = element_text(face = "plain"), #family = "Futura", 
        legend.title = element_text(face = "bold", #family = "Futura", 
                                    size = 10),
        legend.text = element_text(size = 10) #family = "Futura", 
  )


ggsave("output/figure_7.pdf", plot = ML_graph, width = 8, height = 6, 
       units = "in", useDingbats = FALSE)

ggsave("output/figure_7.1.pdf", plot = ML_graph_2019_2020, width = 8, height = 6, 
       units = "in", useDingbats = FALSE)


#extrafont::embed_fonts("output/figure_7.pdf")


### FIGURE 8 - principal residence map #########################################

### cannot do the principal residence map since we do not have the principal_res_2019 field
pr_map <- 
  property %>% 
  st_filter(city) %>% 
  filter(housing, created <= end_date, scraped > end_date - years(1)) %>% 
  ggplot() +
  # geom_sf(data = wards, fill = "white", colour = "transparent", lwd = 0) +
  #geom_sf(data = streets, colour = "grey50", lwd = 0.2) +
  geom_sf(aes(colour = principal_res_2019), size = 1, alpha = 0.8) +
  scale_colour_manual(name = "Principal residence", 
                      values = col_palette[c(1,3)]) +
  guides(colour = 
           guide_legend(override.aes = list(fill = col_palette[c(1,3)],
                                            alpha = 1))
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    text = element_text(face = "plain"), #family = "Futura", 
    legend.title = element_text(face = "bold", size = 10), #family = "Futura", 
    legend.text = element_text(size = 10) #family = "Futura", 
  )

ggsave("output/figure_8.pdf", plot = pr_map, width = 8, height = 8, 
       units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figure_8.pdf")


### FIGURE 10 - Housing loss boroughs ###########################################

patchwork_layout <- "
ABCDE
FGHIJ
KKKKK
"

housing_loss_listings <-
  FREH %>% 
  filter(date > end_date - years(1)) %>% 
  pull(property_ID) %>% 
  {filter(property, housing, property_ID %in% .)} %>% 
  select(property_ID, geometry) %>% 
  rbind(GH %>% 
          filter(date > end_date - years(1)) %>% 
          group_by(ghost_ID) %>% 
          summarize(geometry = st_centroid(st_union(geometry))) %>% 
          transmute(property_ID = as.character(ghost_ID))


borough_loss_map <- 
  housing_loss_listings %>% 
  st_join(mtl_borough_census, ., left = TRUE) %>% 
  group_by(NOM) %>% 
  summarize(listings_borough=n(), dwellings_count=sum(dwellings)/n()) %>% 
  mutate(listing_density=listings_borough/dwellings_count) %>% 
  group_split() %>% 
  map(~{
    .x %>% 
      ggplot() +
      geom_sf(aes(fill = listing_density), lwd = 0, 
              colour = "transparent") +
      scale_fill_gradientn(
        name = "STR housing loss",
        colors = col_palette[c(2, 3, 1)],
        #limits = c(0., 0.02),
        oob = scales::squish,
        labels = scales::percent) +
      #      ggtitle(.x$ward[1]) +
      theme_void() +
      theme(legend.position = "bottom",
            legend.key.width = unit(1.5, "cm"),
            text = element_text(face = "plain"), #family = "Futura", 
            legend.title = element_text(face = "bold", #family = "Futura", 
                                        size = 10),
            legend.text = element_text(size = 9), #family = "Futura", 
            plot.title = element_text(size = 9, #family = "Futura", 
                                      face = "bold"))
  }) %>% 
  wrap_plots() +
  guide_area()

ggsave("output/figure_10.pdf", plot = borough_loss_map, width = 8, height = 8, 
       units = "in", useDingbats = FALSE)

### FIGURE 11 - STR-LTR Matches by boroughs ###########################################

boroughs <- st_read("shapefiles", "montreal_boroughs_2019") 

boroughs <- boroughs %>% 
  st_transform(32618) %>% 
  rename(borough=NOM)

CTs <- CTs %>% 
  st_transform(32618)

borough_ct <- 
  st_join(boroughs, CTs) %>% 
  select(borough, dwellings, GeoUID, geometry)

ltr_bo <- 
  st_join(borough_ct, ltr_mtl)

ltr_mtl %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ab_id)) %>% 
  distinct(ab_id, .keep_all = TRUE) %>% 
  group_by(borough) %>% 
  summarize(n=n(), percentage = n()/2374)
  

View(ltr_mtl %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ab_id)) %>% 
  distinct(ab_id, .keep_all = TRUE) %>%
  filter(price >=425) %>% 
  filter(price <= quantile(price, 0.998, na.rm=TRUE), 
         price >= quantile(price, 0.002, na.rm=TRUE)) %>% 
  group_by(furnished, bedrooms) %>% 
  summarize(n=n(), percentage = n()/2374, median=median(price, na.rm=TRUE)))

View(ltr_mtl %>% 
  st_drop_geometry() %>% 
  filter(is.na(ab_id)) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  filter(price >=425) %>% 
  filter(price <= quantile(price, 0.998, na.rm=TRUE), 
         price >= quantile(price, 0.002, na.rm=TRUE)) %>% 
  group_by(furnished, bedrooms) %>% 
  summarize(n=n(), percentage = n()/68819, median=median(price, na.rm=TRUE)))

weekly_prices_non_matches <- ltr_mtl %>% 
  st_drop_geometry() %>% 
  filter(is.na(ab_id)) %>% 
  arrange(scraped, .by_group=TRUE) %>% 
  distinct(title, scraped, .keep_all = TRUE) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  filter(price >=425) %>% 
  filter(price <= quantile(price, 0.998, na.rm=TRUE), 
         price >= quantile(price, 0.002, na.rm=TRUE)) %>% 
  group_by(week=floor_date(created, "week")) %>%
  #group_by(bedrooms) %>% 
  summarize(n=n(), median_price=median(price, na.rm=TRUE))
  
weekly_prices_matches <- ltr_mtl %>% 
  st_drop_geometry() %>% 
  filter(!is.na(ab_id)) %>% 
  distinct(ab_id, .keep_all = TRUE) %>% 
  filter(price >=425) %>% 
  filter(price <= quantile(price, 0.998, na.rm=TRUE), 
         price >= quantile(price, 0.002, na.rm=TRUE)) %>% 
  group_by(week=floor_date(created, "week")) %>%
  #group_by(bedrooms) %>% 
  summarize(n=n(), median_price=median(price, na.rm=TRUE))

weekly_prices_total <- ltr_mtl %>% 
  st_drop_geometry() %>% 
  arrange(scraped, .by_group=TRUE) %>% 
  distinct(title, scraped, .keep_all = TRUE) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  filter(price>=425) %>% 
  filter(price <= quantile(price, 0.998, na.rm=TRUE), 
         price >= quantile(price, 0.002, na.rm=TRUE)) %>% 
  group_by(week=floor_date(created, "week")) %>%
  #group_by(bedrooms) %>% 
  summarize(n=n(), median_price=median(price, na.rm=TRUE))

ggplot()+ 
  geom_smooth(data=weekly_prices_matches, aes(week, `median_price`), colour="#FF3333", se=FALSE)+
  geom_smooth(data=weekly_prices_non_matches, aes(week, `median_price`), colour="#FFCC00", se=FALSE)+
  geom_smooth(data=weekly_prices_total, aes(week, `median_price`), colour="#00CC66", se=FALSE)+
  labs(title="Median price for matches and non-matches by week in Montreal", 
       subtitle = "Red are LTR+STR matches, yellow are non-matches, and green is the entire dataset")+
  theme_minimal()
  
View(property %>% 
  st_drop_geometry() %>% 
  select(property_ID, superhost, ab_host) %>% 
  rename(ab_id=property_ID) %>% 
  merge(., st_drop_geometry(ltr_mtl), by="ab_id") %>% 
  distinct(ab_id, .keep_all = TRUE) %>% 
  group_by(ab_host, superhost) %>% 
  summarize(n=n(), percentage=n()/2374)) 

property %>% 
  st_drop_geometry() %>% 
  group_by(superhost) %>% 
  summarize(n=n(), percentage=n()/78744)


View(ltr_mtl %>%
  st_drop_geometry() %>% 
  filter(is.na(ab_id)) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  group_by(bedrooms) %>% 
  summarize(n=n(), percentage = n()/68819))

LTM_property %>% 
  st_drop_geometry() %>% 
  distinct(property_ID, .keep_all = TRUE) %>% 
  group_by(bedrooms) %>% 
  summarize(n=n(), percentage=n()/36026)

########### PLOT TO SHOW THE DIFFERENCE IN PRICE FROM DATE CREATED TO LAST SCRAPED ###########

ggplot()+
  geom_line(data= ltr_mtl %>% 
              st_drop_geometry() %>% 
              group_by(created) %>% 
              arrange(scraped) %>% 
              distinct(id, .keep_all = TRUE) %>% 
              filter(price>=425) %>% 
              filter(price <= quantile(price, 0.998, na.rm=TRUE), 
                     price >= quantile(price, 0.002, na.rm=TRUE)) %>% 
              group_by(day=floor_date(created, "day")) %>% 
              summarize(n = n(), `Median price of listings` = median(price, na.rm=TRUE)), 
            aes(day, `Median price of listings`, color="Median price on date created"))+
  geom_line(data= ltr_mtl %>% 
              st_drop_geometry() %>% 
              group_by(created) %>% 
              arrange(desc(scraped)) %>% 
              distinct(id, .keep_all = TRUE) %>% 
              filter(price>=425) %>% 
              filter(price <= quantile(price, 0.998, na.rm=TRUE), 
                     price >= quantile(price, 0.002, na.rm=TRUE)) %>% 
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
  theme_minimal()+
  theme(legend.position = "bottom")
  
  

