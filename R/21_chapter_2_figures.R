#### 21 CHAPTER 2 FIGURES ######################################################

#' This script produces the graphs and maps for chapter 2. It runs quickly.
#' 
#' Output:
#' - `figure_2_1.pdf`
#' - `figure_2_2.pdf`
#' - `figure_2_3.pdf`
#' - `figure_2_4.pdf`
#' - `figure_2_5.pdf`
#' - `figure_2_6.pdf`
#' - `figure_2_7.pdf`
#' - `figure_2_8.pdf`
#' 
#' Script dependencies:
#' - `09_str_processing.R`
#' - `13_raffle_condo.R`
#' 
#' External dependencies:
#' - The Futura and Futura Condensed fonts, which can be imported in 
#'   `01_startup.R`

source("R/01_startup.R")
library(slider)
library(patchwork)

load("output/str_processed.Rdata")
load("output/geometry.Rdata")


# Figure 2.1 - Active daily listings --------------------------------------

active_listings <- 
  daily %>% 
  filter(housing, status != "B") %>% 
  count(date, listing_type) %>% 
  mutate(n = if_else(date <= "2017-05-31" & listing_type == "Entire home/apt",
                     # Adjusted to account for addition of HA on 2017-06-01
                     n + 800L, n)) %>% 
  group_by(listing_type) %>% 
  mutate(n = slider::slide_dbl(n, mean, .before = 6, .complete = TRUE)) %>% 
  ungroup()

active_listings <- 
  daily %>% 
  filter(housing, status != "B") %>% 
  count(date) %>% 
  mutate(n = if_else(date <= "2017-05-31", n + 960, as.numeric(n)),
         n = data.table::frollmean(n, 7),
         listing_type = "All listings") %>% 
  bind_rows(active_listings) %>% 
  arrange(date, listing_type)

figure_2_1 <- 
  active_listings %>% 
  ggplot(aes(date, n , colour = listing_type, size = listing_type)) +
  geom_line() +
  annotate("rect", xmin = as.Date("2020-03-14"), xmax = as.Date("2020-06-25"),
           ymin = 0, ymax = Inf, alpha = .2) +
  annotate("curve", x = as.Date("2019-08-01"), xend = as.Date("2020-05-01"),
           y = 12000, yend = 10500, curvature = -.2, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  annotate("text", x = as.Date("2019-05-01"), y = 11700,
           label = "STRs banned \nby Province", family = "Futura Condensed") +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-01-01"), NA)) +
  scale_colour_manual(name = NULL, values = col_palette[c(5, 1:3)],
                      guide = guide_legend(
                        override.aes = list(size = c(1.5, 0.75, 0.75, 0.75)))
                      ) +
  scale_size_manual(values = c("All listings" = 1.5, "Entire home/apt" = 0.75,
                               "Private room" = 0.75, "Shared room" = 0.75),
                    guide = "none") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        # panel.grid.major.x = element_blank(),
        text = element_text(family = "Futura"))

ggsave("output/figures/figure_2_1.pdf", plot = figure_2_1, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_2_1.pdf")


# Figure 2.2 Active listings as a share of dwellings ----------------------

active_borough <-
  daily %>%
  filter(housing, status != "B", date >= LTM_start_date, 
         date <= LTM_end_date) %>%
  count(borough, date) %>% 
  group_by(borough) %>% 
  summarize(n = mean(n, na.rm = TRUE)) %>%
  left_join(boroughs, .) %>% 
  mutate(percentage = n / dwellings, n = round(n, digit = -1)) %>% 
  select(borough, n, dwellings, percentage)

active_DA <-
  daily %>%
  filter(housing, status != "B", date >= LTM_start_date,
         date <= LTM_end_date) %>%
  left_join(select(st_drop_geometry(property), property_ID, GeoUID)) %>% 
  count(GeoUID, date) %>% 
  group_by(GeoUID) %>% 
  summarize(n = mean(n, na.rm = TRUE)) %>%
  left_join(DA, .) %>% 
  mutate(percentage = n / dwellings, n = round(n, digit = -1),
         percentage = if_else(dwellings <= 4, NA_real_, percentage)) %>% 
  relocate(geometry, .after = last_col())

make_listing_map <- function(df) {
  ggplot(df) +
    geom_sf(data = province, colour = "transparent", fill = "grey93") +
    geom_sf(aes(fill = percentage), lwd = 0, colour = NA) +
    scale_fill_gradientn(colors = col_palette[c(3, 4, 1)], na.value = "grey80",
                         limits = c(0, 0.05), oob = scales::squish, 
                         labels = scales::percent)  +
    guides(fill = guide_colourbar(title = "STRs/\ndwelling",
                                  title.vjust = 1)) + 
    gg_bbox(active_borough) +
    theme_void() +
    theme(text = element_text(family = "Futura", face = "plain"),
          legend.title = element_text(family = "Futura", face = "bold",
                                      size = 7),
          legend.title.align = 0.9,
          legend.text = element_text(family = "Futura", size = 5),
          panel.border = element_rect(colour = "white", size = 2))
}

figure_2_2_left <-
  active_borough %>% 
  make_listing_map()

figure_2_2_right <-
  active_DA %>% 
  make_listing_map()

figure_2_2 <- 
  figure_2_2_left + figure_2_2_right + plot_layout(ncol = 2) + 
  plot_layout(guides = 'collect') & theme(legend.position = "bottom")

ggsave("output/figures/figure_2_2.pdf", plot = figure_2_2, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_2_2.pdf")




### FIGURE 2.3 - Estimated percentage of listings located in condos ############

load("output/raffle_condo.Rdata")

DA_probabilities_2019 %>% 
  ggplot()+
  geom_sf(aes(fill=p_condo), colour=NA)+
  scale_fill_gradientn(name="Percentage of condo STRs",
                       colors = col_palette[c(3, 4, 1)],
                       labels = scales::percent)+ #limits = c(0,1000)
  theme_void()+
  theme(legend.position = "bottom",
        #text = element_text(family = "Futura"),
  )

### FIGURE 2.4 - Relationship between the percentage of condos and STR concentration by borough ####

listing_probabilities_2019 %>%
  left_join(., property, by="property_ID") %>%         #join to property_ID to get the borough
  left_join(., DA, by="GeoUID") %>%                    #join to DA to get the dwellings
  #st_drop_geometry() %>% 
  group_by(GeoUID, borough) %>% 
  summarize(`Percentage of STRs by dwellings`= n()/(sum(dwellings)/n()),
            `Percentage of DA that is condos` = sum(p_condo)/n()
            ) %>%
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
        legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())


### FIGURE 2.5 - Year-over-year rate of growth of active daily listings ########
daily_variation <- 
  left_join(
    (daily %>% 
       filter(housing, status == "R", date >= "2017-01-01") %>% 
       group_by(date) %>% 
       summarize(daily_rev = sum(price)) %>% 
       mutate(rev_var = as.numeric(NA)) %>% 
       filter(date != "2020-02-29")),
    (daily %>% 
       filter(housing, status != "B", date >= "2017-01-01") %>% 
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
  geom_line(aes(date, variation, color = group), lwd=1)+
  #geom_smooth(aes(date, variation, color = group), se = F)+
  #ggtitle("Bi-weekly variation compared to same date one year before (Frollmean 14)")+
  xlab("Date (bi-weekly)")+
  ylab("Variation compared to a year before (%)") +
  scale_x_date(limits = as.Date(c("2018-01-12","2020-05-31")),
               date_labels = "%Y (%b)")+
  # ylim(c(-20,15))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(name = " ",
                     values = col_palette[c(1,3)],
                     labels = c("Active listings", "Revenue"))+
  annotate("rect", xmin = as.Date("2020-03-14"), xmax = as.Date("2020-05-31"), ymin = -Inf, ymax = Inf, alpha = .2)+
  theme_minimal() +
  theme(legend.position = "right",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(#family = "Futura", face = "bold", 
          size = 10),
        legend.text = element_text(#family = "Futura", 
          size = 10)
  )


### FIGURE 2.6 - STR host revenue distribution in Montreal #####################################################

color <- colorRampPalette(RColorBrewer::brewer.pal(name="RdYlBu", n = 9))(10)


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

daily %>% 
  filter(status != "B") %>% 
  group_by(date) %>% 
  summarize(Listings = mean(multi),
            Revenue = sum(price * (status == "R") * multi, na.rm = TRUE) / 
              sum(price * (status == "R") , na.rm = TRUE)) %>% 
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
  )+
  annotate("rect", xmin = as.Date("2020-03-14"), xmax = as.Date("2020-06-25"), ymin = -Inf, ymax = Inf, alpha = .2)



