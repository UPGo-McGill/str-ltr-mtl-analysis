#### 51 CHAPTER 5 FIGURES ######################################################

#' This script produces the graphs and maps for chapter 5. It runs quickly.
#' 
#' Output:
#' - `figure_5_1.pdf`
#' - `figure_5_2.pdf`
#' - `figure_5_3.pdf`
#' - `figure_5_4.pdf`
#' - `figure_5_5.pdf`
#' 
#' Script dependencies:
#' - `02_geometry_import.R`
#' - `07_ltr_listing_match.R`
#' - `09_str_processing.R`
#' - `11_FREH_model.R`
#' 
#' External dependencies:
#' - The Futura and Futura Condensed fonts, which can be imported in 
#'   `01_startup.R`

source("R/01_startup.R")
library(imager)
library(patchwork)

load("output/geometry.Rdata")
load("output/str_processed.Rdata")
load("output/ltr_processed.Rdata")
load("output/matches_raw.Rdata")

# Prepare objects for upcoming figures -----------------------------------------------------

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



# Figure 5.1 Airbnb/Kijiji image comparison -------------------------------

first_photo_pair <- 
  kj_matches %>% 
  mutate(across(c(x_name, y_name), str_replace, "mtl", "montreal")) %>% 
  slice(1:100) %>% 
  filter(confirmation == "match") %>% 
  slice(4)

second_photo_pair <- 
  cl_matches %>% 
  mutate(across(c(x_name, y_name), str_replace, "mtl", "montreal")) %>% 
  filter(confirmation == "match") %>% 
  slice(2633)

titles <- list(
  
  first_photo_pair$x_name %>% 
    str_extract('ab-.*(?=\\.jpg)') %>% 
    {filter(property, property_ID == .)} %>% 
    pull(listing_title),
  
  first_photo_pair$y_name %>% 
    str_extract('kj-.*(?=-[:digit:]\\.jpg)') %>% 
    {filter(ltr, id == .)} %>% 
    slice(1) %>% 
    pull(title) %>% 
    str_remove(' \\|.*'),
  
  property %>% 
    filter(map_lgl(all_PIDs, ~{
      str_extract(second_photo_pair$x_name, 'ab-.*(?=\\.jpg)') %in% .x})) %>% 
    pull(listing_title),
  
  second_photo_pair$y_name %>% 
    str_extract('cl-.*(?=-[:digit:]\\.jpg)') %>% 
    {filter(ltr, id == .)} %>% 
    slice(1) %>% 
    pull(title) %>% 
    str_remove(' - apts.*')
)

photos <- 
  map2(list(first_photo_pair$x_name, first_photo_pair$y_name, 
            second_photo_pair$x_name, second_photo_pair$y_name), 
       titles, ~{
         .x %>% 
           load.image() %>% 
           as.data.frame(wide = "c") %>% 
           mutate(rgb = rgb(c.1, c.2, c.3)) %>% 
           ggplot(aes(x, y)) +
           geom_raster(aes(fill = rgb)) + 
           scale_fill_identity() +
           scale_y_continuous(trans = scales::reverse_trans()) +
           ggtitle(
             case_when(str_detect(.x, "ab-") ~ "Airbnb",
                       str_detect(.x, "cl-") ~ "Craigslist",
                       str_detect(.x, "kj-") ~ "Kijiji"), 
             subtitle = paste0('"', .y, '"')) +
           theme_void() +
           theme(plot.title = element_text(family = "Futura Condensed",
                                           face = "bold", size = 9),
                 plot.subtitle = element_text(family = "Futura Condensed",
                                              face = "plain", size = 9))})

figure_5_1 <- wrap_plots(photos)

ggsave("output/figures/figure_5_1.pdf", plot = figure_5_1, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_5_1.pdf")



# Figure 5.1. Concentration of STR listings matched with LTR listings by borough ------------------

figure_5_1 <-
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

ggsave("output/figures/figure_5_1.pdf", plot = figure_5_1, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_5_1.pdf")


# Figure 5.2. Median asking rents for matches and non-matches through time -----------------------------------------------------

figure_5_2 <- unique_ltr %>% 
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

ggsave("output/figures/figure_5_2.pdf", plot = figure_5_2, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_5_2.pdf")


# Figure 5.3. Distribution of matches and non-matches by date created on STR platforms -----------------------------------------------------

figure_5_3 <- property %>% 
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

ggsave("output/figures/figure_5_3.pdf", plot = figure_5_3, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_5_3.pdf")


# Figure 5.4. Distribution of matches by revenue earned in 2019 ---------------------------------------------------------------

figure_5_4 <- daily %>%
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

ggsave("output/figures/figure_5_4.pdf", plot = figure_5_4, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_5_4.pdf")


# Figure 5.5. Length of stay of matches and non-matches on LTR platforms --------------------------------------------------

figure_5_5 <- unique_ltr %>% 
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

ggsave("output/figures/figure_5_5.pdf", plot = figure_5_5, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_5_5.pdf")

