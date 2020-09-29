#### 31 CHAPTER 3 FIGURES ######################################################

#' This script produces the graphs and maps for chapter 3. It runs quickly.
#' 
#' Output:
#' - `figure_3_1.pdf`
#' - `figure_3_2.pdf`
#' - `figure_3_3.pdf`
#' - `figure_3_4.pdf`
#' - `figure_3_5.pdf`
#' - `figure_3_6.pdf`
#' 
#' Script dependencies:
#' - `05_cmhc_data_import.R`
#' - `09_str_processing.R`
#' - `11_FREH_model.R`
#' - `12_rent_increases.R`
#' - `13_condo_analysis.R`
#' 
#' External dependencies:
#' - The Futura and Futura Condensed fonts, which can be imported in 
#'   `01_startup.R`

source("R/01_startup.R")

load("output/str_processed.Rdata")
load("output/geometry.Rdata")
load("output/cmhc.Rdata")
load("output/rent_increases.Rdata")
load("output/condo_analysis.Rdata")


# Figure 3.1 Housing loss -------------------------------------------------

FREH_total <- 
  daily %>% 
  filter(date >= "2016-01-01") %>% 
  group_by(date) %>% 
  summarize(across(c(FREH, FREH_3), sum)) %>%
  filter(substr(date, 9, 10) == "01")

GH_total <-
  GH %>%
  st_drop_geometry() %>%
  filter(status != "B") %>% 
  group_by(date) %>%
  summarize(GH = sum(housing_units)) %>%
  mutate(GH = slide_dbl(GH, mean, .before = 29))
  
housing_loss <-
  FREH_total %>%
  select(date, FREH_3) %>% 
  left_join(GH_total, by = "date") %>%
  rename(`Entire home/apt` = FREH_3, `Private room` = GH) %>%
  pivot_longer(c(`Entire home/apt`, `Private room`), 
               names_to = "Listing type",
               values_to = "Housing units") %>% 
  mutate(`Listing type` = factor(`Listing type`, 
                                 levels = c("Private room", "Entire home/apt")))  

# Housing loss graph
figure_3_1 <- 
  housing_loss %>% 
  ggplot(aes(date, `Housing units`, fill = `Listing type`)) +
  annotate("rect", xmin = as.Date("2020-03-29"), xmax = as.Date("2020-06-25"),
           ymin = 0, ymax = Inf, alpha = .2) +
  geom_col(lwd = 0) +
  annotate("curve", x = as.Date("2019-04-05"), xend = as.Date("2020-05-01"),
           y = 5500, yend = 5800, curvature = -.2, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  annotate("text", x = as.Date(LTM_start_date), y = 5500,
           label = "STRs banned \nby Province", family = "Futura Condensed") +
  scale_fill_manual(values = col_palette[c(1, 5)]) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-10-01"), NA)) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        text = element_text(face = "plain", family = "Futura"),
        legend.title = element_text(face = "bold", family = "Futura", 
                                    size = 10),
        legend.text = element_text( size = 10, family = "Futura"))

ggsave("output/figures/figure_3_1.pdf", plot = figure_3_1, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_3_1.pdf")


# Figure 3.2 Housing loss share of listings -------------------------------

housing_loss_share <- 
  daily %>% 
  filter(housing, status != "B", listing_type %in% c("Entire home/apt",
                                                     "Private room")) %>% 
  group_by(date) %>% 
  summarize(
    `Entire home/apt` = mean(FREH_3[listing_type == "Entire home/apt"] > 0.5),
    `Private room` = mean(GH[listing_type == "Private room"])) %>% 
  filter(date >= "2016-10-01") %>% 
  pivot_longer(-date, names_to = "Listing type", 
               values_to = "housing_loss_pct") %>% 
  group_by(`Listing type`) %>% 
  mutate(housing_loss_pct = slide_dbl(housing_loss_pct, mean, .before = 13))

figure_3_2 <- 
  housing_loss_share %>% 
  ggplot(aes(date, housing_loss_pct, colour = `Listing type`)) +
  geom_line(lwd = 1) +
  annotate("rect", xmin = as.Date("2020-03-29"), xmax = as.Date("2020-06-25"),
           ymin = 0, ymax = Inf, alpha = .2) +
  scale_y_continuous(name = NULL, labels = scales::percent) +
  scale_colour_manual(values = col_palette[c(5, 1)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold"),
        legend.text = element_text(family = "Futura"))

ggsave("output/figures/figure_3_2.pdf", plot = figure_3_2, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_3_2.pdf")


# Figure 3.3 Housing loss by borough --------------------------------------

FREH_borough <- 
  daily %>% 
  filter(date == "2019-12-01") %>% 
  group_by(borough) %>% 
  summarize(FREH = sum(FREH_3))

FREH_DA <- 
  daily %>% 
  filter(date == "2019-12-01") %>% 
  left_join(select(st_drop_geometry(property), property_ID, GeoUID)) %>% 
  group_by(GeoUID) %>% 
  summarize(FREH = sum(FREH_3))

GH_borough <- 
  GH %>% 
  filter(date == LTM_end_date) %>% 
  mutate(geometry = st_centroid(geometry)) %>% 
  st_join(boroughs) %>% 
  st_drop_geometry() %>% 
  group_by(borough) %>% 
  summarize(GH = sum(housing_units, na.rm = TRUE)) %>% 
  as_tibble()

GH_DA <- 
  GH %>% 
  filter(date == LTM_end_date) %>% 
  mutate(geometry = st_centroid(geometry)) %>% 
  st_join(DA) %>% 
  st_drop_geometry() %>% 
  group_by(GeoUID) %>% 
  summarize(GH = sum(housing_units, na.rm = TRUE)) %>% 
  as_tibble()

housing_loss_borough <- 
  boroughs %>% 
  left_join(FREH_borough) %>% 
  left_join(GH_borough) %>% 
  mutate(GH = if_else(is.na(GH), 0L, GH),
         housing_loss_pct = (FREH + GH) / dwellings)

housing_loss_DA <- 
  DA %>% 
  left_join(FREH_DA) %>% 
  left_join(GH_DA) %>% 
  mutate(GH = if_else(is.na(GH), 0L, GH),
         housing_loss_pct = (FREH + GH) / dwellings)

make_housing_map <- function(df) {
  ggplot(df) +
    geom_sf(data = province, colour = "transparent", fill = "grey93") +
    geom_sf(aes(fill = housing_loss_pct),
            colour = if (nrow(df) == 19) "white" else "transparent") +
    scale_fill_gradientn(colors = col_palette[c(3, 4, 1, 2)], 
                         na.value = "grey80",
                         limits = c(0, 0.10), oob = scales::squish, 
                         labels = scales::percent)  +
    guides(fill = guide_colourbar(title = "% housing\nlost to STR",
                                  title.vjust = 1)) + 
    gg_bbox(df) +
    theme_void() +
    theme(text = element_text(family = "Futura", face = "plain"),
          legend.title = element_text(family = "Futura", face = "bold",
                                      size = 7),
          legend.title.align = 0.9,
          legend.text = element_text(family = "Futura", size = 5),
          panel.border = element_rect(colour = "white", size = 2))
}

figure_3_3_left <- make_housing_map(housing_loss_borough)

figure_3_3_right <- 
  make_housing_map(housing_loss_DA) +
  geom_rect(xmin = 607000, ymin = 5038000, xmax = 614000, ymax = 5045000,
            fill = NA, colour = "black", size = 0.3)

fig_zoom <- 
  figure_3_3_right +
  geom_sf(data = streets_downtown, size = 0.3, colour = "white") +
  coord_sf(xlim = c(607000, 614000), ylim = c(5038000, 5045000),
           expand = FALSE) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA, colour = "black", size = 0.6))

layout <- c(
  area(1, 1, 42, 40),
  area(1, 41, 42, 80),
  area(3, 41, 22, 60)
)

figure_3_3 <- 
  figure_3_3_left + figure_3_3_right + fig_zoom + plot_layout(design = layout) + 
  plot_layout(guides = 'collect') & theme(legend.position = "bottom")

ggsave("output/figures/figure_3_3.pdf", plot = figure_3_3, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_3_3.pdf")


# Figure 3.4 Changes in housing supply ------------------------------------

renter_zone <- 
  DA_probabilities_2019 %>% 
  mutate(across(c(p_condo, p_renter), ~{.x * dwellings})) %>% 
  mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, as.numeric(.x)))) %>% 
  select(dwellings, p_condo, p_renter, geometry) %>% 
  st_interpolate_aw(cmhc, extensive = TRUE) %>% 
  st_drop_geometry() %>% 
  select(-Group.1) %>% 
  rename(n_condo = p_condo, n_renter = p_renter) %>% 
  cbind(cmhc, .) %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  mutate(p_renter = n_renter / dwellings) %>% 
  select(zone, p_renter)

daily_cmhc <- 
  property %>% 
  st_intersection(cmhc) %>% 
  st_drop_geometry() %>% 
  select(property_ID, zone) %>% 
  left_join(daily, .) %>% 
  filter(housing, date %in% as.Date(c("2019-12-01", "2019-12-31", "2018-12-01",
                                      "2018-12-31"))) %>% 
  mutate(FREH_3 = if_else(substr(date, 9, 9) == 0, FREH_3, 0),
         GH     = if_else(substr(date, 9, 9) == 3, GH, FALSE)) %>% 
  mutate(date = as.integer(substr(date, 1, 4))) %>% 
  group_by(zone, date) %>% 
  summarize(housing_loss = sum(FREH_3) + sum(GH)) %>% 
  ungroup()

strs_by_zone <- 
  property %>% 
  st_intersection(cmhc) %>% 
  st_drop_geometry() %>% 
  select(property_ID, zone) %>% 
  left_join(daily, .) %>% 
  filter(housing, date >= LTM_start_date, date <= LTM_end_date, 
         status != "B") %>% 
  count(zone) %>% 
  mutate(active_strs = n / 365)

unit_change <-
  annual_units %>% 
  filter(dwelling_type == "Total", bedroom == "Total") %>% 
  inner_join(daily_cmhc) %>% 
  left_join(renter_zone) %>% 
  mutate(housing_loss = housing_loss * p_renter) %>% 
  group_by(zone) %>% 
  summarize(unit_change = units[date == 2019] - units[date == 2018],
            housing_loss_change = housing_loss[date == 2019] - 
              housing_loss[date == 2018]) %>% 
  mutate(net_unit_change = unit_change - housing_loss_change) %>% 
  left_join(strs_by_zone) %>% 
  arrange(-active_strs) %>% 
  slice(1:10) %>% 
  left_join(st_drop_geometry(cmhc)) %>% 
  select(zone, zone_name, unit_change:net_unit_change)
  
figure_3_4 <- 
  unit_change %>%
  mutate(zone_name = factor(zone_name, levels = zone_name)) %>% 
  select(-zone) %>% 
  pivot_longer(-zone_name) %>% 
  filter(name != "housing_loss_change") %>%
  mutate(name = factor(name, levels = c("unit_change", "net_unit_change"))) %>% 
  ggplot() +
  geom_col(aes(zone_name, value, fill = name),
           position = position_dodge(width = 0.5)) +
  geom_segment(data = unit_change, 
               mapping = aes(x = zone_name, xend = zone_name, y = unit_change, 
                             yend = net_unit_change),
               size = 1.2,
               arrow = arrow(length = unit(0.3, "cm")),
               position = position_nudge(x = -0.125)) +
  scale_fill_manual(name = NULL, values = col_palette[c(3, 1)], labels = c(
    "Rental unit change", 
    "Rental unit change with STRs")) +
  scale_y_continuous(name = NULL, breaks = c(-200, 0, 200, 400, 600)) +
  scale_x_discrete(name = NULL,
                   labels = 
                     unit_change$zone_name %>% 
                     str_replace_all("/", "/\n") %>% 
                     str_replace_all("Plateau-", "Plateau-\n") %>% 
                     str_replace_all("laga-", "laga-\n")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Futura"),
        axis.text.x = element_text(family = "Futura", size = 6),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())

ggsave("output/figures/figure_3_4.pdf", plot = figure_3_4,  width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_3_4.pdf")


# Figure 3.5 Vacancy rates ------------------------------------------------

vacancy_for_map <-
  annual_vacancy %>% 
  filter(dwelling_type == "Total", bedroom == "Total", !is.na(vacancy)) %>% 
  group_by(zone) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  left_join(cmhc) %>% 
  mutate(vacant_units = vacancy * renter_households) %>% 
  left_join(select(filter(daily_cmhc, date == 2019), zone, housing_loss)) %>% 
  left_join(renter_zone) %>% 
  filter(housing_loss >= 50) %>% 
  arrange(zone) %>% 
  mutate(units_returning = housing_loss * p_renter,
         new_vacant = vacant_units + units_returning,
         new_vacancy = new_vacant / renter_households) %>% 
  select(zone, zone_name, vacancy, new_vacancy, geometry) %>% 
  rename(`Current vacancy rate` = vacancy, 
         `Dedicated STRs back to market` = new_vacancy) %>% 
  pivot_longer(-c(zone, zone_name, geometry), names_to = "status",
               values_to = "vacancy") %>% 
  st_as_sf()

figure_3_5 <- 
  vacancy_for_map %>% 
  mutate(label = scales::percent(vacancy, accuracy = 0.1)) %>% 
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(data = streets, size = 0.2, colour = "white") +
  geom_sf(aes(fill = vacancy), colour = "white", alpha = 0.7) +
  geom_sf_label(aes(label = label), size = 2, family = "Futura") +
  scale_fill_gradientn(colors = col_palette[c(2, 4, 6)], 
                       na.value = "grey80",
                       limits = c(0, 0.05), oob = scales::squish,
                       labels = scales::percent)  +
  guides(fill = guide_colourbar(title = "% housing\nlost to STR",
                                title.vjust = 1)) + 
  facet_wrap(vars(status), nrow = 1) +
  gg_bbox(vacancy_for_map) +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold",
                                    size = 7),
        legend.title.align = 0.9,
        legend.text = element_text(family = "Futura", size = 5),
        strip.text = element_text(family = "Futura", face = "bold", size = 12),
        panel.border = element_rect(colour = "white", size = 2))

ggsave("output/figures/figure_3_5.pdf", plot = figure_3_5, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_3_5.pdf")


# Figure 3.6 STR-induced rent increases -----------------------------------

rent_increase_for_map <- 
  rent_increase_zone %>% 
  group_by(zone) %>% 
  slice(-1) %>% 
  mutate(rent_increase = 1 + rent_increase) %>% 
  summarize(total_rent_increase = prod(rent_increase)) %>% 
  mutate(total_rent_increase = total_rent_increase - 1) %>% 
  left_join(cmhc, .) %>% 
  filter(zone %in% c(1, 6, 9, 2, 5, 8, 17, 7, 4))

figure_3_6 <- 
  rent_increase_for_map %>% 
  mutate(label = scales::percent(total_rent_increase, accuracy = 0.1)) %>% 
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(data = streets, size = 0.2, colour = "white") +
  geom_sf(aes(fill = total_rent_increase), colour = "white", alpha = 0.8) +
  geom_sf_label(aes(label = label), size = 2, family = "Futura") +
  scale_fill_gradientn(name = "2015-2019 rent increase",
                       colors = col_palette[c(3, 2)], 
                       na.value = "grey80",
                       limits = c(0.02, 0.04),
                       labels = scales::label_percent(accuracy = 0.1)) +
  gg_bbox(rent_increase_for_map) +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold",
                                    size = 7),
        legend.title.align = 0.9,
        legend.text = element_text(family = "Futura", size = 5),
        panel.border = element_rect(colour = "white", size = 2))

ggsave("output/figures/figure_3_6.pdf", plot = figure_3_6, width = 3.0, 
       height = 4.2, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_3_6.pdf")


# Clean up ----------------------------------------------------------------

rm(annual_avg_rent, annual_units, annual_vacancy, boroughs, boroughs_raw,
   city, city_avg_rent, city_units, city_vacancy, cmhc, DA,
   DA_probabilities_2017, DA_probabilities_2019, daily_cmhc, fig_zoom,
   figure_3_1, figure_3_2, figure_3_3, figure_3_3_left, figure_3_3_right,
   figure_3_4, figure_3_5, figure_3_6, FREH_borough, FREH_DA, FREH_total,
   GH_borough, GH_DA, GH_total, housing_loss, housing_loss_borough,
   housing_loss_DA, housing_loss_share, layout, listing_probabilities_2017,
   listing_probabilities_2019, province, rent_increase, rent_increase_for_map,
   rent_increase_zone, renter_zone, streets, streets_downtown, strs_by_zone,
   unit_change, vacancy_for_map, make_housing_map)
