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
#' 
#' Script dependencies:
#' - `09_str_processing.R`
#' - `13_raffle_condo.R`
#' 
#' External dependencies:
#' - The Futura and Futura Condensed fonts, which can be imported in 
#'   `01_startup.R`

source("R/01_startup.R")
library(patchwork)

load("output/str_processed.Rdata")
load("output/geometry.Rdata")


# Figure 2.1 - Active daily listings --------------------------------------

active_listingsF <- 
  daily %>% 
  filter(housing, status != "B") %>% 
  count(date, listing_type) %>% 
  group_by(listing_type) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6, .complete = TRUE)) %>% 
  ungroup()

active_listingsF <- 
  daily %>% 
  filter(housing, status != "B") %>% 
  count(date) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6, .complete = TRUE),
         listing_type = "All listings") %>% 
  bind_rows(active_listingsF) %>% 
  arrange(date, listing_type)

figure_2_1F <- 
  active_listingsF %>% 
  ggplot(aes(date, n, colour = listing_type, size = listing_type)) +
  annotate("rect", xmin = as.Date("2020-03-14"), xmax = as.Date("2020-06-25"),
           ymin = 0, ymax = Inf, alpha = .2) +
  annotate("curve", x = as.Date("2019-11-01"), xend = as.Date("2020-05-01"),
           y = 12000, yend = 10500, curvature = -.2, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  annotate("text", x = as.Date("2019-05-01"), y = 11700,
           label = "Interdiction des LCT \npar la province", 
           #family = "Futura Condensed"
           ) +
  geom_line() +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-01-01"), NA)) +
  scale_colour_manual(name = NULL, values = col_palette[c(5, 1:3)],
                      labels = c("Toutes les annonces", "Logement entier", "Chambre privée", "Chambre partagée"),
                      guide = guide_legend(
                        override.aes = list(size = c(1.5, 0.75, 0.75, 0.75)))
  ) +
  scale_size_manual(values = c("All listings" = 1.5, "Entire home/apt" = 0.75,
                               "Private room" = 0.75, "Shared room" = 0.75),
                    guide = "none") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        #text = element_text(family = "Futura")
        )

ggsave("output/figures/figure_2_1F.pdf", plot = figure_2_1F, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_2_1F.pdf")

# Figure 2.2 YOY listing and revenue growth rates -------------------------

daily_variationF <- 
  daily %>% 
  filter(housing, status != "B", date >= "2015-12-16", date != "2020-02-29") %>% 
  group_by(date) %>% 
  summarize("Annonces actives" = n(), Revenu = sum(price[status == "R"])) %>% 
  mutate(across(where(is.numeric), 
                function(x) slide_dbl(x, ~{(.x[366] - .x[1]) / .x[1]}, 
                                      .before = 365, .complete = FALSE))) %>% 
  pivot_longer(-date, names_to = "var", values_to = "value") %>% 
  group_by(var) %>% 
  mutate(value = slide_dbl(value, mean, .before = 13, .complete = TRUE)) %>% 
  ungroup() %>% 
  filter(date >= "2017-05-01")

figure_2_2F <- 
  daily_variationF %>% 
  ggplot(aes(date, value, colour = var)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  annotate("rect", xmin = as.Date("2020-03-14"), xmax = as.Date("2020-06-25"),
           ymin = -Inf, ymax = Inf, alpha = .2) +
  geom_line(lwd = 1) +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = NULL, limits = c(-1, NA), 
                     labels = scales::percent) +
  scale_color_manual(name = NULL, values = col_palette[c(5, 1)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        #text = element_text(family = "Futura")
        )

ggsave("output/figures/figure_2_2F.pdf", plot = figure_2_2F, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_2_2F.pdf")



# Figure 2.3 Active listings as a share of dwellings ----------------------

active_boroughF <-
  daily %>%
  filter(housing, status != "B", date >= LTM_start_date, 
         date <= LTM_end_date) %>%
  count(borough, date) %>% 
  group_by(borough) %>% 
  summarize(n = mean(n, na.rm = TRUE)) %>%
  left_join(boroughs, .) %>% 
  mutate(percentage = n / dwellings, n = round(n, digit = -1)) %>% 
  select(borough, n, dwellings, percentage)

active_DAF <-
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

make_listing_mapF <- function(df) {
  ggplot(df) +
    geom_sf(data = province, colour = "transparent", fill = "grey93") +
    geom_sf(aes(fill = percentage),
            colour = if (nrow(df) == 19) "white" else "transparent") +
    scale_fill_gradientn(colors = col_palette[c(3, 4, 1)], na.value = "grey80",
                         limits = c(0, 0.05), oob = scales::squish, 
                         labels = scales::percent)  +
    guides(fill = guide_colourbar(title = "LCT/\nlogements",
                                  title.vjust = 1)) + 
    gg_bbox(df) +
    theme_void() +
    theme(text = element_text(#family = "Futura", 
      face = "plain"),
          legend.title = element_text(#family = "Futura", 
            face = "bold",
                                      size = 7),
          legend.title.align = 0.9,
          legend.text = element_text(#family = "Futura", 
            size = 5),
          panel.border = element_rect(#colour = "white", 
            size = 2))
}

figure_2_3F_left <- make_listing_map(active_boroughF)
figure_2_3F_right <-  make_listing_map(active_DAF)

figure_2_3F <- 
  figure_2_3F_left + figure_2_3F_right + plot_layout(ncol = 2) + 
  plot_layout(guides = 'collect') & theme(legend.position = "bottom")

ggsave("output/figures/figure_2_3F.pdf", plot = figure_2_3F, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_2_3F.pdf")


# Figure 2.4 Percentage of listings in condos -----------------------------

load("output/raffle_condo.Rdata")

active_condos_boroughF <- 
  daily %>% 
  filter(housing, date >= "2019-01-01", date <= "2019-12-31", status != "B") %>% 
  left_join(listing_probabilities_2019) %>% 
  group_by(date, borough) %>% 
  summarize(n_listings = n(),
            n_condo = sum(p_condo, na.rm = TRUE)) %>% 
  group_by(borough) %>% 
  summarize(n_listings_2019 = mean(n_listings),
            n_condo_listings_2019 = mean(n_condo)) %>% 
  left_join(boroughs, .) %>% 
  mutate(p_condo = n_condo_listings_2019 / n_listings_2019)

make_condo_mapF <- function(df) {
  ggplot(df) +
    geom_sf(data = province, colour = "transparent", fill = "grey93") +
    geom_sf(aes(fill = p_condo), 
            colour = if (nrow(df) == 19) "white" else "transparent") +
    scale_fill_gradientn(colors = col_palette[c(3, 4, 1)], na.value = "grey80",
                         limits = c(0, 1), oob = scales::squish, 
                         labels = scales::percent)  +
    guides(fill = guide_colourbar(title = "% de LCT\nétant des copropriétés",
                                  title.vjust = 1)) + 
    gg_bbox(df) +
    theme_void() +
    theme(text = element_text(#family = "Futura", 
      face = "plain"),
          legend.title = element_text(#family = "Futura", 
            face = "bold",
                                      size = 7),
          legend.title.align = 0.9,
          legend.text = element_text(#family = "Futura", 
            size = 5),
          panel.border = element_rect(colour = "white", size = 2))
}

figure_2_4F_left <- make_condo_map(active_condos_boroughF)
figure_2_4F_right <- make_condo_map(DA_probabilities_2019)

figure_2_4F <- 
  figure_2_4F_left + figure_2_4F_right + plot_layout(ncol = 2) + 
  plot_layout(guides = 'collect') & theme(legend.position = "bottom")

ggsave("output/figures/figure_2_4F.pdf", plot = figure_2_4F, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_2_4F.pdf")


# Figure 2.5 condo scatterplot --------------------------------------------

condo_scatterF <-
  listing_probabilities_2019 %>%
  left_join(select(st_drop_geometry(property), property_ID, GeoUID)) %>%
  count(GeoUID) %>% 
  left_join(select(DA_probabilities_2019, GeoUID, dwellings, p_condo, geometry), 
            .) %>% 
  mutate(str_pct = n / dwellings) %>% 
  select(GeoUID, dwellings, p_condo, str_pct, geometry) %>% 
  left_join(select(st_drop_geometry(st_join(st_centroid(DA), boroughs)), 
                   -dwellings.x, -dwellings.y)) %>% 
  mutate(borough = case_when(
    borough == "Ville-Marie" ~ "Ville-Marie",
    borough == "Le Plateau-Mont-Royal" ~ "Le Plateau-Mont-Royal",
    TRUE ~ "Autre")) %>% 
  mutate(borough = factor(borough, levels = c("Autre", "Le Plateau-Mont-Royal",
                                              "Ville-Marie")))

figure_2_5F <- 
  condo_scatterF %>% 
  ggplot(aes(p_condo, str_pct, colour = borough)) +
  geom_point() + 
  geom_point(data = filter(condo_scatterF, borough == "Autre"),
             colour = "grey") +
  geom_point(data = filter(condo_scatterF, borough == "Le Plateau-Mont-Royal"),
             colour = col_palette[5]) +
  geom_point(data = filter(condo_scatterF, borough == "Ville-Marie"),
             colour = col_palette[1]) +
  geom_smooth(method = lm, se = FALSE) +
  scale_colour_manual(name = "Arrondissement", 
                      values = c("grey", col_palette[c(5, 1)])) +
  scale_x_continuous(name = "% de copropriétés",
                     labels = scales::percent) +
  scale_y_continuous(name = "% de LCT", 
                     labels = scales::percent_format(accuracy = 1), 
                     limits = c(NA, 0.5)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        #text = element_text(family = "Futura"),
        legend.title = element_text(#family = "Futura", 
          face = "bold"))

ggsave("output/figures/figure_2_5F.pdf", plot = figure_2_5F, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_2_5F.pdf")


# Figure 2.6 Host revenue distribution ------------------------------------

host_decilesF <-
  daily %>%
  filter(housing, date >= LTM_start_date, date <= LTM_end_date, 
         status == "R", !is.na(host_ID)) %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price)) %>% 
  summarize(all = sum(rev),
            top_10 = sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_20 = sum(rev[rev > quantile(rev, c(0.80))] / all) - 
              sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_30 = sum(rev[rev > quantile(rev, c(0.70))] / all) - 
              sum(rev[rev > quantile(rev, c(0.80))] / all),
            top_40 = sum(rev[rev > quantile(rev, c(0.60))] / all) - 
              sum(rev[rev > quantile(rev, c(0.70))] / all),
            top_50 = sum(rev[rev > quantile(rev, c(0.50))] / all) - 
              sum(rev[rev > quantile(rev, c(0.60))] / all),
            top_60 = sum(rev[rev > quantile(rev, c(0.40))] / all) - 
              sum(rev[rev > quantile(rev, c(0.50))] / all),
            top_70 = sum(rev[rev > quantile(rev, c(0.30))] / all) - 
              sum(rev[rev > quantile(rev, c(0.40))] / all),
            top_80 = sum(rev[rev > quantile(rev, c(0.20))] / all) - 
              sum(rev[rev > quantile(rev, c(0.30))] / all),
            top_90 = sum(rev[rev > quantile(rev, c(0.10))] / all) - 
              sum(rev[rev > quantile(rev, c(0.20))] / all),
            top_100 = sum(rev[rev > quantile(rev, c(0.00))] / all) - 
              sum(rev[rev > quantile(rev, c(0.10))] / all)) %>% 
  select(-all) %>% 
  pivot_longer(everything(), names_to = "percentile", values_to = "value") %>% 
  mutate(percentile = factor(percentile, levels = paste0("top_", 1:10 * 10))) %>% 
  mutate(perfect_distribution = 0.1,
         decile = 1:10,
         dummy_1 = perfect_distribution,
         dummy_2 = value) %>%  
  rename("0" = perfect_distribution, "1" = value, "0.25" = dummy_1, 
         "0.75" = dummy_2) %>%
  pivot_longer(c("0","0.25", "0.75", "1"), names_to = "position") %>% 
  mutate(position = as.numeric(position),
         display_val = scales::percent(value, .1)) %>% 
  group_by(position) %>% 
  mutate(absolute_val = slide_dbl(value, ~{.x[1] / 2 + sum(.x[-1])}, 
                                  .after = 9)) %>% 
  ungroup() %>% 
  mutate(
    display_val = paste0("a gagné ", display_val, "\ndu revenu"),
    display_percentile = case_when(
      percentile == "top_10" ~ "Top 10% des hôtes...",
      percentile == "top_20" ~ "Prochain 10% des hôtes...",
      TRUE ~ NA_character_))

figure_2_6F <- 
  host_decilesF %>% 
  ggplot(aes(position, value, group = decile, fill = decile)) +
  geom_area(colour = "white", lwd = 1.2) +
  geom_text(aes(x = 0.02, y = absolute_val, label = display_percentile),
          data = filter(host_decilesF, position == 0, decile <= 2),
  family = "Futura",
         hjust = 0) +
  geom_text(aes(x = 0.98, y = absolute_val, label = display_val),
          data = filter(host_decilesF, position == 1, decile <= 2),
  family = "Futura",
         hjust = 1) +
  scale_y_continuous(name = "Décile d'hôtes", label = scales::label_percent(1),
                     breaks = seq(0, 1, by = 0.1), limits = c(0, 1),
                     sec.axis = sec_axis(~., 
                                         name = "% du revenu total",
                                         labels = derive(), 
                                         breaks = derive())) +
  scale_fill_gradientn(colours = revenue_colour) +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "Futura"),
        axis.text.y = element_text(hjust = 1),
        axis.title.y.left = element_text(
          angle = 90, margin = margin(0, 10, 0, 0)),
        axis.title.y.right = element_text(
          angle = 270, margin = margin(0, 0, 0, 10)),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank())

ggsave("output/figures/figure_2_6F.pdf", plot = figure_2_6F, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_2_6F.pdf")


# Figure 2.7 Multilistings ------------------------------------------------

MLF <- 
  daily %>% 
  filter(status != "B") %>% 
  group_by(date) %>% 
  summarize(Annonces = mean(multi),
            Revenu = sum(price[status == "R" & multi], na.rm = TRUE) / 
              sum(price[status == "R"], na.rm = TRUE)) %>% 
  mutate(across(where(is.numeric), slide_dbl, mean, .before = 13)) %>% 
  pivot_longer(c(Annonces, Revenu), names_to = "Pourcentage de multi-annonces",
               values_to = "value")

figure_2_7F <- 
  MLF %>% 
  ggplot() +
  geom_line(aes(date, value, colour = `Pourcentage de multi-annonces`), lwd = 1) +
  annotate("rect", xmin = as.Date("2020-03-14"), xmax = as.Date("2020-06-25"), 
           ymin = -Inf, ymax = Inf, alpha = .2) +
  scale_x_date(name = NULL, limits = c(as.Date("2017-06-01"), NA)) +
  scale_y_continuous(name = NULL, 
                     label = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(values = col_palette[c(5, 1)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        text = element_text(#family = "Futura", 
          face = "plain"),
        legend.title = element_text(#family = "Futura", 
          face = "bold"),
        #legend.text = element_text(family = "Futura")
        )

ggsave("output/figures/figure_2_7F.pdf", plot = figure_2_7F, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_2_7F.pdf")


