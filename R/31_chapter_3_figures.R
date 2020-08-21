#### 31 CHAPTER 3 FIGURES ######################################################

#' This script produces the graphs and maps for chapter 3. It runs quickly.
#' 
#' Output:
#' - `figure_3_1.pdf`
#' - `figure_3_2.pdf`
#' - `figure_3_3.pdf`
#' 
#' Script dependencies:
#' - `09_str_processing.R`
#' - `11_FREH_model.R`
#' 
#' External dependencies:
#' - The Futura and Futura Condensed fonts, which can be imported in 
#'   `01_startup.R`

source("R/01_startup.R")
library(patchwork)

load("output/str_processed.Rdata")
load("output/geometry.Rdata")


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
  annotate("rect", xmin = as.Date("2020-03-14"), xmax = as.Date("2020-06-25"),
           ymin = 0, ymax = Inf, alpha = .2) +
  geom_col(lwd = 0) +
  annotate("curve", x = as.Date("2019-03-20"), xend = as.Date("2020-05-01"),
           y = 5500, yend = 5800, curvature = -.2, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  annotate("text", x = as.Date("2019-01-01"), y = 5500,
           label = "STRs banned \nby Province", family = "Futura Condensed") +
  scale_fill_manual(values = col_palette[c(1, 3, 2)]) +
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
  annotate("rect", xmin = as.Date("2020-03-14"), xmax = as.Date("2020-06-25"),
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
