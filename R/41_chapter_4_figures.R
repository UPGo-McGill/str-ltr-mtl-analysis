#### 41 CHAPTER 4 FIGURES ######################################################

#' This script produces the graphs and maps for chapter 4. It runs quickly.
#' 
#' Output:
#' - `figure_4_1.pdf`
#' - `figure_4_2.pdf`
#' 
#' Script dependencies:
#' - `09_str_processing.R`
#' - `11_FREH_model.R`
#' 
#' External dependencies:
#' - The Futura and Futura Condensed fonts, which can be imported in 
#'   `01_startup.R`

source("R/01_startup.R")
library(tsibble)
library(feasts)
library(fabletools)

load("output/str_processed.Rdata")
load("output/geometry.Rdata")


# Figure 4.1 - Active and reserved listings since 2018 --------------------

active_by_status <- 
  daily %>% 
  filter(housing, date >= "2016-01-01", status != "B") %>% 
  count(date, status) %>% 
  group_by(status) %>% 
  mutate(n = slide_dbl(n, mean, .before = 13)) %>% 
  ungroup()

figure_4_1 <-
  active_by_status %>% 
  filter(date >= "2018-01-01") %>% 
  ggplot(aes(date, n, color = status)) +
  annotate("rect", xmin = as.Date("2020-03-14"), xmax = as.Date("2020-06-25"), 
           ymin = -Inf, ymax = Inf, alpha = 0.2) +
  annotate("curve", x = as.Date("2019-02-01"), xend = as.Date("2020-05-01"),
           y = 7500, yend = 7500, curvature = -.3, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  annotate("text", x = as.Date("2018-12-11"), y = 7500,
           label = "STRs banned \nby Province", family = "Futura Condensed") +
  geom_line(lwd = 1) +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = NULL, limits = c(0, NA), label = scales::comma) +
  scale_color_manual(name = "Status", labels = c("Available", "Reserved"), 
                     values = col_palette[c(5, 1)]) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        text = element_text(face = "plain", family = "Futura"), 
        legend.title = element_text(face = "bold", family = "Futura", 
                                    size = 10),
        legend.text = element_text( size = 10, family = "Futura"))

ggsave("output/figures/figure_4_1.pdf", plot = figure_4_1, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_4_1.pdf")



# Figure 4.2 Actual and trend reservations in 2020 ------------------------

# Create reservations time series
reservations <- 
  active_by_status %>% 
  filter(status == "R") %>% 
  as_tsibble() %>% 
  index_by(yearmon = yearmonth(date)) %>% 
  summarize(n = sum(n))

# Get March-August seasonal
mar_aug_seasonal <- 
  reservations %>% 
  filter(yearmon <= yearmonth("2020-02")) %>% 
  model(x11 = feasts:::X11(n, type = "additive")) %>% 
  components() %>%
  slice(39:44) %>% 
  pull(seasonal)

# Get Feb trend
feb_trend <- 
  reservations %>% 
  filter(yearmon <= yearmonth("2020-02")) %>% 
  model(x11 = feasts:::X11(n, type = "additive")) %>% 
  components() %>% 
  slice(50) %>% 
  pull(trend)

# Apply March-Aug seasonal component to Feb trend
mar_aug_trend <- feb_trend + mar_aug_seasonal

# Convert to daily average and assign to month midpoints
trends <-
  tibble(
    date = as.Date(c("2020-03-16", "2020-04-16", "2020-05-16", "2020-06-16",
                     "2020-07-16", "2020-07-31")),
    trend = mar_aug_trend / c(31, 30, 31, 30, 31, 31))

# Set July 31 value to average of July and August
trends[6,]$trend <- mean(trends[5:6,]$trend)

reservations_with_trend <- 
  active_by_status %>% 
  filter(date >= "2019-01-01", status == "R") %>% 
  left_join(trends) %>% 
  select(-status) %>% 
  mutate(trend = if_else(date == "2020-03-01", n, trend)) %>% 
  filter(date >= "2020-03-01") %>% 
  mutate(trend = na.approx(trend))

reservations_with_trend <- 
  active_by_status %>% 
  filter(date >= "2019-01-01", date <= "2020-02-29", status == "R") %>% 
  select(-status) %>% 
  bind_rows(reservations_with_trend) 

figure_4_2 <-
  reservations_with_trend %>% 
  pivot_longer(-date) %>% 
  filter(!is.na(value)) %>%
  ggplot() +
  annotate("rect", xmin = as.Date("2020-03-14"), xmax = as.Date("2020-06-25"),
           ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_ribbon(aes(x = date, ymin = n, ymax = trend, group = 1),
              data = reservations_with_trend, fill = col_palette[3], 
              alpha = 0.3) +
  geom_line(aes(date, value, color = name), lwd = 1) +
  geom_text(aes(x = as.Date("2020-07-31"), 
                y = mean(value[date == as.Date("2020-07-31")]),
                label = paste(
                  prettyNum(round(abs(diff(
                    value[date == as.Date("2020-07-31")])), -1), ","),
                  "daily", "reservations", "below", "trend", sep = "\n")), 
            family = "Futura Condensed", inherit.aes = FALSE, hjust = 1,
            nudge_x = -4) +
  geom_segment(aes(x = as.Date("2020-07-31"), xend = as.Date("2020-07-31"),
                   y = min(value[date == as.Date("2020-07-31")]),
                   yend = max(value[date == as.Date("2020-07-31")])),
               colour = col_palette[3],
               arrow = arrow(length = unit(0.1, "cm"), ends = "both",
                             type = "open")) +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = NULL, limits = c(0, NA), label = scales::comma) +
  scale_color_manual(name = NULL, 
                     labels = c("Actual reservations", "Trend reservations"), 
                     values = col_palette[c(5, 1)]) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        text = element_text(face = "plain", family = "Futura"), 
        legend.title = element_text(face = "bold", family = "Futura", 
                                    size = 10),
        legend.text = element_text( size = 10, family = "Futura"))

ggsave("output/figures/figure_4_2.pdf", plot = figure_4_2, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_4_2.pdf")
