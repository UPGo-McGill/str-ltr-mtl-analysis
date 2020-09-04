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
library(feasts)
library(fabletools)

load("output/str_processed.Rdata")


# Figure 4.1 - Active and reserved listings since 2018 --------------------

active_by_status <- 
  daily %>% 
  filter(housing, date >= "2016-01-01", status != "B") %>% 
  count(date, status)

figure_4_1 <-
  active_by_status %>% 
  group_by(status) %>% 
  mutate(n = slide_dbl(n, mean, .before = 13)) %>% 
  ungroup() %>% 
  filter(date >= "2018-01-01") %>% 
  ggplot(aes(date, n, color = status)) +
  annotate("rect", xmin = as.Date("2020-03-29"), xmax = as.Date("2020-06-25"), 
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

# Create and decompose reservations time series
reservations <- 
  active_by_status %>% 
  filter(status == "R") %>% 
  tsibble::as_tsibble() %>% 
  tsibble::index_by(yearmon = tsibble::yearmonth(date)) %>% 
  summarize(n = sum(n)) %>% 
  filter(yearmon <= tsibble::yearmonth("2020-02")) %>% 
  model(x11 = feasts:::X11(n, type = "additive")) %>% 
  components()

# Get March-August seasonal
mar_aug_seasonal <- 
  reservations %>% 
  slice(39:44) %>% 
  pull(seasonal)

# Get Feb trend
feb_trend <- 
  reservations %>% 
  slice(50) %>% 
  pull(trend)

# Apply March-Aug seasonal component to Feb trend
trends <-
  tibble(
    date = as.Date(c("2020-03-16", "2020-04-16", "2020-05-16", "2020-06-16",
                     "2020-07-16", "2020-07-31")),
    trend = (feb_trend + mar_aug_seasonal) / c(31, 30, 31, 30, 31, 31))

# Set July 31 value to average of July and August
trends[6,]$trend <- mean(trends[5:6,]$trend)

reservations <- 
  active_by_status %>% 
  filter(status == "R") %>% 
  mutate(n = slide_dbl(n, mean, .before = 6)) %>% 
  filter(date >= "2019-01-01") %>% 
  left_join(trends) %>% 
  select(-status) %>% 
  mutate(trend = if_else(date == "2020-03-01", n, trend)) %>% 
  filter(date >= "2020-03-01") %>% 
  mutate(trend = zoo::na.approx(trend))

reservations <- 
  active_by_status %>% 
  filter(status == "R") %>% 
  mutate(n = slide_dbl(n, mean, .before = 6)) %>% 
  filter(date >= "2019-01-01", date <= "2020-02-29") %>% 
  select(-status) %>% 
  bind_rows(reservations) 

figure_4_2 <-
  reservations %>% 
  pivot_longer(-date) %>% 
  filter(!is.na(value)) %>%
  ggplot() +
  annotate("rect", xmin = as.Date("2020-03-29"), xmax = as.Date("2020-06-25"),
           ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_ribbon(aes(x = date, ymin = n, ymax = trend, group = 1),
              data = reservations, fill = col_palette[3], 
              alpha = 0.3) +
  geom_line(aes(date, value, color = name), lwd = 1) +
  geom_text(aes(x = as.Date("2020-07-31"), 
                y = mean(value[date == as.Date("2020-07-31")]),
                label = paste(
                  prettyNum(round(abs(diff(
                    value[date == as.Date("2020-07-31")])), -1), ","),
                  "fewer", "reservations", "than", "expected", sep = "\n")), 
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
                     labels = c("Actual reservations", "Expected reservations"), 
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


# Figure 4.3 Average nightly price ----------------------------------------

# Get average nightly prices
average_prices <- 
  daily %>% 
  filter(housing, status == "R", date >= "2016-01-01",
         listing_type == "Entire home/apt") %>% 
  group_by(date) %>% 
  summarize(price = mean(price))

# Create monthly price time series
monthly_prices <- 
  average_prices %>% 
  tsibble::as_tsibble() %>% 
  tsibble::index_by(yearmon = tsibble::yearmonth(date)) %>% 
  summarize(price = mean(price))

# Get March-August seasonal
mar_jul_price_seasonal <- 
  monthly_prices %>% 
  filter(yearmon <= tsibble::yearmonth("2020-02")) %>% 
  model(x11 = feasts:::X11(price, type = "additive")) %>% 
  components() %>%
  slice(39:43) %>% 
  pull(seasonal)

# Get Feb trend
feb_price_trend <- 
  monthly_prices %>% 
  filter(yearmon <= tsibble::yearmonth("2020-02")) %>% 
  model(x11 = feasts:::X11(price, type = "additive")) %>% 
  components() %>% 
  slice(50) %>% 
  pull(trend)

# Apply March-Aug seasonal component to Feb trend
mar_jul_price_trend <- 
  tibble(yearmon = tsibble::yearmonth(c("2020-03", "2020-04", "2020-05", 
                                        "2020-06", "2020-07")),
         trend = feb_price_trend + mar_jul_price_seasonal)

# Apply to daily averages to get trend
average_prices <- 
  average_prices %>% 
  mutate(yearmon = tsibble::yearmonth(date)) %>% 
  inner_join(mar_jul_price_trend) %>% 
  group_by(yearmon) %>% 
  mutate(trend = price * trend / mean(price)) %>% 
  ungroup() %>% 
  select(date, trend) %>% 
  left_join(average_prices, .)

figure_4_3 <- 
  average_prices %>% 
  mutate(across(c(price, trend), slide_dbl, mean, na.rm = TRUE, 
                .before = 6)) %>% 
  mutate(trend = if_else(date == "2020-02-29", price, trend)) %>% 
  filter(date >= "2019-01-01") %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(date, value, colour = name)) +
  annotate("rect", xmin = as.Date("2020-03-29"), xmax = as.Date("2020-06-25"), 
           ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_line(lwd = 1) +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = NULL, limits = c(50, NA),
                     label = scales::label_dollar(accuracy = 1)) +
  scale_color_manual(name = NULL, values = col_palette[c(5, 1)],
                     labels = c("Actual nightly price", 
                                "Expected nightly price")) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        text = element_text(face = "plain", family = "Futura"), 
        legend.title = element_text(face = "bold", family = "Futura", 
                                    size = 10),
        legend.text = element_text( size = 10, family = "Futura"))

ggsave("output/figures/figure_4_3.pdf", plot = figure_4_3, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_4_3.pdf")


# Figure 4.4 Deactivated and blocked listings -----------------------------

FREH_2020 <- 
  daily %>% 
  filter(housing, date >= "2020-01-01", date <= "2020-02-29", FREH_3 > 0.5) %>% 
  pull(property_ID) %>% 
  unique() %>% 
  {filter(property, property_ID %in% .)} %>% 
  st_drop_geometry()
  
non_FREH_2020 <- 
  property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% {daily %>% 
      filter(housing, date >= "2020-01-01", date <= "2020-02-29", 
             status != "B") %>% 
      pull(property_ID) %>% 
      unique()}, !property_ID %in% FREH_2020$property_ID)
  
FREH_2019 <- 
  daily %>% 
  filter(housing, date >= "2019-01-01", date <= "2019-02-28", FREH_3 > 0.5) %>% 
  pull(property_ID) %>% 
  unique() %>% 
  {filter(property, property_ID %in% .)} %>% 
  st_drop_geometry()

non_FREH_2019 <- 
  property %>% 
  st_drop_geometry() %>% 
  filter(property_ID %in% {daily %>% 
      filter(housing, date >= "2019-01-01", date <= "2019-02-28", 
             status != "B") %>% 
      pull(property_ID) %>% 
      unique()}, !property_ID %in% FREH_2019$property_ID)

total_2020 <- 
  tibble(
    group = c("FREH", "non-FREH"),
    year = 2020,
    variable = "total listings",
    value = c(nrow(FREH_2020), nrow(non_FREH_2020))
    )

total_2019 <- 
  tibble(
    group = c("FREH", "non-FREH"),
    year = 2019,
    variable = "total listings",
    value = c(nrow(FREH_2019), nrow(non_FREH_2019))
  )

deactivated_2020 <- 
  tibble(
    group = c("FREH", "non-FREH"),
    year = 2020,
    variable = "deactivated",
    value = c({
      FREH_2020 %>% 
        summarize(total = sum(scraped <= "2020-07-31")) %>% 
        pull(total)}, {
          non_FREH_2020 %>% 
            summarize(total = sum(scraped <= "2020-07-31")) %>% 
            pull(total)})
  )

deactivated_2019 <- 
  tibble(
    group = c("FREH", "non-FREH"),
    year = 2019,
    variable = "deactivated",
    value = c({
      FREH_2019 %>% 
        summarize(total = sum(scraped <= "2019-07-31")) %>% 
        pull(total)}, {
          non_FREH_2019 %>% 
            summarize(total = sum(scraped <= "2019-07-31")) %>% 
            pull(total)})
  )

blocked_PIDs_2020 <- 
  daily %>% 
  filter(housing, date >= "2020-07-01", date <= "2020-07-31") %>% 
  group_by(property_ID) %>% 
  filter(mean(status == "B") == 1) %>% 
  pull(property_ID) %>% 
  unique()

blocked_PIDs_2019 <- 
  daily %>% 
  filter(housing, date >= "2019-07-01", date <= "2019-07-31") %>% 
  group_by(property_ID) %>% 
  filter(mean(status == "B") == 1) %>% 
  pull(property_ID) %>% 
  unique()

blocked_2020 <-
  tibble(
    group = c("FREH", "non-FREH"),
    year = 2020,
    variable = "blocked",
    value = c(nrow(filter(FREH_2020, property_ID %in% blocked_PIDs_2020)),
              nrow(filter(non_FREH_2020, property_ID %in% blocked_PIDs_2020)))
  )

blocked_2019 <-
  tibble(
    group = c("FREH", "non-FREH"),
    year = 2019,
    variable = "blocked",
    value = c(nrow(filter(FREH_2019, property_ID %in% blocked_PIDs_2019)),
              nrow(filter(non_FREH_2019, property_ID %in% blocked_PIDs_2019)))
  )

active_2020 <- 
  tibble(
    group = c("FREH", "non-FREH"),
    year = 2020,
    variable = "active",
    value = c(
      nrow(FREH_2020) - deactivated_2020[1,]$value - blocked_2020[1,]$value,
      nrow(non_FREH_2020) - deactivated_2020[2,]$value - blocked_2020[2,]$value)
  )

active_2019 <- 
  tibble(
    group = c("FREH", "non-FREH"),
    year = 2019,
    variable = "active",
    value = c(
      nrow(FREH_2019) - deactivated_2019[1,]$value - blocked_2019[1,]$value,
      nrow(non_FREH_2019) - deactivated_2019[2,]$value - blocked_2019[2,]$value)
  )

comparison_df <- 
  bind_rows(total_2020, total_2019, deactivated_2020, deactivated_2019,
          blocked_2020, blocked_2019, active_2020, active_2019)

# Figure parameters
n_segs <- 1000
offset <- 1000
max_y <- comparison_df %>% filter(value == max(value)) %>% 
  mutate(value = value + 2 * offset) %>% pull(value)

fig_polys <- 
  comparison_df %>% 
  # filter(group == "FREH", year == 2020) %>% 
  group_by(group, year, variable) %>%
  summarize(
    x = c(3, 5) - (variable == "total listings") * 3,
    ymax = rep(value, 2)
  ) %>% 
  ungroup() %>% 
  arrange(desc(variable)) %>% 
  group_by(group, year) %>% 
  mutate(
    ymin = case_when(
      variable == "total listings" ~ offset,
      variable == "active"         ~ 0,
      variable == "blocked"        ~ ymax[variable == "active"][1] + offset,
      variable == "deactivated"    ~ ymax[variable == "blocked"][1] + 
        ymax[variable == "active"][1] + offset * 2
      ),
    ymax = ymax + ymin, .before = ymax) %>% 
  mutate(adjustment = max_y / 2 - mean(c(ymax[variable == "total listings"], 
                                         ymin[variable == "total listings"])),
         ymin = ymin + adjustment,
         ymax = ymax + adjustment) %>% 
  ungroup()

fig_segments <-
  fig_polys %>% 
  filter(variable != "total listings") %>% 
  mutate(orientation = case_when(
    variable == "deactivated" ~ -1,
    variable == "blocked"     ~ 0,
    variable == "active"      ~ 1)) %>% 
  group_by(group, year, variable) %>% 
  mutate(across(c(ymin, ymax), ~{.x + c(offset, 0) * orientation})) %>% 
  ungroup() %>% 
  select(-orientation) %>% 
  mutate(x = (x + 1) / 2) %>% 
  group_by(group, year) %>% 
  mutate(ramp = 1:6) %>% 
  group_by(group, year, variable) %>% 
  mutate(ymin_slope = diff(ymin) / diff(x),
         ymin_intercept = ymin[1] - ymin_slope * x[1],
         ymax_slope = diff(ymax) / diff(x),
         ymax_intercept = ymax[1] - ymax_slope * x[1]) %>% 
  summarize(
    x = seq(min(x), max(x), length.out = n_segs),
    xend = x,
    y = x * ymin_slope[1] + ymin_intercept[1],
    yend = xend * ymax_slope[1] + ymax_intercept[1],
    ramp = x - min(x) + min(ramp)
  ) %>% 
  ungroup()

fig_labels <-
  fig_polys %>% 
  group_by(group, year, variable) %>% 
  summarize(x = mean(x),
            y = mean(c(ymin, ymax)),
            label = mean(ymax) - mean(ymin)) %>% 
  ungroup() %>% 
  group_by(group, year) %>% 
  mutate(label = case_when(
    variable == "total listings" ~ paste0(label, " ", variable, "\nin Jan/Feb"),
    variable == "active" ~ paste0(label, " (", 
                                  round(label/sum(label) * 100, 1), "%) ", 
                                  variable, "\nin July"),
    TRUE ~ paste0(label, " (", round(label/sum(label) * 100, 1), "%) ",
                  variable))) %>% 
  ungroup()
  
figure_4_4 <- 
  fig_polys %>% 
  ggplot() +
  geom_ribbon(aes(x = x, ymin = ymin, ymax = ymax, fill = variable)) +
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend, group = variable,
                   colour = ramp), data = fig_segments, inherit.aes = FALSE) +
  geom_text(aes(x, y, group = variable, label = label), data = fig_labels,
            colour = "white", family = "Futura", size = 2) +
  scale_colour_gradientn(colours = col_palette[c(5, 3, 5, 2, 5, 1)]) +
  scale_fill_manual(values = c("total listings" = col_palette[5], 
                               "active" = col_palette[1],
                               "blocked" = col_palette[2],
                               "deactivated" = col_palette[3])) +
  facet_grid(rows = vars(group), cols = vars(year), switch = "y",
             scales = "free_y", space = "free_y") +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(face = "plain", family = "Futura"), 
        strip.text = element_text(face = "bold", family = "Futura"))

ggsave("output/figures/figure_4_4.pdf", plot = figure_4_4, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_4_4.pdf")



FREH_in_jan_feb <- 
  daily %>% 
  filter(housing, date >= "2020-01-01", date <= "2020-02-28", FREH_3 > 0.5) %>% 
  pull(property_ID) %>% 
  unique()

monthly_reservation_trajectories <- 
  daily %>% 
  filter(housing, status == "R", date >= "2019-01-01", 
         listing_type == "Entire home/apt") %>% 
  mutate(FREH_feb = if_else(property_ID %in% FREH_in_jan_feb, TRUE, FALSE)) %>% 
  tsibble::as_tsibble(index = date, key = property_ID) %>% 
  tsibble::group_by_key() %>% 
  tsibble::index_by(yearmon = tsibble::yearmonth(date)) %>% 
  summarize(n = sum(status == "R"),
            FREH_feb = as.logical(prod(FREH_feb))) %>% 
  group_by(FREH_feb) %>% 
  tsibble::index_by(yearmon) %>% 
  summarize(n = mean(n)) %>% 
  arrange(yearmon, FREH_feb)

daily_reservation_trajectories <- 
  daily %>% 
  filter(housing, status == "R", date >= "2019-01-01", 
         listing_type == "Entire home/apt") %>% 
  mutate(FREH_feb = if_else(property_ID %in% FREH_in_jan_feb, TRUE, FALSE)) %>% 
  count(date, FREH_feb)

figure_4_5_left <- 
  daily_reservation_trajectories %>% 
  group_by(FREH_feb) %>% 
  mutate(n = slide_dbl(n, mean, .before = 13)) %>% 
  ungroup() %>% 
  ggplot(aes(date, n, colour = FREH_feb)) +
  annotate("rect", xmin = as.Date("2020-03-29"), xmax = as.Date("2020-06-25"), 
           ymin = 0, ymax = Inf, alpha = 0.2) +
  geom_line(lwd = 1) +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = "Total daily reservations", limits = c(0, NA), 
                     label = scales::comma) +
  scale_color_manual(name = "FREH status in January-February 2020", 
                     values = col_palette[c(5, 1)]) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        text = element_text(face = "plain", family = "Futura"), 
        legend.title = element_text(face = "bold", family = "Futura", 
                                    size = 10),
        legend.text = element_text( size = 10, family = "Futura"))

figure_4_5_right <- 
  monthly_reservation_trajectories %>% 
  mutate(date = as.Date(yearmon)) %>% 
  ggplot(aes(date, n, colour = FREH_feb)) +
  annotate("rect", xmin = as.Date("2020-03-29"), xmax = as.Date("2020-06-25"), 
           ymin = 0, ymax = Inf, alpha = 0.2) +
  geom_line(lwd = 1) +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = "Average monthly reservations", limits = c(0, NA), 
                     label = scales::label_number(accuracy = 1)) +
  scale_color_manual(name = "FREH status in January-February 2020", 
                     values = col_palette[c(5, 1)]) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        text = element_text(face = "plain", family = "Futura"), 
        legend.title = element_text(face = "bold", family = "Futura", 
                                    size = 10),
        legend.text = element_text( size = 10, family = "Futura"))

figure_4_5 <- figure_4_5_left + figure_4_5_right + 
  plot_layout(guides = 'collect') & theme(legend.position = "bottom")

ggsave("output/figures/figure_4_5.pdf", plot = figure_4_5, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_4_5.pdf")


# Clean up ----------------------------------------------------------------

rm(active_2019, active_2020, active_by_status, average_prices, blocked_2019,
   blocked_2020, comparison_df, daily_reservation_trajectories, 
   deactivated_2019, deactivated_2020, fig_labels, fig_polys, fig_segments,
   figure_4_1, figure_4_2, figure_4_3, figure_4_4, figure_4_5, figure_4_5_left, 
   figure_4_5_right, FREH_2019, FREH_2020, mar_jul_price_trend, monthly_prices, 
   monthly_reservation_trajectories, non_FREH_2019, non_FREH_2020,
   reservations, total_2019, total_2020, trends, blocked_PIDs_2019,
   blocked_PIDs_2020, feb_price_trend, feb_trend, FREH_in_jan_feb, 
   mar_aug_seasonal, mar_jul_price_seasonal, max_y, n_segs, offset)
