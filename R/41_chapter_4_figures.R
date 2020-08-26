#### 41 CHAPTER 4 FIGURES ######################################################

#' This script produces the graphs and maps for chapter 4. It runs quickly.
#' 
#' Output:
#' - `figure_4_1.pdf`
#' - `figure_4_2.pdf`
#' 
#' Script dependencies:
#' - `09_str_processing.R`
#' 
#' External dependencies:
#' - The Futura and Futura Condensed fonts, which can be imported in 
#'   `01_startup.R`
#' - `11_FREH_model.R`

source("R/01_startup.R")

load("output/str_processed.Rdata")


# Figure 4.1 - Active and reserved listings since 2018 --------------------

figure_4_1 <-
  daily %>% 
  filter(housing, date >= "2018-01-01", status != "B") %>% 
  count(date, status) %>% 
  group_by(status) %>% 
  mutate(n = slide_dbl(n, mean, .before = 13)) %>% 
  ungroup() %>% 
  ggplot(aes(date, n, color = status)) +
  # geom_smooth(se = FALSE, lwd = 1) +
  scale_color_manual(name = "Status", labels = c("Available", "Reserved"), 
                     values = col_palette[c(5, 1)]) +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = "Number of listings",
                     limits = c(0, NA)) +
  annotate("rect", xmin = as.Date("2020-03-14"), xmax = as.Date("2020-06-25"), 
           ymin = -Inf, ymax = Inf, alpha = 0.2) +
  annotate("curve", x = as.Date("2019-02-01"), xend = as.Date("2020-05-01"),
           y = 7500, yend = 7500, curvature = -.3, lwd = 0.25,
           arrow = arrow(length = unit(0.05, "inches"))) +
  annotate("text", x = as.Date("2018-12-11"), y = 7500,
           label = "STRs banned \nby Province", family = "Futura Condensed") +
  geom_line(lwd = 1) +
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


# Figure 4.2 - Active and reserved listings in 2020 --------------------------------------

figure_4_2 <- ggplot()+
  geom_line(data = daily %>% 
              filter(housing, date >= "2020-01-01", status == "R") %>% 
              count(date, status) %>% 
              mutate(n = data.table::frollmean(n, 7)),
            aes(date, n, color = status), alpha = 0.4)+
  stat_smooth(geom='line', data = daily %>% 
                filter(housing, date >= "2020-01-01", status == "R") %>% 
                count(date, status) %>% 
                mutate(n = data.table::frollmean(n, 7)),
              aes(date, n, color = status), se = F, lwd=0.9)+
  geom_line(data = daily %>% 
              filter(housing, date >= "2020-01-01", status == "A") %>% 
              count(date, status) %>% 
              mutate(n = data.table::frollmean(n, 7)),
            aes(date, n, color = status), alpha = 0.4)+
  stat_smooth(geom='line', data = daily %>% 
                filter(housing, date >= "2020-01-01", status == "A") %>% 
                count(date, status) %>% 
                mutate(n = data.table::frollmean(n, 7)),
              aes(date, n, color = status), se = F, lwd = 0.9)+
  scale_color_manual(name="Status", labels = c("Available", "Reserved"),values = col_palette[c(1, 3)]) +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = "Number of listings") +
  annotate("rect", xmin = as.Date("2020-03-14"), xmax = as.Date("2020-06-25"), ymin = -Inf, ymax = Inf, alpha = .2)+
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(face = "plain"), #family = "Futura", 
        legend.title = element_text(face = "bold", #family = "Futura", 
                                    size = 10),
        legend.text = element_text( size = 10) #family = "Futura",
  )

ggsave("output/figures/figure_4_2.pdf", plot = figure_4_2, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_4_2.pdf")

