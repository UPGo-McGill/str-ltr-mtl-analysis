#### 61 CHAPTER 6 FIGURES ######################################################

#' This script produces the graphs and maps for chapter 6. It runs quickly.
#' 
#' Output:
#' - `figure_6_1.pdf`
#' 
#' Script dependencies:
#' - `05_cmhc_data_import.R`
#' - `07_ltr_listing_match.R`
#' 
#' External dependencies:
#' - The Futura and Futura Condensed fonts, which can be imported in 
#'   `01_startup.R`

source("R/01_startup.R")
library(patchwork)


# Load previous data ------------------------------------------------------

load("output/cmhc.Rdata")
load("output/ltr_processed.Rdata")
load("output/str_processed.Rdata")


# Prepare objects for figure 6.1 ------------------------------------------------------

#' [1] DA information with fields for tenure
DA <-
  cancensus::get_census(
    dataset = "CA16", regions = list(CSD = "2466023"), level = "DA",
    vector = c("v_CA16_4836", "v_CA16_4838"),
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(GeoUID, CSD_UID, Population, Dwellings, 
         "v_CA16_4836: Total - Private households by tenure - 25% sample data", "v_CA16_4838: Renter") %>% 
  set_names(c("GeoUID", "CMA_UID", "population", "dwellings", "parent_tenure", "renter", "geometry")) %>% 
  mutate(p_renter = renter/parent_tenure) %>% 
  st_set_agr("constant") %>% 
  select(-parent_tenure, -renter) %>% 
  st_as_sf()


#' [2] Get returning STRs info
ltr_unique_ab_id <- 
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

#' [3] Prepare returning STRs dataframe to work with
returning_STRs <- property %>% 
  filter(property_ID %in% ltr_unique_ab_id$property_ID,
         scraped < "2020-06-01") %>% 
  select(property_ID, geometry)


#' [4] Get total vacancy rate for zones
cmhc_vacancy <- annual_vacancy %>% 
  filter(date == "2019", 
         dwelling_type == "Total",
         bedroom == "Total") %>% 
  distinct(zone, .keep_all = TRUE)


# add important vacancy rates for central zones with NAs

cmhc_vacancy[6, 6] = as.numeric(0.015) ##Plateau 2018
cmhc_vacancy[8, 6] = as.numeric(0.032) ##Hochelaga 2017
cmhc_vacancy[17, 6] = as.numeric(0.009) ##Mercier 2018
cmhc_vacancy[11, 6] = as.numeric(0.040) ##Mercier 2018


# Intersect Zones geometry to DA geometry to get census info on CMHC geometry ------------------------------------------------------

cmhc_vacancy_summarized <- 
  st_intersect_summarize(
    data = DA, 
    destination = cmhc_vacancy, 
    population = population, 
    group_vars = vars(zone), 
    count_vars = vars(dwellings),
    mean_vars =  vars(p_renter)
  ) %>% 
  select(-population, -dwellings, -p_renter)


# Get each returning STRs in their respective zones ------------------------------------------------------

str_vacancy_impact <- cmhc_vacancy_summarized %>% 
  left_join(., cmhc_vacancy, by="zone") %>% 
  mutate(n_renter=population*p_renter) %>% 
  select(-dwelling_type, -bedroom, -quality) %>% 
  st_intersection(., returning_STRs)


# Estimate the change in vacancy rates ------------------------------------------------------

vacancy_change <- str_vacancy_impact %>% 
  st_drop_geometry() %>% 
  left_join(cmhc_vacancy_summarized, ., by = "zone") %>% 
  mutate(n_vacant_unit = n_renter*vacancy) %>% 
  group_by(zone, n_vacant_unit, n_renter) %>% 
  summarise(n_STRs = n(), 
            n_vacant_unit = sum(n_vacant_unit)/n(), 
            n_renter = sum(n_renter)/n(), 
            vacancy=sum(vacancy)/n()
  ) %>% 
  mutate(e_units = as.numeric(n_vacant_unit) + as.numeric(n_STRs)) %>% 
  mutate(new_vacancy = e_units/n_renter) %>% 
  mutate(percent_change = (as.numeric(new_vacancy) - as.numeric(vacancy))/as.numeric(vacancy),
         point_percent_diff = as.numeric(new_vacancy)-as.numeric(vacancy)
  ) %>% 
  select(zone, vacancy, new_vacancy, percent_change, percent_diff, geometry)


# Figure 6.1. Impact of returning STRs on rental vacancy rate

vacancy_change %>% 
  mutate(ppp_diff=point_percent_diff*100) %>% 
  ggplot()+
  geom_sf(aes(fill=ppp_diff))+    
  scale_fill_gradientn(colors = col_palette[c(3, 4, 1)], na.value = "grey80",
                       #limits = c(0, 0.05), oob = scales::squish, 
                       #labels = scales::percent
  )  +
  guides(fill = guide_colourbar(title = "Vacancy change\npercent points",
                                title.vjust = 1)) + 
  theme_void()+
  theme(text = element_text(face = "plain"),
        legend.title = element_text(face = "bold", size = 7),
        legend.title.align = 0.9,
        legend.text = element_text(size = 5),
        #panel.border = element_rect(colour = "white", size = 2)
  )
