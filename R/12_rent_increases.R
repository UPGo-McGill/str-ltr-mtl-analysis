#### 12 RENT INCREASES #########################################################

#' This script is fast to run; it should be rerun whenever STR data changes.

#' Output:
#' - `rent_increases.Rdata`
#' 
#' Script dependencies:
#' - `05_cmhc_data_import.R`
#' - `09_str_processing.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")


# Load data ---------------------------------------------------------------

load("output/cmhc.Rdata")
load("output/str_processed.Rdata")

# The magic value derived from Barron et al.
magic_value <- 0.00651547619


# Table for entire city ---------------------------------------------------

# Calculate rent increase as the magic number * the proportion of total listings
# created in a given year
rent_increase <-
  property %>% 
  st_drop_geometry() %>% 
  count(year_created = substr(created, 1, 4)) %>% 
  filter(!is.na(year_created)) %>% 
  mutate(year_created = if_else(year_created <= "2014", "old", 
                                year_created)) %>%
  group_by(year_created) %>% 
  summarize(n = sum(n)) %>% 
  slice(n(), 1:(n() - 1)) %>% 
  mutate(
    rent_increase = slide_dbl(n, ~{
      magic_value * .x[length(.x)] / sum(.x[-length(.x)])}, .before = n() - 1),
    rent_increase = if_else(is.infinite(rent_increase), NA_real_, rent_increase)
    )

# Cumulative rent increase, 2016-2019
rent_increase %>% 
  filter(!is.na(rent_increase)) %>% 
  summarize(total = prod(1 + rent_increase)) %>% 
  pull(total)


# Table for CMHC zones ----------------------------------------------------

rent_increase_zone <-
  property %>% 
  st_intersection(cmhc) %>% 
  st_drop_geometry() %>% 
  group_by(zone) %>% 
  count(year_created = substr(created, 1, 4)) %>% 
  filter(!is.na(year_created)) %>% 
  mutate(year_created = if_else(year_created <= "2014", "old", 
                                year_created)) %>% 
  group_by(zone, year_created) %>% 
  summarize(n = sum(n)) %>% 
  slice(n(), 1:(n() - 1)) %>% 
  mutate(
    rent_increase = slide_dbl(n, ~{
      magic_value * .x[length(.x)] / sum(.x[-length(.x)])}, .before = n() - 1),
    rent_increase = if_else(is.infinite(rent_increase), NA_real_, 
                            rent_increase)) %>% 
  ungroup()


# Save output -------------------------------------------------------------

rm(magic_value)
save(rent_increase, rent_increase_zone, file = "output/rent_increases.Rdata")
