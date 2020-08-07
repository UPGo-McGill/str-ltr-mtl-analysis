#### 12 RENT INCREASES #########################################################

source("R/01_startup.R")

# The magic value derived from Barron et al.
magic_value <- 0.00651547619


# Table for entire city ---------------------------------------------------

# Create table with number of listings created each year
rent_increase <-
  property %>% 
  st_drop_geometry() %>% 
  count(year_created = substr(created, 1, 4)) %>% 
  filter(!is.na(year_created)) %>% 
  mutate(year_created = if_else(year_created <= "2015", "old", year_created)) %>%
  group_by(year_created) %>% 
  summarize(n = sum(n)) %>% 
  slice(n(), 1:(n() - 1)) %>% 
  mutate(rent_increase = NA_real_)

# Calculate rent increase as the magic number * the proportion of total listings
# created in a given year

for (i in 2:6) rent_increase[i,]$rent_increase <- 
  magic_value * rent_increase[i,]$n / sum(rent_increase[seq_len(i - 1),]$n)
  

# Table for CMHC zones ----------------------------------------------------

rent_increase_zone <- 
  property %>% 
  st_drop_geometry() %>% 
  group_by(zone) %>% 
  count(year_created = substr(created, 1, 4)) %>% 
  filter(!is.na(year_created)) %>% 
  mutate(year_created = if_else(year_created <= "2015", "old", year_created)) %>%
  group_by(zone, year_created) %>% 
  summarize(n = sum(n)) %>% 
  slice(n(), 1:(n() - 1)) %>% 
  mutate(rent_increase = NA_real_)

rent_increase_zone <- 
  rent_increase_zone %>% 
  group_split() %>% 
  map_dfr(~{
    for (i in 2:6) .x[i,]$rent_increase <- 
        magic_value * .x[i,]$n / sum(.x[seq_len(i - 1),]$n)
    .x
  })





