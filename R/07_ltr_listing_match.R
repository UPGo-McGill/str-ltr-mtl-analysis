#### 07 LTR LISTING MATCH ######################################################

#' This script runs relatively quickly, and should be rerun anytime the raw STR, 
#' LTR or image matching data changes.
#' 
#' Output:
#' - `str_processed.Rdata`
#' - `ltr_processed.Rdata`
#' - `matches_processed.Rdata`
#' 
#' Script dependencies:
#' - `03_str_data_import.R`
#' - `04_ltr_data_import.R`
#' - `06_image_match.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")
library(matchr)


# Load previous data ------------------------------------------------------

qload("output/str_raw.qsm", nthreads = availableCores())
qload("output/matches_raw.qsm", nthreads = availableCores())
ltr <- qread("output/ltr_raw.qs", nthreads = availableCores())
dl_location <- "/Volumes/Data 2/Scrape photos/montreal"


# Clean up matches --------------------------------------------------------

cl_matches <-
  cl_matches %>%
  filter(match == "match") %>%
  mutate(
    x_name = str_replace_all(vctrs::field(x_sig, "file"),
                             paste0(dl_location, "/ab/|-[:digit:]+.jpg"), ""),
    y_name = str_replace_all(vctrs::field(y_sig, "file"),
                             paste0(dl_location, "/cl/|-[:digit:]+.jpg"), ""))

kj_matches <-
  kj_matches %>%
  filter(match == "match") %>%
  mutate(
    x_name = str_replace_all(vctrs::field(x_sig, "file"),
                             paste0(dl_location, "/ab/|-[:digit:]+.jpg"), ""),
    y_name = str_replace_all(vctrs::field(y_sig, "file"),
                             paste0(dl_location, "/kj/|-[:digit:]+.jpg"), ""))

matches <-
  bind_rows(kj_matches, cl_matches) %>%
  select(property_ID = x_name, ltr_ID = y_name)

# Make sure matches is in sync with property file
matches <-
  matches %>%
  filter(property_ID %in% property$property_ID)

rm(cl_matches, kj_matches)


# Connect STR and LTR listings --------------------------------------------

property_nest <-
  property %>%
  st_drop_geometry() %>%
  left_join(matches, by = "property_ID") %>%
  select(property_ID, ltr_ID)

property <-
  property_nest %>%
  group_by(property_ID) %>%
  summarize(ltr_ID = list(ltr_ID)) %>%
  left_join(property, .) %>%
  select(-geometry, everything(), geometry) %>%
  as_tibble() %>%
  st_as_sf()

ltr_nest <-
  ltr %>%
  st_drop_geometry() %>%
  left_join(matches, by = c("id" = "ltr_ID")) %>%
  select(id, property_ID)

ltr <-
  ltr_nest %>%
  group_by(id) %>%
  summarize(property_ID = list(property_ID)) %>%
  left_join(ltr, .) %>%
  select(-geometry, everything(), geometry) %>%
  mutate(property_ID = map(property_ID, ~unique(unlist(.x))))

rm(property_nest, ltr_nest)


# Save output -------------------------------------------------------------

qsavem(property, daily, host, file = "output/str_processed.qsm",
       nthreads = availableCores())
qsave(ltr, file = "output/ltr_processed.qs",
      nthreads = availableCores())
qsavem(matches, ab_matches, file = "output/matches_processed.qsm",
       nthreads = availableCores())
