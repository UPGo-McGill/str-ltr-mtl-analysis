#### 06 IMAGE MATCH ############################################################

#' This script is extremely time-consuming and memory-intensive to run, so it 
#' should only be rerun when image matching needs to be rebuilt from scratch. In 
#' addition, the script downloads hundreds of thousands of photos to a specified 
#' folder, so it requires approximately 50 GB of free storage space.
#' 
#' Output:
#' - `img_sigs.Rdata`
#' - `matches_raw.Rdata`
#' - `match_changes.Rdata`
#' 
#' Script dependencies:
#' - `03_str_data_import.R`
#' - `04_ltr_data_import.R`
#' 
#' External dependencies:
#' - Access to the UPGo database
#' - Listings scraped from Kijiji and Craigslist with upgo::upgo_scrape_kj and
#'   upgo::upgo_scrape_cl

source("R/01_startup.R")
library(matchr)
library(furrr)


# Load previous data ------------------------------------------------------

load("output/str_raw.Rdata")
load("output/ltr_raw.Rdata")
rm(daily, host)


# Specify location on drive to download photos ----------------------------

dl_location <- "/Volumes/Data/Scrape photos/montreal"


# Get image URLs ----------------------------------------------------------

# Get AB urls
ab_urls <- 
  property$ab_image_url %>% 
  str_replace('(?<=jpg).*', '')

# Get AB IDs
ab_ids <- property$property_ID

# Get KJ urls
kj_urls <-
  ltr %>% 
  st_drop_geometry() %>% 
  filter(str_starts(id, "kj-")) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  pull(photos)

# Get KJ IDs
kj_ids <- 
  ltr %>% 
  st_drop_geometry() %>% 
  filter(str_starts(id, "kj-")) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  pull(id)

# Get CL urls
cl_urls <-
  ltr %>% 
  st_drop_geometry() %>% 
  filter(str_starts(id, "cl-")) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  pull(photos)

# Get CL IDs
cl_ids <- 
  ltr %>% 
  st_drop_geometry() %>% 
  filter(str_starts(id, "cl-")) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  pull(id)

rm(property, ltr)


# Make download subfolders ------------------------------------------------

if (!dir.exists(dl_location)) dir.create(dl_location, 
                                               recursive = TRUE)

if (!dir.exists(paste0(dl_location, "/ab"))) {
  dir.create(paste0(dl_location, "/ab"))
}

if (!dir.exists(paste0(dl_location, "/cl"))) {
  dir.create(paste0(dl_location, "/cl"))
}

if (!dir.exists(paste0(dl_location, "/kj"))) {
  dir.create(paste0(dl_location, "/kj"))
}


# Download images ---------------------------------------------------------

future_map2(ab_urls, ab_ids, ~{
  try(download.file(.x, paste0(
    dl_location, "/ab/", .y, ".jpg")))
})

future_map2(cl_urls, cl_ids, ~{
  try(download.file(.x, paste0(
    dl_location, "/cl/", .y, "-", seq_along(.x), ".jpg"))) 
})

future_map2(kj_urls, kj_ids, ~{
  try(download.file(.x, paste0(
    dl_location, "/kj/", .y, "-", seq_along(.x), ".jpg"))) 
})


# Get new paths -----------------------------------------------------------

ab_paths <- list.files(paste0(dl_location, "/ab"), full.names = TRUE)
cl_paths <- list.files(paste0(dl_location, "/cl"), full.names = TRUE)
kj_paths <- list.files(paste0(dl_location, "/kj"), full.names = TRUE)

rm(dl_location, ab_urls, ab_ids, cl_urls, cl_ids, kj_urls, kj_ids)


# Get signatures ----------------------------------------------------------

ab_sigs <- identify_image(ab_paths, batch_size = 100)
save(ab_sigs, file = "output/img_sigs.Rdata")

cl_sigs <- identify_image(cl_paths, batch_size = 2000)
save(ab_sigs, cl_sigs, file = "output/img_sigs.Rdata")

kj_sigs <- identify_image(kj_paths, batch_size = 2000)
save(ab_sigs, cl_sigs, kj_sigs, file = "output/img_sigs.Rdata")

rm(ab_paths, cl_paths, kj_paths)


# Match images ------------------------------------------------------------

ab_matrix <- match_signatures(ab_sigs)
ab_matches <- identify_matches(ab_matrix)
ab_matches <- confirm_matches(ab_matches)
# Temporarily need to remove duplicates!
ab_matches <- ab_matches %>% filter(x_name != y_name)
ab_changes <- compare_images(ab_matches)
ab_matches <- integrate_changes(ab_matches, ab_changes)

kj_matrix <- match_signatures(ab_sigs, kj_sigs)
kj_matches <- identify_matches(kj_matrix)
kj_matches <- confirm_matches(kj_matches)
kj_changes <- compare_images(kj_matches)
kj_matches <- integrate_changes(kj_matches, kj_changes)

cl_matrix <- match_signatures(ab_sigs, cl_sigs)
cl_matches <- identify_matches(cl_matrix)
cl_matches <- confirm_matches(cl_matches)
cl_changes <- compare_images(cl_matches)
cl_matches <- integrate_changes(cl_matches, cl_changes)


# Save output -------------------------------------------------------------

save(ab_matches, cl_matches, kj_matches, file = "output/matches_raw.Rdata")
save(ab_changes, cl_changes, kj_changes, file = "output/match_changes.Rdata")
