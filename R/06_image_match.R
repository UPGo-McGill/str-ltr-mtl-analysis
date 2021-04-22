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
library(foreach)
library(progressr)
handlers(global = TRUE)


# Load previous data ------------------------------------------------------

qload("output/str_raw.qsm", nthreads = availableCores())
ltr <- qread("output/ltr_raw.qs", nthreads = availableCores())
rm(daily, host, exchange_rates)


# Specify location on drive to download photos ----------------------------

dl_location <- "/Volumes/Data 2/Scrape photos/montreal"


# Get AB image URLs -------------------------------------------------------

# Get AB urls
ab_urls <-
  property %>%
  st_drop_geometry() %>% 
  transmute(urls = coalesce(ab_image_url, ha_image_url)) %>%
  pull(urls) %>%
  str_replace('(?<=(jpg|JPEG|jpeg|JPG)).*', '')

# Get AB IDs
ab_ids <- property$property_ID

# Remove already downloaded images
ab_paths <-
  list.files(paste0(dl_location, "/ab")) %>%
  str_remove(".(jpg|jpeg|JPG|JPEG)") %>%
  str_remove("-[:digit:]$")

ab_urls <- ab_urls[!ab_ids %in% ab_paths]
ab_ids <- ab_ids[!ab_ids %in% ab_paths]



# Get KJ image urls -------------------------------------------------------

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

# Remove already downloaded images
kj_paths <-
  list.files(paste0(dl_location, "/kj")) %>%
  str_remove(".(jpg|jpeg|JPG|JPEG)") %>%
  str_remove("-[:digit:]$")

kj_urls <- kj_urls[!kj_ids %in% kj_paths]
kj_ids <- kj_ids[!kj_ids %in% kj_paths]


# Get CL image urls -------------------------------------------------------

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

# Remove already downloaded images
cl_paths <-
  list.files(paste0(dl_location, "/cl")) %>%
  str_remove(".(jpg|jpeg|JPG|JPEG)") %>%
  str_remove("-[:digit:]$")

cl_urls <- cl_urls[!cl_ids %in% cl_paths]
cl_ids <- cl_ids[!cl_ids %in% cl_paths]

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

foreach(i = seq_along(ab_urls)) %do% {
  try(download.file(ab_urls[[i]], paste0(
    dl_location, "/ab/", ab_ids[[i]], "-", seq_along(ab_urls[[i]]), ".jpg")))
}

foreach(i = seq_along(cl_urls)) %do% {
  try(download.file(cl_urls[[i]], paste0(
    dl_location, "/cl/", cl_ids[[i]], "-", seq_along(cl_urls[[i]]), ".jpg")))
}

foreach(i = seq_along(kj_urls)) %do% {
  try(download.file(kj_urls[[i]], paste0(
    dl_location, "/kj/", kj_ids[[i]], "-", seq_along(kj_urls[[i]]), ".jpg")))
}


# Get new paths -----------------------------------------------------------

ab_paths <- list.files(paste0(dl_location, "/ab"), full.names = TRUE)
cl_paths <- list.files(paste0(dl_location, "/cl"), full.names = TRUE)
kj_paths <- list.files(paste0(dl_location, "/kj"), full.names = TRUE)

rm(dl_location, ab_urls, ab_ids, cl_urls, cl_ids, kj_urls, kj_ids)


# Get signatures ----------------------------------------------------------

ab_sigs <- create_signature(ab_paths)

qsavem(ab_sigs, file = "output/img_sigs.qsm", nthreads = availableCores())

cl_sigs <- create_signature(cl_paths)

qsavem(ab_sigs, cl_sigs, file = "output/img_sigs.qsm",
       nthreads = availableCores())

kj_sigs <- create_signature(kj_paths)

qsavem(ab_sigs, cl_sigs, kj_sigs, file = "output/img_sigs.qsm",
       nthreads = availableCores())

rm(ab_paths, cl_paths, kj_paths)


# Match images ------------------------------------------------------------

ab_matches <- identify_matches(ab_sigs)
ab_changes <- compare_images(ab_matches)
ab_matches <- integrate_changes(ab_matches, ab_changes)
qsavem(ab_matches, file = "output/matches_raw.qsm", nthreads = availableCores())

cl_matrix <- match_signatures(ab_sigs, cl_sigs)
cl_matches <- identify_matches(cl_matrix)
cl_matches_new <- cl_matches

cl_matches <-
  cl_matches %>%
  relocate(correlation, .after = y_sig)

cl_matches <-
  cl_matches_new %>%
  anti_join(cl_matches, by = c("x_sig", "y_sig")) %>%
  mutate(confirmed = FALSE) %>%
  bind_rows(cl_matches)

cl_changes <- compare_images(cl_matches)
cl_matches <- integrate_changes(cl_matches, cl_changes)
qsavem(ab_matches, cl_matches, file = "output/matches_raw.qsm",
       nthreads = availableCores())
rm(cl_matrix)

kj_matrix <- match_signatures(ab_sigs, kj_sigs)
kj_matches <- identify_matches(kj_matrix)
kj_matches_new <- kj_matches

kj_matches <-
  kj_matches %>%
  relocate(correlation, .after = y_sig)

kj_matches <-
  kj_matches_new %>%
  anti_join(kj_matches, by = c("x_sig", "y_sig")) %>%
  mutate(confirmed = FALSE) %>%
  bind_rows(kj_matches)

kj_changes <- compare_images(kj_matches)
kj_matches <- integrate_changes(kj_matches, kj_changes) %>%
  relocate(correlation, .after = y_sig)

qsavem(ab_matches, cl_matches, kj_matches, file = "output/matches_raw.qsm",
       nthreads = availableCores())
rm(kj_matrix)

qload("output/matches_raw_old.qsm", nthreads = 32)



# Save output -------------------------------------------------------------

qsavem(ab_matches, cl_matches, kj_matches, file = "output/matches_raw.qsm",
       nthreads = availableCores())
qsavem(ab_changes, cl_changes, kj_changes, file = "output/match_changes.qsm",
       nthreads = availableCores())
