#### 04 KJ/CL DATA IMPORT ######################################################

#' This script should only be rerun when new KJ/CL data arrives or it otherwise
#' needs to be rebuilt from scratch.
#' 
#' Output:
#' - `ltr_raw.Rdata`
#' 
#' Script dependencies:
#' - None
#' 
#' External dependencies:
#' - `kj.rds` and `cl.rds`, the results of scraping Kjiji and Craigslist 
#'   listings, respectively.
#' - `rclalq.xls`, a spreadsheet with Kijiji listing scrapes performed by the
#'   RCLALQ, shared with UPGo.

source("R/01_startup.R")


# Load and filter data ----------------------------------------------------

kj <- readRDS("data/ltr/kj.rds") %>% filter(city == "Montreal")
cl <- readRDS("data/ltr/cl.rds") %>% filter(city == "montreal")

rclalq <- 
  readxl::read_xlsx("data/ltr/rclalq.xlsx") %>% 
  filter(location == "VilledeMontréal") %>% 
  select(-location)


# Get geometry from KJ listings -------------------------------------------

# Find previously geocoded addresses
upgo_connect(geolocation = TRUE)

processed_addresses <- 
  geolocation_remote %>% 
  filter(entity %in% !!kj$location) %>% 
  collect()

processed_addresses <- 
  geolocation_remote %>% 
  filter(entity %in% !!rclalq$address) %>% 
  collect() %>% 
  rbind(processed_addresses) %>% 
  distinct()

upgo_disconnect()

kj_old_geography <- 
  kj %>% 
  inner_join(processed_addresses, by = c("location" = "entity"))

kj_new_geography <-
  kj %>%
  filter(!location %in% processed_addresses$entity,
         !is.na(location))

if (nrow(kj_new_geography) > 0) {
  
  to_geocode <- kj_new_geography$location %>% unique()
  
  output <- ggmap::geocode(to_geocode)
  
  output <- tibble(location = to_geocode,
                   lon = output$lon,
                   lat = output$lat)
  
  kj_new_geography <- 
    kj_new_geography %>% 
    left_join(output, by = "location")
  
} else kj_new_geography <- 
  mutate(kj_new_geography, lon = numeric(), lat = numeric())

rclalq_old_geography <- 
  rclalq %>% 
  inner_join(processed_addresses, by = c("address" = "entity"))

rclalq_new_geography <- 
  rclalq %>%
  filter(!address %in% processed_addresses$entity,
         !is.na(address))

if (nrow(rclalq_new_geography) > 0) {
  rclalq_new_geography <- 
    rclalq_new_geography %>% 
    ggmap::mutate_geocode(address)
} else rclalq_new_geography <- mutate(rclalq_new_geography, lon = numeric(), 
                                      lat = numeric())

locations_new <- 
  kj_new_geography %>% 
  select(entity = location, lon, lat) %>% 
  bind_rows(select(rclalq_new_geography, entity = address, lon, lat))

locations_new <- 
  locations_new %>% 
  distinct(entity, .keep_all = TRUE)
  
# Upload new geocoding results to server (ONLY WORKS WITH ADMIN PRIVILEGES)
upgo_connect(geolocation = TRUE)
RPostgres::dbWriteTable(upgo:::.upgo_env$con, "geolocation", locations_new, 
                        append = TRUE)

upgo_disconnect()

# Rbind results
kj <- bind_rows(kj_old_geography, kj_new_geography)
rclalq <- bind_rows(rclalq_old_geography, rclalq_new_geography)

suppressWarnings(rm(processed_addresses, kj_old_geography, kj_new_geography, 
   rclalq_old_geography, rclalq_new_geography, locations_new, output,
   to_geocode))


# Clean up KJ file --------------------------------------------------------

kj <- 
  kj %>% 
  mutate(bedrooms = str_replace(bedrooms, pattern = "Bachelor/Studio|Studio", 
                                replacement = "0"),
         bedrooms = as.numeric(str_sub(bedrooms, end = 1L)),
         bathrooms = str_sub(bathrooms, end = 3L),
         bathrooms = str_replace(bathrooms, pattern = "Ut|U|\\+", 
                                 replacement = ""),
         bathrooms = as.numeric(bathrooms),
         type = details %>% 
           str_extract("(?<=Agreement Type).*?(?=(Move-In)|(Pet Friendly))") %>% 
           str_remove('</dd.*') %>% 
           str_remove('.*">'),
         type = if_else(type == "Not Available", NA_character_, type))

kj <- 
  kj %>% 
  select(id, short_long:furnished, type, lat, lon, title, text, photos) %>% 
  mutate(kj = TRUE)

kj_with_geom <- 
  kj %>% 
  filter(!is.na(lon), !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

kj_without_geom <-
  kj %>%
  filter(is.na(lon) | is.na(lat)) %>% 
  mutate(geometry = st_sfc(st_point())) %>% 
  st_as_sf(crs = 4326) %>% 
  select(-lon, -lat)

kj <-
  rbind(kj_with_geom, kj_without_geom) %>% 
  st_as_sf() %>% 
  arrange(scraped, id)

rm(kj_with_geom, kj_without_geom)


# Clean up CL file --------------------------------------------------------

cl <-
  cl %>% 
  select(id, created:furnished, title, text, photos) %>% 
  separate(location, c("lat", "lon"), sep = ";") %>% 
  mutate(city = "Montreal",
         bedrooms = as.numeric(bedrooms),
         bathrooms = as.numeric(bathrooms),
         created = as.Date(created),
         scraped = as.Date(scraped),
         lat = as.numeric(lat), 
         lon = as.numeric(lon),
         short_long = NA,
         location = NA,
         type = NA,
         kj = FALSE)

cl_with_geom <- 
  cl %>% 
  filter(!is.na(lon), !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

cl_without_geom <-
  cl %>%
  filter(is.na(lon) | is.na(lat)) %>% 
  mutate(geometry = st_sfc(st_point())) %>% 
  st_as_sf(crs = 4326) %>% 
  select(-lon, -lat)

cl <-
  rbind(cl_with_geom, cl_without_geom) %>% 
  st_as_sf() %>% 
  arrange(scraped, id)

rm(cl_with_geom, cl_without_geom)


# Clean up RCLALQ file ----------------------------------------------------

rclalq <- 
  rclalq %>% 
  mutate(
    id = paste0("kj-", id),
    posted = str_sub(posted, start = 16L),
    posted = str_replace_all(posted, c(
      "jours|jour" = "", 
      "plus d'un mois|un mois" = "30",
      "[:digit:]+ heures|une heure|[:digit:]+ minutes|moins d'une minute|une minute" = "0",
      "un" = "1")) %>% 
      as.numeric()) %>% 
  mutate(city = "Montreal",
         scraped = as.Date(date, tryFormats = c("%m/%d/%Y")),
         details = as.numeric(substr(details, 1, 1)),
         bedrooms = (details - 2),
         bedrooms = case_when(bedrooms <= 0 ~ 0, TRUE ~ bedrooms),
         created = scraped - posted,
         location = address,
         short_long = NA,
         bathrooms = NA,
         furnished = NA,
         type = NA,
         text = NA,
         photos = map(1:n(), ~NA),
         kj = TRUE) %>% 
  select(id, short_long, created, scraped, price, city, location, bedrooms, 
         bathrooms, furnished, type, lat, lon, title, kj, text, photos)

rclalq_with_geom <- 
  rclalq %>% 
  filter(!is.na(lon), !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

rclalq_without_geom <-
  rclalq %>%
  filter(is.na(lon) | is.na(lat)) %>% 
  mutate(geometry = st_sfc(st_point())) %>% 
  st_as_sf(crs = 4326) %>% 
  select(-lon, -lat)

rclalq <-
  rbind(rclalq_with_geom, rclalq_without_geom) %>% 
  st_as_sf() %>% 
  arrange(scraped, id)

rm(rclalq_with_geom, rclalq_without_geom)


# Merge KJ with RCLALQ ----------------------------------------------------

kj <-
  rclalq %>% 
  anti_join(st_drop_geometry(kj), by = "id") %>% 
  rbind(kj)

kj <-
  kj %>% 
  left_join(st_drop_geometry(rclalq)[,c("id", "bedrooms", "price")], 
            by = "id") %>% 
  mutate(bedrooms = if_else(is.na(bedrooms.x), bedrooms.y, bedrooms.x),
         price = if_else(is.na(price.x), price.y, price.x)) %>% 
  select(-c(price.x, price.y, bedrooms.x, bedrooms.y)) %>% 
    select(id:scraped, price, city, location, bedrooms, bathrooms:geometry)

rm(rclalq)


# Rbind into one table ----------------------------------------------------

ltr <- rbind(kj, cl)

rm(kj, cl)


# Add geometry ------------------------------------------------------------

load("output/geometry.Rdata")

ltr <- st_transform(ltr, 32618)

ltr <- 
  ltr %>% 
  st_join(boroughs_raw) %>% 
  select(-dwellings)

ltr <-
  ltr %>% 
  st_join(DA) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  select(-CMA_UID, -population)

rm(boroughs, boroughs_raw, city, DA, province)


# Save output -------------------------------------------------------------

save(ltr, file = "output/ltr_raw.Rdata")
