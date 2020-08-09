#### 02 GEOMETRY IMPORT ########################################################

#' This script should only be rerun when geometry needs to be rebuilt from 
#' scratch.
#' 
#' Output:
#' - `geometry.Rdata`
#' 
#' Script dependencies:
#' - None
#' 
#' External dependencies:
#' - `montreal_boroughs_2019.shp`: Shapefile of Montreal borough boundaries

source("R/01_startup.R")
library(cancensus)


# Quebec province ---------------------------------------------------------

province <- 
  get_census("CA16", regions = list(PR = "24"), geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(geometry)


# Montreal DAs ------------------------------------------------------------

DA <-
  get_census(
    dataset = "CA16", regions = list(CSD = "2466023"), level = "DA",
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(GeoUID, Dwellings) %>% 
  set_names(c("GeoUID", "dwellings", "geometry")) %>% 
  st_set_agr("constant")


# Montreal boroughs -------------------------------------------------------

boroughs_raw <-
  read_sf("data/shapefiles/montreal_boroughs_2019.shp") %>% 
  filter(TYPE == "Arrondissement") %>% 
  select(borough = NOM) %>% 
  st_set_agr("constant") %>% 
  st_transform(32618) 

boroughs <- 
  boroughs_raw %>% 
  st_intersection(province)

boroughs <- 
  DA %>% 
  select(dwellings) %>% 
  st_interpolate_aw(boroughs, extensive = TRUE) %>% 
  st_drop_geometry() %>% 
  select(dwellings) %>% 
  cbind(boroughs, .) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  arrange(borough)


# Montreal CSD ------------------------------------------------------------

city <-
  boroughs_raw %>% 
  st_combine() %>% 
  st_union() %>% 
  st_cast("POLYGON") %>% 
  st_union() %>% plot()
  smoothr::fill_holes(400)


# Save output -------------------------------------------------------------

save(province, DA, boroughs, boroughs_raw, city, file = "output/geometry.Rdata")
