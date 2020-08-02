#### 02 GEOMETRY IMPORT ########################################################

#' This script produces the `geometry.Rdata` object. It should only be rerun 
#' when geometry needs to be rebuilt from scratch.

#' External dependencies:
#' - `montreal_boroughs_2019.shp`: Shapefile of Montreal borough boundaries

source("R/01_startup.R")


# Montreal DAs ------------------------------------------------------------

DA <-
  cancensus::get_census(
    dataset = "CA16", regions = list(CSD = "2466023"), level = "DA",
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  select(GeoUID, CSD_UID, Population, Dwellings) %>% 
  set_names(c("GeoUID", "CMA_UID", "population", "dwellings", "geometry")) %>% 
  st_set_agr("constant")


# Montreal boroughs -------------------------------------------------------

boroughs <-
  read_sf("data/shapefiles/montreal_boroughs_2019.shp") %>% 
  filter(TYPE == "Arrondissement") %>% 
  select(borough = NOM) %>% 
  st_transform(32618) %>%
  st_join(DA) %>%
  group_by(borough) %>%
  summarize(dwellings = sum(dwellings))


# Montreal CSD ------------------------------------------------------------

city <-
  boroughs %>% 
  st_combine() %>% 
  st_union() %>% 
  st_cast("POLYGON") %>% 
  st_union() %>% 
  smoothr::fill_holes(400)


# Save output -------------------------------------------------------------

save(DA, boroughs, city, file = "output/geometry.Rdata")
