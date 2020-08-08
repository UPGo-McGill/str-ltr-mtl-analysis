#### 09 STR PROCESSING #########################################################

#' This script is very time-consuming to run, and should be rerun when STR data 
#' has changed.
#' 
#' Output:
#' - `str_processed.Rdata` (updated)
#' 
#' Script dependencies:
#' - `08_str_listing_match.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")
library(foreach)
doParallel::registerDoParallel()


# Load previous data ------------------------------------------------------

load("output/str_processed.Rdata")


# Calculate multilistings -------------------------------------------------

daily <- 
  daily %>% 
  strr_multi(host) %>% 
  as_tibble()


# Calculate ghost hostels -------------------------------------------------

GH <- 
  property %>% 
  strr_ghost(start_date = "2016-01-01", end_date = max(daily$date))

# Add daily status to GH
daily_GH <- 
  daily %>% 
  filter(property_ID %in% unique(unlist(GH$property_IDs)))

setDT(daily_GH)

daily_GH <- daily_GH %>% select(property_ID:status)

status_fun <- function(x, y) {
  status <- unique(daily_GH[date == x & property_ID %in% y, status])
  fcase("R" %in% status, "R", "A" %in% status, "A", "B" %in% status, "B")
}

upgo:::handler_upgo("Analyzing row")

with_progress({
  
  pb <- progressor(nrow(GH))

  status <- foreach(i = 1:nrow(GH), .combine = "c") %dopar% {
    pb()
    status_fun(GH$date[[i]], GH$property_IDs[[i]])
  }

})

GH$status <- status
GH <- GH %>% select(ghost_ID, date, status, host_ID:data, geometry)

rm(daily_GH, pb, status_fun, status)


# Save output -------------------------------------------------------------

save(property, daily, GH, file = "output/str_processed.Rdata")
