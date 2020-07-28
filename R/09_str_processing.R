#### 09 STR PROCESSING #########################################################

#' This script updates the `str_processed.Rdata` object. The script is very 
#' time-consuming to run, and should be rerun when STR data has changed.

source("R/01_startup.R")
library(data.table)


# Load previous data ------------------------------------------------------

load("data/str_processed.Rdata")


# Calculate multilistings -------------------------------------------------

daily <- 
  daily %>% 
  strr_multi(host)


# Calculate FREH ----------------------------------------------------------

FREH <-
  daily %>%
  strr_FREH() %>%
  filter(FREH == TRUE) %>%
  select(-FREH)


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

upgo:::handler_upgo("Analyzing row")

with_progress({
  
  pb <- progressor(nrow(GH))
  
  GH <- 
    GH %>% 
    mutate(status = furrr::future_map2_chr(date, property_IDs, ~{
      pb()
      status <- unique(daily_GH[date == .x & property_ID %in% .y, status])
      status <- case_when(
        "R" %in% status ~ "R",
        "A" %in% status ~ "A",
        "B" %in% status ~ "B",
        TRUE ~ NA_character_
      )
    })) %>% 
    select(ghost_ID, date, status, host_ID:data, geometry)
  
})

rm(daily_GH, pb)


# Save output -------------------------------------------------------------

save(property, daily, host, FREH, GH, file = "data/str_processed.Rdata")
