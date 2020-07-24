### load libraries and data ###########################################


library(tidyverse)
library(lubridate)
library(sf)
library(gt)
library(future)
library(ggplot2)
library(data.table)
library(caret)

library(future.apply)

library(strr)
library(upgo)

Sys.setenv(LANG = "en")

memory.limit(size = 48000)
# plan(multiprocess, workers = 4)

# load("data/montreal_str_processed_a.Rdata")

# save(daily, file = "data/daily.Rdata")

load("data/daily.Rdata")

### linear modelling FREH ############################################

# If you treat FREH status on a given day as the dependent variable, 
# and have R and A values for each of the last 12 months as the independent variables, 
# what is the model that best predicts FREH status? And then same story with fewer months of data.

# y = FREH at a given day
# x = R and A values for each of the last 12 months

# Two ways of treating FREH in the past 12 months: 
# Looking at which months (jan, feb, ...) there are R and A listings and look at the pattern of FREH.
# Looking at the last 12 months, look at 1st month, 2nd month, regardless of the month of the year.




# try to do something with how the FREH function is written


test_status <- function(daily, start_date = NULL, end_date = NULL,
                      property_ID = property_ID, date = date, status = status,
                      status_types = c("R", "A"), listing_type = listing_type,
                      entire_home = "Entire home/apt", quiet = FALSE) {
  
  
  ### ERROR CHECKING AND ARGUMENT INITIALIZATION ###############################
  
  start_time <- Sys.time()
  
  
  ## Input checking ------------------------------------------------------------
  
  strr:::helper_check_daily(rlang::ensyms(property_ID, date, status))
  strr:::helper_check_quiet()

  
  # stopifnot(exprs = {
  #   length(status_types) == 2
  #   n_days > 0
  #   r_cut > 0 && r_cut <= n_days
  #   ar_cut > 0 && ar_cut <= n_days && ar_cut >= r_cut
  # })
  
  # Check that status_types arguments are plausible
  if (nrow(dplyr::filter(daily, {{status}} == status_types[1])) == 0) {
    warning(paste0("The first supplied argument to `status_types` returns no ",
                   "matches in the input table. Are you sure the argument ",
                   "is correct?"))
  }
  
  if (nrow(dplyr::filter(daily, {{status}} == status_types[2])) == 0) {
    warning(paste0("The second supplied argument to `status_types` returns no ",
                   "matches in the input table. Are you sure the argument ",
                   "is correct?"))
  }
  
  # Set lt_flag and check validity of listing_type
  lt_flag <-
    tryCatch({
      # If listing_type is a field in points, set lt_flag = TRUE
      dplyr::pull(daily, {{listing_type}})
      TRUE},
      error = function(e) {tryCatch({
        # If listing_type == FALSE, set lt_flag = FALSE
        if (!listing_type) { FALSE
        } else stop("`listing_type` must be a valid field name or FALSE.")
      },
      error = function(e2) {
        # Otherwise, fail with an informative error
        stop("`listing_type` must be a valid field name or FALSE.")
      }
      )})
  
  # Check entire_home argument
  if (lt_flag) {
    if (nrow(dplyr::filter(daily, {{listing_type}} == entire_home)) == 0) {
      warning(paste0("The supplied argument to `entire_home` returns no ",
                     "matches in the input table. Are you sure the argument ",
                     "is correct?"))
    }
  }
  
  
  ## Prepare data.table options ------------------------------------------------
  
  # Silence R CMD check for data.table fields
  R <- AR <- NULL
  
  # Make sure data.table is single-threaded within the helper
  threads <- data.table::setDTthreads(1)
  
  # Set up on.exit expression for errors
  on.exit({
    
    # Restore data.table threads
    data.table::setDTthreads(threads)
  })
  
  
  ### PREPARE TABLE FOR ANALYSIS ###############################################
  
  ## Drop geometry if table is sf ----------------------------------------------
  
  if (inherits(daily, "sf")) {
    daily <- sf::st_drop_geometry(daily)
  }
  
  
  ## Wrangle dates -------------------------------------------------------------
  
  if (missing(start_date)) {
    start_date <- min(dplyr::pull(daily, {{date}}), na.rm = TRUE)
  } else {
    start_date <- tryCatch(as.Date(start_date), error = function(e) {
      stop(paste0('The value of `start_date`` ("', start_date,
                  '") is not coercible to a date.'))
    })}
  
  if (missing(end_date)) {
    end_date <- max(dplyr::pull(daily, {{date}}), na.rm = TRUE)
  } else {
    end_date <- tryCatch(as.Date(end_date), error = function(e) {
      stop(paste0('The value of `end_date` ("', end_date,
                  '") is not coercible to a date.'))
    })}
  
  
  ## Rename fields to make data.table functions work ---------------------------
  
  daily <- dplyr::rename(daily,
                         property_ID = {{property_ID}},
                         date = {{date}},
                         status = {{status}})
  
  
  ## Filter daily file and select necessary columns ----------------------------
  
  data.table::setDT(daily)
  
  if (lt_flag) {
    
    daily <- dplyr::rename(daily, listing_type = {{listing_type}})
    daily <- daily[listing_type == entire_home]
    
  }
  
  daily <-
    daily[#status %in% c("A", "R") & 
            date >= start_date - 364 & date <= end_date]
  
  # Only select needed fields, to reduce object size for remote transfer
  daily[, setdiff(names(daily), c("property_ID", "date", "status")) := NULL]
  
  
  ### PERFORM CALCULATIONS #####################################################
  
  strr:::helper_message("(1/2) Analyzing data, using ",  strr:::helper_plan(), ".")
  
  
  ## Function to be iterated over ----------------------------------------------
  
  date_fun <- function(.x, ...) {
    .strr_env$pb()
    daily <- daily[date >= .x - 364 & date <= .x]
    daily[, R := sum(status == "R"), by = "property_ID"]
    daily[, AR := (sum(status == "R") + sum(status == "A")), by = "property_ID"]
    daily[, list(date = as.Date(.x, origin = "1970-01-01"),
                 status_R = mean(R),
                 status_A_R = mean(AR)),
          by = "property_ID"]
  }
  
  
  ## Run function --------------------------------------------------------------
  
  strr:::handler_strr("Analyzing date")
  
  strr:::with_progress({
    
    # Initialize progress bar
    .strr_env$pb <-  strr:::progressor(along = start_date:end_date)
    
    daily <-
      strr:::par_lapply(start_date:end_date, date_fun, future.globals =
                   c("start_date", "end_date", "daily"))
    
    daily <- data.table::rbindlist(daily)
  })
  
  
  ### ARRANGE TABLE THEN RENAME FIELDS TO MATCH INPUT FIELDS ###################
  
  strr:::helper_message("(2/2) Arranging output.", .type = "open")
  
  data.table::setorderv(daily, c("property_ID", "date"))
  
  daily <- dplyr::rename(daily,
                         {{property_ID}} := .data$property_ID,
                         {{date}} := .data$date)
  
  daily <- dplyr::as_tibble(daily)
  
  strr:::helper_message("(2/2) Output arranged.", .type = "close")
  
  
  ### RETURN OUTPUT ############################################################
  
  strr:::helper_message("Analysis complete.", .type = "final")
  
  return(daily)
  
}

daily_status <- 
  daily %>%
  test_status("2018-01-01", max(daily$date))




save(daily_status, file = "data/daily_status.Rdata")


