#### 08 STR LISTING MATCH ######################################################

#' This script produces the TKTK objects. The script is extremely time-consuming and 
#' memory-intensive to run, so it should only be rerun when image matching needs 
#' to be rebuilt from scratch. In addition, the script downloads hundreds of 
#' thousands of photos to a specified folder, so it requires approximately 50 GB 
#' of free storage space.

#' External dependencies:
#' - Access to the UPGo database
#' - Listings scraped from Kijiji and Craigslist with upgo::upgo_scrape_kj and
#'   upgo::upgo_scrape_cl

source("R/01_startup.R")


# Load previous data ------------------------------------------------------





# Load image match results ------------------------------------------------

# NOT REPRODUCIBLE: Load pre-computed set of matches
load("~/Documents/Academic/Code/image-recognition/data/mtl_ab_matches.Rdata")

# Clean up
mtl_ab_matches <- 
  mtl_ab_matches %>% 
  filter(confirmation == "match") %>% 
  select(x_name, y_name) %>% 
  mutate(across(c(x_name, y_name), str_extract, '(?<=ab.).*(?=.jpg)'))


# Identify groupings ------------------------------------------------------

# Convert to list of pairs
pair_list <- map2(mtl_ab_matches$x_name, mtl_ab_matches$y_name, ~c(.x, .y))
pair_list <- map(pair_list, list)

# Helper functions to merge lists
can_merge <- function(x, y) length(intersect(x, y)) > 0
merge_fun <- function(x, y) sort(union(x, y))
reduce_fun <- function(pairs) {
  Reduce(function(acc, curr) {
    curr_vec <- curr[[1]]
    to_merge_id_x <- Position(f = function(x) can_merge(x, curr_vec), acc)
    if (is.na(to_merge_id_x)) acc[[length(acc) + 1]] <- curr_vec else {
      acc[[to_merge_id_x]] <- merge_fun(acc[[to_merge_id_x]], curr_vec)
    }
    return(acc)
  }, pairs)
}

# Merge lists
groupings <- reduce_fun(pair_list)

rm(mtl_ab_changes, mtl_ab_matches, pair_list)


# Modify host_ID from groupings -------------------------------------------

host_IDs <- 
  groupings %>% 
  map(~{
    property %>% 
      filter(property_ID %in% .x) %>% 
      pull(host_ID) %>% 
      unique()
  })

# Need to repeat this several times for it to be stable
host_IDs <- host_IDs %>% map(list) %>% reduce_fun()
host_IDs <- host_IDs %>% map(list) %>% reduce_fun()
host_IDs <- host_IDs %>% map(list) %>% reduce_fun()

host_IDs <- map(host_IDs, sort)
host_IDs <- host_IDs[lengths(host_IDs) > 0]

host_change_table <- 
  map_dfr(host_IDs, ~tibble(host_ID = .x, new_host = .x[[1]]))

property <-
  property %>% 
  left_join(host_change_table) %>% 
  mutate(old_host = if_else(is.na(new_host), NA_character_, host_ID),
         host_ID = if_else(is.na(new_host), host_ID, new_host)) %>% 
  select(-new_host)

daily <-
  daily %>% 
  left_join(host_change_table) %>% 
  mutate(old_host = if_else(is.na(new_host), NA_character_, host_ID),
         host_ID = if_else(is.na(new_host), host_ID, new_host)) %>% 
  select(-new_host)

rm(host_change_table, host_IDs, can_merge, merge_fun, reduce_fun)


# Get matches -------------------------------------------------------------

matches <- 
  groupings %>% 
  map(~{
    property %>% 
      filter(property_ID %in% .x,
             listing_type =="Entire home/apt",
             scraped - created >= 7)
  })

matches <- matches[map_int(matches, nrow) > 0]
matches <- matches %>% map(arrange, created)

matches <- 
  matches %>%
  map(~{
    next_created <- c(.x$created, NA)
    # Drop the first element to shift all created dates up a row
    next_created <- next_created[2:length(next_created)] 
    .x %>%
      mutate(next_created = next_created) %>%
      filter(scraped < next_created | is.na(next_created)) 
  })

matches <- matches[map_int(matches, nrow) > 1]

rm(groupings)


# Collapse property_IDs ---------------------------------------------------

property_change_table <- 
  map_dfr(matches, 
          ~tibble(
            property_ID = .x$property_ID, 
            new_PID = 
              filter(.x, scraped - created == max(scraped - created)) %>% 
              slice(1) %>% 
              pull(property_ID),
            new_created = min(.x$created),
            new_scraped = max(.x$scraped)
          ))

daily <-
  daily %>% 
  left_join(property_change_table) %>% 
  mutate(old_PID = if_else(is.na(new_PID), NA_character_, property_ID),
         property_ID = if_else(is.na(new_PID), property_ID, new_PID)) %>% 
  select(-new_PID)

property_change_collapsed <-
  property_change_table %>% 
  group_by(new_PID, new_created, new_scraped) %>% 
  summarize(all_PIDs = list(property_ID))

property_to_delete <-
  property_change_table %>% 
  filter(property_ID != new_PID)

property <- 
  property %>% 
  left_join(property_change_collapsed, by = c("property_ID" = "new_PID")) %>% 
  filter(!property_ID %in% property_to_delete$property_ID) %>% 
  mutate(created = if_else(!is.na(new_created), new_created, created),
         scraped = if_else(!is.na(new_scraped), new_scraped, scraped)) %>% 
  select(-new_created, -new_scraped)

rm(matches, property_change_collapsed, property_change_table, 
   property_to_delete)


# Recalculate host table --------------------------------------------------

host <- strr_host(daily)
