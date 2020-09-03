#### 08 STR LISTING MATCH ######################################################

#' This script is moderately time-consuming to run, and should be rerun when STR 
#' data or image data has changed.
#' 
#' Output:
#' - `str_processed.Rdata` (updated)
#' - `ltr_processed.Rdata` (updated)
#' 
#' Script dependencies:
#' - `07_ltr_listing_match.R`
#' 
#' External dependencies:
#' - None

source("R/01_startup.R")


# Load previous data ------------------------------------------------------

load("output/str_processed.Rdata")
load("output/ltr_processed.Rdata")
load("output/matches_processed.Rdata")
dl_location <- "/Volumes/Data/Scrape photos/mtl"


# Clean up ab_matches -----------------------------------------------------

ab_matches <-
  ab_matches %>% 
  filter(confirmation == "match") %>%
  mutate(
    x_name = str_replace_all(x_name, paste0(dl_location, "/ab/|.jpg"), ""),
    y_name = str_replace_all(y_name, paste0(dl_location, "/ab/|.jpg"), "")
  ) %>% 
  select(x_name, y_name)

rm(dl_location, matches)


# Identify groupings ------------------------------------------------------

# Convert to list of pairs
pair_list <- pmap(ab_matches, c)
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

rm(ab_matches, pair_list)


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

# Get final activity date
property <- 
  daily %>% 
  filter(status != "B") %>% 
  group_by(property_ID) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  select(property_ID, active = date) %>% 
  left_join(property, .) %>% 
  select(property_ID:scraped, active, housing:ltr_ID, old_host, geometry)

group_matches <- 
  groupings %>% 
  map(~{
    property %>% 
      filter(property_ID %in% .x, listing_type == "Entire home/apt") %>% 
      mutate(active = if_else(is.na(active), created, active)) %>% 
      filter(active >= created)
  })

group_matches <- group_matches[map_int(group_matches, nrow) > 0]
group_matches <- group_matches %>% map(arrange, created)

group_matches <- 
  group_matches %>%
  map(~{
    next_created <- c(.x$created, NA)
    # Drop the first element to shift all created dates up a row
    next_created <- next_created[2:length(next_created)] 
    .x %>%
      mutate(next_created = next_created) %>%
      filter(active < next_created | is.na(next_created)) 
  })

group_matches <- group_matches[map_int(group_matches, nrow) > 1]

rm(groupings)


# Collapse property_IDs ---------------------------------------------------

property_change_table <- 
  map_dfr(group_matches, 
          ~tibble(
            property_ID = .x$property_ID, 
            new_PID = 
              filter(.x, active - created == max(active - created)) %>% 
              slice(1) %>% 
              pull(property_ID),
            new_created = min(.x$created, na.rm = TRUE),
            new_scraped = max(.x$scraped, na.rm = TRUE),
            new_active = max(.x$active, na.rm = TRUE),
            new_ltr_IDs = list(unique(unlist(.x$ltr_ID)))
          ))

daily <-
  daily %>% 
  left_join(property_change_table) %>% 
  mutate(old_PID = if_else(is.na(new_PID), NA_character_, property_ID),
         property_ID = if_else(is.na(new_PID), property_ID, new_PID)) %>% 
  select(-new_PID, -new_created, -new_scraped, -new_active)

property_change_collapsed <-
  property_change_table %>% 
  group_by(new_PID, new_created, new_scraped, new_active) %>% 
  summarize(all_PIDs = list(property_ID),
            new_ltr_ID = list(unique(unlist(new_ltr_IDs))))

property_to_delete <-
  property_change_table %>% 
  filter(property_ID != new_PID)

property <-
  property %>% 
  left_join(property_change_collapsed, by = c("property_ID" = "new_PID")) %>% 
  filter(!property_ID %in% property_to_delete$property_ID) %>% 
  mutate(created = if_else(!is.na(new_created), new_created, created),
         scraped = if_else(!is.na(new_scraped), new_scraped, scraped),
         active = if_else(!is.na(new_active), new_active, active),
         ltr_ID = map2(ltr_ID, new_ltr_ID, ~{if (is.null(.y)) .x else .y}),
         ltr_ID = map(ltr_ID, unique)) %>% 
  select(-new_created, -new_scraped, -new_active, -new_ltr_ID) %>% 
  select(-geometry, everything(), geometry)

rm(group_matches, property_change_collapsed, property_change_table, 
   property_to_delete)


# Trim LTR data -----------------------------------------------------------

property_map <- 
  property %>% 
  st_drop_geometry() %>% 
  select(property_ID, all_PIDs) %>% 
  unnest(all_PIDs)

ltr <- 
  ltr %>% 
  mutate(property_ID = map(property_ID, ~{
    tibble(all_PIDs = .x) %>% 
      left_join(property_map, by = "all_PIDs") %>% 
      mutate(property_ID = if_else(is.na(property_ID), all_PIDs, 
                                   property_ID)) %>% 
      pull(property_ID) %>% 
      unique()
  }))


# Save output -------------------------------------------------------------

save(property, daily, host, file = "output/str_processed.Rdata")
save(ltr, file = "output/ltr_processed.Rdata")
