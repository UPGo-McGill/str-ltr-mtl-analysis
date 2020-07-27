### load libraries and data ###########################################


library(tidyverse)
library(lubridate)
library(sf)
library(gt)
library(future)
library(ggplot2)
library(data.table)
library(caret)

Sys.setenv(LANG = "en")

memory.limit(size = 48000)
# plan(multiprocess, workers = 4)

load("data/montreal_str_processed_a.Rdata")
load("data/daily_status.Rdata")

rm(borough_geometries, boroughs, city, CTs, GH, key_date, end_date, exchange_rate, host, FREH_2020, daily)


### linear modelling FREH ############################################

# If you treat FREH status on a given day as the dependent variable, 
# and have R and A values for each of the last 12 months as the independent variables, 
# what is the model that best predicts FREH status? And then same story with fewer months of data.

# y = FREH at a given day
# x = R and A values for each of the last 12 months

# Two ways of treating FREH in the past 12 months: 
# Looking at which months (jan, feb, ...) there are R and A listings and look at the pattern of FREH.
# Looking at the last 12 months, look at 1st month, 2nd month, regardless of the month of the year.


### join tables together ######################################################

daily_FREH <- 
left_join(daily_status,
          (property %>% 
            st_drop_geometry() %>% 
            select(property_ID, created)), by = "property_ID")%>% 
            mutate(days_created = date - created)

daily_FREH <- 
left_join(daily_FREH,
          mutate(FREH, FREH = T), by = c("property_ID", "date")) %>%
  mutate(FREH = ifelse(is.na(FREH), F, FREH))


### take only the newest listing to estimate the newly created ones ##########
newer_listings <- 
left_join(
  (daily_FREH %>% 
  filter(days_created >= 30, days_created <= 365)
  ),
  (daily_FREH %>% 
    filter(days_created >= 30, days_created <= 365,
           property_ID %in% (daily_FREH %>% 
                               filter(days_created > 30, days_created < 365, FREH == T))$property_ID) %>% 
    mutate(FREH_later = T) %>% 
     select(property_ID, date, FREH_later)
   ),
  by = c("property_ID", "date")) %>% 
  mutate(FREH_later = ifelse(is.na(FREH_later), F, FREH_later)) %>% 
  mutate(days_created = as.numeric(days_created),
         days_old_group = 0,
         days_old_group = ifelse(days_created >= 30, 30, days_old_group),
         days_old_group = ifelse(days_created >= 60, 60, days_old_group),
         days_old_group = ifelse(days_created >= 90, 90, days_old_group),
         days_old_group = ifelse(days_created >= 120, 120, days_old_group),
         days_old_group = ifelse(days_created >= 150, 150, days_old_group),
         days_old_group = ifelse(days_created >= 180, 180, days_old_group),
         days_old_group = ifelse(days_created >= 210, 210, days_old_group),
         days_old_group = ifelse(days_created >= 240, 240, days_old_group),
         days_old_group = ifelse(days_created >= 270, 270, days_old_group),
         days_old_group = ifelse(days_created >= 300, 300, days_old_group),
         days_old_group = ifelse(days_created >= 330, 330, days_old_group),
         days_old_group = ifelse(days_created >= 360, 360, days_old_group)
         ) %>% 
  filter(days_created == days_old_group) %>% 
  mutate(days_old_group = as.factor(days_old_group),
         created_month = as.factor(lubridate::month(created)))
  


### train the model and see if it works ##################
# # Split the data into training and test set
# training.samples <- newer_listings$FREH_later %>%
#   createDataPartition(p = 0.80, list = FALSE)
# train.data  <- newer_listings[training.samples, ]
# test.data <- newer_listings[-training.samples, ]
# 
# 
# 
# 
# # train.data %>%
# #   filter(days_old_group == 90) %>%
# #   mutate(prob = ifelse(FREH_later == T, 1, 0)) %>%
# #   ggplot(aes(status_R, prob)) +
# #   geom_point(alpha = 0.2) +
# #   geom_smooth(method = "glm",
# #               method.args = list(family = "binomial")) +
# #   labs(
# #     title = "Linear Model (90 days old listings)",
# #     x = "Status_R",
# #     y = "Probability of being FREH inside of year after creation"
# #   )
# 
# 
# 
# 
# # Fit the model
# model <- glm(FREH_later ~ status_R + status_A_R + created_month + days_old_group, data = train.data, family = binomial)
# 
# 
# # used to test
# probabilities <- model %>% predict(test.data, type = "response")
# head(probabilities)
# contrasts(test.data$FREH_later)
# predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
# mean(predicted.classes == test.data$FREH_later)





### Fit the model ################################################
model <- glm(FREH_later ~ status_R + status_A_R + created_month + days_old_group, data = newer_listings, family = binomial)


daily_pred <- 
daily_status %>% 
  left_join( 
  (property %>% 
     st_drop_geometry() %>% 
     select(property_ID, created)), by = "property_ID") %>% 
  mutate(days_created = date - created) %>% 
  mutate(days_created = as.numeric(days_created),
         days_old_group = 0,
         days_old_group = ifelse(days_created >= 30, 30, days_old_group),
         days_old_group = ifelse(days_created >= 60, 60, days_old_group),
         days_old_group = ifelse(days_created >= 90, 90, days_old_group),
         days_old_group = ifelse(days_created >= 120, 120, days_old_group),
         days_old_group = ifelse(days_created >= 150, 150, days_old_group),
         days_old_group = ifelse(days_created >= 180, 180, days_old_group),
         days_old_group = ifelse(days_created >= 210, 210, days_old_group),
         days_old_group = ifelse(days_created >= 240, 240, days_old_group),
         days_old_group = ifelse(days_created >= 270, 270, days_old_group),
         days_old_group = ifelse(days_created >= 300, 300, days_old_group),
         days_old_group = ifelse(days_created >= 330, 330, days_old_group),
         days_old_group = ifelse(days_created >= 360, 360, days_old_group)
  ) %>% 
  filter(days_created == days_old_group,
         days_old_group != 0) %>% 
  mutate(days_old_group = as.factor(days_old_group),
         created_month = as.factor(lubridate::month(created))) %>% 
    arrange(desc(date)) %>% 
  modelr::add_predictions(model, type = "response") %>% 
  mutate(FREH_later = ifelse(pred > 0.5, T, F))
    



rbind( 
daily_pred %>% 
  filter(date >= "2020-01-01", date < "2020-02-01",
         FREH_later == T) %>% 
  distinct(property_ID),
FREH %>% 
  filter(date >= "2020-01-01", date < "2020-02-01") %>% 
  distinct(property_ID)
) %>% 
  distinct(property_ID)



daily_pred %>% 
  filter(date >= "2020-01-01", date < "2020-02-01",
         FREH_later == T) %>% 
  distinct(property_ID, .keep_all = T) %>% 
  View()

save(daily_pred, file = "data/daily_pred.Rdata")


