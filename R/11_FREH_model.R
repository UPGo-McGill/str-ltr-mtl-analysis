#### New FREH modelling attempt ################################################

source("R/01_startup.R")
library(caret)
library(slider)


# Load data ---------------------------------------------------------------

load("output/str_processed.Rdata")


# Wrangle data ------------------------------------------------------------

FREH <- 
  FREH %>% 
  mutate(FREH = TRUE)

year_long <- 
  daily %>% 
  filter(date <= "2019-12-31") %>% 
  filter(listing_type == "Entire home/apt") %>% 
  select(property_ID, date, status) %>% 
  group_by(property_ID) %>% 
  filter(n() >= 395) %>% 
  ungroup() %>% 
  left_join(FREH) %>% 
  mutate(FREH = if_else(is.na(FREH), FALSE, FREH),
         year = as.numeric(substr(date, 1, 4)),
         month = as.numeric(substr(date, 6, 7))) %>% 
  left_join(select(property, property_ID, created)) %>% 
  # Trim listings to the start of the month--can later test if the extra
  # complexity produces meaningful model improvement
  mutate(created = if_else(substr(created, 9, 10) == "01", created,
                           floor_date(created, "month") %m+% months(1))) %>% 
  filter(date >= created) %>% 
  mutate(created_year = as.numeric(substr(created, 1, 4)),
         created_month = as.numeric(substr(created, 6, 7)),
         month_since_created = (year - created_year) * 12 + 
           (month - created_month))

# Just take exactly one year of data
one_year <- 
  year_long %>% 
  filter(date < created %m+% months(12))
  
# Summarize data by month
year_month <- 
  one_year %>% 
  group_by(property_ID, year, month) %>% 
  summarize(month_since_created = first(month_since_created),
            month_R = sum(status == "R"),
            month_AR = sum(status %in% c("R", "A"))) %>% 
  ungroup()

ever_FREH <- 
  one_year %>% 
  group_by(property_ID) %>% 
  summarize(FREH = as.logical(ceiling(sum(FREH))))

year_month <- 
  year_month %>% 
  left_join(ever_FREH) %>% 
  group_by(property_ID) %>% 
  mutate(month = month.name[.data$month],
         cum_R = cumsum(month_R),
         cum_AR = cumsum(month_AR))
  

# Fit model ---------------------------------------------------------------

model <- glm(FREH ~ cum_R + cum_AR + month_since_created + month, 
             data = year_month, family = binomial)


# Model testing -----------------------------------------------------------

# Split the data into training and test set
training_samples <- 
  year_month$FREH %>% createDataPartition(p = 0.80, list = FALSE)

train_data  <- year_month[training_samples, ]
test_data <- year_month[-training_samples, ]

# Fit the model
model <- glm(FREH ~ cum_R + cum_AR + month_since_created + month, 
             data = train_data, family = binomial)

# Test model
probabilities <- model %>% predict(test_data, type = "response")
predicted_classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted_classes == test_data$FREH)




# Model based on last three months ----------------------------------------

# Summarize by month
after_one_year_month <- 
  year_long %>% 
  group_by(property_ID, year, month) %>% 
  summarize(month_since_created = first(month_since_created),
            month_R = sum(status == "R"),
            month_AR = sum(status %in% c("R", "A")),
            FREH = as.logical(ceiling(sum(FREH)))) %>% 
  ungroup() %>%
  group_by(property_ID) %>% 
  mutate(month = month.name[.data$month],
         cum_R = slide_int(month_R, sum, .before = 2, .complete = TRUE),
         cum_AR = slide_int(month_AR, sum, .before = 2, .complete = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(cum_R), !is.na(cum_AR), month_since_created >= 12)


# Fit model ---------------------------------------------------------------

model_after <- glm(FREH ~ cum_R + cum_AR + month, data = after_one_year_month, 
             family = binomial)


# Model testing -----------------------------------------------------------

# Split the data into training and test set
training_samples_after <- 
  after_one_year_month$FREH %>% createDataPartition(p = 0.80, list = FALSE)

train_data_after <- after_one_year_month[training_samples_after, ]
test_data_after <- after_one_year_month[-training_samples_after, ]

# Fit the model
model_after <- glm(FREH ~ cum_R + cum_AR + month, data = train_data_after, 
             family = binomial)

# Test model
probabilities_after <- 
  model_after %>% predict(test_data_after, type = "response")
predicted_classes_after <- ifelse(probabilities_after > 0.5, "TRUE", "FALSE")
mean(predicted_classes_after == test_data_after$FREH)
