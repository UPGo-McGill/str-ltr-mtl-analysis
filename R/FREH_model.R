#### New FREH modelling attempt ################################################

source("R/01_startup.R")
library(caret)


# Wrangle data ------------------------------------------------------------

FREH <- 
  daily %>% 
  strr_FREH() %>% 
  filter(FREH == TRUE)

year_long <- 
  daily %>% 
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
  left_join(ever_FREH)

year_month <- 
  year_month %>% 
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



