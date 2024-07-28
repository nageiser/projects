# Load necessary libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(ranger)
library(xgboost)
library(ggplot2)

# Load and inspect the training and testing datasets
df_train <- read.csv("df_train.csv")
df_test <- read.csv("df_test.csv")

## Explore the structure of the dataset
glimpse(df_train)
glimpse(df_test)

fmla <- as.formula("power_consumption ~ semester + quarter + day_in_week + week_in_year + day_in_year + month")

# Function to format test and train datasets and one-hot encode categorical variables for the
# gradient-boosted tree

format <- function(df) {
  df <- df %>%
    select(-year) %>%
    mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
    mutate(across(c(semester, quarter, day_in_week, week_in_year, day_in_year, month), as.factor))
  encoded_df <- as.data.frame(model.matrix(fmla, data = df))
  df <- df %>%
    bind_cols(encoded_df) %>%
    return(df)
}

df_test <- format(df_test)
df_train <- format(df_train)

df_train_features <- df_train %>%
  select(-c(date, power_consumption, semester, quarter, day_in_week, week_in_year, day_in_year, month))

df_test_features <- df_test %>%
  select(-c(date, power_consumption, semester, quarter, day_in_week, week_in_year, day_in_year, month))

outcome_train <- df_train["power_consumption"]
outcome_test <- df_test["power_consumption"]

# Train the models and get predictions on the test dataset

# Baseline linear model

model.lm <- lm(fmla, data = df_train)
summary(model.lm)

# Random forest model

set.seed(1221)
model.rf <- ranger(formula = fmla,
                   data = df_train,
                   mtry = 1,
                   num.trees = 100)

df_test$pred.lm <- predict(model.lm, newdata = df_test)
df_test$pred.rf <- predict(model.rf, df_test)$predictions

# Get RMSE for the linear model and random forest model

df_test %>%
  mutate(residual = pred.lm - power_consumption) %>%
  summarize(rmse.lm = sqrt(mean(residual^2)))

df_test %>%
  mutate(residual = pred.rf - power_consumption) %>%
  summarize(rmse.rf = sqrt(mean(residual^2)))

# The test and train datasets for this project do not have the same number of columns/features, and xgboost doesn't
# automatically correct for these differences. I therefore used only common features for both datasets for the
# gradient-boosted tree

set.seed(126)
cross_val <- xgb.cv(data = as.matrix(df_train_features),
                    label = as.matrix(outcome_train),
                    objective = "reg:squarederror",
                    nrounds = 100,
                    nfold = 5,
                    eta = 0.3,
                    max_depth = 5
)

eval_log <- cross_val$evaluation_log

eval_log %>%
  summarize(ntrees.train = which.min(train_rmse_mean),
            ntrees.test = which.min(test_rmse_mean))

intersect <- intersect(colnames(df_test_features), colnames(df_train_features))

df_train_features <- subset(df_train_features, select = intersect)
df_test_features <- subset(df_test_features, select = intersect)

set.seed(123)
model.xgb <- xgboost(data = as.matrix(df_train_features),
                     label = as.matrix(outcome_train),
                     nrounds = 11,
                     objective = "reg:squarederror",
                     eta = 0.3,
                     max_depth = 5,
                     verbose = FALSE
)

df_test$pred.xgb <- predict(model.xgb, newdata = as.matrix(df_test_features))
df_test %>%
  mutate(residuals = pred.xgb - power_consumption) %>%
  summarize(rmse = sqrt(mean(residuals^2)))

## The random forest model has the lowest RMSE of all three:

selected_rmse <- df_test %>%
  mutate(residual = pred.rf - power_consumption) %>%
  summarize(rmse.rf = sqrt(mean(residual^2))) %>%
  pull()

## Compare models in plot

df_test %>%
  pivot_longer(cols = c(pred.xgb, pred.lm, pred.rf), names_to = "model", values_to = "predicted") %>%
  ggplot() +
  geom_line(aes(x = date, y = power_consumption), alpha = 0.5) +
  geom_line(aes(x = date, y = predicted, color = model)) +
  labs(x = "Date", y = "Power Consumption (kwH)") +
  theme_minimal() +
  ggtitle("Power Consumption Over Time")
