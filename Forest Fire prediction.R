## load the necessary packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)
library(tidyr)
library(tidyselect)
## set the working directory
setwd("C:/Users/Baha/OneDrive/Documents/Welcome to Data Science Project")
## load the dataset into the program
forest <-read.csv("forestfires.csv")
## view the first 10 observations of the data
forest %>% 
  head(n=10)
## check the structure of the loaded dataset
str(forest) ## the dataset have 2 categorical variables

################ DATA PREPARATION ####################
## check for missing values in the dataset
colSums(is.na(forest)) ## the dataset has no missing values
# check and remove the duplicate records
#$ Check for duplicate rows
forest %>% duplicated() %>% sum()
# Remove duplicate records
forest <- forest %>% distinct()
# Confirm removal
cat("Number of records after removing duplicates:", nrow(forest), "\n")
## Handling outliers in numerical variables
# List of numerical variables excluding 'area'
numeric_vars <- c("X", "Y", "FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain")
# Function to cap outliers at 1st and 99th percentiles
cap_outliers <- function(x) {
  lower <- quantile(x, 0.01)
  upper <- quantile(x, 0.99)
  x <- ifelse(x < lower, lower, x)
  x <- ifelse(x > upper, upper, x)
  return(x)
}
# Apply capping to numerical variables
forest <- forest %>%
  mutate_at(vars(numeric_vars), cap_outliers)
## variable transformation
# Visualize the distribution of the target variable 'area'
ggplot(forest, aes(x = area)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  theme_minimal() +
  ggtitle("Distribution of Burned Area")
# Apply logarithmic transformation to 'area' to reduce skewness
forest <- forest %>%
  mutate(log_area = log(area + 1))  # Adding 1 to avoid log(0)
# Visualize the transformed 'log_area'
ggplot(forest, aes(x = log_area)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black") +
  theme_minimal() +
  ggtitle("Distribution of Log-Transformed Burned Area")
# convert the day and month variables into numeric
# Create dummy variables for 'month' and 'day'
dummy_model <- dummyVars(" ~ month + day", data = forest)
# Apply the dummy model to the dataset to create new dummy variables
dummy_data <- predict(dummy_model, newdata = forest)
# Convert dummy variables to data frame
dummy_data <- as.data.frame(dummy_data)
# Bind the dummy variables back to the original dataset, excluding the original 'month' and 'day' columns
forest <- cbind(forest %>% select(-month, -day), dummy_data)
# View the updated dataset
forest %>% 
  head(n=10)

################### DATA EXPLORATION ############################
# Compute correlations between numeric variables and 'area'
numeric_vars <- forest %>%
  select(FFMC, DMC, DC, ISI, temp, RH, wind, rain, area)  # Select numeric columns
# Compute correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)
# Visualize the correlation matrix
corrplot(cor_matrix, method = "circle", type = "lower", tl.cex = 0.8, tl.col = "black")
# Scatter plots to visualize relationships between independent variables and 'area'
# FFMC vs. area
ggplot(forest, aes(x = FFMC, y = area)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  ggtitle("FFMC vs Burned Area") +
  theme_minimal()
# Temp vs. area
ggplot(forest, aes(x = temp, y = area)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  ggtitle("Temperature vs Burned Area") +
  theme_minimal()
# Wind vs. area
ggplot(forest, aes(x = wind, y = area)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  ggtitle("Wind Speed vs Burned Area") +
  theme_minimal()
# Apply standardization (z-score normalization)
forest <- forest %>%
  mutate_at(vars(numeric_vars), scale)
forest %>% 
  head(n=10)

############## SAMPLING THE DATA ##############################
# Set seed for reproducibility
set.seed(123)
# Create a stratified sample by the "area" variable
trainIndex <- createDataPartition(forest$area, p = 0.7, 
                                  list = FALSE, 
                                  times = 1)
# Split the data into training and test sets (70% training, 30% test)
forest_train <- forest[trainIndex, ]
dplyr::glimpse(forest_train)
forest_test <- forest[-trainIndex, ]
dplyr::glimpse(forest_test)

############## MODEL BUILDING #################################
library(gbm)
library(gbm3)
library(e1071)
# Cross-validation for linear regression
train_control <- trainControl(method = "cv", number = 10)
cv_linear <- train(log(area + 1) ~ ., data = forest_train, method = "lm", trControl = train_control)
print(cv_linear)

# Random Forest with cross-validation
cv_rf <- train(log(area + 1) ~ ., data = forest_train, method = "rf", trControl = train_control)
print(cv_rf)

# Gradient Boosting Model (ensemble method)
gbm_model <- gbm(log(area + 1) ~ ., data = forest_train, 
                 distribution = "gaussian", n.trees = 100, interaction.depth = 3)
print(gbm_model)

# Cross-validation for SVM
cv_svm <- train(log(area + 1) ~ ., data = forest_train, 
                method = "svmRadial", trControl = trainControl(method = "cv", number = 10))
print(cv_svm)

################ MODEL EVALUATION ##############################
## linear regression
# Predict on the test set
lm_preds <- predict(cv_linear, newdata = forest_test)
lm_mse <- mean((lm_preds - forest_test$area)^2)
lm_rmse <- sqrt(lm_mse)

# Random Forest
# Predict on the test set
rf_preds <- predict(cv_rf, newdata = forest_test)
rf_mse <- mean((rf_preds - forest_test$area)^2)
rf_rmse <- sqrt(rf_mse)

# Gradient Boosting
# Predict on the test set
gbm_preds <- predict(gbm_model, newdata = forest_test, n.trees = 100)
gbm_mse <- mean((gbm_preds - forest_test$area)^2)
gbm_rmse <- sqrt(gbm_mse)

# SVM Model for Regression (SVR)
svm_model <- svm(log(area + 1) ~ ., data = forest_train, kernel = "radial")
# Predict on the test set
svm_preds <- predict(svm_model, newdata = forest_test)
# Support Vector Machine
svm_mse <- mean((exp(svm_preds) - 1 - forest_test$area)^2)
svm_rmse <- sqrt(svm_mse)

# Summarize results in a data frame
model_comparison <- data.frame(
  Model = c("Linear Regression", "Random Forest", "Gradient Boosting", "SVM"),
  MSE = c(lm_mse, rf_mse, gbm_mse, svm_mse),
  RMSE = c(lm_rmse, rf_rmse, gbm_rmse, svm_rmse)
)
print(model_comparison)
# R-squared for each model
lm_model <- lm(log(area + 1) ~ ., data = forest_train)
lm_r2 <- summary(lm_model)$r.squared
rf_r2 <- 1 - (rf_mse / var(forest_test$area))
gbm_r2 <- 1 - (gbm_mse / var(forest_test$area))
svm_r2 <- 1 - (svm_mse / var(forest_test$area))
# Add R-squared to the comparison
model_comparison$R2 <- c(lm_r2, rf_r2, gbm_r2, svm_r2)
print(model_comparison)

# Visualize model performance
ggplot(model_comparison, aes(x = Model, y = RMSE, fill = Model)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Model Comparison - RMSE", x = "Model", y = "RMSE")
