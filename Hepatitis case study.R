#loading the required packages
library(tidyverse)
library(caret)   # for pre-processing functions
library(mlr)    # for one-hot encoding
#set the working directory
setwd("C:/Users/Baha/Downloads")
hepatitis_data <-read.csv("C:/Users/Baha/Downloads/malik30993-attachments/midterm dataset hepatitis.csv", stringsAsFactors = FALSE)
#replace '?' with NA in the dataset
hepatitis_data[hepatitis_data=="?"] <-NA
# 1) Handling Missing Values
# Inspect the dataset for missing values
colSums(is.na(hepatitis_data))
# Exclude rows with NA values
hepatitis_data<- na.omit(hepatitis_data)
# Inspect the cleaned data
colSums(is.na(hepatitis_data))
# 2)  Convert categorical variables to factors
hepatitis_data$class <- as.factor(hepatitis_data$class)
hepatitis_data$sex <- as.factor(hepatitis_data$sex)
hepatitis_data$steroid <- as.factor(hepatitis_data$steroid)
hepatitis_data$antiviral <- as.factor(hepatitis_data$antiviral)
hepatitis_data$fatigue <- as.factor(hepatitis_data$fatigue)
hepatitis_data$malaise <- as.factor(hepatitis_data$malaise)
hepatitis_data$anorexia <- as.factor(hepatitis_data$anorexia)
hepatitis_data$liver.big <- as.factor(hepatitis_data$liver.big)
hepatitis_data$liver.firm <- as.factor(hepatitis_data$liver.firm)
hepatitis_data$spleen <- as.factor(hepatitis_data$spleen)
hepatitis_data$spiders <- as.factor(hepatitis_data$spiders)
hepatitis_data$ascites <- as.factor(hepatitis_data$ascites)
hepatitis_data$varices <- as.factor(hepatitis_data$varices)
hepatitis_data$histology <- as.factor(hepatitis_data$histology)

# 3)  convert the character variables into integer/numeric data type
hepatitis_data$bilirubin <-as.integer(hepatitis_data$bilirubin)
hepatitis_data$phosphate <- as.integer(hepatitis_data$phosphate)
hepatitis_data$sgot <- as.integer(hepatitis_data$sgot)
hepatitis_data$albumin <- as.integer(hepatitis_data$albumin)
hepatitis_data$protime <- as.integer(hepatitis_data$protime)
#check the structure of the pre-processed data
str(hepatitis_data)

####exploratory data analysis
#univariate analysis; continuous variables
continuous_variable <- c("age", "bilirubin", "phosphate", "sgot", "albumin", "protime")
#age
ggplot(hepatitis_data, aes(x = age)) +
    geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
    theme_minimal() +
    labs(title = paste("Distribution of", "age")) +
    theme(plot.title = element_text(hjust = 0.5))
#protime
ggplot(hepatitis_data, aes(x = protime)) +
  geom_histogram(bins = 30, fill = "grey", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = paste("Distribution of", "protime")) +
  theme(plot.title = element_text(hjust = 0.5))
#phosphate
ggplot(hepatitis_data, aes(x = phosphate)) +
  geom_histogram(bins = 30, fill = "darkblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = paste("Distribution of", "phosphate")) +
  theme(plot.title = element_text(hjust = 0.5))
#sgot
ggplot(hepatitis_data, aes(x = sgot)) +
  geom_histogram(bins = 30, fill = "darkred", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = paste("Distribution of", "sgot")) +
  theme(plot.title = element_text(hjust = 0.5))

# Categorical variables
categorical_variable <- c("class", "sex", "steroid", "antiviral", "fatigue", "malaise", "anorexia",
                      "liver_big", "liver_firm", "spleen", "spiders", "ascites", "varices", "histology")
#class
ggplot(hepatitis_data, aes(x = class)) +
    geom_bar(fill = "blue", color = "black", alpha = 0.7) +
    theme_minimal() +
    labs(title = paste("Distribution of", "class")) +
    theme(plot.title = element_text(hjust = 0.5))

#sex
ggplot(hepatitis_data, aes(x = sex)) +
  geom_bar(fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = paste("Distribution of", "sex")) +
  theme(plot.title = element_text(hjust = 0.5))

#spleen
ggplot(hepatitis_data, aes(x = spleen)) +
  geom_bar(fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = paste("Distribution of", "spleen")) +
  theme(plot.title = element_text(hjust = 0.5))

#varices
ggplot(hepatitis_data, aes(x = varices)) +
  geom_bar(fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = paste("Distribution of", "varices")) +
  theme(plot.title = element_text(hjust = 0.5))
#bivariate analysis
#sex
ggplot(hepatitis_data, aes(x = "class", fill = sex)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = paste("Sex", "by Class")) +
  theme(plot.title = element_text(hjust = 0.5))

#spleen
ggplot(hepatitis_data, aes(x = "class", fill = spleen)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = paste("Spleen", "by Class")) +
  theme(plot.title = element_text(hjust = 0.5))

#liver firm
ggplot(hepatitis_data, aes(x = "class", fill = liver.firm)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = paste("Liver firm", "by Class")) +
  theme(plot.title = element_text(hjust = 0.5))

# Feature Selection
library(corrplot)
# Correlation matrix for continuous variables
cor_matrix <- cor(hepatitis_data[continuous_variable])
corrplot(cor_matrix, method = "circle")

# Select relevant features based on correlation with the target variable
selected_features <- c("age", "bilirubin", "sgot", "albumin", "protime", 
                       "sex", "steroid", "antiviral", "fatigue", "malaise", "anorexia",
                       "liver_big", "liver_firm", "spleen", "spiders", "ascites", "varices", "histology")
hepatitis_data <- hepatitis_data %>% 
  select(-phosphate)

###split the data
# Set seed for reproducibility
set.seed(123)

# Split the data
trainIndex <- createDataPartition(hepatitis_data$class, p = .7, 
                                  list = FALSE, 
                                  times = 1)
hepatitisTrain <- hepatitis_data[ trainIndex,]
hepatitisTest  <- hepatitis_data[-trainIndex,]
glimpse(hepatitisTrain)
glimpse(hepatitisTest)
###train a logistic regression model using the training data
# Train logistic regression model
library(modelr)
library(tidymodels)

logistic_model<- logistic_reg(mode ="classification") |> 
  set_engine("glm") |> 
  fit(class ~ ., data = hepatitisTrain)
logistic_model
# Predict on test data
logistic_pred <- predict(logistic_model, new_data = hepatitisTest)
# Ensure predictions and true labels are factors with the same levels
logistic_pred <- factor(logistic_pred, levels = levels(hepatitisTest$class))
hepatitisTest$class <- factor(hepatitisTest$class)

# Check the levels
levels(logistic_pred)
levels(hepatitisTest$class)
# Confusion matrix and accuracy
confusionMatrix(logistic_pred, hepatitisTest$class)
