################ DATA MESSY ASSIGNMENT #####################

## Load the required packages for this task
library(tidyverse)
library(tidyr)
library(dplyr)

## Load the dataset into the program
Messy_data <-read.csv("C:/Users/USER/Downloads/messy_data_header.csv")
head(Messy_data)
messy_data <-Messy_data %>% 
  select(Percapy, localxpa, cl2valpera_ch1)
head(messy_data)

########### QUESTION 1 ###################

# Calculate the old mean of Percapy
old_mean <- mean(messy_data$Percapy, na.rm = TRUE)
# Convert Percapy to thousands of dollars
messy_data$Percapy <- messy_data$Percapy / 1000
# Calculate the new mean of Percapy
new_mean <- mean(messy_data$Percapy, na.rm = TRUE)
# Print the results
cat("Old Mean (in dollars):", old_mean, "\n")
cat("New Mean (in thousands of dollars):", new_mean, "\n")


############## QUESTION 2 #################
# Convert '.' to NA in localxpa
messy_data$localxpa[messy_data$localxpa == "."] <- NA
# Convert localxpa to numeric
messy_data$localxpa <- as.numeric(messy_data$localxpa)
# Calculate the old mean, ignoring missing values
old_mean <- mean(messy_data$localxpa, na.rm = TRUE)
# Identify outliers using the IQR method
Q1 <- quantile(messy_data$localxpa, 0.25, na.rm = TRUE)
Q3 <- quantile(messy_data$localxpa, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
# Define lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
# Find outliers
outliers <- messy_data$localxpa < lower_bound | messy_data$localxpa > upper_bound
# Replace outliers with NA
messy_data$localxpa[outliers] <- NA
# Calculate the new mean after handling outliers
new_mean <- mean(messy_data$localxpa, na.rm = TRUE)
# Print results
cat("Old Mean of localxpa:", old_mean, "\n")
cat("New Mean of localxpa (after handling outliers):", new_mean, "\n")
# Print number of outliers detected
cat("Number of outliers detected:", sum(outliers, na.rm = TRUE), "\n")

#################### QUESTION 3 #####################
#install.packages("mice")
library(mice)
# Convert '.' to NA in cl2valpera_ch1
messy_data$cl2valpera_ch1[messy_data$cl2valpera_ch1 == "."] <- NA
# Convert cl2valpera_ch1 to numeric
messy_data$cl2valpera_ch1 <- as.numeric(messy_data$cl2valpera_ch1)
# Compute old mean before imputation
old_mean <- mean(messy_data$cl2valpera_ch1, na.rm = TRUE)
# Perform multiple imputation using MICE (default method: PMM - Predictive Mean Matching)
imputed_data <- mice(messy_data, method = "pmm", m = 5, maxit = 5, seed = 123)
# Extract the completed dataset (1st imputed set)
completed_data <- complete(imputed_data, 1)
# Compute new mean after imputation
new_mean <- mean(completed_data$cl2valpera_ch1, na.rm = TRUE)
# Print results
cat("Old Mean of cl2valpera_ch1:", old_mean, "\n")
cat("New Mean of cl2valpera_ch1 (after multiple imputation):", new_mean, "\n")
