## load the required libraries
library(tidyverse)
library(ggplot2)
library(tidyquant)
library(car)
library(kableExtra)
library(fixest)
library(officer)
library(tidyr)
library(dplyr)
library(ggthemes)
## load the dataset
load("C:/Users/Baha/OneDrive/Documents/Welcome to Data Science Project/Nutrition_data-1.RData")
head(Nutrition_data)
## convert the categorical variable into factors
Nutrition_data$`Income group` <-as.factor(Nutrition_data$`Income group`)
head(Nutrition_data)
objects(Nutrition_data)
str(Nutrition_data)
### create the scatter plots 
# Scatter plot for GNI per capita, PPP
ggplot(Nutrition_data, aes(x = `GNI per capita, PPP (current intertiol $)`, 
                           y = `Percent of population with low food budget`)) +
  geom_point() +
  labs(title = "Low Food Budget and GNI per capita",
       x = "GNI per capita, PPP (current international $)", y = "Percent of population with low food budget") +
  theme_tq() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Scatter plot for Cost of a healthy diet
ggplot(Nutrition_data, aes(x = `Cost of a healthy diet`, 
                           y = `Percent of population with low food budget`)) +
  geom_point() +
  labs(title = "Low Food Budget and Cost of a Healthy Diet", 
       x = "Cost of a Healthy Diet", y = "Percent of population with low food budget") +
  theme_tq() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Scatter plot for Access to electricity, rural
ggplot(Nutrition_data, aes(x = `Access to electricity, rural (% of rural population)`, 
                           y = `Percent of population with low food budget`)) +
  geom_point() +
  labs(title = "Low Food Budget and Access to electricity, rural (% of rural population)", 
       x = "Electricity,rural (% of rural population)", y = "Percent of population with low food budget") + theme_tq() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Scatter plot for Exports of goods and services (% of GDP)
ggplot(Nutrition_data, aes(x = `Exports of goods and services (% of GDP)`,
                           y = `Percent of population with low food budget`)) +
  geom_point() +
  labs(title = "Low Food Budget and Exports of goods and services (% of GDP)", 
       x = "Exports of goods and services (% of GDP)",
       y = "Percent of population with low food budget") + 
  theme_tq() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter plot for Rural population growth (annual %)
ggplot(Nutrition_data, aes(x = `Rural population growth (annual %)`, 
                           y = `Percent of population with low food budget`)) +
  geom_point() +
  labs(title = "Low Food Budget and Rural population growth (annual %)", 
       x = "Rural population growth (annual %)", 
       y = "Percent of population with low food budget") + 
  theme_tq() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))


#### Modeling Low Food Budget
# Fit the OLS regression model
model <- lm(`Percent of population with low food budget` ~ log(`GNI per capita, PPP (current intertiol $)`) +
              `Cost of a healthy diet` +
              `Access to electricity, rural (% of rural population)` +
              log(`Exports of goods and services (% of GDP)`) +
              `Rural population growth (annual %)`,
            data = Nutrition_data)
summary_results <-summary(model)
summary_results
## check for multicollinearity 
library(car)  # Load the car package for VIF
vif_values <- vif(model)
print(vif_values)
# Tidy the model output using the broom package
library(broom)
library(flextable)
model_summary <- tidy(model)
# Convert the summary into a flextable
summary_flextable <- flextable(model_summary)
# Create a new Word document and add the flextable
doc <- read_docx() %>%
  body_add_par("OLS Regression Results", style = "heading 1") %>%
  body_add_flextable(summary_flextable)
# Save the Word document
print(doc, target = "OLS_Regression_Summary.docx")
plot(model)
# Load the library for robust standard errors
library(sandwich)
library(lmtest)
# Fit the model with robust standard errors
coeftest(model, vcov = vcovHC(model, type = "HC1"))
mse <- mean(residuals(model)^2)
rmse <- sqrt(mse)
print(c("MSE:", mse, "RMSE:", rmse))
