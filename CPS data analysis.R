## load the required packages 
library(tidyverse)
library(tidyquant)
library(ggplot2)
## set the working directory
setwd("C:/Users/Baha/Downloads")
## load the dataset into the program
cps_data <-read.csv("CPS_ES-HS_ProgressReport_2011-2012.csv", na.strings = c("NA"))
## view the first 10 observations of the loaded data
cps_data %>% 
  head(n=10)
## view the data structure
str(cps_data)
########################## Data description ##############
## five number summary of the selected variables
# Five-number summary for each variable
fivenum_summary <- cps_data %>%
  select(Average_Student_Attence, 
         Rate_of_Misconducts, 
         Average_Teacher_Attence, 
         X9_Grade_Explore_2009, 
         X11_Grade_Average_ACT_2011, 
         College_Eligibility, 
         Graduation_Rate, 
         Freshman_on_Track_Rate, 
         Probation) %>%
  summarise_all(fivenum)
fivenum_summary
# Box plot for response variable
ggplot(cps_data, aes(x = factor(1), y = College_Enrollment)) + 
  geom_boxplot() + 
  labs(x = "College Enrollment", y = "Number of Students Enrolled")
# Histogram for College Enrollment
ggplot(cps_data, aes(x = College_Enrollment)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Histogram of College Enrollment", x = "College Enrollment", y = "Frequency")
# Histograms for each predictor variable
for (col in c("Average_Student_Attence", 
         "Rate_of_Misconducts", 
         "Average_Teacher_Attence", 
         "X9_Grade_Explore_2009", 
         "X11_Grade_Average_ACT_2011", 
         "College_Eligibility", 
         "Graduation_Rate", 
         "Freshman_on_Track_Rate", 
         "Probation")) {
  print(ggplot(cps_data, aes_string(x = col)) +
          geom_histogram(bins = 30, fill = "orange", alpha = 0.7) +
          labs(title = paste("Histogram of", col), x = col, y = "Frequency"))
}
## indicator variables 
# Count of 0s and 1s for Probation
table(cps_data$Probation)
############ Multiple linear regression model ################
# Fit the multiple linear regression model
model <- lm(College_Enrollment ~ Average_Student_Attence + Rate_of_Misconducts +
              Average_Teacher_Attence + X9_Grade_Explore_2009 + X11_Grade_Average_ACT_2011 +
              College_Eligibility + Graduation_Rate + Freshman_on_Track_Rate + Probation, data = cps_data)
# Summary of the model
summary(model)
## check for multicollinearity 
library(car)
#calculate vif values
vif_values <-vif(model)
print(vif_values)
# Refit the model after removing X11_Grade_Average_ACT_2011
model_refit1 <- lm(College_Enrollment ~ Average_Student_Attence + Rate_of_Misconducts +
                     Average_Teacher_Attence + X9_Grade_Explore_2009 +
                     College_Eligibility + Graduation_Rate + Freshman_on_Track_Rate + Probation, data = cps_data)
vif(model_refit1)
# Refit the model after removing X9_Grade_Explore_2009
model_refit2 <- lm(College_Enrollment ~ Average_Student_Attence + Rate_of_Misconducts +
                     Average_Teacher_Attence + College_Eligibility +
                     Graduation_Rate + Freshman_on_Track_Rate + Probation, data = cps_data)
vif(model_refit2)
# Refit the model after removing College_Eligibility
model_final <- lm(College_Enrollment ~ Average_Student_Attence + Rate_of_Misconducts +
                    Average_Teacher_Attence + Graduation_Rate + Freshman_on_Track_Rate + Probation, data = cps_data)
vif(model_final)
## model assumptions
# linearity assumption
# Residuals vs. Fitted plot
plot(model_final$fitted.values, model_final$residuals, 
     main = "Residuals vs. Fitted", 
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red") ## linearity assumption holds
# independence of residuals
library(lmtest)
durbinWatsonTest(model_final) ## assumption holds rho=0
# normality of residuals
# Q-Q plot
qqnorm(model_final$residuals)
qqline(model_final$residuals, col = "blue") ## assumption holds, the residuals are normal
# Histogram of residuals
hist(model_final$residuals, main = "Histogram of Residuals", xlab = "Residuals", breaks = 20)
# Equal variances (Homoscedasticity)
# Scale-Location plot
plot(model_final, which = 3) ## there is homoscedasticity, the assumption holds.
## Overall hypothesis testing on the model
summary(model_final)
# individual variables
summary(model_final)$coefficients

################## performing backward selection##################
# Step wise selection using backward elimination
# Create a new data frame with only complete cases for the relevant variables
cps_data_complete <- na.omit(cps_data[, c("College_Enrollment", "Average_Student_Attence", 
                                          "Rate_of_Misconducts", "Average_Teacher_Attence", 
                                          "X9_Grade_Explore_2009", "X11_Grade_Average_ACT_2011", 
                                          "College_Eligibility", "Graduation_Rate", 
                                          "Freshman_on_Track_Rate", "Probation")])
# Fit the model with the complete cases
model_full <- lm(College_Enrollment ~ Average_Student_Attence + Rate_of_Misconducts +
                   Average_Teacher_Attence + X9_Grade_Explore_2009 + X11_Grade_Average_ACT_2011 +
                   College_Eligibility + Graduation_Rate + Freshman_on_Track_Rate + Probation, 
                 data = cps_data_complete)
# Perform backward selection
model_best <- stepAIC(model_full, direction = "backward", trace = FALSE)
summary(model_best)
## check model assumptions of the best model
# Linearity and homoscedasticity
plot(model_best, which = 1)  # Residuals vs. Fitted
# Normality
plot(model_best, which = 2)  # Q-Q plot of residuals
shapiro.test(residuals(model_best))  # Shapiro-Wilk test
# Independence
library(car)
durbinWatsonTest(model_best)  # Durbin-Watson test for autocorrelation
