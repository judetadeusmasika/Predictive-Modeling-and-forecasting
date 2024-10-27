library(tidyverse)
library(readxl)
## import the datasets
X <-read_xlsx("C:/Users/Baha/OneDrive/Desktop/income.xlsx")
head(X)
objects(X)
Y <-read_xlsx("C:/Users/Baha/OneDrive/Desktop/health.xlsx")
head(Y)
objects(Y)
## reshape the data
library(tidyr)
library(dplyr)
# Reshaping the X dataset: pivoting years into rows
X_long <- X %>%
  pivot_longer(cols = starts_with("20"), 
               names_to = "Year", 
               values_to = "Inequality") # Here we are renaming the column for X variable
head(X_long)
# Reshaping the Y dataset: pivoting years into rows
Y_long <- Y %>%
  pivot_longer(cols = starts_with("20"), 
               names_to = "Year", 
               values_to = "Healthcare_Expenditure") # Renaming the column for Y variable
head(Y_long)
# Combining the two datasets on geo\\TIME_PERIOD, freq, and Year
combined_data <- X_long %>%
  inner_join(Y_long, by = c("geo\\TIME_PERIOD", "freq", "Year"))
# Check the first few rows of the combined dataset
head(combined_data)
colSums(is.na(combined_data))
combined_data$Inequality <-replace(combined_data$Inequality, combined_data$Inequality==":", 0)
tail(combined_data)
combined_data$Healthcare_Expenditure <-replace(combined_data$Healthcare_Expenditure, combined_data$Healthcare_Expenditure==":",0)
tail(combined_data)
tail(combined_data, 20)
# Remove suffix letters from `inequality` column
combined_data$Inequality <- gsub("[a-zA-Z]", "", combined_data$Inequality)
# Remove suffix letters from `healthcare_expenditure` column
combined_data$Healthcare_Expenditure <- gsub("[a-zA-Z]", "", combined_data$Healthcare_Expenditure)
# Convert the cleaned columns to numeric type
combined_data$Inequality <- as.numeric(combined_data$Inequality)
combined_data$Healthcare_Expenditure <- as.numeric(combined_data$Healthcare_Expenditure)
# Check the cleaned data
print(combined_data)
## descriptive statistics
library(ggplot2)
plot1 <- ggplot(combined_data, aes(x = Inequality)) + 
  geom_density(fill = "blue", alpha = 0.6) + 
  labs(title = "Density Plot of Income Inequality (X Variable)", 
       x = "Inequality of Income Distribution (index)", 
       y = "Density") + 
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5))  # Center the title
print(plot1)

options(scipen = 999)
plot2 <- ggplot(combined_data, aes(x = Healthcare_Expenditure)) + 
  geom_density(fill = "gray", alpha = 0.6) + 
  labs(title = "Density Plot of Total Health Care Expenditure (Y Variable)", 
       x = "Total Health Care Expenditure", 
       y = "Density") + 
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5))  # Center the title
print(plot2)

## summary statistics
library(dplyr)
library(tidyr)
library(kableExtra)

# Create a summary statistics table
summary_table <- combined_data %>%
  summarise(
    Mean = c(mean(Inequality, na.rm = TRUE), mean(Healthcare_Expenditure, na.rm = TRUE)),
    SD = c(sd(Inequality, na.rm = TRUE), sd(Healthcare_Expenditure, na.rm = TRUE)),
    Min = c(min(Inequality, na.rm = TRUE), min(Healthcare_Expenditure, na.rm = TRUE)),
    Max = c(max(Inequality, na.rm = TRUE), max(Healthcare_Expenditure, na.rm = TRUE))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value") %>%
  mutate(Variable = rep(c("Inequality", "Health Expenditure"), each = 4)) %>%
  select(Variable, Statistic, Value)
summary_table
# Create a clean summary statistics table
summary_table %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  kable(caption = "Summary Statistics for Variables X and Y") %>%
  kable_styling(full_width = F)
# Create a flextable from the summary statistics table
summary_flextable <- flextable(summary_table) %>%
  set_caption("Summary Statistics for Variables X and Y") %>%
  theme_vanilla() # You can choose other themes as well
# Create a Word document
doc <- read_docx()
# Add the flextable to the Word document
doc <- doc %>%
  body_add_flextable(summary_flextable)
# Save the Word document
print(doc, target = "summary_statistics.docx")

### summary statistics by country
summary_stats_by_country <- combined_data %>%
  group_by(`geo\\TIME_PERIOD`) %>%
  summarise(
    Mean_X = mean(Inequality, na.rm = TRUE),
    SD_X = sd(Inequality, na.rm = TRUE),
    Mean_Y = mean(Healthcare_Expenditure, na.rm = TRUE),
    SD_Y = sd(Healthcare_Expenditure, na.rm = TRUE)
  ) %>%
  rename(Country = `geo\\TIME_PERIOD`)
# Display the summary statistics table
summary_stats_by_country
# Create a clean summary statistics table for countries
summary_stats_by_country_clean <- summary_stats_by_country %>%
  pivot_longer(
    cols = c(Mean_X, SD_X, Mean_Y, SD_Y),
    names_to = c(".value", "Variable"),
    names_pattern = "(.+)_(.+)"
  ) %>%
  rename(Country = Country)
# Display the formatted summary statistics table
summary_stats_by_country_clean %>%
  kable(caption = "Summary Statistics by Country for Variables X and Y") %>%
  kable_styling(full_width = F)

library(officer)
library(flextable)
# Create a clean summary statistics table for countries
summary_stats_by_country_clean <- summary_stats_by_country %>%
  pivot_longer(
    cols = c(Mean_X, SD_X, Mean_Y, SD_Y),
    names_to = c(".value", "Variable"),
    names_pattern = "(.+)_(.+)"
  ) %>%
  rename(Country = Country)
# Create a flextable from the summary statistics table
summary_flextable <- flextable(summary_stats_by_country_clean) %>%
  set_caption("Summary Statistics by Country for Variables X and Y") %>%
  theme_vanilla() # You can choose other themes as well
# Create a Word document
doc <- read_docx()
# Add the flextable to the Word document
doc <- doc %>%
  body_add_flextable(summary_flextable)
# Save the Word document
print(doc, target = "summary_statistics_by_country.docx")

## the average data

# Calculate average values for X and Y over the years
average_data <- combined_data %>%
  group_by(`geo\\TIME_PERIOD`) %>%
  summarize(
    average_X = mean(Inequality, na.rm = TRUE),  
    average_Y = mean(Healthcare_Expenditure, na.rm = TRUE)  
  )
head(average_data)
# Plot for variable X
plot3 <-ggplot(data = average_data, aes(x = `geo\\TIME_PERIOD`, y = average_X)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Average Inequality of Income Distribution Over the Years",
       x = "Year",
       y = "Average Inequality") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
print(plot3)

# Plot for variable Y
ggplot(data = average_data, aes(x = `geo\\TIME_PERIOD`, y = average_Y)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Average Health Care Expenditure Over the Years",
       x = "Year",
       y = "Average Expenditure") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Define the countries you are interested in
selected_countries <- c("HU", "UK")
# Filter data for selected countries
filtered_data <- combined_data %>%
  filter(`geo\\TIME_PERIOD` %in% selected_countries)
head(filtered_data)
# Calculate average values for X and Y over the years for the selected countries
average_data_selected <- filtered_data %>%
  group_by(`geo\\TIME_PERIOD`) %>%
  summarize(
    average_X = mean(Inequality, na.rm = TRUE),  
    average_Y = mean(Healthcare_Expenditure, na.rm = TRUE)
  )
head(average_data_selected)

# Plot for variable X (Inequality) using a bar plot
ggplot(data = average_data_selected, aes(x = `geo\\TIME_PERIOD`, y = average_X, fill = `geo\\TIME_PERIOD`)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average Inequality of Income Distribution for Selected Countries",
       x = "Country",
       y = "Average Inequality") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  )

# Plot for variable Y (Health Expenditure)
ggplot(data = average_data_selected, aes(x = `geo\\TIME_PERIOD`, y = average_Y, fill = `geo\\TIME_PERIOD`)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average Health Care Expenditure for Selected Countries",
       x = "Country",
       y = "Average Expenditure") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  )


# Compute the change in X and Y
change_data <- combined_data %>%
  group_by(`geo\\TIME_PERIOD`) %>%
  summarise(
    change_X = last(Inequality) - first(Inequality),
    change_Y = last(Healthcare_Expenditure) - first(Healthcare_Expenditure)
  )

# Rank the countries based on the changes
change_data <- change_data %>%
  mutate(
    rank_X = rank(-change_X),  # Negative sign for descending order
    rank_Y = rank(-change_Y)
  )
head(change_data)
# Reshape the data for plotting
change_data_long <- change_data %>%
  pivot_longer(cols = starts_with("change_"), names_to = "Variable", values_to = "Change") %>%
  mutate(Variable = factor(Variable, levels = c("change_X", "change_Y"),
                           labels = c("Change in Inequality", "Change in Health Expenditure")))

# Plot the changes in a faceted bar graph
ggplot(change_data_long, aes(x = reorder(`geo\\TIME_PERIOD`, -Change), y = Change, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Change in Variables X and Y by Country",
       x = "Country",
       y = "Change",
       fill = "Variable") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~ Variable, scales = "free_y")  # Create separate panels for X and Y

# Create the scatter plot with linear fit
ggplot(data = combined_data, aes(x = Inequality, y = Healthcare_Expenditure)) +
  geom_point(color = "blue", alpha = 0.6) +  # Points with some transparency
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear fit without confidence interval
  labs(title = "Relationship Between Inequality of Income Distribution and Health Care Expenditure",
       x = "Inequality of Income Distribution (X)",
       y = "Total Health Care Expenditure (Y)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# Create the scatter plot with quadratic fit
ggplot(data = combined_data, aes(x = Inequality, y = Healthcare_Expenditure)) +
  geom_point(color = "blue", alpha = 0.6) +  # Points with some transparency
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Quadratic fit
  labs(title = "Quadratic Relationship Between Inequality of Income Distribution and Health Care Expenditure",
       x = "Inequality of Income Distribution (X)",
       y = "Total Health Care Expenditure (Y)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# Calculate within-country changes
str(combined_data)
# Calculate within-country changes
within_country_changes <- combined_data %>%
  group_by(`geo\\TIME_PERIOD`) %>%
  arrange(`geo\\TIME_PERIOD`, combined_data$Year) %>%  
  mutate(change_X = Inequality - lag(Inequality),  # Change in X
         change_Y = Healthcare_Expenditure - lag(Healthcare_Expenditure)) %>%  # Change in Y
  ungroup() %>%
  filter(!is.na(change_X) & !is.na(change_Y))  # Remove NA values

# Create the scatter plot with linear fit
ggplot(data = within_country_changes, aes(x = change_X, y = change_Y)) +
  geom_point(color = "blue", alpha = 0.6) +  # Points with transparency
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear fit
  labs(title = "Within-Country Changes in Inequality vs. Health Care Expenditure",
       x = "Change in Inequality of Income Distribution (X)",
       y = "Change in Total Health Care Expenditure (Y)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

## Regression analysis
# Run the regression
regression_results <- lm(Healthcare_Expenditure ~ Inequality, data = combined_data)
# Summarize the results
summary_results <- summary(regression_results)
# Extract coefficients and statistics
results_table <- data.frame(
  Estimate = summary_results$coefficients[, 1],
  Std_Error = summary_results$coefficients[, 2],
  t_value = summary_results$coefficients[, 3],
  p_value = summary_results$coefficients[, 4]
)
# Add row names
results_table <- cbind(Variable = rownames(results_table), results_table)
rownames(results_table) <- NULL
# Create a flextable from the results table
summary_flextable <- flextable(results_table) %>%
  set_caption("Summary Statistics by Country for Variables X and Y") %>%
  theme_vanilla() # You can choose other themes as well
# Create a Word document
doc <- read_docx()
# Add the flextable to the Word document
doc <- doc %>%
  body_add_flextable(summary_flextable)
# Save the Word document
print(doc, target = "results_table.docx")

library(kableExtra)
library(fixest)
# Simple linear regression (without country fixed effects)
regression_simple <- lm(Healthcare_Expenditure ~ Inequality, data = combined_data)
# Fixed effects regression with country fixed effects
regression_fixed <- feols(Healthcare_Expenditure ~ Inequality | `geo\\TIME_PERIOD`, data = combined_data)
summary(regression_fixed)
# Extract summary for simple regression
summary_simple <- summary(regression_simple)
# Extract coefficients and statistics from the fixed effects model
coef_fixed <- coef(regression_fixed)
se_fixed <- se(regression_fixed)
tstat_fixed <- tstat(regression_fixed)
pvalue_fixed <- pvalue(regression_fixed)
# Combine the results into one table
results_table_combined <- data.frame(
  Variable = c("Intercept", "X Variable"),
  Estimate_Simple = coef(summary_simple)[, 1],
  Std_Error_Simple = coef(summary_simple)[, 2],
  t_value_Simple = coef(summary_simple)[, 3],
  p_value_Simple = coef(summary_simple)[, 4],
  Estimate_Fixed = c(coef_fixed["(Intercept)"], coef_fixed["Inequality"]),
  Std_Error_Fixed = c(se_fixed["(Intercept)"], se_fixed["Inequality"]),
  t_value_Fixed = c(tstat_fixed["(Intercept)"], tstat_fixed["Inequality"]),
  p_value_Fixed = c(pvalue_fixed["(Intercept)"], pvalue_fixed["Inequality"])
)
# Clean up the table
results_table_combined <- results_table_combined %>%
  mutate(across(2:9, round, 3))
# Display the table
results_table_combined %>%
  kable(
    caption = "Regression Results of Y on X (Simple and Fixed Effects Models)",
    col.names = c("Variable", "Estimate (Simple)", "Std. Error (Simple)", "t-value (Simple)", "p-value (Simple)",
                  "Estimate (Fixed)", "Std. Error (Fixed)", "t-value (Fixed)", "p-value (Fixed)")
  ) %>%
  kable_styling(full_width = F)

# Create a flextable from the results table
summary_flextable <- flextable(results_table_combined) %>%
  set_caption("Summary of the regression results for Variables X and Y") %>%
  theme_vanilla() # You can choose other themes as well
# Create a Word document
doc <- read_docx()
# Add the flextable to the Word document
doc <- doc %>%
  body_add_flextable(summary_flextable)
# Save the Word document
print(doc, target = "results_table.docx")

##############################################
# Extract summary for simple regression
summary_simple <- summary(regression_simple)
# Extract coefficients and statistics from the country fixed effects model
coef_fixed_country <- coef(regression_fixed)
se_fixed_country <- se(regression_fixed)
tstat_fixed_country <- tstat(regression_fixed)
pvalue_fixed_country <- pvalue(regression_fixed)

# Run the regression controlling for year and country fixed effects
regression_year_country_fixed <- feols(Healthcare_Expenditure ~ Inequality | `geo\\TIME_PERIOD` + Year, data = combined_data)

# Extract coefficients and statistics from the year and country fixed effects model
coef_fixed_year_country <- coef(regression_year_country_fixed)
se_fixed_year_country <- se(regression_year_country_fixed)
tstat_fixed_year_country <- tstat(regression_year_country_fixed)
pvalue_fixed_year_country <- pvalue(regression_year_country_fixed)

# Combine the results into one table
results_table_combined <- data.frame(
  Variable = c("Intercept", "X Variable"),
  
  # Simple regression results
  Estimate_Simple = coef(summary_simple)[, 1],
  Std_Error_Simple = coef(summary_simple)[, 2],
  t_value_Simple = coef(summary_simple)[, 3],
  p_value_Simple = coef(summary_simple)[, 4],
  
  # Country fixed effects regression results
  Estimate_Fixed_Country = c(coef_fixed_country["(Intercept)"], coef_fixed_country["Inequality"]),
  Std_Error_Fixed_Country = c(se_fixed_country["(Intercept)"], se_fixed_country["Inequality"]),
  t_value_Fixed_Country = c(tstat_fixed_country["(Intercept)"], tstat_fixed_country["Inequality"]),
  p_value_Fixed_Country = c(pvalue_fixed_country["(Intercept)"], pvalue_fixed_country["Inequality"]),
  
  # Year and country fixed effects regression results
  Estimate_Fixed_Year_Country = c(coef_fixed_year_country["(Intercept)"], coef_fixed_year_country["Inequality"]),
  Std_Error_Fixed_Year_Country = c(se_fixed_year_country["(Intercept)"], se_fixed_year_country["Inequality"]),
  t_value_Fixed_Year_Country = c(tstat_fixed_year_country["(Intercept)"], tstat_fixed_year_country["Inequality"]),
  p_value_Fixed_Year_Country = c(pvalue_fixed_year_country["(Intercept)"], pvalue_fixed_year_country["Inequality"])
)

# Clean up the table
results_table_combined <- results_table_combined %>%
  mutate(across(2:13, round, 3))

# Display the table
results_table_combined %>%
  kable(
    caption = "Regression Results of Y on X (Simple, Country Fixed Effects, and Year + Country Fixed Effects Models)",
    col.names = c("Variable", "Estimate (Simple)", "Std. Error (Simple)", "t-value (Simple)", "p-value (Simple)",
                  "Estimate (Country FE)", "Std. Error (Country FE)", "t-value (Country FE)", "p-value (Country FE)",
                  "Estimate (Year + Country FE)", "Std. Error (Year + Country FE)", "t-value (Year + Country FE)", "p-value (Year + Country FE)")
  ) %>%
  kable_styling(full_width = F)

# Create a flextable from the results table
summary_flextable <- flextable(results_table_combined) %>%
  set_caption("Summary of the regression results for Variables X and Y") %>%
  theme_vanilla() # You can choose other themes as well
# Create a Word document
doc <- read_docx()
# Add the flextable to the Word document
doc <- doc %>%
  body_add_flextable(summary_flextable)
# Save the Word document
print(doc, target = "results_combined_table.docx")