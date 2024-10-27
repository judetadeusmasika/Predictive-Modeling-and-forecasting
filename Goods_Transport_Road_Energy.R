## load the required packages
library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(kableExtra)
library(officer)
library(flextable)
## set the working directory
setwd("C:/Users/Baha/OneDrive/Desktop")
## import the datasets
X <-read_xlsx("Goods_transport_road.xlsx")
head(X)
objects(X)
Y <-read_xlsx("Energy_Productivity.xlsx")
head(Y)
objects(Y)
## reshape the data
library(tidyr)
library(dplyr)
# Reshape the X dataset 
X <- X %>%
  mutate(across(starts_with("20"), as.numeric))  # Converting to numeric
X_long <- X %>%
  pivot_longer(cols = starts_with("20"), 
               names_to = "Year",
               values_to = "Goods_transport_road")
head(X_long)
# Reshaping the Y dataset: pivoting years into rows
Y <- Y %>%
  mutate(across(starts_with("20"), as.numeric))
Y_long <- Y %>%
  pivot_longer(cols = starts_with("20"), 
               names_to = "Year", 
               values_to = "Energy_Productivity") 
head(Y_long)
# Convert Year columns to numeric 
X_long$Year <- as.numeric(X_long$Year)
Y_long$Year <- as.numeric(Y_long$Year)
# Join the two datasets on GEO//TIME and Year
combined_data <- X_long %>%
  inner_join(Y_long, by = c("GEO//TIME", "Year"))
# View the combined data
print(combined_data)
colSums(is.na(combined_data))
## remove the missing values
combined_data <-combined_data %>% 
  na.omit(combined_data)
str(combined_data)
combined_data$Goods_transport_road <-replace(combined_data$Goods_transport_road, combined_data$Goods_transport_road==":", 0)
combined_data$Energy_Productivity <-replace(combined_data$Energy_Productivity, combined_data$Energy_Productivity==":",0)
# suffix letters from `Goods_transport_road` column
combined_data$Goods_transport_road <- gsub("[a-zA-Z]", "", combined_data$Goods_transport_road)
# suffix letters from `Energy_Productivity` column
combined_data$Energy_Productivity <- gsub("[a-zA-Z]", "", combined_data$Energy_Productivity)
# change the cleaned columns to numeric type
combined_data$Goods_transport_road <- as.numeric(combined_data$Goods_transport_road)
combined_data$Energy_Productivity <- as.numeric(combined_data$Energy_Productivity)
# Check the cleaned data
print(combined_data, n=20)
## descriptive statistics
## the X variable
library(ggplot2)
options(scipen = 999)
ggplot(combined_data, aes(x = Goods_transport_road)) + 
  geom_density(fill = "black", alpha = 0.6) + 
  labs(title = "Density Plot of Goods transport by road", 
       x = "Goods Transport by road", 
       y = "Density") + 
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5))

#### the Y variable
ggplot(combined_data, aes(x = Energy_Productivity)) + 
  geom_density(fill = "orange", alpha = 0.6) + 
  labs(title = "Density Plot of Energy productivity", 
       x = "Energy Productivity", 
       y = "Density") + 
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5))  # Center the title

## summary statistics
summary_table <- combined_data %>%
  summarise(
    Mean = c(mean(Goods_transport_road, na.rm = TRUE), mean(Energy_Productivity, na.rm = TRUE)),
    SD = c(sd(Goods_transport_road, na.rm = TRUE), sd(Energy_Productivity, na.rm = TRUE)),
    Min = c(min(Goods_transport_road, na.rm = TRUE), min(Energy_Productivity, na.rm = TRUE)),
    Max = c(max(Goods_transport_road, na.rm = TRUE), max(Energy_Productivity, na.rm = TRUE))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value") %>%
  mutate(Variable = rep(c("Goods Transport by Road", "Energy Productivity"), each = 4)) %>%
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
print(doc, target = "summary_statistics2.docx")

### summary statistics by country
summary_stats_by_country <- combined_data %>%
  group_by(`GEO//TIME`) %>%
  summarise(
    Mean_X = mean(Goods_transport_road, na.rm = TRUE),
    SD_X = sd(Goods_transport_road, na.rm = TRUE),
    Mean_Y = mean(Energy_Productivity, na.rm = TRUE),
    SD_Y = sd(Energy_Productivity, na.rm = TRUE)
  ) %>%
  rename(Country = `GEO//TIME`)
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
  kable(caption = "Summary Statistics by Country") %>%
  kable_styling(full_width = F)

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
  set_caption("Summary Statistics by Country") %>%
  theme_vanilla() # You can choose other themes as well
# Create a Word document
doc <- read_docx()
# Add the flextable to the Word document
doc <- doc %>%
  body_add_flextable(summary_flextable)
# Save the Word document
print(doc, target = "summary_statistics_by_country2.docx")

## the average data

# Calculate average values for X and Y over the years
average_data <- combined_data %>%
  group_by(`GEO//TIME`) %>%
  summarize(
    average_X = mean(Goods_transport_road, na.rm = TRUE),  
    average_Y = mean(Energy_Productivity, na.rm = TRUE)  
  )
head(average_data)
library(tidyquant)
# Plot for variable X
ggplot(data = average_data, aes(x = `GEO//TIME`, y = average_X)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Average goods transport by road Over the Years",
       x = "Country",
       y = "Average goods transport by road") +
  theme_tq() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# Plot for variable Y
ggplot(data = average_data, aes(x = `GEO//TIME`, y = average_Y)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Average energy productivity Over the Years",
       x = "Country",
       y = "Average energy productivity") +
  theme_tq() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# select the countries you are interested in
selected_countries <- c("DK", "EU27_2020")
# Filter data for selected countries
filtered_data <- combined_data %>%
  filter(`GEO//TIME` %in% selected_countries)
head(filtered_data)
# Calculate average values for X and Y over the years for the selected countries
average_data_selected <- filtered_data %>%
  group_by(`GEO//TIME`) %>%
  summarize(
    average_X = mean(Goods_transport_road, na.rm = TRUE),  
    average_Y = mean(Energy_Productivity, na.rm = TRUE)
  )
head(average_data_selected)
# Define custom colors
custom_colors1 <- c("DK" = "steelblue", "EU27_2020" = "orange")
custom_colors2 <- c("DK" = "lightgreen", "EU27_2020" = "darkred")
# Plot for variable X using a bar plot with specified colors
ggplot(data = average_data_selected, aes(x = `GEO//TIME`, y = average_X, fill = `GEO//TIME`)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = custom_colors1) +  # Apply custom colors
  labs(title = "Average Goods Transport by Road for Selected Countries",
       x = "Country",
       y = "Average Goods Transport by Road") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  )

# Plot for variable Y with specified colors
ggplot(data = average_data_selected, aes(x = `GEO//TIME`, y = average_Y, fill = `GEO//TIME`)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = custom_colors2) +  # Apply custom colors
  labs(title = "Average Energy Productivity for Selected Countries",
       x = "Country",
       y = "Average Energy Productivity") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  )

# Compute the change in X and Y
change_data <- combined_data %>%
  group_by(`GEO//TIME`) %>%
  summarise(
    change_X = last(Goods_transport_road) - first(Goods_transport_road),
    change_Y = last(Energy_Productivity) - first(Energy_Productivity)
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
                           labels = c("Change in Goods Transport by road", "Change in Energy Productivity")))
head(change_data_long)
# Plot the changes in a faceted bar graph
ggplot(change_data_long, aes(x = reorder(`GEO//TIME`, -Change), y = Change, fill = Variable)) +
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
ggplot(data = combined_data, aes(x = Goods_transport_road, y = Energy_Productivity)) +
  geom_point(color = "red", alpha = 0.6) +  # Points with some transparency
  geom_smooth(method = "lm", color = "black", se = FALSE) +  # Linear fit without confidence interval
  labs(title = "Linear relationship between Goods transport by road and Energy productivity",
       x = "Goods transport by road",
       y = "Energy Productivity") +
  theme_tq() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# Create the scatter plot with quadratic fit
ggplot(data = combined_data, aes(x = Goods_transport_road, y = Energy_Productivity)) +
  geom_point(color = "blue", alpha = 0.6) +  # Points with some transparency
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black", se = FALSE) +  # Quadratic fit
  labs(title = "Quadratic relationship between Goods transport by road and Energy productivity",
       x = "Goods transport by road",
       y = "Energy productivity") +
  theme_tq() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# Calculate within-country changes
str(combined_data)
# Calculate within-country changes
within_country_changes <- combined_data %>%
  group_by(`GEO//TIME`) %>%
  arrange(`GEO//TIME`, combined_data$Year) %>%  
  mutate(change_X = Goods_transport_road - lag(Goods_transport_road),  # Change in X
         change_Y = Energy_Productivity - lag(Energy_Productivity)) %>%  # Change in Y
  ungroup() %>%
  filter(!is.na(change_X) & !is.na(change_Y))  # Remove NA values

# Create the scatter plot with linear fit
ggplot(data = within_country_changes, aes(x = change_X, y = change_Y)) +
  geom_point(color = "blue", alpha = 0.6) +  # Points with transparency
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +  # Linear fit
  labs(title = "Within-Country Changes in goods transport by road and energy productivity",
       x = "Change in goods transport by road",
       y = "Change in Energy productivity") +
  theme_tq() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

## Regression analysis
# Run the regression
regression_results <- lm(Energy_Productivity ~ Goods_transport_road, data = combined_data)
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
  set_caption("regression results") %>%
  theme_vanilla() # You can choose other themes as well
# Create a Word document
doc <- read_docx()
# Add the flextable to the Word document
doc <- doc %>%
  body_add_flextable(summary_flextable)
# Save the Word document
print(doc, target = "results_table2.docx")

library(kableExtra)
library(fixest)
# Simple linear regression (without country fixed effects)
regression_simple <- lm(Energy_Productivity ~ Goods_transport_road, data = combined_data)
# Fixed effects regression with country fixed effects
regression_fixed <- feols(Energy_Productivity ~ Goods_transport_road | `GEO//TIME`, data = combined_data)
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
  Estimate_Fixed = c(coef_fixed["(Intercept)"], coef_fixed["Goods_transport_road"]),
  Std_Error_Fixed = c(se_fixed["(Intercept)"], se_fixed["Goods_transport_road"]),
  t_value_Fixed = c(tstat_fixed["(Intercept)"], tstat_fixed["Goods_transport_road"]),
  p_value_Fixed = c(pvalue_fixed["(Intercept)"], pvalue_fixed["Goods_transport_road"])
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
  set_caption("Summary of the regression results") %>%
  theme_vanilla() # You can choose other themes as well
# Create a Word document
doc <- read_docx()
# Add the flextable to the Word document
doc <- doc %>%
  body_add_flextable(summary_flextable)
# Save the Word document
print(doc, target = "results_fixed_table1.docx")

##############################################
# Extract summary for simple regression
summary_simple <- summary(regression_simple)
# Extract coefficients and statistics from the country fixed effects model
coef_fixed_country <- coef(regression_fixed)
se_fixed_country <- se(regression_fixed)
tstat_fixed_country <- tstat(regression_fixed)
pvalue_fixed_country <- pvalue(regression_fixed)

# Run the regression controlling for year and country fixed effects
regression_year_country_fixed <- feols(Energy_Productivity ~ Goods_transport_road | `GEO//TIME` + Year, data = combined_data)

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
  Estimate_Fixed_Country = c(coef_fixed_country["(Intercept)"], coef_fixed_country["Goods_transport_road"]),
  Std_Error_Fixed_Country = c(se_fixed_country["(Intercept)"], se_fixed_country["Goods_transport_road"]),
  t_value_Fixed_Country = c(tstat_fixed_country["(Intercept)"], tstat_fixed_country["Goods_transport_road"]),
  p_value_Fixed_Country = c(pvalue_fixed_country["(Intercept)"], pvalue_fixed_country["Goods_transport_road"]),
  
  # Year and country fixed effects regression results
  Estimate_Fixed_Year_Country = c(coef_fixed_year_country["(Intercept)"], coef_fixed_year_country["Goods_transport_road"]),
  Std_Error_Fixed_Year_Country = c(se_fixed_year_country["(Intercept)"], se_fixed_year_country["Goods_transport_road"]),
  t_value_Fixed_Year_Country = c(tstat_fixed_year_country["(Intercept)"], tstat_fixed_year_country["Goods_transport_road"]),
  p_value_Fixed_Year_Country = c(pvalue_fixed_year_country["(Intercept)"], pvalue_fixed_year_country["Goods_transport_road"])
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
  set_caption("Summary of the regression results with fixed effects") %>%
  theme_vanilla() # You can choose other themes as well
# Create a Word document
doc <- read_docx()
# Add the flextable to the Word document
doc <- doc %>%
  body_add_flextable(summary_flextable)
# Save the Word document
print(doc, target = "results_combined_table1.docx")