## load the necessary packages
library(tidyverse)
library(readxl)
## import the datasets
## set the working directory
setwd("C:/Users/Baha/OneDrive/Documents/Welcome to Data Science Project")
Y <-read_xlsx("Air transport.xlsx")
head(Y)
objects(Y)
X <-read_xlsx("Net greenhouse gas.xlsx")
head(X)
objects(X)
## reshape the data
library(tidyr)
library(dplyr)
# Reshape the X dataset (pivot years into rows)
X_long <- X %>%
  pivot_longer(cols = `1990`:`2022`, # Select columns for years 1990-2022
               names_to = "Year",
               values_to = "Gas_Emissions")
head(X_long)
# Reshaping the Y dataset: pivoting years into rows
Y_long <- Y %>%
  pivot_longer(cols = starts_with("20"), 
               names_to = "Year", 
               values_to = "Air_Transport") # Renaming the column for Y variable
head(Y_long)
# Convert Year columns to numeric for both datasets to allow the join
X_long$Year <- as.numeric(X_long$Year)
Y_long$Year <- as.numeric(Y_long$Year)
# Join the two datasets on GEO//TIME and Year
combined_data <- X_long %>%
  inner_join(Y_long, by = c("GEO//TIME", "Year"))
# View the combined data
print(combined_data)
colSums(is.na(combined_data))
combined_data$Gas_Emissions <-replace(combined_data$Gas_Emissions, combined_data$Gas_Emissions==":", 0)
head(combined_data)
combined_data$Air_Transport <-replace(combined_data$Air_Transport, combined_data$Air_Transport==":",0)
head(combined_data)
# Remove suffix letters from `Gas_Emissions` column
combined_data$Gas_Emissions <- gsub("[a-zA-Z]", "", combined_data$Gas_Emissions)
# Remove suffix letters from `Air_Transport` column
combined_data$Air_Transport <- gsub("[a-zA-Z]", "", combined_data$Air_Transport)
# Convert the cleaned columns to numeric type
combined_data$Gas_Emissions <- as.numeric(combined_data$Gas_Emissions)
combined_data$Air_Transport <- as.numeric(combined_data$Air_Transport)
# Check the cleaned data
print(combined_data)
## descriptive statistics
## the X variable
library(ggplot2)
ggplot(combined_data, aes(x = Gas_Emissions)) + 
  geom_density(fill = "red", alpha = 0.6) + 
  labs(title = "Density Plot of Net Gas emissions (X Variable)", 
       x = "Net greenhouse gas emissions (Tonnes per capita)", 
       y = "Density") + 
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5))  # Center the title
#### the Y variable
options(scipen = 999)
ggplot(combined_data, aes(x = Air_Transport)) + 
  geom_density(fill = "gray", alpha = 0.6) + 
  labs(title = "Density Plot of  Air transport of goods by country (Y Variable)", 
       x = " Air transport of goods by country (yearly data)", 
       y = "Density") + 
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5))  # Center the title

## summary statistics
library(dplyr)
library(tidyr)
library(kableExtra)
library(officer)
library(flextable)
# Create a summary statistics table
summary_table <- combined_data %>%
  summarise(
    Mean = c(mean(Gas_Emissions, na.rm = TRUE), mean(Air_Transport, na.rm = TRUE)),
    SD = c(sd(Gas_Emissions, na.rm = TRUE), sd(Air_Transport, na.rm = TRUE)),
    Min = c(min(Gas_Emissions, na.rm = TRUE), min(Air_Transport, na.rm = TRUE)),
    Max = c(max(Gas_Emissions, na.rm = TRUE), max(Air_Transport, na.rm = TRUE))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value") %>%
  mutate(Variable = rep(c("Gas emissions", "Air Transport"), each = 4)) %>%
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
  group_by(`GEO//TIME`) %>%
  summarise(
    Mean_X = mean(Gas_Emissions, na.rm = TRUE),
    SD_X = sd(Gas_Emissions, na.rm = TRUE),
    Mean_Y = mean(Air_Transport, na.rm = TRUE),
    SD_Y = sd(Air_Transport, na.rm = TRUE)
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
  kable(caption = "Summary Statistics by Country for Variables X and Y") %>%
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
  group_by(`GEO//TIME`) %>%
  summarize(
    average_X = mean(Gas_Emissions, na.rm = TRUE),  
    average_Y = mean(Air_Transport, na.rm = TRUE)  
  )
head(average_data)
library(tidyquant)
# Plot for variable X
ggplot(data = average_data, aes(x = `GEO//TIME`, y = average_X)) +
  geom_line(color = "darkblue") +
  geom_point(color = "darkblue") +
  labs(title = "Average Net gas emissions Over the Years",
       x = "Country",
       y = "Average net gas emissions") +
  theme_tq() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# Plot for variable Y
ggplot(data = average_data, aes(x = `GEO//TIME`, y = average_Y)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Average air transport goods Over the Years",
       x = "Country",
       y = "Average air transport goods") +
  theme_tq() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Define the countries you are interested in
selected_countries <- c("United Kingdom", "Luxembourg")
# Filter data for selected countries
filtered_data <- combined_data %>%
  filter(`GEO//TIME` %in% selected_countries)
head(filtered_data)
# Calculate average values for X and Y over the years for the selected countries
average_data_selected <- filtered_data %>%
  group_by(`GEO//TIME`) %>%
  summarize(
    average_X = mean(Gas_Emissions, na.rm = TRUE),  
    average_Y = mean(Air_Transport, na.rm = TRUE)
  )
head(average_data_selected)

# Plot for variable X (Gas_Emissions) using a bar plot
ggplot(data = average_data_selected, aes(x = `GEO//TIME`, y = average_X, fill = `GEO//TIME`)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average net gas emissions for Selected Countries",
       x = "Country",
       y = "Average gas emissions") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  )

# Plot for variable Y (Air Transport goods)
ggplot(data = average_data_selected, aes(x = `GEO//TIME`, y = average_Y, fill = `GEO//TIME`)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average air transport goods for Selected Countries",
       x = "Country",
       y = "Average air transport goods") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  )


# Compute the change in X and Y
change_data <- combined_data %>%
  group_by(`GEO//TIME`) %>%
  summarise(
    change_X = last(Gas_Emissions) - first(Gas_Emissions),
    change_Y = last(Air_Transport) - first(Air_Transport)
  )

# Rank the countries based on the changes
change_data <- change_data %>%
  mutate(
    rank_X = rank(change_X),  
    rank_Y = rank(change_Y)
  )
head(change_data)
# Reshape the data for plotting
change_data_long <- change_data %>%
  pivot_longer(cols = starts_with("change_"), names_to = "Variable", values_to = "Change") %>%
  mutate(Variable = factor(Variable, levels = c("change_X", "change_Y"),
                           labels = c("Change in Gas Emissions", "Change in Air Transport")))
head(change_data_long)
# Plot the changes in a faceted bar graph
ggplot(change_data_long, aes(x = reorder(`GEO//TIME`, Change), y = Change, fill = Variable)) +
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



library(ggforce)  # To use reorder_within

# Reshape the data for plotting and rank within each variable (facet)
change_data_long <- change_data %>%
  pivot_longer(cols = starts_with("change_"), names_to = "Variable", values_to = "Change") %>%
  mutate(Variable = factor(Variable, levels = c("change_X", "change_Y"),
                           labels = c("Change in Gas Emissions", "Change in Air Transport")))

# Define custom colors for the variables
custom_colors <- c("Change in Gas Emissions" = "blue", "Change in Air Transport" = "gray")

# Reorder countries within each Variable
change_data_long <- change_data_long %>%
  group_by(Variable) %>%
  mutate(GEO_ordered = reorder(`GEO//TIME`, Change))

# Plot the changes in a faceted bar graph, reordering within facets
ggplot(change_data_long, aes(x = GEO_ordered, y = Change, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
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
  facet_wrap(~ Variable, scales = "free_y")



# Create the scatter plot with linear fit
ggplot(data = combined_data, aes(x = Gas_Emissions, y = Air_Transport)) +
  geom_point(color = "blue", alpha = 0.6) +  # Points with some transparency
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear fit without confidence interval
  labs(title = "Linear relationship Between Net greenhouse gas emissions and Air transport goods ",
       x = "Net greenhouse gas emissions(X)",
       y = "Air transport goods(Y)") +
  theme_tq() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# Create the scatter plot with quadratic fit
ggplot(data = combined_data, aes(x = Gas_Emissions, y = Air_Transport)) +
  geom_point(color = "blue", alpha = 0.6) +  # Points with some transparency
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +  # Quadratic fit
  labs(title = "Quadratic Relationship Between Net greenhouse gas emissions and Air transport goods",
       x = "Net greenhouse gas emissions (X)",
       y = "Air transport goods (Y)") +
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
  mutate(change_X = Gas_Emissions - lag(Gas_Emissions),  # Change in X
         change_Y = Air_Transport - lag(Air_Transport)) %>%  # Change in Y
  ungroup() %>%
  filter(!is.na(change_X) & !is.na(change_Y))  # Remove NA values

# Create the scatter plot with linear fit
ggplot(data = within_country_changes, aes(x = change_X, y = change_Y)) +
  geom_point(color = "blue", alpha = 0.6) +  # Points with transparency
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear fit
  labs(title = "Within-Country Changes in net greenhouse Gas Emissions versus Air Transport goods",
       x = "Change in net greenhouse gas emissions (X)",
       y = "Change in air transport goods (Y)") +
  theme_tq() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

## Regression analysis
# Run the regression
regression_results <- lm(Air_Transport ~ Gas_Emissions, data = combined_data)
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
print(doc, target = "results_table.docx")

library(kableExtra)
library(fixest)
# Simple linear regression (without country fixed effects)
regression_simple <- lm(Air_Transport ~ Gas_Emissions, data = combined_data)
# Fixed effects regression with country fixed effects
regression_fixed <- feols(Air_Transport ~ Gas_Emissions | `GEO//TIME`, data = combined_data)
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
  Estimate_Fixed = c(coef_fixed["(Intercept)"], coef_fixed["Gas_Emissions"]),
  Std_Error_Fixed = c(se_fixed["(Intercept)"], se_fixed["Gas_Emissions"]),
  t_value_Fixed = c(tstat_fixed["(Intercept)"], tstat_fixed["Gas_Emissions"]),
  p_value_Fixed = c(pvalue_fixed["(Intercept)"], pvalue_fixed["Gas_Emissions"])
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
print(doc, target = "results_fixed_table.docx")

##############################################
# Extract summary for simple regression
summary_simple <- summary(regression_simple)
# Extract coefficients and statistics from the country fixed effects model
coef_fixed_country <- coef(regression_fixed)
se_fixed_country <- se(regression_fixed)
tstat_fixed_country <- tstat(regression_fixed)
pvalue_fixed_country <- pvalue(regression_fixed)

# Run the regression controlling for year and country fixed effects
regression_year_country_fixed <- feols(Air_Transport ~ Gas_Emissions | `GEO//TIME` + Year, data = combined_data)

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
  Estimate_Fixed_Country = c(coef_fixed_country["(Intercept)"], coef_fixed_country["Gas_Emissions"]),
  Std_Error_Fixed_Country = c(se_fixed_country["(Intercept)"], se_fixed_country["Gas_Emissions"]),
  t_value_Fixed_Country = c(tstat_fixed_country["(Intercept)"], tstat_fixed_country["Gas_Emissions"]),
  p_value_Fixed_Country = c(pvalue_fixed_country["(Intercept)"], pvalue_fixed_country["Gas_Emissions"]),
  
  # Year and country fixed effects regression results
  Estimate_Fixed_Year_Country = c(coef_fixed_year_country["(Intercept)"], coef_fixed_year_country["Gas_Emissions"]),
  Std_Error_Fixed_Year_Country = c(se_fixed_year_country["(Intercept)"], se_fixed_year_country["Gas_Emissions"]),
  t_value_Fixed_Year_Country = c(tstat_fixed_year_country["(Intercept)"], tstat_fixed_year_country["Gas_Emissions"]),
  p_value_Fixed_Year_Country = c(pvalue_fixed_year_country["(Intercept)"], pvalue_fixed_year_country["Gas_Emissions"])
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
print(doc, target = "results_combined_table.docx")