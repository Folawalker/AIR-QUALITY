library(ggplot2)
library(dplyr)
install.packages("tidyr")
library(tidyr)


data <- read.csv("C:/Users/Tisab/Documents/My work/PORTFOLIO/AITI PROJECT/AIR QUALITY/Air_Quality.csv")

head(data)

# Convert Date to Date type
data$Date <- as.Date(data$Date)


# Overall AQI Across Cities
ggplot(data, aes(x = Date, y = AQI, color = City)) +
  geom_line(size = 1) +
  labs(title = "AQI Trends Over Time by City",
       x = "Date",
       y = "Air Quality Index (AQI)",
       color = "City") +
  theme_minimal()


# Overall PM2.5 Across Cities
ggplot(data, aes(x = Date, y = PM2.5, color = City)) +
  geom_line(size = 1) +
  labs(title = "PM2.5 Trends Over Time by City",
       x = "Date",
       y = "PM2.5 Concentration (μg/m³)",
       color = "City") +
  theme_minimal()


# Comparing Pollutants Across Cities
# Reshape data to long format
pollutants_long <- data %>%
  pivot_longer(cols = c(PM2.5, PM10, NO2, SO2, CO, O3), 
               names_to = "Pollutant", 
               values_to = "Value")

# Faceted plot
ggplot(pollutants_long, aes(x = Date, y = Value, color = City)) +
  geom_line(size = 1) +
  facet_wrap(~Pollutant, scales = "free_y") +
  labs(title = "Trends in Pollutant Levels Over Time",
       x = "Date",
       y = "Concentration",
       color = "City") +
  theme_minimal()


# Concentration of NO2 Across Cities
ggplot(pollutants_long %>% filter(Pollutant == "NO2"), 
       aes(x = Date, y = Value, color = City)) +
  geom_line(size = 1) +
  labs(title = "NO2 Levels Over Time by City",
       x = "Date",
       y = "NO2 Concentration (ppb)",
       color = "City") +
  theme_minimal()


# Concentration of PM10 Across Cities
ggplot(pollutants_long %>% filter(Pollutant == "PM10"), 
       aes(x = Date, y = Value, color = City)) +
  geom_line(size = 1) +
  labs(title = "PM10 Levels Over Time by City",
       x = "Date",
       y = "PM10 Concentration (μg/m³)",
       color = "City") +
  theme_minimal()


# Visualizing Pollutant Levels for New York
ggplot(pollutants_long %>% filter(City == "New York"), 
       aes(x = Date, y = Value, color = Pollutant)) +
  geom_line(size = 1) +
  labs(title = "Pollutant Levels Over Time in New York",
       x = "Date",
       y = "Concentration",
       color = "Pollutant") +
  theme_minimal()

# Visualizing Pollutant Levels for Los Angeles
ggplot(pollutants_long %>% filter(City == "Los Angeles"), 
       aes(x = Date, y = Value, color = Pollutant)) +
  geom_line(size = 1) +
  labs(title = "Pollutant Levels Over Time in New York",
       x = "Date",
       y = "Concentration",
       color = "Pollutant") +
  theme_minimal()


# Visualizing Pollutant Levels for Chicago
ggplot(pollutants_long %>% filter(City == "Chicago"), 
       aes(x = Date, y = Value, color = Pollutant)) +
  geom_line(size = 1) +
  labs(title = "Pollutant Levels Over Time in New York",
       x = "Date",
       y = "Concentration",
       color = "Pollutant") +
  theme_minimal()


# Trend Analysis For pollutants Over Time
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Reshape data to long format
pollutants_long <- data %>%
  pivot_longer(
    cols = c(PM2.5, PM10, NO2, SO2, CO, O3),
    names_to = "Pollutant",
    values_to = "Concentration"
  )

# Line plot with facets for each pollutant
ggplot(pollutants_long, aes(x = Date, y = Concentration, color = Pollutant)) +
  geom_line(size = 1) +
  facet_wrap(~Pollutant, scales = "free_y") + # Separate plots for each pollutant
  labs(
    title = "Trend Analysis of Pollutants Over Time",
    x = "Date",
    y = "Concentration",
    color = "Pollutant"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"), # Style for facet titles
    axis.text.x = element_text(angle = 45, hjust = 1)   # Adjust x-axis labels
  )


# Comparisons Across Cities and how Specific Pollutants Differ.
pollutants_long <- data %>%
  pivot_longer(
    cols = c(PM2.5, PM10, NO2, SO2, CO, O3),
    names_to = "Pollutant",
    values_to = "Concentration"
  )


ggplot(pollutants_long, aes(x = Date, y = Concentration, color = City)) +
  geom_line(size = 1) +
  facet_wrap(~Pollutant, scales = "free_y") +
  labs(
    title = "Comparison of Pollutant Trends Across Cities",
    x = "Date",
    y = "Concentration",
    color = "City"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



# Summarize data by City and Pollutant
summary_stats <- pollutants_long %>%
  group_by(City, Pollutant) %>%
  summarize(
    Average = mean(Concentration, na.rm = TRUE),
    Median = median(Concentration, na.rm = TRUE),
    Min = min(Concentration, na.rm = TRUE),
    Max = max(Concentration, na.rm = TRUE),
    .groups = "drop"
  )
print(summary_stats)



# Calculate average concentrations for each pollutant and city
avg_concentrations <- pollutants_long %>%
  group_by(City, Pollutant) %>%
  summarize(Average = mean(Concentration, na.rm = TRUE), .groups = "drop")

# Bar plot
ggplot(avg_concentrations, aes(x = Pollutant, y = Average, fill = City)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Pollutant Levels by City",
    x = "Pollutant",
    y = "Average Concentration",
    fill = "City"
  ) +
  theme_minimal()



ggplot(pollutants_long %>% filter(Pollutant == "CO"), 
       aes(x = Date, y = Concentration, color = City)) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.Date("2024-06-01"), linetype = "dashed", color = "red") +
  labs(
    title = "CO Levels Over Time with Highlighted Event",
    x = "Date",
    y = "CO Concentration (ppm)",
    color = "City"
  ) +
  theme_minimal()






