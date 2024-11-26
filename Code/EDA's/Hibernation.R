# Load CSV file
data <- read.csv("Ursuna.csv", skip =4)

# Load ggplot2
library(ggplot2)
library(lubridate)
library(dplyr) 


mean(data$Temperature..C.)
median(data$Temperature..C.)
# Convert the "GMT.Time" column to POSIXct format
data$GMT.Time <- as.POSIXct(data$GMT.Time, format = "%d.%m.%Y %H:%M:%S", tz = "GMT")
data$Temperature = log(data$Temperature..C.)

                            ### ALl Months side by side ###
# Extract month and year as separate columns for faceting
data <- data %>%
  mutate(Month = format(GMT.Time, "%Y-%m"))

# Calculate median temperature to use as the threshold
mean_temp <- log(mean(data$Temperature..C.))
mean_log <- mean(data$Temperature..C.)

# Plot temperature over time with separate facets for each month and a median threshold line
ggplot(data, aes(x = GMT.Time, y = Temperature)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", color = "darkblue", se = FALSE) + 
  geom_hline(yintercept = mean_temp, linetype = "dashed", color = "red") +  # Add median threshold line
  labs(title = "Temperature Over Time", x = "Date", y = "Temperature") +
  theme_minimal() +
  facet_wrap(~ Month, scales = "free_x")  # Create a separate plot for each month

                              ###Month specific data ###

# Filter data for a month
july_data <- data %>% filter(Month == "2019-05")

# Plot temperature
ggplot(july_data, aes(x = GMT.Time, y = Temperature..C.)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", color = "darkblue", se = FALSE) +  # Smoothen the line
  geom_hline(yintercept = mean_log, linetype = "dashed", color = "red") +  # Add median threshold line
  labs(x = "Date", y = "Temperature") +
  theme_minimal()