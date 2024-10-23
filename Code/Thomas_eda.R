library(ggplot2)
library(GGally)
library(dplyr)

data <- read.csv("GPS_91037_20200113_Thomas.csv",header=TRUE, sep = ",", skip = 4)

#Apply logarithmic transformation
data$log_latitude <- log(data$Latitude)
data$log_longitude <- log(data$Longitude)

#Lat vs long plot
ggplot(data, aes(x = log_longitude, y = log_latitude)) +
  geom_point(color = 'blue') +
  labs(title = "Latitude vs Longitude", x = "Log(Longitude)", y = "Log(Latitude)") +
  theme_minimal()


#Correlation plot
selected_vars <- data[, c("Latitude", "Longitude", "Altitude","Temperature", "Duration")]
ggpairs(selected_vars, title = "Correlation Plot")



###Adjust data to be usable for specific Date plotting
data$time <- as.POSIXct(data$GMT.Time, format="%d.%m.%Y %H:%M:%S")
data$month <- format(data$time, "%m")
july_data <- subset(data, month == "07")


# Plot lat over time
ggplot(july_data, aes(x = time, y = Latitude)) +
  geom_line() +
  geom_point() +
  labs(x = "Time", y = "Latitude") +
  theme_minimal()

# Plot Long over time
ggplot(july_data, aes(x = time, y = Longitude)) +
  geom_line() +
  geom_point() +
  labs(x = "Time", y = "Longitude") +
  theme_minimal()


##Plotting each day separately
data$date <- as.Date(data$time)
july_data <- subset(data, month == "07") #Month chosen = July

#Plot whatever data you want over the month July (in this case Temperature, you can choose lat or Long)
ggplot(july_data, aes(x = time, y = Temperature, group = date, color = as.factor(date))) +
  geom_line() +
  geom_point() +
  labs(x = "Time", color = "Date") +
  theme_minimal()




###temp plot idea
ggplot(data, aes(x = Latitude, y = Longitude)) +
  geom_point(color = 'blue') +
  geom_path(aes(group = 1)) +  # Connect the points with lines (movement path)
  labs(title = "Animal Movement: Altitude vs Latitude", x = "Latitude", y = "Altitude") +
  theme_minimal()


###temp lot Idead 2
july_data <- july_data %>%
  group_by(date) %>%
  mutate(time_since_start = as.numeric(difftime(time, min(time), units = "secs"))) %>%
  ungroup()

# Plot x over time for each day in July
ggplot(july_data, aes(x = time_since_start, y = Longitude, group = date, color = as.factor(date))) +
  geom_line() +
  geom_point() +
  labs(title = "Plot of X over Time", x = "Time", y = "X", color = "Date") +
  theme_minimal()