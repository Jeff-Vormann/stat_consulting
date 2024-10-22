library(ggplot2)
library(GGally)

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

#temp plot idea
ggplot(data, aes(x = Latitude, y = Longitude)) +
  geom_point(color = 'blue') +
  geom_path(aes(group = 1)) +  # Connect the points with lines (movement path)
  labs(title = "Animal Movement: Altitude vs Latitude", x = "Latitude", y = "Altitude") +
  theme_minimal()
