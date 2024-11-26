library(depmixS4)
library(geosphere)

data <- read.csv("GPS_91037_20200113_Thomas.csv",header=TRUE, sep = ",", skip = 4)


###Prepossessing for later use

#Log data for better visualization
data$log_lat <- log(data$Latitude)
data$log_long <- log(data$Longitude)

#create Step length
distances <- distGeo(data[-nrow(data), c("Longitude", "Latitude")], 
                     data[-1, c("Longitude", "Latitude")])
data$distance <- c(NA, distances)
step_lengths <- data$distance

#create Bearings
bearings <- bearing(data[-nrow(data), c("Longitude", "Latitude")], 
                    data[-1, c("Longitude", "Latitude")])
data$bearing <- c(NA, bearings)
bearings <- data$bearing



###HMM

#Model with arbitrary chosen 3 hidden states
model <- depmix(response = list(step_lengths ~ 1, bearings ~ 1),
                data = data, nstates = 2, family = list(gaussian(), gaussian()))

#Fitting
fit_model <- fit(model)
summary(fit_model)

#extract predicted States
posterior_probs <- posterior(fit_model)
data$predicted_state <- posterior_probs[, "state"]


#Plot predicted states in Lat vs Long
ggplot(data, aes(x = log_long, y = log_lat, color = as.factor(predicted_state))) +
  geom_point() +
  labs(
       x = "Longitude",
       y = "Latitude",
       color = "Predicted State") +
  theme_minimal()




###Temp Idea
# Plot step lengths by state
ggplot(data, aes(x = predicted_state, y = distance, fill = as.factor(predicted_state))) +
  geom_boxplot() +
  labs(title = "Step Lengths by Predicted Hidden State",
       x = "Predicted State",
       y = "Step Length (Distance)",
       fill = "Predicted State") +
  theme_minimal()

# Plot bearings by state
ggplot(data, aes(x = predicted_state, y = bearing, fill = as.factor(predicted_state))) +
  geom_boxplot() +
  labs(title = "Bearings by Predicted Hidden State",
       x = "Predicted State",
       y = "Bearing",
       fill = "Predicted State") +
  theme_minimal()