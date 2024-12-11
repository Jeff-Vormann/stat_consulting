# Pakete installieren (falls noch nicht installiert)
#install.packages("moveHMM")
#install.packages("gamlss")
#install.packages("gamlss.dist")
#install.packages("tidyr")



# Pakete laden
library(moveHMM)
library(gamlss)
library(gamlss.dist)
library(dplyr)
library(ggplot2)
library(tidyr)
#Fox Thomas
library(dplyr)

df2 <- read.table("order_126368_data.txt", stringsAsFactors = FALSE, skip = 2,header = TRUE)
df1 <- read.csv("Thomas.csv", header = TRUE, sep = ",", skip = 4)


# Convert the column
df1$time <- format(as.POSIXct(df1$GMT.Time, format = "%d.%m.%Y %H:%M"), "%Y%m%d%H")

# Define the range
start_time <- 2019050305
end_time <- 2020011307

# Filter the data in df2
filtered_df2 <- df2 %>%
  filter(as.numeric(time) >= start_time & as.numeric(time) <= end_time)

# Convert GMT.Time in df1 to numeric time format
df1 <- df1 %>%
  mutate(time = format(as.POSIXct(GMT.Time, format = "%d.%m.%Y %H:%M"), "%Y%m%d%H"),
         time = as.numeric(time))  # Ensure 'time' is numeric


# Perform a left join to add the temperature column from filtered_df2 to df1
df1 <- df1 %>%
  left_join(filtered_df2 %>% select(time, tre200h0), by = "time")

# Filter the data in df2
filtered_df2 <- df2 %>%
  filter(as.numeric(time) >= 2019050305 & as.numeric(time) <= 2020011307)

# Perform a left join to add the temperature column from filtered_df2 to df1
df1 <- df1 %>%
  left_join(filtered_df2 %>% select(time, tre200h0), by = "time")


data <- df1
#add L2_Norm
data$L2_norm <- sqrt(data$X^2 + data$Y^2)

data$time <- as.POSIXct(data$GMT.Time, format = "%d.%m.%Y %H:%M:%S")
#data <- subset(data, format(time, "%m") == "01")

#erstelle dummy koardinaten
data$cum_x <- cumsum(data$L2_norm)
data$cum_y <- 0  

hmm_data <- prepData(data, type = "UTM", coordNames = c("cum_x", "cum_y"))
print(hmm_data)

mu0 <- c(7,0.1) 
sigma0 <- c(3 ,0.1) 
zeromass0 <- c(0.01,0.99) 
stepPar0 <- c(mu0,sigma0,zeromass0)

head(data)
hmm_model <- fitHMM(
  data = hmm_data,
  nbStates = 2,
  stepDist = "gamma",
  angleDist = "none", 
  stepPar0 = stepPar0,
  #formula = ~ Temperature..C.,
  formula = ~ tre200h0.x,
  nlmPar = list(iterlim = 1000)  
)


summary(hmm_model)

states <- viterbi(hmm_model)
hmm_data$state <- states
data$state <- states



print(ggplot(data, aes(x = time, y = L2_norm, color = factor(state))) +  
        geom_point() +
        labs(color = "Zustand", x = "Zeit", y = "Schrittweite") +
        theme_minimal()
      
)

step_params <- hmm_model$mle$stepPar
print(step_params)
nbStates <- hmm_model$nbStates


x_vals <- seq(min(data$L2_norm), max(data$L2_norm), length.out = 1000)


gamma_density <- function(x, shape, scale) {
  dgamma(x, shape = shape, scale = scale)
}


mu1 <- step_params[1]
sigma1 <- step_params[2]
shape1 <- (mu1 / sigma1)^2
scale1 <- (sigma1^2) / mu1

mu2 <- step_params[4]
sigma2 <- step_params[5]
shape2 <- (mu2 / sigma2)^2
scale2 <- (sigma2^2) / mu2


density_data <- data.frame(
  x = x_vals,
  State1 = gamma_density(x_vals, shape1, scale1),
  State2 = gamma_density(x_vals, shape2, scale2)
)


density_long <- pivot_longer(
  data = density_data,
  cols = starts_with("State"),
  names_to = "State",
  names_prefix = "State",
  values_to = "Density"
)


hist_with_density <- ggplot(data, aes(x = L2_norm)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.6) +
  geom_line(data = density_long, aes(x = x, y = Density, color = State), linewidth = 1) +
  labs(x = "Schrittweite",
       y = "Dichte",
       color = "Zustand") +
  theme_minimal() +coord_cartesian(xlim = c(0, 15))


print(hist_with_density)


print(hmm_model)
print( table(data$state))
print(step_params)