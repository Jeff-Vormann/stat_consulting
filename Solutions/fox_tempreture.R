# Imports
library(moveHMM)
library(gamlss)
library(gamlss.dist)
library(dplyr)
library(ggplot2)
library(tidyr)

#Fox Thomas
data <- read.csv("Thomas.csv",header=TRUE, sep = ",", skip = 4)

#add L2_Norm
data$L2_norm <- sqrt(data$X^2 + data$Y^2)

#Convert Time data
data$time <- as.POSIXct(data$GMT.Time, format = "%d.%m.%Y %H:%M:%S")
#data <- subset(data, format(time, "%m") == "01")

#Dummy coordinates for data Prep
data$cum_x <- cumsum(data$L2_norm)
data$cum_y <- 0  

#Prep Data
hmm_data <- prepData(data, type = "UTM", coordNames = c("cum_x", "cum_y"))

#Starting Parameters
mu0 <- c(7,0.1) 
sigma0 <- c(3 ,0.1) 
zeromass0 <- c(0.01,0.99) 
stepPar0 <- c(mu0,sigma0,zeromass0)


#Fit Model
hmm_model <- fitHMM(
  data = hmm_data,
  nbStates = 2,
  stepDist = "gamma",
  angleDist = "none",
  stepPar0 = stepPar0,
  formula = ~ Temperature..C.,  # Include temperature as a covariate
  nlmPar = list(iterlim = 1000)
)
#summary(hmm_model)

#Get States
states <- viterbi(hmm_model)
hmm_data$state <- states
data$state <- states

print(ggplot(data, aes(x = time, y = L2_norm, color = factor(state))) +  
        geom_point() +
        labs(color = "Zustand", x = "Zeit", y = "Schrittweite") +
        theme_minimal()
      
)

#Print Distribution values
step_params <- hmm_model$mle$stepPar
print(step_params)

nbStates <- hmm_model$nbStates
x_vals <- seq(min(data$L2_norm), max(data$L2_norm), length.out = 1000)


gamma_density <- function(x, shape, scale) {
  dgamma(x, shape = shape, scale = scale)
}


#Plot Data with Distribution
mu1 <- step_params[1]
sigma1 <- step_params[2]
shape1 <- (mu1 / sigma1)^2
scale1 <- (sigma1^2) / mu1

mu2 <- step_params[3]
sigma2 <- step_params[4]
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