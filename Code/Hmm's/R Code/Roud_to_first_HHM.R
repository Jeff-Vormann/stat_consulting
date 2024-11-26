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

data <- read.csv("../Data/Fox/Act_91037_20200113_Thomas.csv",header=TRUE, sep = ",", skip = 4)
head(data)
#add L2_Norm
data$L2_norm <- sqrt(data$X^2 + data$Y^2)

data$time <- as.POSIXct(data$GMT.Time, format = "%d.%m.%Y %H:%M:%S")

#erstelle dummy koardinaten
data$cum_x <- cumsum(data$L2_norm)
data$cum_y <- 0  

hmm_data <- prepData(data, type = "UTM", coordNames = c("cum_x", "cum_y"))

mu0 <- c(0.1,5) 
sigma0 <- c(0.1,0.1) 
zeromass0 <- c(1,0.05) 
stepPar0 <- c(mu0,sigma0,zeromass0)


hmm_model <- fitHMM(
  data = hmm_data,
  nbStates = 2,
  stepDist = "gamma",
  angleDist = "none", 
  stepPar0 = stepPar0,
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
  theme_minimal()


print(hist_with_density)


print(hmm_model)
print( table(data$state))
print(step_params)
