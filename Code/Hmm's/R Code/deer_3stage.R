# Pakete installieren (falls noch nicht installiert)
#install.packages("moveHMM")
#install.packages("gamlss")
#install.packages("gamlss.dist")
#install.packages("tidyr")
library(moveHMM)
library(gamlss)
library(gamlss.dist)
library(dplyr)
library(ggplot2)
library(tidyr)

data <- read.csv("../Data/red_deer/h46_act_2020.csv",header=FALSE, sep = ";", skip = 1)
data$L2_norm <- sqrt(data$V8^2 + data$V9^2)
data$L2_norm <- data$V8 + data$V9
data$time <- format(as.POSIXct(data$V3, format="%Y-%m-%dT%H:%M:%OSZ"), "%Y-%m-%d %H:%M:%S %Z")

# Dummy-Koordinaten erstellen
data$cum_x <- cumsum(data$L2_norm)
data$cum_y <- 0  

# kürze daten
data_short <- data %>%
  select(time, cum_x, cum_y)
hmm_data <- prepData(data_short, type = "UTM", coordNames = c("cum_x", "cum_y"))


#mu0 <- c(109, 10, 0.1) 
#sigma0 <- c(35, 9, 0.1) 
#zeromass0 <- c(0.1, 0.2, 0.9) 
#stepPar0 <- c(mu0, sigma0, zeromass0)

mu0 <- c(110, 17, 0.1) 
sigma0 <- c(48, 12, 0.1) 
zeromass0 <- c(0.1, 0.2, 0.9) 
stepPar0 <- c(mu0, sigma0, zeromass0)


# HMM-Modell anpassen
hmm_model <- fitHMM(
  data = hmm_data,
  nbStates = 3,
  stepDist = "gamma",
  angleDist = "none", 
  stepPar0 = stepPar0
)

dateiname="deer_3stage"
#########################################################
readRDS(hmm_model, file = paste0(dateiname, ".rds"))
########################################################

# Zustände zuweisen
states <- viterbi(hmm_model)
hmm_data$state <- states
data$state <- states
step_params <- hmm_model$mle$stepPar


mu1 <- step_params[1]  
sigma1 <- step_params[2]  
mu2 <- step_params[4]  
sigma2 <- step_params[5] 
mu3 <- step_params[7]  
sigma3 <- step_params[8]


shape1 <- ifelse(sigma1 != 0, (mu1 / sigma1)^2, 1) 
scale1 <- ifelse(sigma1 != 0, (sigma1^2) / mu1, 1)
shape2 <- ifelse(sigma2 != 0, (mu2 / sigma2)^2, 1) 
scale2 <- ifelse(sigma2 != 0, (sigma2^2) / mu2, 1)
shape3 <- ifelse(sigma3 != 0, (mu3 / sigma3)^2, 1) 
scale3 <- ifelse(sigma3 != 0, (sigma3^2) / mu3, 1)

x_vals <- seq(min(data$L2_norm), max(data$L2_norm), length.out = 1000)
total_count <- nrow(data)
state_counts <- table(data$state)
state_proportions <- prop.table(state_counts) 

print(state_proportions)
density_data <- data.frame(
  x = x_vals,
  State1 = dgamma(x_vals, shape = shape1, scale = scale1) * state_proportions[1] * (1 - zeromass1),
  State2 = dgamma(x_vals, shape = shape2, scale = scale2) * state_proportions[2] * (1 - zeromass2),
  State3 = dgamma(x_vals, shape = shape3, scale = scale3) * state_proportions[3] * (1 - zeromass3),
  State5 = dgamma(x_vals, shape = shape1, scale = scale1) * state_proportions[1] * (1 - zeromass1)  +
    dgamma(x_vals, shape = shape2, scale = scale2) * state_proportions[2] * (1 - zeromass2) +
    dgamma(x_vals, shape = shape3, scale = scale3) * state_proportions[3]* (1 - zeromass3)
)

density_long <- density_data %>%
  pivot_longer(
    cols = starts_with("State"),
    names_to = "State",
    names_prefix = "State",
    values_to = "Density"
  )

stillstand_count <- sum(data$state == 3)
stillstand_proportion <- stillstand_count / total_count

#Histogram
hist_with_density <- ggplot(data, aes(x = L2_norm)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightblue", color = "black", alpha = 0.6) +
  geom_line(data = density_long, aes(x = x, y = Density, color = State), linewidth = 1) +
  labs(
    x = "L2-Norm",
    y = "Dichte",
    color = "Zustand"
  ) +
  theme_minimal() +
  #######################Ausschnitt####################
coord_cartesian(ylim = c(0, 0.025), xlim = c(0, 400)) +  
  #####################################################
geom_vline(xintercept = 0, color = "yellow", linetype = "dashed", size = 1) +
  annotate("text", x = 5, y = 0.095, label = paste0("Stillstand: ", round(stillstand_proportion * 100, 2), "%"), 
           hjust = 0, color = "yellow", size = 5)
print(hist_with_density)


# Zustandsverteilung
print(table(data$state))
print(hmm_model)
print(step_params)
scatter_plot=ggplot(data, aes(x = time, y = L2_norm, color = factor(state))) +  
        geom_point() +
        labs(color = "Zustand", x = "Zeit", y = "Schrittweite") +
        theme_minimal()
      
print(scatter_plot)
state_counts <- table(data$state)
state_proportions <- prop.table(state_counts) 
print(hmm_model)
print(state_proportions)
#########################################################
saveRDS(hmm_model, file = paste0(dateiname, ".rds"))
########################################################
#          state 1     state 2      state 3
#mean      1.097455e+02 11.92129060 1.744121e-02
#sd        4.832025e+01 11.04930894 1.247151e+04
#zero-mass 5.397298e-04  0.05338193 1.000000e+00

ggsave(
  filename = paste0("scatter_plot_", dateiname, ".png"),
  plot = scatter_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = paste0("hist_", dateiname, ".png"),
  plot = hist_with_density,
  width = 8,
  height = 6,
  dpi = 300
)

