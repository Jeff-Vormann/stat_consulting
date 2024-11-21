library(moveHMM)
library(gamlss)
library(gamlss.dist)
library(dplyr)
library(ggplot2)
library(tidyr)
data <- read.csv("../Data/Gamsbock/ACT_Collar46723_20221104175921_fromVectronic_exportedviaGPSPlusX.csv", 
                 header = FALSE, sep = ";", skip = 1)
data$L2_norm <- sqrt(data$V12^2 + data$V13^2)
data$time <- as.POSIXct(paste(data$V3, data$V4), format = "%d.%m.%Y %H:%M:%S")

# Dummy-Koordinaten erstellen
data$cum_x <- cumsum(data$L2_norm)
data$cum_y <- 0  

# Daten vorbereiten für moveHMM
hmm_data <- prepData(data, type = "UTM", coordNames = c("cum_x", "cum_y"))

mu0 <- c(131, 100,0.01, 9)          
sigma0 <- c(10, 30, 0.01, 9)         
zeromass0 <- c(0, 0.1, 1, 0.0) 
stepPar0 <- c(mu0, sigma0, zeromass0)

# HMM-Modell anpassen
hmm_model <- fitHMM(
  data = hmm_data,
  nbStates = 4,
  stepDist = "gamma",
  angleDist = "none", 
  stepPar0 = stepPar0
)

dateiname="gams_4stage"
#########################################################
#readRDS(hmm_model, file = paste0(dateiname, ".rds"))
########################################################


# Zustände zuweisen
states <- viterbi(hmm_model)
hmm_data$state <- states
data$state <- states
step_params <- hmm_model$mle$stepPar


mu1 <- step_params[1]  
sigma1 <- step_params[2]
zeromass1 <- step_parms[3]
mu2 <- step_params[4]  
sigma2 <- step_params[5]
zeromass2 <- step_parms[6]
mu3 <- step_params[7]  
sigma3 <- step_params[8] 
zeromass3 <- step_parms[6]
mu4 <- step_params[10] 
sigma4 <- step_params[11] 
zeromass4 <- step_parms[12]


shape1 <- ifelse(sigma1 != 0, (mu1 / sigma1)^2, 1) 
scale1 <- ifelse(sigma1 != 0, (sigma1^2) / mu1, 1)
shape2 <- ifelse(sigma2 != 0, (mu2 / sigma2)^2, 1) 
scale2 <- ifelse(sigma2 != 0, (sigma2^2) / mu2, 1)
shape3 <- ifelse(sigma3 != 0, (mu3 / sigma3)^2, 1) 
scale3 <- ifelse(sigma3 != 0, (sigma3^2) / mu3, 1)
shape4 <- ifelse(sigma4 != 0, (mu4 / sigma4)^2, 1) 
scale4 <- ifelse(sigma4 != 0, (sigma4^2) / mu4, 1)

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
  State4 = dgamma(x_vals, shape = shape4, scale = scale4) * state_proportions[4] * (1 - zeromass4),
  State5 = dgamma(x_vals, shape = shape1, scale = scale1) * state_proportions[1] * (1 - zeromass1)  +
    dgamma(x_vals, shape = shape2, scale = scale2) * state_proportions[2] * (1 - zeromass2) +
    dgamma(x_vals, shape = shape3, scale = scale3) * state_proportions[3]* (1 - zeromass3)+
    dgamma(x_vals, shape = shape4, scale = scale4) * state_proportions[4] * (1 - zeromass4)
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
coord_cartesian(ylim = c(0, 0.025), xlim = c(0, 200)) +  
  #####################################################
geom_vline(xintercept = 0, color = "yellow", linetype = "dashed", size = 1) +
  annotate("text", x = 5, y = 0.095, label = paste0("Stillstand: ", round(stillstand_proportion * 100, 2), "%"), 
           hjust = 0, color = "yellow", size = 5)
print(hist_with_density)


# Zustandsverteilung
print(table(data$state))
print(hmm_model)
print(step_params)
scatter_plot <- ggplot(data, aes(x = time, y = L2_norm, color = factor(state))) +  
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

#               state 1      state 2 state 3      state 4
#mean      1.314315e+02 9.117600e+01    0.01 8.931981e+00
#sd        2.111576e+01 3.943806e+01    0.01 9.258507e+00
#zero-mass 1.001262e-08 1.001503e-08    1.00 1.000573e-08

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


