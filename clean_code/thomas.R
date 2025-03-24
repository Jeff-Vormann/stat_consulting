
library(gamlss)
library(gamlss.dist)
library(dplyr)
library(ggplot2)
library(tidyr)
library(yaml)
library(momentuHMM)
#  Externes Preprocessing-Skript einbinden (dies liest config.yaml intern)
source("clean_code/data_preprocessing.R", local = FALSE)

#  Vorverarbeitete Daten holen (alles erfolgt über config.yaml)
hmm_data <- prepareHMMData()
#  Initiale Parameter aus der Config holen (config wird intern geladen)
source("clean_code/init_params.R", local = FALSE)
init_params <- getInitialParameters()
print(hmm_data)

config <- yaml::yaml.load_file("config.yaml")


#  Eindeutigen Dateinamen für das RDS erzeugen:

  rds_name <- paste0("clean_code/Rds/covariates/",
                     "M_",
                     config$dataset_name, "_", 
                     config$nbStates, "states_", 
                     config$preprocessing, 
                     "_T=",config$temperature,
                     "_F=",config$formula,
                     "_Mu=",config$emission_mean,
                      ".rds")


#  HMM-Training nur durchführen, wenn das RDS nicht existiert:
if (file.exists(rds_name)) {
  cat("RDS existiert bereits. Lade Modell:", rds_name, "\n")
  hmm_model <- readRDS(rds_name)
} else {
  # beta muss auch init parameter damit das model konvergiert
  beta0= c(-3.093331,0,0,0,0, -1.979652,0,0,0,0)
  
  hmm_model <- momentuHMM::fitHMM(
    data = hmm_data,
    nbStates = config$nbStates,
    dist = list(step = "gamma"),
    formula = as.formula(config$formula),
    Par0 = list(step = t(init_params)),
    DM = list(step = list(mean = as.formula(config$emission_mean), sd = ~1, zeromass = ~1)),
    nlmPar = list(print.level = 2),
    beta0 <- matrix(beta0, nrow = 5, ncol = 2)
  )
  # Modell speichern
  #saveRDS(hmm_model, file = rds_name)
  #cat("Neues Modell trainiert und gespeichert als:", rds_name, "\n")
}

  AIC(hmm_model)
  
  #Nachvertarbeitung der HMM
  hmm_data$state <- momentuHMM::viterbi(hmm_model)
  sm <- as.matrix(hmm_model$CIreal$step$est)
  state_proportions <- prop.table(table(hmm_data$state)) 
  print(state_proportions)
  
  #################################################################################
  #########Erstelle Plot mit der Dichte############################################
  #################################################################################
x_vals <- seq(min(hmm_data$step, na.rm = TRUE), max(hmm_data$step, na.rm = TRUE), length.out = 1000)
density_data <- data.frame(x  = x_vals)

for(i in 1:config$nbStates) {
  density_data[[paste0("State", i)]] <- dgamma(x_vals,
                                               shape = (sm["mean", i] / sm["sd", i])^2,
                                               scale = (sm["sd", i]^2) / sm["mean", i]
  ) * state_proportions[i] * (1 - sm["zeromass", i])
}

density_data$Summe <- rowSums(density_data[ , -1], na.rm = TRUE)

density_long <- density_data %>%
  pivot_longer(
    cols = starts_with("State"),
    names_to = "State",
    names_prefix = "State",
    values_to = "Density"
  )

x_upper <- quantile(hmm_data$step, probs = 0.99, na.rm = TRUE)
x_upper <- x_upper * 1.05
if(config$nbStates==3)
  {max_in_hist=0.01}else{
    max_in_hist=0.1
  }
# Plot: Histogramm + Dichtekurven
hist_with_density <- ggplot(hmm_data, aes(x = step)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightblue", alpha = 0.6) +
  geom_line(data = density_data, aes(x = x, y = Summe, color = "Summe"), linewidth = 1) +
  geom_line(data = density_long, aes(x = x, y = Density, color = State), linewidth = 1) +
  labs(
    x = config$preprocessing,
    y = "Density",
    color = "State"
  ) +
  scale_color_manual(values = c("All" = "black", "1" = "green", "2" = "blue", "3" = "red", "4" = "orange")) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, max_in_hist), xlim = c(0, x_upper)) +
  geom_vline(xintercept = 0, color = "yellow", linetype = "dashed", size = 1)

print(hist_with_density)

#ggsave(
 # filename = paste0(rds_name,"_hist", ".png"),
  #plot = hist_with_density,
 # width = 16,
 # height = 9,
 # dpi = 1000,
 # bg = "white"
#)

#################################################################################
#########Erstelle Scatter Plot############################################
#################################################################################
scatter_plot <- ggplot(hmm_data, aes(x = time, y = step, color = factor(state))) +  
  scale_color_manual(values = c("Summe" = "black", "1" = "green", "2" = "blue", "3" = "red", "4" = "orange")) +
  geom_point(size = 1,alpha = 0.2) +
  labs(colour= "State", x = "Time", y = "Activity") +
  theme_minimal()

print(scatter_plot)

#ggsave(
 # filename = paste0(rds_name,"_scatter_plot", ".png"),
 # plot = scatter_plot,
 # width = 16,
 # height = 9,
 # dpi = 1000,
 # bg = "white"
#)


#################################################################################
#########Uebersicht der Ergebisse in Zahlen############################################
#################################################################################
print(rds_name)

#readRDS(hmm_model, file = rds_name)
sink("thomas.txt")
source("clean_code/transition_effects.R", local = FALSE)
source("clean_code/impact_mean.R", local = FALSE)
print(calculate_state_effects("mean_2"))

stationary_effect_all_2state()

print(calculate_transition_effects_for_2_states())
print(hmm_model)

sink()
