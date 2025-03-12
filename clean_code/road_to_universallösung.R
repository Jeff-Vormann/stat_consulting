library(moveHMM)
library(gamlss)
library(gamlss.dist)
library(dplyr)
library(ggplot2)
library(tidyr)
library(yaml)

# 1) Externes Preprocessing-Skript einbinden (dies liest config.yaml intern)
source("clean_code/data_preprocessing.R", local = FALSE)

# 2) Vorverarbeitete Daten holen (alles erfolgt über config.yaml)
hmm_data <- prepareHMMData()

# 3) Initiale Parameter aus der Config holen (config wird intern geladen)
source("clean_code/init_params.R", local = FALSE)
init_params <- getInitialParameters()
print("--init_params--")
print(init_params)

# 4) Konfigurationsdatei laden, um weitere Parameter (wie nbStates, dataset_name, preprocessing) zu verwenden
config <- yaml::yaml.load_file("config.yaml")


# 5) Eindeutigen Dateinamen für das RDS erzeugen:
if (config$temperature == "None"){
  rds_name <- paste0("clean_code/Rds/", 
                   config$dataset_name, "_", 
                   config$nbStates, "states_", 
                   config$preprocessing, ".rds")
} else {
  rds_name <- paste0("clean_code/Rds/", 
                     config$dataset_name, "_", 
                     config$nbStates, "states_", 
                     config$preprocessing, "_temperature", ".rds")
}

# 6) HMM-Training nur durchführen, wenn das RDS nicht existiert:
if (file.exists(rds_name)) {
  cat("RDS existiert bereits. Lade Modell:", rds_name, "\n")
  hmm_model <- readRDS(rds_name)
} else if(config$temperature == "None"){
  # HMM-Modell anpassen – nbStates aus config verwenden
  hmm_model <- fitHMM(
    data = hmm_data,
    nbStates = config$nbStates,
    stepDist = "gamma",
    angleDist = "none", 
    stepPar0 = init_params
  )
  # Modell speichern
  saveRDS(hmm_model, file = rds_name)
  cat("Neues Modell trainiert und gespeichert als:", rds_name, "\n")
} else {
  # HMM-Modell anpassen – nbStates aus config verwenden
  hmm_model <- fitHMM(
    data = hmm_data,
    nbStates = config$nbStates,
    stepDist = "gamma",
    angleDist = "none", 
    stepPar0 = init_params,
    formula = ~ temperature
  )
  # Modell speichern
  saveRDS(hmm_model, file = rds_name)
  cat("Neues Modell trainiert und gespeichert als:", rds_name, "\n")
}

print(hmm_model)

# Zustände zuweisen
states <- viterbi(hmm_model)
hmm_data$state <- states

# Für weitere Auswertungen: (Annahme: Die ursprünglichen Daten liegen in "data")
# Hier müsste ggf. der originale Datensatz geladen werden, falls nicht bereits vorhanden.
# data$state <- states

step_params <- hmm_model$mle$stepPar

# (Beispielhafte Weiterverarbeitung: Berechnung von gamma-Parametern und Plotting)
mu1 <- step_params[1]  
sigma1 <- step_params[2]
zeromass1 <- step_params[3]
mu2 <- step_params[4]  
sigma2 <- step_params[5]
zeromass2 <- step_params[6]
if (config$nbStates >= 3) {
  mu3 <- step_params[7]  
  sigma3 <- step_params[8] 
  zeromass3 <- step_params[9]
}
if (config$nbStates >= 4) {
  mu4 <- step_params[10] 
  sigma4 <- step_params[11] 
  zeromass4 <- step_params[12]
}

# Beispiel: Berechnung der gamma-Verteilungsparameter
shape1 <- ifelse(sigma1 != 0, (mu1 / sigma1)^2, 1) 
scale1 <- ifelse(sigma1 != 0, (sigma1^2) / mu1, 1)
if (config$nbStates >= 2) {
  shape2 <- ifelse(sigma2 != 0, (mu2 / sigma2)^2, 1) 
  scale2 <- ifelse(sigma2 != 0, (sigma2^2) / mu2, 1)
}
if (config$nbStates >= 3) {
  shape3 <- ifelse(sigma3 != 0, (mu3 / sigma3)^2, 1) 
  scale3 <- ifelse(sigma3 != 0, (sigma3^2) / mu3, 1)
}
if (config$nbStates >= 4) {
  shape4 <- ifelse(sigma4 != 0, (mu4 / sigma4)^2, 1) 
  scale4 <- ifelse(sigma4 != 0, (sigma4^2) / mu4, 1)
}

# Beispiel für ein Histogramm mit Dichtekurven:
x_vals <- seq(min(hmm_data$step, na.rm = TRUE), max(hmm_data$step, na.rm = TRUE), length.out = 1000)
total_count <- nrow(hmm_data)
state_counts <- table(hmm_data$state)
state_proportions <- prop.table(state_counts) 
print(state_proportions)
density_data <- data.frame(
  x = x_vals,
  State1 = dgamma(x_vals, shape = shape1, scale = scale1) * state_proportions[1] * (1 - zeromass1),
  State2 = dgamma(x_vals, shape = shape2, scale = scale2) * state_proportions[2] * (1 - zeromass2),
  State3 = if (config$nbStates >= 3)
    dgamma(x_vals, shape = shape3, scale = scale3) * state_proportions[3] * (1 - zeromass3) else NA,
  State4 = if (config$nbStates >= 4)
    dgamma(x_vals, shape = shape4, scale = scale4) * state_proportions[4] * (1 - zeromass4) else NA
)

# Berechnung der Summen-Dichte als Summe aller Zustandsdichten (NA werden dabei ignoriert)
density_data$Summe <- rowSums(density_data[ , -1], na.rm = TRUE)

# Umformen in langes Format für die Zustandsdichten
density_long <- density_data %>%
  pivot_longer(
    cols = starts_with("State"),
    names_to = "State",
    names_prefix = "State",
    values_to = "Density"
  )

# Bestimme das obere 99%-Perzentil als X-Limit mit einem kleinen Puffer
x_upper <- quantile(hmm_data$step, probs = 0.99, na.rm = TRUE)
x_upper <- x_upper * 1.05

# Plot: Histogramm + Dichtekurven
hist_with_density <- ggplot(hmm_data, aes(x = step)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightblue", alpha = 0.6) +
  # Zuerst die summierte Dichte (schwarz, als "ALL") im Hintergrund zeichnen:
  geom_line(data = density_data, aes(x = x, y = Summe, color = "Summe"), linewidth = 1) +
  # Dann die einzelnen Zustandsdichten darüber:
  geom_line(data = density_long, aes(x = x, y = Density, color = State), linewidth = 1) +
  labs(
    x = config$preprocessing,
    y = "Density",
    color = "State"
  ) +
  # Manuelle Farbanpassung: "Summe" soll schwarz sein
  scale_color_manual(values = c("All" = "black", "1" = "green", "2" = "blue", "3" = "red", "4" = "orange")) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.1), xlim = c(0, x_upper)) +
  geom_vline(xintercept = 0, color = "yellow", linetype = "dashed", size = 1)

print(hist_with_density)

ggsave(
  filename = paste0(rds_name,"_hist", ".png"),
  plot = hist_with_density,
  width = 16,
  height = 9,
  dpi = 100
)

# Scatterplot (Beispiel)
scatter_plot <- ggplot(hmm_data, aes(x = time, y = step, color = factor(state))) +  
  scale_color_manual(values = c("Summe" = "black", "1" = "green", "2" = "blue", "3" = "red", "4" = "orange")) +
  geom_point(size = 1,alpha = 0.2) +
  labs(colour= "State", x = "Time", y = "Activity") +
  theme_minimal()

print(scatter_plot)



ggsave(
  filename = paste0(rds_name,"_scatter_plot", ".png"),
  plot = scatter_plot,
  width = 16,
  height = 9,
  dpi = 100
)
print(rds_name)