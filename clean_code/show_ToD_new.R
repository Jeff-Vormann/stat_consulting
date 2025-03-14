library(yaml)
library(dplyr)
library(ggplot2)
library(gridExtra)

config_path <- "config.yaml"
config <- yaml::yaml.load_file(config_path)


hmm_data$time <- ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", hmm_data$time),
                   paste0(hmm_data$time, " 00:00:00"),
                   hmm_data$time)
# 3) Zeitspalte in POSIXct umwandeln (ggf. anpassen je nach Datenformat)
hmm_data$time <- as.POSIXct(hmm_data$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# 4) Stunde des Tages extrahieren (inkl. Minutenanteil => hh + mm/60)
hmm_data$hourOfDay <- as.numeric(format(hmm_data$time, "%H")) + 
  as.numeric(format(hmm_data$time, "%M")) / 60

# Falls der Zustand in hmm_data$state numerisch ist, stell sicher, dass es Werte 1,2,3 sind
# z.B. table(hmm_data$state)

# -- Plot für Zustand 1 (oben rechts) --
plot_state1 <- ggplot(
  filter(hmm_data, state == 1), 
  aes(x = hourOfDay, y = preprocessed)
) +
  geom_point(alpha = 0.05, color = "green") +
  labs(x = "Time of Day", y = "Activity") +
  theme_minimal()

# -- Plot für Zustand 2 (oben links) --
plot_state2 <- ggplot(
  filter(hmm_data, state == 2), 
  aes(x = hourOfDay, y = preprocessed)
) +
  geom_point(alpha = 0.05, color = "blue") +
  labs(x = "Time of Day", y = "Activity") +
  theme_minimal()

# -- Plot für Zustand 3 (unten links) --
plot_state3 <- ggplot(
  filter(hmm_data, state == 3), 
  aes(x = hourOfDay, y = preprocessed)
) +
  geom_point(alpha = 0.05, color = "red") +
  labs(x = "Time of Day", y = "Activity") +
  theme_minimal()

# -- Plot mit ALLEN Zuständen (unten rechts) --
plot_all <- ggplot(
  hmm_data, 
  aes(x = hourOfDay, y = preprocessed, color = factor(state))
) +
  geom_point(alpha = 0.05) +
  scale_color_manual(
    values = c("1" = "green", "2" = "blue", "3" = "red"),
    name = "State"
  ) +
  labs(x = "Time of Day", y = "Activity") +
  theme_minimal()

# 5) Anordnung (2×2):
#   oben links:  plot_state2  (Zustand 2)
#   oben rechts: plot_state1  (Zustand 1)
#   unten links: plot_state3  (Zustand 3)
#   unten rechts: plot_all    (alle Zustände)
grid_plot <- grid.arrange(plot_state2, plot_state1, plot_state3, plot_all,
                          nrow = 2, ncol = 2)

# 6) Anzeigen und Speichern
print(grid_plot)
ggsave(
  filename = paste0("clean_code/Rds/ToD/", config$dataset_name, "_ToD_4plots.png"),
  plot = grid_plot,
  width = 16,
  height = 9,
  dpi = 400,
  bg = "white"
)

####################################################
library(ggplot2)
library(dplyr)

# Annahme: hmm_data ist bereits geladen und enthält die Spalten 'hourOfDay', 'preprocessed' und 'state'

# Dichteplot erstellen
density_plot <- ggplot(hmm_data, aes(x = hourOfDay, color = factor(state))) +
  geom_density(adjust = 0.5, size = 3) +  # 'adjust' steuert die Bandbreite, 'size' die Linienstärke
  scale_color_manual(
    values = c("1" = "green", "2" = "blue", "3" = "red"),
    name = "State"
  ) +
  labs(
    x = "Time of Day",
    y = "Density"
  ) +
  theme_minimal()

# Plot anzeigen
print(density_plot)

# Plot speichern
ggsave(
  filename = paste0("clean_code/Rds/ToD/", config$dataset_name, "_DensityPlot.png"),
  plot = density_plot,
  width = 16,
  height = 9,
  dpi = 100,
  bg = "white"
)
