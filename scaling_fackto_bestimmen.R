#install.packages("gridExtra")
library(ggplot2)
library(gridExtra)

# Mapping von Datensatznamen zu CSV-Dateipfaden
datasetFileMap <- list(
  "Gamsbock" = "Data/Gamsbock/Gamsbock_clean_since_18_04_2021.csv",
  "Fuchs"    = "Data/Fox/Thomas_clean.csv",
  "Ursina"   = "Data/Fox/Ursina_clean.csv",
  "Hirsch"   = "Data/red_deer/deer_clean.csv"
)

# Funktion, die einen Datensatz einliest, den Faktor bestimmt und den Scatterplot erstellt.
analyze_dataset <- function(dataset_name, file_path) {
  
  # CSV einlesen (hier wird angenommen, dass die CSV mit Komma getrennt ist und ein Header vorhanden ist)
  # Falls die Datei kein Header hat, passen Sie header und skip an.
  data <- read.csv(file_path, header = TRUE, sep = ",")
  
  # Prüfe, ob die nötigen Spalten vorhanden sind
  req_cols <- c("X_acceleration", "Y_acceleration")
  if (!all(req_cols %in% names(data))) {
    stop("Fehlende Spalten im Datensatz ", dataset_name, ". Erwartet werden: ", paste(req_cols, collapse = ", "))
  }
  
  # Sicherstellen, dass die Variablen numerisch sind
  data$X_acceleration <- as.numeric(as.character(data$X_acceleration))
  data$Y_acceleration <- as.numeric(as.character(data$Y_acceleration))
  
  # Entferne evtl. NA-Werte
  data <- data[!is.na(data$X_acceleration) & !is.na(data$Y_acceleration), ]
  
  # Regression ohne Interzept: lm(Y ~ X - 1) erzwingt, dass die Gerade durch (0,0) geht
  model <- lm(Y_acceleration ~ X_acceleration - 1, data = data)
  slope <- coef(model)[1]
  
  # Erstelle einen Scatterplot mit der Regressionslinie
  p <- ggplot(data, aes(x = X_acceleration, y = Y_acceleration)) +
    geom_point(color = "blue", alpha = 0.6) +
    geom_abline(slope = slope, intercept = 0, color = "red", linetype = "dashed", size = 1) +
    labs(title = paste0(dataset_name, ": Y = ", round(slope, 3), " * X"),
         x = "X Acceleration",
         y = "Y Acceleration") +
    theme_minimal()
  
  # Konsolenausgabe
  cat("Dataset:", dataset_name, "\n")
  cat("Ermittelte Steigung (Faktor):", slope, "\n")
  cat("Zusammenfassung des Modells:\n")
  print(summary(model))
  cat("\n----------------------------------\n")
  
  # Speichern des Plots (optional)
  ggsave(filename = paste0("scatter_", dataset_name, ".png"),
         plot = p, width = 8, height = 6, dpi = 300)
  
  return(p)
}

# Für alle vier Datensätze den Analyse-Workflow ausführen
plots <- list()
for(name in names(datasetFileMap)){
  cat("Analysiere Datensatz:", name, "\n")
  p <- analyze_dataset(name, datasetFileMap[[name]])
  plots[[name]] <- p
}

# Alle Plots in einem Gitter anzeigen (2 Spalten)
grid.arrange(grobs = plots, ncol = 2)




################################################
##########Rechnung
# Funktion zur Berechnung der Umrechnungsfaktoren
compute_conversion_factors <- function(k) {
  conv_x    <- 1 / sqrt(1 + k^2)            # Faktor, damit x = L2 * conv_x
  conv_sum  <- (1 + k) / sqrt(1 + k^2)        # Faktor, damit x+y = L2 * conv_sum
  var_factor_x   <- 1 / (1 + k^2)             # Varianzfaktor für x: Var(x)=Var(L2)/(1+k^2)
  var_factor_sum <- (1 + k)^2 / (1 + k^2)       # Varianzfaktor für x+y: Var(x+y)=Var(L2)*((1+k)^2/(1+k^2))
  list(conv_x = conv_x, conv_sum = conv_sum,
       var_factor_x = var_factor_x, var_factor_sum = var_factor_sum)
}

# k-Werte für die einzelnen Datensätze:
k_values <- c("Gamsbock" = 0.8, 
              "Thomas"   = 0.524, 
              "Ursina"   = 0.437, 
              "Hirsch"   = 0.927)

# Ausgabe der Faktoren für jeden Datensatz
for (dataset in names(k_values)) {
  k <- k_values[dataset]
  factors <- compute_conversion_factors(k)
  cat("Dataset:", dataset, "\n")
  cat("k =", k, "\n")
  cat("Faktor für x (x = L2 / sqrt(1+k^2)): ", round(factors$conv_x, 3), "\n")
  cat("Faktor für Summe (x+y = L2 * (1+k)/sqrt(1+k^2)): ", round(factors$conv_sum, 3), "\n")
  cat("Varianzfaktor für x (Var(x)=Var(L2)/(1+k^2)): ", round(factors$var_factor_x, 3), "\n")
  cat("Varianzfaktor für Summe (Var(x+y)=Var(L2)*((1+k)^2/(1+k^2))): ", round(factors$var_factor_sum, 3), "\n")
  cat("\n")
}
