# data_preprocessing.R
library(moveHMM)

# Mapping: dataset_name -> CSV-Dateipfad
datasetFileMap <- list(
  "Gams" = "Data/Gamsbock/Gamsbock_clean_since_18_04_2021.csv",
  "Thomas"    = "Data/Fox/Thomas_clean.csv",
  "Ursina"   = "Data/Fox/Ursina_clean.csv",
  "Deer"   = "Data/red_deer/deer_clean.csv"
)

prepareHMMData <- function() {
  
  # 1) YAML-Config laden
  config_path <- "config.yaml"
  config <- yaml::yaml.load_file(config_path)
  
  # 2) Aus der Config lesen: dataset_name und preprocessing
  if (!"dataset_name" %in% names(config)) {
    stop("In der config.yaml fehlt 'dataset_name'!")
  }
  if (!"preprocessing" %in% names(config)) {
    stop("In der config.yaml fehlt 'preprocessing'!")
  }
  
  dataset_name <- config$dataset_name
  preprocessing <- config$preprocessing
  
  # 3) Dateipfad ermitteln
  if (!dataset_name %in% names(datasetFileMap)) {
    stop("Unbekannter Datensatzname: ", dataset_name)
  }
  file_path <- datasetFileMap[[dataset_name]]
  
  # 4) CSV-Datei einlesen (hier Annahmen zu header=FALSE, sep=";", skip=1)
  data <- read.csv(
    file   = file_path,
    header = TRUE,
    sep    = ",",
    skip   = 0
  )
  
  if (preprocessing == "L2norm") {
    data$preprocessed <- sqrt(data$X_acceleration^2 + data$Y_acceleration^2)
  } else if (preprocessing == "onlyX") {
    data$preprocessed <- data$X_acceleration
  } else if (preprocessing == "sum") {
    data$preprocessed <- (data$X_acceleration + data$Y_acceleration)
  } else {
    stop("Unbekannte Preprocessing-Methode: ", preprocessing)
  }

  # 6) moveHMM vorbereiten
  data$cum_x <- cumsum(data$preprocessed)
  data$cum_y <- 0
  
  hmm_data <- prepData(data, type = "UTM", coordNames = c("cum_x", "cum_y"))
  
  return(hmm_data)
}
