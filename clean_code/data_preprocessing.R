# data_preprocessing.R
library(moveHMM)
library(zoo)

# Mapping: dataset_name -> CSV-Dateipfad
datasetFileMap <- list(
  "Gams" = "Data/Gamsbock/Gamsbock_finished.csv",
  "Thomas"    = "Data/Fox/Thomas_finished.csv",
  "Ursina"   = "Data/Fox/Ursina_finished.csv",
  "Deer"   = "Data/red_deer/red_deer_finished.csv"
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
  temperature <- config$temperature
  
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
  
  
  
  if (temperature == "None") {
    data$temperature <- 1
  } else if (temperature == "Intern") {
    data$temperature <- data$temperature_intern
  } else if (temperature == "Extern") {
    data$temperature <- data$temperature_extern
  } else if (temperature == "Mix") {
    data$temperature <- (data$temperature_intern^2 * data$temperature_extern)
  }else if (temperature == "Humidity") {
    data$temperature <- (data$temperature_intern + data$Rain2^2)
  }else if (temperature == "Rain") {
    data$temperature <- data$Rain2*100
  }else {
    stop("Unbekannte tempreture-Methode: ", temperature)
  }
  
  # 6) moveHMM vorbereiten
  data$cum_x <- cumsum(data$preprocessed)
  data$cum_y <- 0
  
  hmm_data <- prepData(data, type = "UTM", coordNames = c("cum_x", "cum_y"))
  
  return(hmm_data)
}