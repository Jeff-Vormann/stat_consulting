# data_preprocessing.R
library(moveHMM)
library(zoo)
library(momentuHMM)

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
  
  
  # 6) moveHMM vorbereiten
  data$cum_x <- cumsum(data$preprocessed)
  data$cum_y <- 0
  
  hmm_data <- momentuHMM::prepData(data, type = "UTM", coordNames = c("cum_x", "cum_y"))
  
  # 7) Adding different Temperature to the data if wanted
  data_temp <- read.table("Data/Temperature/order_126368_data.txt"
                          , stringsAsFactors = FALSE, 
                          skip = 2,
                          header = TRUE, col.names=c("ID", "Time2", "Temp", "Rain", "Rain2"))
  
  data$Time2 <- gsub("[^0-9]", "", format(as.POSIXct(data$time), "%Y%m%d%H"))
  joined_df <- merge(data, data_temp, by = ("Time2"), all.x = TRUE)
  data <- joined_df[order(joined_df$time), ]
  
  if (temperature == "None") {
    hmm_data$temperature <- 0
  } else if (temperature == "Intern") {
    hmm_data$temperature <- data$temperature
  } else if (temperature == "Extern") {
    hmm_data$temperature <- data$Temp
  } else if (temperature == "Mix") {
    hmm_data$temperature <- (data$Temp^2 * data$temperature)
  }else if (temperature == "Rain") {
    hmm_data$temperature <- (data$Temp + data$Rain2^2)
  }else if (temperature == "only_Rain") {
    hmm_data$temperature <- (data$Rain2)
  }else {
    stop("Unbekannte tempreture-Methode: ", temperature)
  }
  hmm_data$temp <- na.locf(hmm_data$temperature)
  
  #todo tod und season
  time_UTC <- ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", hmm_data$time),
                          paste0(hmm_data$time, " 00:00:00"),
                          hmm_data$time)

  # 3) Zeitspalte in POSIXct umwandeln (ggf. anpassen je nach Datenformat)
  time_UTC <- as.POSIXct(time_UTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  # 2) Stunde/Minute extrahieren, in [0,24) umwandeln
  hourNumeric <- as.numeric(format(time_UTC, "%H"))
  minNumeric  <- as.numeric(format(time_UTC, "%M"))
  # => Stundenanteil  +  Minuten/60
  hourOfDay   <- hourNumeric + minNumeric/60
  
  # 3) Fraction in [0,1) => (hourOfDay / 24)
  fracOfDay   <- hourOfDay / 24
  
  # 4) Cosinus => 0 Uhr = Hochpunkt (cos(0)=1), 12 Uhr = Tiefpunkt (cos(pi)=-1)
  ToD_values  <- cos(2 * pi * fracOfDay)
  
  # 5) In hmm_data speichern (Zeilen-zu-Zeilen)
  hmm_data$ToD <- ToD_values
  0
  dayOfYear <- as.numeric(format(time_UTC, "%j"))
  tail(dayOfYear)
  
  # Offset so, dass Tag 172 (21. Juni) => cos(0)=1 => Hochpunkt = Sommer
  fracOfYear <- (dayOfYear - 172) / 365
  
  na_indices <- which(is.na(dayOfYear))

  
  # Saisonalität als Cosinus => 1 ~ Sommer, -1 ~ Winter
  Season_values <- cos(2 * pi * fracOfYear)
  hmm_data$season <- Season_values
  #nan durch vorgänger ersetzen
  # Season: fehlende Werte interpolieren und dann standardisieren
  hmm_data$season <- na.approx(hmm_data$season, na.rm = FALSE, rule = 2)
  cat("Es wurden", sum(is.na(hmm_data$season)), "fehlende Werte in 'season' ersetzt.\n")
  hmm_data$season <- as.vector(scale(hmm_data$season))
  cat("Season: mean =", mean(hmm_data$season), "sd =", sd(hmm_data$season), "\n")
  # Temperature: fehlende Werte interpolieren und dann standardisieren
  hmm_data$temp <- na.approx(hmm_data$temp, na.rm = FALSE, rule = 2)
  cat("Es wurden", sum(is.na(hmm_data$temp)), "fehlende Werte in 'temp' ersetzt.\n")
  hmm_data$temp <- as.vector(scale(hmm_data$temp))
  cat("temp: mean =", mean(hmm_data$temp), "sd =", sd(hmm_data$temp), "\n")
  # Time of Day: fehlende Werte interpolieren und dann standardisieren
  hmm_data$ToD <- na.approx(hmm_data$ToD, na.rm = FALSE, rule = 2)
  cat("Es wurden", sum(is.na(hmm_data$ToD)), "fehlende Werte in 'ToD' ersetzt.\n")
  hmm_data$ToD <- as.vector(scale(hmm_data$ToD))
  cat("ToD: mean =", mean(hmm_data$ToD), "sd =", sd(hmm_data$ToD), "\n")
  return(hmm_data)
}