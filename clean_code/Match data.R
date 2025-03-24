data_temp2 <- read.csv(file = "Data/red_deer/Deer_GPS_clean.csv"
                       , stringsAsFactors = FALSE)

data_temp2$Time <- as.POSIXct(data_temp2$Time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")


# 4) CSV-Datei einlesen (hier Annahmen zu header=FALSE, sep=";", skip=1)
data <- read.csv(
  file   = "Data/red_deer/Deer_clean.csv",
  header = TRUE,
  sep    = ",",
  skip   = 0
)

data$time <- as.POSIXct(data$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


data$Time_key <- format(data$time, "%Y-%m-%d %H")
data_temp2$Time_key <- format(data_temp2$Time, "%Y-%m-%d %H")



joined_df <- merge(data, data_temp2, by = ("Time_key"), all.x = TRUE)
data <- joined_df[order(joined_df$time), ]
