library(dplyr)
library(lubridate)

# read and tidy data --------
thomas <- read.csv("Act_91037_20200113_Thomas.csv", header = TRUE)
thomas$GMT.Time <- as.POSIXct(thomas$GMT.Time, format = "%d.%m.%Y %H:%M:%S")

ursina <- read.csv("Act_91038_20200121_Ursina.csv", header = TRUE)
ursina$GMT.Time <- as.POSIXct(ursina$GMT.Time, format = "%d.%m.%Y %H:%M:%S")

gams <- read.csv2("ACT_Collar46723_20221104175921_fromVectronic_exportedviaGPSPlusX.csv", header = TRUE)
gams$UTC_Time <- as.POSIXct(paste(gams$UTC_Date, gams$UTC_Time), format = "%d.%m.%Y %H:%M:%S")
gams$LMT_Time <- as.POSIXct(paste(gams$LMT_Date, gams$LMT_Time), format = "%d.%m.%Y %H:%M:%S")
gams$SCTS_Time <- as.POSIXct(paste(gams$SCTS_Date, gams$SCTS_Time), format = "%d.%m.%Y %H:%M:%S")

gams <- gams %>% select(-c(
  UTC_Date, LMT_Date, LMT_Time, SCTS_Date, SCTS_Time, AnimalID, GroupID,
  Origin, CollarID, DT, Act.Mode
))

reddeer <- read.csv2("h46_act_2020.csv", header = TRUE)
reddeer$acquisition_time <- as.POSIXct(reddeer$acquisition_time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
reddeer <- reddeer %>% select(-c(id_collar, scts, origin_code, activity_mode_code, activity_mode_dt,
                                 activity_3))

# same for GPS data
thomas_gps <- read.csv("GPS_91037_20200113_Thomas.csv", header = TRUE)
thomas_gps$GMT.Time <- as.POSIXct(thomas_gps$GMT.Time, format = "%d.%m.%Y %H:%M:%S")
ursina_gps <- read.csv("GPS_91038_20200121_Ursina.csv", header = TRUE)
ursina_gps$GMT.Time <- as.POSIXct(ursina_gps$GMT.Time, format = "%d.%m.%Y %H:%M:%S")
gams_gps <- read.csv2("GPS_Collar46723_20220427181533_final.csv", header = TRUE)
gams_gps$UTC_Time <- as.POSIXct(paste(gams_gps$UTC_Date, gams_gps$UTC_Time), format = "%d.%m.%Y %H:%M:%S")
gams_gps$LMT_Time <- as.POSIXct(paste(gams_gps$LMT_Date, gams_gps$LMT_Time), format = "%d.%m.%Y %H:%M:%S")
gams_gps$SCTS_Time <- as.POSIXct(paste(gams_gps$SCTS_Date, gams_gps$SCTS_Time), format = "%d.%m.%Y %H:%M:%S")
gams_gps <- gams_gps %>% select(-c(
  UTC_Date, LMT_Date, LMT_Time, SCTS_Date, SCTS_Time, AnimalID, GroupID,
  Origin, CollarID
))
reddeer_gps <- read.csv2("h46_gps_2020.csv", header = TRUE)
reddeer_gps$acquisition_time <- as.POSIXct(reddeer_gps$acquisition_time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
reddeer_gps <- reddeer_gps %>% select(c(x, y, acquisition_time, latitude, longitude))

# foxes -----
# Thomas
summary(thomas)
summary(thomas_gps)
par(mfrow = c(2, 1))
# plot(thomas$GMT.Time, thomas$X, type = "h", ylim = c(0, 20), xlab = "Time", ylab = "Activity X", main = "Thomas")
# plot(thomas$GMT.Time, thomas$Y, type = "h", ylim = c(0, 20), xlab = "Time", ylab = "Activity Y", main = "Thomas")

# subset July
plot(thomas[month(thomas$GMT.Time) == 7, ]$GMT.Time,
  thomas[month(thomas$GMT.Time) == 7, ]$X,
  type = "h", ylim = c(0, 20), xlab = "Time", ylab = "Activity X", main = "Thomas"
)
plot(thomas[month(thomas$GMT.Time) == 7, ]$GMT.Time,
  thomas[month(thomas$GMT.Time) == 7, ]$Y,
  type = "h", ylim = c(0, 20), xlab = "Time", ylab = "Activity Y", main = "Thomas"
)

# Ursina
summary(ursina)
summary(ursina_gps)
par(mfrow = c(2, 1))

# subset July
plot(ursina[month(ursina$GMT.Time) == 7, ]$GMT.Time,
  ursina[month(ursina$GMT.Time) == 7, ]$X,
  type = "h", ylim = c(0, 20), xlab = "Time", ylab = "Activity X", main = "Ursina"
)
plot(ursina[month(ursina$GMT.Time) == 7, ]$GMT.Time,
  ursina[month(ursina$GMT.Time) == 7, ]$Y,
  type = "h", ylim = c(0, 20), xlab = "Time", ylab = "Activity Y", main = "Ursina"
)

# chamois ------
summary(gams)
summary(gams_gps)
par(mfrow = c(2, 1))

# subset first 5 days of July
plot(gams[23056:24056, ]$UTC_Time,
  gams[23056:24056, ]$ActivityX,
  type = "h", ylim = c(0, 255), xlab = "Time", ylab = "Activity X", main = "Gams"
)
plot(gams[23056:24056, ]$UTC_Time,
  gams[23056:24056, ]$ActivityY,
  type = "h", ylim = c(0, 255), xlab = "Time", ylab = "Activity Y", main = "Gams"
)

# red deer ---------
summary(reddeer)
summary(reddeer_gps)
par(mfrow = c(2, 1))

# subset first 5 days of July
plot(reddeer[51701:52701, ]$acquisition_time,
  reddeer[51701:52701, ]$activity_1,
  type = "h", ylim = c(0, 255), xlab = "Time", ylab = "Activity", main = "Red deer"
)
plot(reddeer[51701:52701, ]$acquisition_time,
  reddeer[51701:52701, ]$activity_2,
  type = "h", ylim = c(0, 255), xlab = "Time", ylab = "Activity", main = "Red deer"
)

