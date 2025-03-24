#install.packages("elevatr")
#install.packages("sf")
#install.packages("progress")
#install.packages("raster")
#install.packages("readr")
library(dplyr)
library(elevatr)
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(readr)

data <- read.csv("Data/Fox/GPS_91038_20200121_Ursina.csv", sep=",",skip=4 , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
data <- data %>%
  select(Longitude = Longitude, Latitude = Latitude, Time = GMT.Time, Temperature=Temperature)

data$Longitude[data$Longitude == 0.00000] <- NA

data <- data %>%
  fill(everything(), .direction = "down")



data_sf <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)

elev_raster <- get_elev_raster(data_sf, z = 10, src = "aws")

# Extract elevation values for the points
elevations <- raster::extract(elev_raster, data_sf)

# Add elevations to your original dataframe
data$elevation <- elevations

#Convert Time
data$Time <- as.POSIXct(paste(data$Date, data$Time), format = "%d.%m.%Y %H:%M:%S", tz="UTC")
data <- data %>% select(-Date)

data$Time <- as.POSIXct(data$Time, format = "%d.%m.%Y %H:%M:%S")
#Save
write_csv(data, "C:/Users/jeffj/Documents/GitHub/stat_consulting/Data/Fox/Ursina_GPS_clean.csv")




data_temp2 <- read.csv(file = "Data/Fox/Ursina_GPS_clean.csv"
                       , stringsAsFactors = FALSE)

data_temp2$Time <- as.POSIXct(data_temp2$Time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")