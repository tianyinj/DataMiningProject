library(weatherData)

dep2015$FULL_DATE <- as.Date(paste(dep2015$YEAR, dep2015$MONTH, dep2015$DAY_OF_WEEK, sep = "-"),
                             format = "%Y-%m-%d")

flight2016_guess$FULL_DATE <- as.Date(paste(flight2016_guess$YEAR, flight2016_guess$MONTH,
                                            flight2016_guess$DAY_OF_WEEK, sep = "-"), format = "%Y-%m-%d")


tempinfo <- c()
humidityinfo <- c()
for(row in 12994:nrow(dep2015)) {
  day_temp <- getDetailedWeather("KPIT", dep2015[row,]$FULL_DATE,
                                  opt_custom_columns = T, custom_columns = c(2, 4))
  if(!is.null(day_temp)) {
    day_temp <- filter(day_temp, TemperatureF != -9999.0, Humidity != "NA")
  }
  avg_temp <- mean(day_temp$TemperatureF)
  avg_humidity <- mean(day_temp$Humidity)
  tempinfo <- c(tempinfo, avg_temp)
  humidityinfo <- c(humidityinfo, avg_humidity)
}

tempinfo2016_guess <- c()
humidityinfo2016_guess <- c()

for(row in 1:nrow(flight2016_guess)) {
  day_temp <- getDetailedWeather("KPIT", flight2016_guess[row,]$FULL_DATE,
                                 opt_custom_columns = T, custom_columns = c(2, 4))
  if(!is.null(day_temp)) {
    day_temp <- filter(day_temp, TemperatureF != -9999.0, Humidity != "NA")
  }
  avg_temp <- mean(day_temp$TemperatureF)
  avg_humidity <- mean(day_temp$Humidity)
  tempinfo2016_guess <- c(tempinfo2016_guess, avg_temp)
  humidityinfo2016_guess <- c(humidityinfo2016_guess, avg_humidity)
}