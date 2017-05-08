simplify <- function(df){
  #Use airport sequence id to indicate origin and dest
  #Modify this part if you want to see the actual airport/state names
  df <- subset(df, select = -c(ORIGIN_CITY_NAME, ORIGIN_STATE_ABR, 
                               ORIGIN_STATE_NM, ORIGIN_STATE_FIPS,
                               ORIGIN_AIRPORT_ID, ORIGIN_CITY_MARKET_ID,
                               ORIGIN_WAC))
  df <- subset(df, select = -c(DEST_CITY_NAME, DEST_STATE_ABR, 
                               DEST_STATE_NM, DEST_STATE_FIPS,
                               DEST_AIRPORT_ID, DEST_CITY_MARKET_ID,
                               DEST_WAC))
  #Drop FL_DATE; keep YEAR, QUARTER, MONTH, DAY_OF_MONTH, DAY_OF_WEEK
  df <- subset(df, select = -c(FL_DATE))
  #Drop CARRIER; keep UNIQUE_CARRIER
  df <- subset(df, select = -c(CARRIER))
  #Drop Flights, value is 1 for all rows
  df <- subset(df, select = -c(FLIGHTS))
  
  return(df)
}




df1<-read.csv("flights2015.csv")
df2<-read.csv("flights2016_visible.csv")

flights.normal <- read.csv("flights2016_guess.csv")

df <- rbind(df1, df2)
flights.normal <- df[which(df$CANCELLED==0 & df$DIVERTED==0),]

flights.normal <- simplify(flights.normal)

dep_flights <- flights.normal[flights.normal$ORIGIN=="PIT",]
arv_flights <- flights.normal[flights.normal$ORIGIN!="PIT",]

tail_num_idx = which(names(dep_flights) == "TAIL_NUM")
carrier_idx = which(names(dep_flights) == "UNIQUE_CARRIER")
year_idx = which(names(dep_flights) == "YEAR")
month_idx = which(names(dep_flights) == "MONTH")
day_idx = which(names(dep_flights) == "DAY_OF_MONTH")
dep_time_idx = which(names(dep_flights) == "CRS_DEP_TIME")

arv_time_idx = which(names(arv_flights) == "CRS_ARR_TIME")
arv_depdel_idx = which(names(arv_flights) == "ARR_DEL15")

################ Prev Arrival #######################
find.prev.delay <- function(row){
  tailnum = row[tail_num_idx]
  carrier = row[carrier_idx]
  year = row[year_idx]
  month = row[month_idx]
  day = row[day_idx]
  dep_time = row[dep_time_idx]
  
  aux = which(arv_flights$YEAR==year & arv_flights$MONTH==month & arv_flights$DAY_OF_MONTH==day
              & arv_flights$CRS_ARR_TIME<=dep_time & arv_flights$TAIL_NUM==tailnum 
              & arv_flights$UNIQUE_CARRIER == carrier)
  if (length(aux) == 0) {
    return(0)
  }
  maxRow = aux[1]
  
  if (length(aux) == 1){
    return(arv_flights[maxRow,arv_depdel_idx])
  }
  for (i in 2:length(aux)){
    curRow = aux[i]
    if (arv_flights[curRow,arv_time_idx] > arv_flights[maxRow,arv_time_idx]) {
      maxRow = curRow
    }
  }
  return (arv_flights[maxRow,arv_depdel_idx])
}

dep_flights$has_prev_delay = apply(dep_flights, 1, find.prev.delay)
################ Prev Arrival #######################

################ Weather #######################
dep_flights <- read.csv("data.csv")

weather15<-read.csv("2015weather.csv")
weather16<-read.csv("2016weather.csv")
weather <- rbind(weather15, weather16)

dep_flights$FULL_DATE <- paste(dep_flights$YEAR, dep_flights$MONTH, dep_flights$DAY_OF_WEEK, sep = "-")


find.weather.data <- function (row, idx){
  date_idx = which(names(dep_flights) == "FULL_DATE")
  
  return(weather[which(weather$EST == row[date_idx]),idx])
}
dep_flights$weather <- apply(dep_flights, 1, find.weather.data, 22)


################ Weather #######################

write.csv(dep_flights, "guess.csv", row.names = FALSE)







