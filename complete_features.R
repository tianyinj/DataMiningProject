########################## Helper Functions ######################  
## Expect hhmm, compute minutes difference
time.diff <- function(early, late) {
  t.early <- strptime(early, "%H%M")
  t.late <- strptime(late, "%H%M")
  return(as.numeric(difftime(t.late,t.early, units = "mins")))
}

#Function to complete dataset with cols
complete.fun <- function(data, cols) {
  compelete.rows <- complete.cases(data[, cols])
  return(data[compelete.rows, ])
}
na.omit.count <- function (x,v1,v2, val) { 
  complete_rows <- !is.na(x[,v1]) == !is.na(x[,v2])
  return(sum(complete_rows))
} 

filling.fun <- function(data=flights_clean, x, y=flights_clean$DEP_DELAY_NEW){
  if (is.na(y) && x==0) {
    return(0)
  }
}

find.prev.delay <- function(row){
  tailnum = row[8]
  carrier = row[6]
  year = row[1]
  month = row[3]
  day = row[4]
  dep_time = row[14]
  
  
  aux = which(arv_flights$YEAR==year & arv_flights$MONTH==month & arv_flights$DAY_OF_MONTH==day
              & arv_flights$CRS_ARR_TIME<=dep_time & arv_flights$TAIL_NUM==tailnum 
              & arv_flights$UNIQUE_CARRIER == carrier)
  if (length(aux) == 0) {
    return(0)
  }
  
  nrow = nrow(aux)
  
  result = arv_flights[aux[length(aux)], 29]
  return(result)
}

# returns a data frame without the columns specified in the writeup
remove.columns = function(data.frame) {
  remove.idx = which(names(data.frame) %in% 
                       unlist(strsplit("DEP_TIME, DEP_DELAY, DEP_DELAY_NEW, DEP_DELAY_GROUP, DEP_TIME_BLK, TAXI_OUT, WHEELS_OFF, WHEELS_ON, TAXI_IN, ARR_TIME, ARR_DELAY, ARR_DELAY_NEW, ARR_DEL15, ARR_DELAY_GROUP, ARR_TIME_BLK, CANCELLED, CANCELLATION_CODE, DIVERTED, ACTUAL_ELAPSED_TIME, AIR_TIME, CARRIER_DELAY, WEATHER_DELAY, NAS_DELAY, SECURITY_DELAY, LATE_AIRCRAFT_DELAY, FIRST_DEP_TIME, TOTAL_ADD_GTIME, LONGEST_ADD_GTIME, NA_DELAY",
                                       split=", ")))
  return(data.frame[, -remove.idx])
}
############################## END ##################################

########################## Main Function ############################ 

prepare.df = function(df, new_filename=NA) {
  ###############################Preliminary Cleaning#####################################
  
  # Time formatting
  #df$DEP_TIME <- formatC(df$DEP_TIME, width = 4,flag = 0)
  #df$ARR_TIME <- formatC(df$ARR_TIME, width = 4,flag = 0)
  #df$CRS_DEP_TIME <- formatC(df$CRS_DEP_TIME, width = 4,flag = 0)
  #df$CRS_ARR_TIME <- formatC(df$CRS_ARR_TIME, width = 4,flag = 0)
  #df$WHEELS_OFF <- formatC(df$WHEELS_OFF, width = 4,flag = 0)
  #df$WHEELS_ON <- formatC(df$WHEELS_ON, width = 4,flag = 0)
  
  
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
  
  ##############################Splitting cancelled flights####################################
  
  #Canceled flights, include both arrival and departures
  flights.cancelled <- df[which(df$CANCELLED==1),]
  flights.diverted <- df[which(df$DIVERTED==1),]
  
  # !!Some flights here are marked as both cancelled and delayed
  flights.normal <- df[which(df$CANCELLED==0 & df$DIVERTED==0),]
  
  ############################## Fill in missing entries ####################################
  # aux <- which(!is.na(flights.normal$ARR_TIME) & is.na(flights.normal$ARR_DELAY))
  # flights.normal[aux,]$ARR_DELAY <- time.diff(flights.normal[aux,]$CRS_ARR_TIME,flights.normal[aux,]$ARR_TIME)
  # 
  # aux <- which(is.na(flights.normal$ARR_DELAY_NEW) & !is.na(flights.normal$ARR_DELAY))
  # flights.normal[aux,]$ARR_DELAY_NEW <- abs(flights.normal[aux,]$ARR_DELAY)
  # 
  # aux <- which(is.na(flights.normal$ARR_DEL15) & !is.na(flights.normal$ARR_DELAY))
  # flights.normal[aux,]$ARR_DEL15 <- ifelse(flights.normal[aux,]$ARR_DELAY>=15, 1, 0)
  # 
  # aux <- which(is.na(flights.normal$ACTUAL_ELAPSED_TIME) & !is.na(flights.normal$ARR_TIME) & !is.na(flights.normal$DEP_TIME))
  # flights.normal[aux,]$ACTUAL_ELAPSED_TIME <- time.diff(flights.normal[aux,]$DEP_TIME, flights.normal[aux,]$ARR_TIME)
  # 
  # aux <- which(is.na(flights.normal$ACTUAL_ELAPSED_TIME) & !is.na(flights.normal$ARR_TIME) & !is.na(flights.normal$DEP_TIME))
  # flights.normal[aux,]$ACTUAL_ELAPSED_TIME <- time.diff(flights.normal[aux,]$DEP_TIME, flights.normal[aux,]$ARR_TIME)
  # 
  # aux <- which(is.na(flights.normal$AIR_TIME) & !is.na(flights.normal$WHEELS_OFF) & !is.na(flights.normal$WHEELS_ON))
  # flights.normal[aux,]$AIR_TIME <- time.diff(flights.normal[aux,]$WHEELS_OFF, flights.normal[aux,]$WHEELS_ON)
  
  for (col in c("CARRIER_DELAY", "WEATHER_DELAY", "NAS_DELAY", "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY")){
    i <- which( colnames(flights.normal) == col )
    aux <- which(is.na(flights.normal[, i]))
    flights.normal[aux,i] <- 0
  }
  
  aux <- which(is.na(flights.normal$FIRST_DEP_TIME))
  flights.normal[aux,]$FIRST_DEP_TIME <- flights.normal[aux,]$DEP_TIME
  flights.normal[aux,]$TOTAL_ADD_GTIME <- 0
  flights.normal[aux,]$LONGEST_ADD_GTIME <- 0
  
  
  
  ######################### Building New Features ##########################
  diff <- as.vector(flights.normal$DEP_DELAY_NEW - flights.normal$CARRIER_DELAY - flights.normal$WEATHER_DELAY - flights.normal$NAS_DELAY 
                    - flights.normal$SECURITY_DELAY - flights.normal$SECURITY_DELAY)
  flights.normal$NA_DELAY <- ifelse(diff > 0, diff, 0)
  
  
  ######################### Separating ##########################
  dep_flights <- flights.normal[flights.normal$ORIGIN=="PIT",]
  arv_flights <- flights.normal[flights.normal$ORIGIN!="PIT",]
  
  ######################### Delay on arrival ####################
  
  dep_flights$HAS_PREV_DELAY = apply(dep_flights, 1, find.prev.delay)
  dep_flights$HAS_PREV_DELAY = as.factor(dep_flights$HAS_PREV_DELAY)
  
  ######################### Holidays ############################
  # November and December get a score of 2
  dep_flights$HOLIDAY = ifelse(dep_flights$MONTH == 11 | dep_flights$MONTH == 12, 2, 0)
  
  # June and July get a score of 1
  dep_flights$HOLIDAY = ifelse(dep_flights$MONTH == 6 | dep_flights$MONTH == 7, 1, 0)
  
  dep_flights$HOLIDAY = as.factor(dep_flights$HOLIDAY)
  
  ################### Flushing out .. ###################
  if(!is.na(new_filename)) {
    write.csv(dep_flights, new_filename, row.names=FALSE)
  }
  
  return(dep_flights)
}

df<-read.csv("flights2015.csv")
flight.data.tr = prepare.df(df, "complete_training.csv")
flight.data.tr = remove.columns(flight.data.tr)

############## Training data set ##########################
delay15.idx = which(names(flight.data.tr) == 'DEP_DEL15')
X_train = flight.data.tr[,-delay15.idx]
y_train = unlist(flight.data.tr[,delay15.idx])
