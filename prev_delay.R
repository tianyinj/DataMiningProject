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

dep_flights$has_prev_delay = apply(dep_flights, 1, find.prev.delay)