
# Function that centers the data for which ever flight data subset you give it

center_data <- function(d) {
  
  # Distance From Sympatric Zone
  d$lat_c<-d$latitude-mean(d$latitude)
  d$sym_dist<-abs(d$latitude-25.49197)

  # Flight Duration (minutes)
  d$minute_duration <- 0
  d$minute_duration <- as.integer(d$total_duration / 60)
  d$minute_duration_c <- d$minute_duration - mean(d$minute_duration)
  
  # Minutes From When Incubator Lights Turned On
  tested <- nrow(d[d$tested == "yes",])
  
  t_IncLights_turn_on <- 8 # AM
  d$min_from_IncStart <- 0
  for(row in 1:tested){
    time <- strptime(d$time_start[row], format="%H:%M:%S")
    min <- minute(time)
    hr <- hour(time)
    d$min_from_IncStart[row] <- 60*(hr - t_IncLights_turn_on) + min
  } 
  d$min_from_IncStart_c <- d$min_from_IncStart-mean(d$min_from_IncStart)
  
  # Days From Starting Time
  d$days_from_start <- 0
  d$test_date[d$tested == "no"] <- d$test_date[1]
  d$test_date <- as_date(d$test_date) 
  dates <- sort(unique(d$test_date))
  
  for (i in 1:length(dates)){
    day_diff <- dates[i] - dates[1]
    for (r in 1:tested){
      if (d$test_date[r] == dates[i]) {
        d$days_from_start[r] = day_diff }
    }
  }
  
  d$days_from_start_c <- d$days_from_start-mean(d$days_from_start)

  # Mass
  d$mass_c <- d$mass-mean(d$mass, na.rm=TRUE) 
  
  # Eggs
  d$total_eggs_c <- d$total_eggs-mean(d$total_eggs, na.rm=TRUE) 
  
  # Morphology
  
  # Beak Length
  d$beak_c <- d$beak-mean(d$beak, na.rm=TRUE)
  
  # Thorax Length
  d$thorax_c <- d$thorax-mean(d$thorax, na.rm=TRUE)
  
  # Body Length
  d$body_c <- d$body-mean(d$body, na.rm=TRUE)
  
  # Wing Length
  d$wing_c <- d$wing-mean(d$wing, na.rm=TRUE)
  
  #wing2body
  d$wing2body <- d$wing / d$body
  d$wing2body_c <- d$wing2body - mean(d$wing2body, na.rm=TRUE)
  
  return(d)
}