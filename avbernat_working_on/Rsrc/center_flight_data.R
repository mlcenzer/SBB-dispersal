library(dplyr)

center_data = function(d, is_not_unique_data = TRUE) {

  # Function that centers the data for which ever flight data subset you give it (full or unique)
  
  # Distance From Sympatric Zone
  d$lat_c =d$latitude-mean(d$latitude)
  
  sym_zone = d[d$population == "Homestead",]$latitude
  d$sym_dist = abs(d$latitude-sym_zone)
  d$sym_dist_s = (d$sym_dist - mean(d$sym_dist)) / sd(d$sym_dist)

  # Flight Duration (minutes) 
  if (is_not_unique_data) {
    d$minute_duration = 0
    d$minute_duration = as.integer(d$recording_duration / 60)
    d$minute_duration_c = d$minute_duration - mean(d$minute_duration)
  }
  
  # Minutes From When Incubator Lights Turned On
  tested = nrow(d[d$tested == "yes",])
  
  t_IncLights_turn_on = 8 # AM
  d$min_from_IncStart = 0
  for(row in 1:tested){
    time = strptime(d$time_start[row], format="%H:%M:%S")
    min = minute(time)
    hr = hour(time)
    d$min_from_IncStart[row] = 60*(hr - t_IncLights_turn_on) + min
  } 
  d$min_from_IncStart_c = d$min_from_IncStart-mean(d$min_from_IncStart)
  d$min_from_IncStart_s = (d$min_from_IncStart-mean(d$min_from_IncStart)) / sd(d$min_from_IncStart)
  
  # Days From Starting Time
  if (is_not_unique_data) {
    d$days_from_start = 0
    d$test_date[d$tested == "no"] = d$test_date[1]
    d$test_date = as_date(d$test_date) 
    dates = sort(unique(d$test_date))
    
    for (i in 1:length(dates)){
      day_diff = dates[i] - dates[1]
      for (r in 1:tested){
        if (d$test_date[r] == dates[i]) {
          d$days_from_start[r] = day_diff }
      }
    }
    
    d$days_from_start_c = d$days_from_start-mean(d$days_from_start)
  }
  
  # Mass 
  if (is_not_unique_data) {
    d$mass_c = d$mass-mean(d$mass, na.rm=TRUE) 
    d$mass_s = (d$mass-mean(d$mass, na.rm=TRUE)) / sd(d$mass, na.rm=TRUE)
    d$avg_mass = 0 
    
    dt = d %>%
      group_by(ID) %>%
      summarise_all(funs(list(na.omit(.))))
    dt$avg_mass = 0
    for(row in 1:length(dt$flew_b)){ 
      dt$avg_mass[[row]] = mean(dt$mass[[row]])
    }
    dt = dt[,c("ID", "avg_mass")]
    
    d = d %>%
      left_join(dt, by = c("ID"), suffix = c("", "")) 
  }
  
  # Average Mass
  d$avg_mass_c = d$avg_mass-mean(d$avg_mass, na.rm=TRUE) 
  d$avg_mass_s = (d$avg_mass-mean(d$avg_mass, na.rm=TRUE)) / sd(d$avg_mass, na.rm=TRUE)
  
  # Eggs
  d$total_eggs_c = d$total_eggs-mean(d$total_eggs, na.rm=TRUE) 
  
  # Morphology
  
  # Beak Length
  d$beak_c = d$beak-mean(d$beak, na.rm=TRUE)
  
  # Thorax Length
  d$thorax_c = d$thorax-mean(d$thorax, na.rm=TRUE)
  d$thorax_s = (d$thorax-mean(d$thorax, na.rm=TRUE)) / sd(d$thorax, na.rm=TRUE)
  
  # Body Length
  d$body_c = d$body-mean(d$body, na.rm=TRUE)
  
  # Wing Length
  d$wing_c = d$wing-mean(d$wing, na.rm=TRUE)
  
  #wing2body
  d$wing2body = d$wing / d$body
  d$wing2body_c = d$wing2body - mean(d$wing2body, na.rm=TRUE)
  d$wing2body_s = (d$wing2body - mean(d$wing2body, na.rm=TRUE)) / sd(d$wing2body, na.rm=TRUE)
  
  return(d)
}