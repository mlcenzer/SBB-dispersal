create_delta_data = function(data) {
  # INPUT     data as the tested_data with each row as a unique ID and trial.
  # OUTPUT    dataset with each row as a unique ID only.
  d <- data %>%
    group_by(ID, sex,population, site, host_plant, latitude, longitude, total_eggs, 
             beak, thorax, wing, body, w_morph, morph_notes, tested,
             host_c, sex_c, w_morph_c, lat_c, sym_dist, sym_dist_s, total_eggs_c, 
             beak_c, thorax_c, thorax_s, body_c, wing_c, wing2body, wing2body_c, wing2body_s) %>%
    summarise_all(funs(list(na.omit(.))))
  
  d$num_flew <- 0
  d$num_notflew <- 0
  d$average_mass <- 0
  
  d$egg_diff <- 0
  d$mass_diff <- 0
  d$flew_diff <- 0
  d$dist_diff <- 0
  d$speed_diff <- 0
  d$mass_per <- 0 
  
  for(row in 1:length(d$flew_b)){
    
    n_flew <- sum(d$flew_b[[row]] == 1) # total number of times did fly among trails
    d$num_flew[[row]] <- n_flew 
    
    n_notflew <- sum(d$flew_b[[row]] == 0) # total number of times did not fly among trails
    d$num_notflew[[row]] <- n_notflew
    
    avg_mass <- mean(d$mass[[row]])
    d$average_mass[[row]] <- avg_mass
    
    # mass, flight, distance, and speed changes between T1 and T2
    
    d$mass_diff[[row]] <- d$mass[[row]][2] - d$mass[[row]][1]  # T2 - T1
    d$mass_per[[row]] <- (d$mass_diff[[row]] / d$mass[[row]][1]) * 100
    d$flew_diff[[row]] <- d$flew_b[[row]][2] - d$flew_b[[row]][1]  # T2 - T1
    d$dist_diff[[row]] <- d$distance[[row]][2] - d$distance[[row]][1]  # T2 - T1
    d$speed_diff[[row]] <- d$average_speed[[row]][2] - d$average_speed[[row]][1]  # T2 - T1
    d$egg_diff[[row]] <- d$eggs_b[[row]][2] - d$eggs_b[[row]][1]  # T2 - T1
    
  }
  
  d <- select(d, -filename, -channel_letter, -set_number)
  
  # Filter out bugs that were not tested twice:
  rows_remove <- c()
  for (row in 1:nrow(d)){
    if (length(d$trial_type[[row]]) < 2) {
      rows_remove <- c(rows_remove, row)
    }
  }
  d <- d[-rows_remove, ]
  
  # for flight case building
  d$no_response_b = NA
  d$no_response_b[d$num_flew == 0] = 0
  d$no_response_b[d$num_flew == 2] = 2
  
  d$flight_case = NA
  d$flight_case = d$num_flew
  d$flight_case[d$flew_diff == -1] = -1
  d$flight_case = as.factor(d$flight_case)
  d$flight_case
  
  return(d)
}