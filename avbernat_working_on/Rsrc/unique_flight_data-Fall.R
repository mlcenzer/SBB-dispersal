
create_delta_data.Fall <- function(data) {
  d <- data %>%
    group_by(ID, sex, population, site, host_plant, latitude, longitude, 
             host_c, sex_c, w_morph_c, lat_c, sym_dist) %>%
    summarise_all(funs(list(na.omit(.))))
 
  # Filter out bugs that were not tested at least twice:
  rows_remove <- c()
  for (row in 1:nrow(d)){
    if (length(d$flew[[row]]) < 2) {
      rows_remove <- c(rows_remove, row)
    }
  }
  d <- d[-rows_remove, ]
  
  d$num_flew <- 0
  d$num_notflew <- 0
  d$average_mass <- 0
  d$num_egg <- 0
  
  d$egg_diff <- 0
  d$mass_diff <- 0
  d$flew_diff <- 0
  d$dist_diff <- 0
  d$speed_diff <- 0
  d$mass_per <- 0 
  d$num_flew_twice <- 0
  d$num_egg_twice <- 0
  
  d$mass_diff2 <- 0
  d$mass_per2 <- 0
  d$flew_diff2 <- 0
  d$egg_diff2 <- 0
  d$num_flew_twice2 <- 0
  d$num_egg_twice2 <- 0
  
  # compare 1st and 2nd trial
  for(row in 1:nrow(d)){
    
    n_flew <- sum(d$flew_b[[row]] == 1) # total number of times did fly among trails
    d$num_flew[[row]] <- n_flew 
    
    n_notflew <- sum(d$flew_b[[row]] == 0) # total number of times did not fly among trails
    d$num_notflew[[row]] <- n_notflew
  
    n_egg <- sum(d$eggs_b[[row]] == 1) # total number of times did fly among trails
    d$num_egg[[row]] <- n_egg 
    
    avg_mass <- mean(d$mass[[row]])
    d$average_mass[[row]] <- avg_mass
    
    # mass, flight, distance, and speed changes between T1 and T2
    
    d$num_flew_twice[[row]] <- d$flew_b[[row]][2] + d$flew_b[[row]][1]
    d$num_egg_twice[[row]] <- d$eggs_b[[row]][2] + d$eggs_b[[row]][1]
    d$mass_diff[[row]] <- d$mass[[row]][2] - d$mass[[row]][1]  # T2 - T1
    d$mass_per[[row]] <- (d$mass_diff[[row]] / d$mass[[row]][1]) * 100
    d$flew_diff[[row]] <- d$flew_b[[row]][2] - d$flew_b[[row]][1]  # T2 - T1
    d$dist_diff[[row]] <- d$distance[[row]][2] - d$distance[[row]][1]  # T2 - T1
    d$speed_diff[[row]] <- d$average_speed[[row]][2] - d$average_speed[[row]][1]  # T2 - T1
    d$egg_diff[[row]] <- d$eggs_b[[row]][2] - d$eggs_b[[row]][1]  # T2 - T1
    
    if (length(d$flew_b[[row]]) > 2) {
      d$mass_diff2[[row]] <- d$mass[[row]][3] - d$mass[[row]][2]
      d$mass_per2[[row]] <- (d$mass_diff2[[row]] / d$mass[[row]][2]) * 100
      d$flew_diff2[[row]] <- d$flew_b[[row]][3] - d$flew_b[[row]][2]
      d$egg_diff2[[row]] <- d$eggs_b[[row]][3] - d$eggs_b[[row]][2] 
      
      d$num_flew_twice2[[row]] <- d$flew_b[[row]][3] + d$flew_b[[row]][2]
      d$num_egg_twice2[[row]] <- d$eggs_b[[row]][3] + d$eggs_b[[row]][2]
    }
  }
  
  d <- select(d, -filename, -channel_letter, -set_number)
  
  # for flight case building
  d$flight_case = NA
  d$flight_case[d$num_flew_twice == 0] = 0
  d$flight_case[d$num_flew_twice == 2] = 2
  d$flight_case[d$flew_diff == -1] = -1
  d$flight_case[d$flew_diff == 1] = 1
  d$flight_case = as.factor(d$flight_case)
  
  d$flight_case2 = NA
  d$flight_case2[d$num_flew_twice2 == 0] = 0
  d$flight_case2[d$num_flew_twice2 == 2] = 2
  d$flight_case2[d$flew_diff2 == -1] = -1
  d$flight_case2[d$flew_diff2 == 1] = 1
  d$flight_case2 = as.factor(d$flight_case2)
  
  # for egg case building
  d$egg_case = NA
  d$egg_case[d$num_egg_twice == 0] = 0
  d$egg_case[d$num_egg_twice == 2] = 2
  d$egg_case[d$egg_diff == -1] = -1
  d$egg_case[d$egg_diff == 1] = 1
  
  d$egg_case2 = NA
  d$egg_case2[d$num_egg_twice2 == 0] = 0
  d$egg_case2[d$num_egg_twice2 == 2] = 2
  d$egg_case2[d$egg_diff2 == -1] = -1
  d$egg_case2[d$egg_diff2 == 1] = 1
  
  return(d)
}