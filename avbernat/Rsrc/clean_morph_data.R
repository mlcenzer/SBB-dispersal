library(zoo)
library(lubridate)
library(dplyr)

read_morph_data = function(path) {
  
  data = read.csv(path, header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)
  
  # Standardize population and host plant names of the all-morphology data
  data$population[data$population=="GainesvilleGRT"]="Gainesville"
  data$population[data$population=="HomesteadBV"]="Homestead"
  data$population[data$population=="HomesteadGRT"]="Homestead"
  data$population[data$population=="Ft.Myers"]="Ft. Myers"
  data$population[data$population=="Key_Largo"]="Key Largo"
  data$population[data$population=="Lake_Placid"]="Lake Placid"
  data$population[data$population=="Lake_Wales"]="Lake Wales"
  data$population[data$population=="North_Key_Largo"]="North Key Largo"
  data$population[data$population=="Plantation_Key"]="Plantation Key"
  
  data$pophost[data$pophost=="C.corindum"]="C. corindum"
  data$pophost[data$pophost=="K.elegans"]="K. elegans"
  
  data = data[order(-data$lat),] 
  lat_order = c("Gainesville", "Leesburg", "Lake Wales", "Lake Placid", "Ft. Myers",  "Homestead",  "North Key Largo", "Key Largo", "Plantation Key")
  data$population = as.factor(data$population)
  data$population = factor(data$population,levels=lat_order)
  
  # Only grab C. corindum and K. elegans host plants
  data=data[data$pophost=="C. corindum" | data$pophost=="K. elegans",]
  data=data[data$sex=="F" | data$sex=="M",]
  
  # Sex recoding
  data$sex_b = -1
  data$sex_b[data$sex=="F"] = 1
  data$pophost_b = -1
  data$pophost_b[data$pophost=="K. elegans"] = 1
  
  # Datetime
  data$date = paste(data$month, data$year, sep="/")
  data$datetime = as.yearmon(data$date, "%B/%Y")
  data$datetime = as.factor(data$datetime)
  n_missing_dates = nrow(data[is.na(data$datetime),])
  
  # merge May 2015 with April 2015 because very few bugs were collected in May 2015.
  # then merge April 2013 with May 2013 to make the time data points more evenly distanced
  data$date[data$date == "May/2015"] = "April/2015"
  data$date[data$date == "May/2013"] = "April/2013"
  
  # convert to yearmon object and then factor
  data$datetime = as.yearmon(data$date, "%B/%Y")
  data$datetime = as.factor(data$datetime)
  
  # convert to datetime object and then extract month number (month_of_year)
  monyeardate = paste(data$datetime," 01",sep="")
  dates = as.Date(monyeardate, "%b %Y %d")
  data$dates = dates
  data$month_of_year = month(dates)
  
  cat("number of missing dates:", n_missing_dates, "\n\n")
  unique(data$datetime)
  
  # morph recording
  cat("morph types:", unique(data$w_morph), "\n")
  cat("   recoding missing morph types...\n   S if wing2thorax <=2.2, L if wing2thorax >=2.5\n\n")
  
  data$wing2thorax = data$wing/data$thorax
  
  data$w_morph[data$w_morph=="" & data$wing2thorax<=2.2]="S"
  data$w_morph[data$w_morph=="" & data$wing2thorax>=2.35]="L"
  
  data$wing_morph_b = NA
  data$wing_morph_b[data$w_morph=="S"]=0
  data$wing_morph_b[data$w_morph=="L"]=1
  
  ambiguous = rbind(data[data$w_morph=="LS",],data[data$w_morph=="SL",])
  cat("ambiguous wing morph bug count: ", nrow(ambiguous), "\n\n")
  
  # Pulling out only long wing morph and computing wing2body ratio
  data_long=data[data$w_morph=="L",]
  data_long$wing2body = data_long$wing/as.numeric(data_long$body)
  cat("filtered out NA wing2body for data_long...\n")
  
  # Remove NA wing2body
  data_long = data_long %>%
    filter(!is.na(wing2body))
  
  # Standardize variables of interest
  data_long$wing2body_c = (data_long$wing2body-mean(data_long$wing2body, na.rm=TRUE))
  data_long$month_of_year_c = (data_long$month_of_year-mean(data_long$month_of_year, na.rm=TRUE))
  data_long$months_since_start_c = (data_long$months_since_start-mean(data_long$months_since_start, na.rm=TRUE))
  
  # Filter out outliers
  data_long = data_long %>% 
    filter(!beak<4.5) %>% 
    filter(!wing2body <0.62) %>% 
    filter(!wing2thorax <2.2) 
  
  # Returning the data
  data_list = list(data, data_long)
  
  return(data_list)
}