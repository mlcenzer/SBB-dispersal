
remove_torn_wings = function(d){
  
  d$drop <- FALSE
  for(row in 1:nrow(d)){
    if(length(unlist(strsplit(strsplit(paste("test ", d$notes[row], 
                                             " test", sep=""), "torn")[[1]], "wing")))>2){
      #browser()	
      d$drop[row] <- TRUE
    }
  }
  df <- d[d$drop==FALSE,]
  return(df)
}

read_morph_data = function(path) {
  
  data = read.csv(path, header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)
  
  # Standardize population and host plant names of the all-morphology data
  data$population[data$population=="Key Largo"]<-"Key_Largo"
  data$population[data$population=="North Key Largo"]<-"North_Key_Largo"
  data$population[data$population=="Plantation Key"]<-"Plantation_Key"
  data$population[data$population=="Lake Wales"]<-"Lake_Wales"
  data$population[data$population=="GainesvilleGRT"]<-"Gainesville"
  data$population[data$population=="HomesteadBV"]<-"Homestead"
  data$population[data$population=="HomesteadGRT"]<-"Homestead"
  
  data$pophost[data$pophost=="C. corindum"]<-"C.corindum"
  data$pophost[data$pophost=="K. elegans"]<-"K.elegans"
  data$pophost[data$pophost=="K. elegans "]<-"K.elegans"
  
  # Only grab C.corindum and K.elegans host plants
  data<-data[data$pophost=="C.corindum" | data$pophost=="K.elegans",]
  data<-data[data$sex=="F" | data$sex=="M",]

  # Sex and wing morph re-coding
  data$sex_binom <- -1
  data$sex_binom[data$sex=="F"] <- 1
  data$pophost_binom <- -1
  data$pophost_binom[data$pophost=="K.elegans"] <- 1
  
  data$wing2thorax <- data$wing/data$thorax
  
  data$w_morph[data$w_morph=="" & data$wing2thorax<=2.2]<-"S"
  data$w_morph[data$w_morph=="" & data$wing2thorax>=2.35]<-"L"
  
  data$wing_morph_binom<-NA
  data$wing_morph_binom[data$w_morph=="S"]<-0
  data$wing_morph_binom[data$w_morph=="L"]<-1
  
  data$month_of_year <- (data$months_since_start+7)%%12+1
  
  # Datetime
  data$date <- paste(data$month, data$year, sep="/")
  data$datetime <- as.yearmon(data$date, "%B/%Y")
  data$datetime <- as.factor(data$datetime)
  n_missing_dates = nrow(data[is.na(data$datetime),])
  
  # merge May 2015 with April 2015 because very few bugs were collected in May 2015.
  # then merge April 2013 with May 2013 to make the time data points more evenly distanced
  data$date[data$date == "May/2015"] = "April/2015"
  data$date[data$date == "May/2013"] = "April/2013"
  
  # convert to yearmon object and then factor
  data$datetime <- as.yearmon(data$date, "%B/%Y")
  data$datetime <- as.factor(data$datetime)
  
  cat("number of missing dates:", n_missing_dates, "\n\n")
  unique(data$datetime)
  
  # Pulling out only long wing morph and computing wing2body ratio
  data_long<-data[data$w_morph=="L",]
  data_long$wing2body <- data_long$wing/as.numeric(data_long$body)
  
  # Returning the data
  data_list = list(data, data_long)
  
  return(data_list)
}