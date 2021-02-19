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
  
  data<-data[data$pophost=="C.corindum" | data$pophost=="K.elegans",]
  data<-data[data$sex=="F" | data$sex=="M",]

  data$w_morph_binom<-NA
  data$wing_morph_binom[data$w_morph=="S"]<-0
  data$wing_morph_binom[data$w_morph=="L"]<-1
  
  data$wing2thorax <- data$wing/data$thorax
  
  data$w_morph[data$w_morph=="" & data$wing2thorax<=2.2]<-"S"
  data$w_morph[data$w_morph=="" & data$wing2thorax>=2.35]<-"L"
  
  data$month_of_year <- (data$months_since_start+7)%%12+1
  
  # Pulling out only long wing morph and removing those with torn wings
  data_long<-data[data$w_morph=="L",]
  data_long$drop <- FALSE
  
  for(row in 1:nrow(data_long)){
    if(length(unlist(strsplit(strsplit(paste("test ", data_long$notes[row], 
                                             " test", sep=""), "torn")[[1]], "wing")))>2){
      #browser()	
      data_long$drop[row] <- TRUE
    }
  }
  
  data_long <- data_long[data_long$drop==FALSE,]
  data_long$wing2body <- data_long$wing/as.numeric(data_long$body)
  
  # Returning the data
  
  data_list = list(data, data_long)
  
  return(data_list)
}