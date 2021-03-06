
clean_flight_data.Fall <- function(path) {
  data_all = read.csv(path,header=TRUE, sep=",", stringsAsFactors=TRUE) 
  
  #ID
  data_all$ID<-as.factor(data_all$ID)
  
  # Yes-No Flew
  data_all$flew_b<-0
  data_all$flew_b[data_all$flew=="Y"]<-1
  
  # Host
  data_all$host_temp[data_all$host_plant=="K.elegans"]<- "K. elegans"
  data_all$host_temp[data_all$host_plant=="C. corindum"]<- "C. corindum"
  data_all$host_c[data_all$host_temp=="K. elegans"]<- 1
  data_all$host_c[data_all$host_plant=="C. corindum"]<- -1
  
  # Sex
  data_all$sex_c<--1
  data_all$sex_c[data_all$sex=="F"]<-1
  
  # Wing Morph
  data_all$w_morph_c <- -1 # short wing
  data_all$w_morph_c[data_all$w_morph=="L"] <- 1 
  data_all$w_morph_c[data_all$w_morph=="l"] <- 1
  data_all$w_morph_c[data_all$w_morph=="LS"]<- 0
  
  # Yes-No Eggs on Flight Trial Day
  data_all$eggs_b<-0
  data_all$eggs_b[data_all$eggs=="Y"]<-1
 
  # Center Data
  ## Distance From Sympatric Zone
  data_all$lat_c<-data_all$latitude-mean(data_all$latitude)
  data_all$sym_dist<-abs(data_all$latitude-25.49197)
  
  ## Average Mass
  data_all$mass_c <- data_all$mass-mean(data_all$mass, na.rm=TRUE) 
  
  return(data_all)
  
}