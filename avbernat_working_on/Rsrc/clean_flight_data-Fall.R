
clean_flight_data.Fall <- function(path, morph_data) {
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
  
  ## Prepping Data to Merge with Morphology Data
  
  # Missing bugs. The morph data has *more* unique IDs than the flight data (which is a good thing). 
  # They may have just not been flown.
  string = paste("Missing ", length(sort(unique(morph_data$ID))) - length(sort(unique(data_all$ID))), 
        " flight bugs from morph measurements. These bugs are: ")
 
  mID = sort(unique(morph_data$ID)) 
  fID = sort(unique(data_all$ID))
  diff = setdiff(mID, fID)
  print(string)
  print(diff)
  
  # But there are also four bugs flown that did not exist in the morph data. 53, 156, 221, and 272. I removed them.
  data_all = data_all[data_all$ID != 53 & data_all$ID != 156 & data_all$ID != 221 & data_all$ID != 272, ]
  
  ## Sex Inaccuracies 
  
  # Also needed to fix inaccurate flight trial sex identification with the proper morph sex identification:
  data_all[data_all$ID == 68,]$sex = morph_data[morph_data$ID == 68,]$sex
  data_all[data_all$ID == 116,]$sex = morph_data[morph_data$ID == 116,]$sex
  data_all[data_all$ID == 118,]$sex = morph_data[morph_data$ID == 118,]$sex
  data_all[data_all$ID == 399,]$sex = morph_data[morph_data$ID == 399,]$sex

  # Sex
  data_all$sex_c<--1
  data_all$sex_c[data_all$sex=="F"]<-1
  
  ## Merging with Morph Data
  data <- merge(data_all, morph_data, by=c("ID"))
  
  # fix the poor merging to match the morph_data writing | not clean code but the best way around it
  data$sex = data$sex.y
  data$population = data$population.y
  data_merged = data %>%
    select(-sex.x, -population.x, -sex.y, -population.y)
  
  # Check that the merge worked:
  string = paste("Did the merge work?", nrow(data_all) == nrow(data))
  print(string)
  
  data_list = list(data_all, data_merged)
  return(data_merged)
  
}