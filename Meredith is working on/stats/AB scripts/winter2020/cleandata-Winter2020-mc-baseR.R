#---
#title: "Binomial Modeling: flight_yes_no"
#author: "Anastasia Bernat"
#date: "3/30/2020"
#output: html_document
#---

# Winter 2020 Flight Trials 

#```{r setup, include=FALSE}

# Reading the data

#Flight Trials Winter 2020 Dataset was conducted from 2/17/2020 - 3/10/2020. Soapberry bugs were flight tested twice in the flight mill and observed from 8 AM to (5-8 PM) each day.
read_data<-function(file_name){
#```{r}
data_all_init<-read.csv(file_name, header=TRUE, sep=",", stringsAsFactors=FALSE)
#```

# Recoding column values

#```{r}
data_all <- subset(data_all_init, select = -c(channel_num, channel_letter, NOTES, duration_check, portion_flying, county))

# Yes-No Flew
data_all$flew_b<-0
data_all$flew_b[data_all$flew=="Y"]<-1

# Host
data_all$host_c[data_all$host_plant=="K.elegans"]<-1
data_all$host_c[data_all$host_plant=="C. corindum"]<- -1

# Sex
data_all$sex_c<--1
data_all$sex_c[data_all$sex=="F"]<-1

# Distance From Sympatric Zone
data_all$lat_c<-data_all$latitude-mean(data_all$latitude)
data_all$sym_dist<-abs(data_all$latitude-25.49197)

# Yes-No Eggs on Flight Trial Day
data_all$eggs_b<-0
data_all$eggs_b[data_all$EWM=="Y"]<-1

# ID
data_all$ID<-as.factor(data_all$ID)

# Flight Duration (minutes)
data_all$minute_duration <- 0
data_all$minute_duration <- as.integer(data_all$total_duration / 60)
data_all$minute_duration_c <- data_all$minute_duration-mean(data_all$minute_duration)

# Minutes From When Incubator Lights Turned On
t_IncLights_turn_on <- 8 # AM
data_all$min_from_IncStart <- 0
for(row in 1:length(data_all$ID)){
  time <- chron(times=data_all$time_start[row])
	minute<- minutes(time)
	hour <- hours(time)
	data_all$min_from_IncStart[row] <- 60*(hour - t_IncLights_turn_on) + minute
} 
data_all$min_from_IncStart_c <- data_all$min_from_IncStart-mean(data_all$min_from_IncStart)

# Days From Starting Time
data_all$days_from_start <- 0
data_all$test_date <- as_date(data_all$test_date_init) 
dates <- sort(unique(data_all$test_date))

for (i in 1:length(dates)){

  day_diff <- dates[i] - dates[1]
  for (r in 1:length(data_all$test_date)){
  	#browser()
    if (data_all$test_date[r] == dates[i]) {
      data_all$days_from_start[r] = day_diff }
  }
}
#browser()
data_all$days_from_start_c <- data_all$days_from_start-mean(data_all$days_from_start)

# Mass
data_all$mass_c <- data_all$mass-mean(data_all$mass, na.rm=TRUE) 

# Eggs
data_all$total_eggs_c <- data_all$total_eggs-mean(data_all$total_eggs, na.rm=TRUE) 

# Morphology

# Wing Morph
data_all$w_morph_c <- 0
data_all$w_morph_c[data_all$w_morph=="L"] <- 1 
data_all$w_morph_c[data_all$w_morph=="LS"]<- -1

# Beak Length
data_all$beak_c <- data_all$beak-mean(data_all$beak, na.rm=TRUE)

# Thorax Length
data_all$thorax_c <- data_all$thorax-mean(data_all$thorax, na.rm=TRUE)

# Body Length
data_all$body_c <- data_all$body-mean(data_all$body, na.rm=TRUE)

# Wing Length
data_all$wing_c <- data_all$wing-mean(data_all$wing, na.rm=TRUE)
#```
data_all
}