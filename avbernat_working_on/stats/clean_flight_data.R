# A script that loads and cleans up the flight data

############ 1. ) Loading the data #####################

setwd("~/Desktop/Rstats-winter2020/")
data_all<-read.csv("complete_flight_data-Winter2020.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

############ 2. ) Remove unnecessary columns ############

data_all <- subset(data_all, select = -c(channel_num, channel_letter, NOTES, duration_check, 
                                         portion_flying, county))

############ 3. ) Recode column values ##################

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
data_all$test_date <- as_date(data_all$test_date) 
dates <- sort(unique(data_all$test_date))

for (i in 1:length(dates)){
  day_diff <- dates[i] - dates[1]
  for (r in 1:length(data_all$test_date)){
    if (data_all$test_date[r] == dates[i]) {
      data_all$days_from_start[r] = day_diff }
  }
}

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