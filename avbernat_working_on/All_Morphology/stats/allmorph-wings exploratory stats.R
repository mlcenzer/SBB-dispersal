
##########base statistics of all morphology for wing morph and wing2body ratio (in long-winged bugs)

#input allmorphology here
raw_data<-read.csv("~/Dropbox/SBB datasheets 2019/Morphology/allmorphology data/allmorphology9.21.20.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)

####standardize population names!
raw_data$population[raw_data$population=="Key Largo"]<-"Key_Largo"
raw_data$population[raw_data$population=="North Key Largo"]<-"North_Key_Largo"
raw_data$population[raw_data$population=="Plantation Key"]<-"Plantation_Key"
raw_data$population[raw_data$population=="Lake Wales"]<-"Lake_Wales"
raw_data$pophost[raw_data$pophost=="C. corindum"]<-"C.corindum"
raw_data$pophost[raw_data$pophost=="K. elegans"]<-"K.elegans"
raw_data$pophost[raw_data$pophost=="K. elegans "]<-"K.elegans"

raw_data<-raw_data[raw_data$pophost=="C.corindum" | raw_data$pophost=="K.elegans",]
raw_data<-raw_data[raw_data$sex=="F" | raw_data$sex=="M",]

###infer missing w_morph values
raw_data_missing<-raw_data[raw_data$w_morph=="",]

par(mfrow=c(3,1))

hist(raw_data$wing[raw_data$w_morph=="L"]/raw_data$thorax[raw_data$w_morph=="L"], breaks=seq(0.5,3.8,by=0.05))
hist(raw_data_missing$wing/raw_data_missing$thorax, breaks=seq(0.5, 3.8, by=0.05))
hist(raw_data$wing[raw_data$w_morph=="S"]/raw_data$thorax[raw_data$w_morph=="S"], breaks=seq(0.5,3.8,by=0.05))

raw_data$wing2thorax <- raw_data$wing/raw_data$thorax

raw_data$w_morph[raw_data$w_morph=="" & raw_data$wing2thorax<=2.2]<-"S"
raw_data$w_morph[raw_data$w_morph=="" & raw_data$wing2thorax>=2.35]<-"L"
###handles vast majority of cases

raw_data$w_morph_binom <- NA
raw_data$wing_morph_binom[raw_data$w_morph=="S"]<-0
raw_data$wing_morph_binom[raw_data$w_morph=="L"]<-1
raw_data$sex_binom <- -1
raw_data$sex_binom[raw_data$sex=="F"] <- 1
raw_data$pophost_binom <- -1
raw_data$pophost_binom[raw_data$pophost=="K.elegans"] <- 1

raw_data$month_of_year <- (raw_data$months_since_start+7)%%12+1



#########wing morph stats
data<-data.frame(R=raw_data$wing_morph_binom, A=raw_data$sex_binom, B=raw_data$pophost_binom, C=(raw_data$month_of_year))

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-binomial glm 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)


###Very robustly, bugs from K.elegans are more likely to be long-winged than those from C. corindum and males are more likely to be long-winged than females. There is no effect of months_since_start.
###There is a strong negative effect of getting later in the season, such that short wing morphs become increasingly likely later in the year.






########wing2body
data_long<-raw_data[raw_data$w_morph=="L",]

###remove individuals with torn wings first.
data_long$drop <- FALSE

for(row in 1:nrow(data_long)){
	if(length(unlist(strsplit(strsplit(paste("test ", data_long$notes[row], " test", sep=""), "torn")[[1]], "wing")))>2){
		 #browser()	
		 data_long$drop[row] <- TRUE
		 }
}

data_long <- data_long[data_long$drop==FALSE,]

data_long$wing2body <- data_long$wing/as.numeric(data_long$body)




######winter 2020 vs. other dates
data_long$compare_dates <- -1
data_long$compare_dates[data_long$months_since_start==81] <- 1

compare_dates_model <- glm(wing2body~compare_dates + pophost_binom*sex_binom, data=data_long)

summary(compare_dates_model)

#Yes, bugs from this collection date have detectably smaller wing2body ratios on average than other collection dates, even when controlling for sex and host plant.



data<-data.frame(R=(data_long$wing2body-mean(data_long$wing2body, na.rm=TRUE)), A=data_long$sex_binom, B=data_long$pophost_binom, C=(data_long$month_of_year-mean(data_long$month_of_year, na.rm=TRUE)), D=(data_long$months_since_start-mean(data_long$months_since_start, na.rm=TRUE)))


library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-gaussian glm 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
#top models: m15, m8, m17, m11

anova(m15, m17, test="Chisq")


##Robustly:

#Females have lower wing2body ratios than males
#Bugs from K. elegans have longer wing2body ratios than those from C. corindum
#wing2body ratios are decreasing moderately towards the present
#Females on K. elegans have longer wings than expected based on host and sex alone

#Possibly, but not definitively: There is a negative effect of month of year on wing length if you are from K. elegans
####Based on the plot below, I do not buy this AT ALL - there is no visible interaction between these, I think this is only being detected because the dataset here is so huge. Nicer figures of this have been generated split by host and sex, still not convincing imo




wing2body_summary<-aggregate(wing2body~pophost*month_of_year, data=data_long, FUN=mean)

plot(wing2body~month_of_year, data=wing2body_summary, pch=19, col=c(1,2)[as.factor(pophost)])















#########load winter experimental bugs
rm(list=ls())
setwd("~/Documents/Florida soapberry project/2019 Dispersal/SBB-dispersal git/avbernat_working_on/Dispersal/Winter_2020/stats")
##load libraries

library(lme4)
library(dplyr)
library(ggplotify)
library(gridExtra)
library(ggformula)
library(tidyselect)
library(zoo)

##source scripts

output_col = TRUE # Recommend changing this to TRUE if working in Base R or RStudio, and FALSE if generating an html
source("src/clean_flight_data.R") # Script that loads and cleans up the data
source("src/regression_output.R") # A script that cleans up regression outputs and prints in color or black and white
source("src/center_flight_data.R")
source("src/get_warnings.R")

##read in and clean up data
data_winter_full <- read_flight_data("data/all_flight_data-Winter2020.csv")
data_winter_all <- data_winter_full[[1]]
data_winter_tested <- data_winter_full[[2]]

###for these analyses, morphology only
data_winter <- data_winter_tested[,c('beak','thorax', 'wing', 'body', 'w_morph', 'morph_notes')]

##load full dataset & pare to long-winged bugs only

##################input allmorphology here
raw_data<-read.csv("~/Dropbox/SBB datasheets 2019/Morphology/allmorphology data/allmorphology9.21.20.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)

####standardize population names!
raw_data$population[raw_data$population=="Key Largo"]<-"Key_Largo"
raw_data$population[raw_data$population=="North Key Largo"]<-"North_Key_Largo"
raw_data$population[raw_data$population=="Plantation Key"]<-"Plantation_Key"
raw_data$population[raw_data$population=="Lake Wales"]<-"Lake_Wales"
raw_data$pophost[raw_data$pophost=="C. corindum"]<-"C.corindum"
raw_data$pophost[raw_data$pophost=="K. elegans"]<-"K.elegans"
raw_data$pophost[raw_data$pophost=="K. elegans "]<-"K.elegans"

raw_data<-raw_data[raw_data$pophost=="C.corindum" | raw_data$pophost=="K.elegans",]
raw_data<-raw_data[raw_data$sex=="F" | raw_data$sex=="M",]
raw_data$wing2thorax <- raw_data$wing/raw_data$thorax

raw_data$w_morph[raw_data$w_morph=="" & raw_data$wing2thorax<=2.2]<-"S"
raw_data$w_morph[raw_data$w_morph=="" & raw_data$wing2thorax>=2.35]<-"L"

data_all_dates<-raw_data[raw_data$w_morph=="L",c('beak','thorax', 'wing', 'body', 'w_morph', 'notes')]
