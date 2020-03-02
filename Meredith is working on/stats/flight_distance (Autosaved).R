rm(list=ls())
setwd("~/Documents/Florida soapberry project/2019 Dispersal/SBB-dispersal git/Meredith is working on/")

data_all<-read.csv("data/flight_summary_latest2.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)

data_all<-data_all[data_all$flew!="",]

data_all$flew_b<-0
data_all$flew_b[data_all$flew=="Y"]<-1

data_all$host_c[data_all$host_plant=="K. elegans"]<-1
data_all$host_c[data_all$host_plant=="C. corindum" | data_all$host_plant=="C. corindum "]<- -1

data_all$sex_c<--1
data_all$sex_c[data_all$sex=="F"]<-1

data_all$lat_c<-data_all$latitude-mean(data_all$latitude)

data_all$sym_dist<-abs(data_all$latitude-25.49197)

data_all$eggs_b<-0
data_all$eggs_b[data_all$eggs=="Y"]<-1

data_all$ID<-as.factor(data_all$ID)

data_all$min_from_start<-0

for(row in 1:length(data_all$days_from_start)){
	minute<-as.numeric(strsplit(strsplit(data_all$time_start[row], ":")[[1]][2], '[ap]m'))
	hour<-as.numeric(strsplit(data_all$time_start[row], ":")[[1]][1])
	if(hour>=7){
		data_all$min_from_start[row]<-60*(hour-8)+minute
	}	
	if(hour<7){
		data_all$min_from_start[row]<-60*(hour+4)+minute
	}
}

data_flew<-data_all[data_all$distance!=0,]



###Break up by trial type
data_short<-data_flew[data_flew$trial_type=="T15" | data_flew$trial_type=="T25" | data_flew$trial_type=="T30",]

#######testing link functions
model_test<-glm(distance~chamber, data=data_short, family=Gamma(link="log")) #equivalent to using log link function in Gamma is to log transform distance, except it won't fuss about non-0 values
summary(model_test)
plot(model_test)

###30 minutes trials:

#######Possible effect of chamber? B-1 looks weird anyway.
summary(glm(distance~chamber, data=data_short, family=Gamma(link="log")))

#######No effect of test date
summary(glm(distance~days_from_start, data=data_short, family=Gamma(link="log")))

#######No effect of test time
summary(glm(distance~min_from_start, data=data_short, family=Gamma(link="log")))


data<-data.frame(R=data_short$distance, A=data_short$host_c, B=data_short$sex_c, C=data_short$sym_dist, X=data_short$chamber)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-Gamma glmer 3-FF log link.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
#4 models did not converge: m11, m14, m33, m34
#top models: m16, m9, m17, m12, m14

anova(m9, m12, test="Chisq") #No effect of adding B
anova(m12, m16, test="Chisq") #No effect of B*C interaction
anova(m16, m17, test="Chisq") #No effect of adding A*B interaction
anova(m12, m14, test="Chisq") #No effect of A*B interaction
####So, let's use m9! No model with chamber was in the top 5.

model30<-glm(distance~host_c*sym_dist, family=Gamma(link="log"), data=data_short)
###Strong negative effect of sym_dist: longer dispersal distances closer to the sympatric zone
###Strong positive interaction between host and sym_dist: the effect of distance from the sympatric zone on dispersal was weaker on GRT bugs

summary30<-aggregate(distance~host_plant*sym_dist, data=data_short, FUN=mean)
summary30BV<-aggregate(distance~sym_dist, data=data_short[data_short$host_c==-1,], FUN=mean)
plot(summary30$distance~summary30$sym_dist, pch=c(19,22)[as.factor(summary30$host_plant)])
plot(summary30BV$distance~summary30BV$sym_dist)


#####Keep ID number in here
data_long<-data_all[data_all$trial_type=="Tlong",]

data<-data.frame(R=data_long$distance, A=data_long$host_c, B=data_long$sex_c, C=data_long$lat_c, X=data_long$ID)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-binomial glmer 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
