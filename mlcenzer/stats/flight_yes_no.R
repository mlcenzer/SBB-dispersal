rm(list=ls())
setwd("~/Documents/Florida soapberry project/2019 Dispersal/SBB-dispersal git/Meredith is working on/")

data_all<-read.csv("data/full_data3.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)

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


##############Excel was built by a goddamn monster

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


##Analyze as binomial; must-includes are trial_type, ID number

###Are individual measures correlated across trials?
####I am having issues of non-convergence that appear to be due to singularity 
library(lme4)
model<-glmer(flew_b~host_c+(1|ID), data=data_all, family=binomial)
##This model will not converge because of singularity in the random factor ID

getME(model, "lower") #this is 0; so, according to the internet (https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html) that means we have to simplify the model. The model can't get any simpler, so sadly it looks like we're going to have to do this trial by trial...which is likely to be a total mess!



###30 minutes trials:

data_short<-data_all[data_all$trial_type=="T15" | data_all$trial_type=="T25" | data_all$trial_type=="T30",]

#######No effect of chamber
summary(glm(flew_b~chamber, data=data_short, family=binomial))

#######No effect of test date
summary(glm(flew_b~days_from_start, data=data_short, family=binomial))

#######No effect of test time
summary(glm(flew_b~min_from_start, data=data_short, family=binomial))


data<-data.frame(R=data_short$flew_b, A=data_short$host_c, B=data_short$sex_c, C=data_short$sym_dist)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-binomial glm 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
#Top models: m16, m17, m13, m18


anova(m16, m17, test="Chisq") ##Adding the A*B interaction does not improve fit
anova(m17, m18, test="Chisq") ##Adding the 3-way interaction does not improve fit
anova(m16, m13, test="Chisq") ##Adding the A*C interaction does improve fit


model30<-glm(flew_b~host_c*sym_dist + sex_c*sym_dist, family=binomial, data=data_short)

summary(model30)
##No detectable effect of being from GRT; 
##strong negative effect of being far from the sympatric zone; 
##strong negative effect of being female; 
##marginal positive interaction between host and distance from the sympatric zone, such that you're more likely to be dispersive if you're on balloon vine and close to the sympatric zone
##strong positive interaction between sex and distance from the sympatric zone, such that the positive effects of being far from the sympatric zone are stronger if you're female

summary30<-aggregate(flew_b~host_plant*sym_dist*sex, data=data_short, FUN=mean)
plot(summary30$flew_b~summary30$sym_dist, col=c(1,2)[as.factor(summary30$sex)], pch=c(19,22)[as.factor(summary30$host_plant)])


####Consider covariates: population
model30A<-glmer(flew_b~host_c*sym_dist + sex_c*sym_dist + (1|population), family=binomial, data=data_short)
##Did not change effect estimates or improve model fit










################Hour-long trials
data_hour<-data_all[data_all$trial_type=="T60",]

#######Possible weak effect of being on the top shelf; A-4 and B-4 have marginal positive effects
summary(glm(flew_b~chamber, data=data_hour, family=binomial))

#######No effect of test date
summary(glm(flew_b~days_from_start, data=data_hour, family=binomial))

#######No effect of test time
summary(glm(flew_b~min_from_start, data=data_hour, family=binomial))


data<-data.frame(R=data_hour$flew_b, A=data_hour$host_c, B=data_hour$sex_c, C=data_hour$sym_dist, X=data_hour$chamber)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-binomial glmer 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
###Top models: m13, m15, m16, m30, m17, m32

anova(m13, m15, test="Chisq")##No improvement from adding A*B interaction
anova(m13, m16, test="Chisq")##No improvement from adding A*C interaction
anova(m15, m17, test="Chisq")##No improvement from adding A*B*C interaction
anova(m16, m17, test="Chisq")##No improvement from adding A*B*C interaction
###anova for glm vs. glmer doesn't do the same thing but adding the random factor didn't significantly improve fit


model60<-glm(flew_b~host_c + sex_c*sym_dist, family=binomial, data=data_hour)


#####strong positive effect of being from GRT
#####strong negative effect of being female
#####strong negative effect of being far from the sympatric zone
#####strong sex*sym_dist interaction, such that females far from the sympatric zone disperse more than expected



summary60<-aggregate(flew_b~host_plant*sym_dist*sex, data=data_hour, FUN=mean)
plot(summary60$flew_b~summary60$sym_dist, col=c(1,2)[as.factor(summary60$sex)], pch=c(19,22)[as.factor(summary60$host_plant)])












###########Ninety minute trials


data_ninety<-data_all[data_all$trial_type=="T90",]

#######No effect of chamber
summary(glm(flew_b~chamber, data=data_ninety, family=binomial))

#######No effect of test date
summary(glm(flew_b~days_from_start, data=data_ninety, family=binomial))

#######No effect of test time
summary(glm(flew_b~min_from_start, data=data_ninety, family=binomial))


data<-data.frame(R=data_ninety$flew_b, A=data_ninety$host_c, B=data_ninety$sex_c, C=data_ninety$sym_dist)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-binomial glm 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
#Top models: m16, m13, m17, m15, m18

anova(m13, m16, test="Chisq")#Marginal positive effect of adding A*C interaction; should consider both m13 and m16
anova(m16, m17, test="Chisq")#No effect of adding A*B
anova(m13, m15, test="Chisq")#No effect of adding A*B interaction
anova(m17, m18, test="Chisq")#No effect of adding A*B*C interaction

model90<-glm(flew_b~host_c + sex_c*sym_dist, family=binomial, data=data_hour)
model90A<-glm(flew_b~host_c*sym_dist + sex_c*sym_dist, family=binomial, data=data_hour)

summary(model90)
#Strong positive effect of being from GRT
#Strong negative effect of being female
#Strong negative effect of being far from the sympatric zone
#Strong sex*sym dist interactions, such that females far from the sympatric zone are better fliers than expected

summary(model90A)
#No effect of being from GRT
#Strong negative effect of being female
#No effect of being far from the sympatric zone
#No effect of host*sym_dist interaction
#Strong sex*sym dist interactions, such that females far from the sympatric zone are better fliers than expected

summary90<-aggregate(flew_b~host_plant*sym_dist*sex, data=data_ninety, FUN=mean)
plot(summary90$flew_b~summary90$sym_dist, col=c(1,2)[as.factor(summary90$sex)], pch=c(19,22)[as.factor(summary90$host_plant)])






#Because we dropped non-fliers, we cannot evaluate yes/no flying in multi-hour trials.
