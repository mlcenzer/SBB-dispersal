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


data<-data.frame(R=data_short$flew_b, A=data_short$host_c, B=data_short$sex_c, C=data_short$lat_c)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-binomial glm 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
#Top models: m16, m17, m18


anova(m16, m17, test="Chisq") ##Adding the A*B interaction does not improve fit
anova(m17, m18, test="Chisq") ##Adding the 3-way interaction does not improve fit

model30<-glm(flew_b~host_c*lat_c + sex_c*lat_c, family=binomial, data=data_short)

summary(model30)
##marginal negative effect of being from GRT; strong positive effect of being from north; strong negative effect of being female; strong negative interaction between host and latitude, such that the positive effective of northiness is reduced if you're on GRT; strong positive interaction between sex and latitude, such that the positive effects of northiness are stronger if you're female

####Consider covariates: population
model30A<-glmer(flew_b~host_c*lat_c + sex_c*lat_c + (1|population), family=binomial, data=data_short)
##Did not change effect estimates or improve model fit










################Hour-long trials
data_hour<-data_all[data_all$trial_type=="T60",]

#######Possible weak effect of being on the top shelf; A-4 and B-4 have marginal positive effects
summary(glm(flew_b~chamber, data=data_hour, family=binomial))

#######No effect of test date
summary(glm(flew_b~days_from_start, data=data_hour, family=binomial))

#######No effect of test time
summary(glm(flew_b~min_from_start, data=data_hour, family=binomial))


data<-data.frame(R=data_hour$flew_b, A=data_hour$host_c, B=data_hour$sex_c, C=data_hour$lat_c, X=data_hour$chamber)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-binomial glmer 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
###Top models: m13, m16, m15, m30, m17, m33

anova(m13, m16, test="Chisq")##No improvement from adding A*C interaction
anova(m13, m15, test="Chisq")##No improvement from adding A*B interaction
anova(m16, m17, test="Chisq")##No improvement from adding A*B*C interaction
anova(m15, m17, test="Chisq")##No improvement from adding A*B*C interaction
###anova for glm vs. glmer doesn't do the same thing but adding the random factor didn't significantly improve fit

#####strong positive effect of being from GRT
#####strong negative effect of being female
#####strong negative effect of being more north
#####strong sex*host interaction, such that females from GRT disperse more often than expected

















data_ninety<-data_all[data_all$trial_type=="T90",]

data<-data.frame(R=data_ninety$flew_b, A=data_ninety$host_c, B=data_ninety$sex_c, C=data_ninety$lat_c)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-binomial glm 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)



#####Keep ID number in here
data_long<-data_all[data_all$trial_type=="Tlong",]

data<-data.frame(R=data_long$flew_b, A=data_long$host_c, B=data_long$sex_c, C=data_long$lat_c, X=data_long$ID)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-binomial glmer 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)






##Factors to test first: host, sex, latitude

data<-data.frame(R=data_all$flew_b, A=data_all$host_c, B=data_all$sex_c, C=data_all$lat_c, X=as.factor(data_all$ID), Y=data_all$trial_type)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-binomial glmer must have 2 RFs.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)

###Models that are not converging: 

summary<-aggregate(flew_b~sym_dist*sex, data=data_all, FUN=mean)
summary$n<-aggregate(flew_b~sym_dist*sex, data=data_all, FUN=length)$flew_b


summary<-aggregate(flew_b~host_plant*sex, data=data_all, FUN=mean)
summary$n<-aggregate(flew_b~host_plant*sex, data=data_all, FUN=length)$flew_b


######Subset to just females and add reproductive status

data_F<-data_all[data_all$sex=="F",]

data<-data.frame(R=data_F$flew_b, A=data_F$host_c, B=data_F$lat_c, C=data_F$eggs_b, X=as.factor(data_F$ID), Y=data_F$trial_type)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-binomial glmer 2-RF + 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)



###Ok, but does host predict eggs or not?

data<-data.frame(R=data_F)


