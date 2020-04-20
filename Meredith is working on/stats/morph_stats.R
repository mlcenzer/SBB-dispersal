rm(list=ls())
setwd("~/Documents/Florida soapberry project/2019 Dispersal/SBB-dispersal git/Meredith is working on/")

data_all<-read.csv("data/full_data3.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)

data_morph<-read.csv("data/morph_to_cp.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)


#data_all<-data_all[data_all$flew!="",]

#data_all$flew_b<-0
#data_all$flew_b[data_all$flew=="Y"]<-1

data_morph$host_c[data_morph$pophost=="K.elegans"]<-1
data_morph$host_c[data_morph$pophost=="C.corindum"]<- -1

data_morph$sex_c<--1
data_morph$sex_c[data_morph$sex=="F"]<-1

data_morph$lat_c<-data_morph$lat-mean(data_morph$lat)

data_morph$sym_dist<-abs(data_morph$lat-25.49197)

data_morph$thor_c<-data_morph$thorax-mean(data_morph$thorax, na.rm=TRUE)

data_morph$w_morph_b<-NA
data_morph$w_morph_b[data_morph$w_morph=="L"]<-1
data_morph$w_morph_b[data_morph$w_morph=="S"]<-0


###Found one sex conflict, double-check sex between data_all and data_morph
test_data<-data.frame(ID=data_morph$ID, morph_sex=data_morph$sex, trial_sex=NA)

for(row in 1:length(test_data$ID)){
	test_data$trial_sex[row]<-data_all$sex[data_all$ID==test_data$ID[row]][1]
}
test_data$ID[which(test_data$trial_sex!=test_data$morph_sex)]
#in addition to the identified 253, possible problems with 68, 116, 118, and 399
##Exclude for the time being
for(n in c(68, 116, 118, 253, 399)){
	data_morph<-data_morph[data_morph$ID!=n,]
}

#data_all$eggs_b<-0
#data_all$eggs_b[data_all$eggs=="Y"]<-1

#data_all$ID<-as.factor(data_all$ID)


#data_all$min_from_start<-0
min_start_fun<-function(){
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
}


#############A few datasheet conflicts that should be resolved before re-running these.



##########Start with thorax width

data<-data.frame(R=data_morph$thorax, A=data_morph$host_c, B=data_morph$sex_c, C=data_morph$sym_dist)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-gaussian glm 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
###top 8: m4, m7, m12, m8, m11, m13, m16, m14

anova(m4, m7, test="Chisq")##No improvement from adding sym_dist
anova(m7, m12, test="Chisq")##No improvement from adding host*sym_dist interaction
anova(m4, m8, test="Chisq")##No improvement from adding host*sex interaction
anova(m8, m11, test="Chisq")##No improvement from adding sym_dist
anova(m14, m11, test="Chisq")##No improvement from adding host*sym_dist
anova(m16, m13, test="Chisq")##No improvement from adding sex*sym_dist

model_thorax<-glm(thorax~host_c + sex_c, data=data_morph, family=gaussian)

summary(model_thorax)

###Results totally unsurprisingly:
#Females are bigger
#Individuals from K. elegans are bigger





##########Beak length, must also include body size as a covariate


data<-data.frame(R=data_morph$beak, A=data_morph$host_c, B=data_morph$sex_c, C=data_morph$sym_dist, D=data_morph$thor_c)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-gaussian glm 4-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
###top 5: m72, m88, m50, m92, m99, m97
##These are unfortunately complex

anova(m72, m50, test="Chisq")##Significant improvement from inclusion of sex*host interaction
anova(m72, m88, test="Chisq")##No improvement from adding sym_dist
anova(m88, m92, test="Chisq")##No improvement from adding host*sym_dist interaction
anova(m88, m97, test="Chisq")##No improvement from adding sex*sym_dist interaction
anova(m88, m99, test="Chisq")##No improvement from adding thorax*sym_dist interaction

model_beak<-glm(beak~host_c*sex_c + host_c*thor_c + sex_c*thor_c, data=data_morph, family=gaussian)
summary(model_beak)
#Given these three interactions, it seems highly possible that a three-way interaction would improve this model so much as I might regret this I'm going to try that model too.
model_beak_3<-glm(beak~host_c*sex_c*thor_c, data=data_morph, family=gaussian)
anova(model_beak, model_beak_3, test="Chisq")#Thank goodness it doesn't improve it


###There is a lot going on here.
#Bugs from K.elegans have shorter beaks
#Females have longer beaks
#bigger bugs have longer beaks (duh)
#The negative effect of being from K.elegans on beak length is weaker for females
#The positive effect of body size on beak length is weaker for bugs from K.elegans
#The positive effect of body size on beak length is stronger for females

#visualize it:
boxplot(beak~pophost*sex, data=data_morph) #without body size
plot(beak~thorax, data=data_morph, col=c(rgb(1,0.5,0.5),rgb(0,1,0.8,0.5))[as.factor(pophost)], pch=c(19,21)[as.factor(sex)])

##########Wing morph

data<-data.frame(R=data_morph$w_morph_b, A=data_morph$host_c, B=data_morph$sex_c, C=data_morph$sym_dist)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-binomial glm 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
#m18 didn't converge - that's fine, just don't use it
#Top 6 models: m15, m13, m5, m17, m16, m9

anova(m13, m15, test="Chisq")##Marginal improvement from host*sex
anova(m15, m17, test="Chisq")##No improvement from adding host*sym_dist
anova(m13, m16, test="Chisq")##No improvement from adding host*sym_dist
anova(m5, m9, test="Chisq")##No improvement from adding host*sym_dist interaction

model_wmorph<-glm(w_morph_b~host_c*sex_c + sex_c*sym_dist, family=binomial, data=data_morph)
summary(model_wmorph)

##
#Bugs from K.elegans are more likely to have long wings
#There is a weak effect of sex, such that females are marginally more likely to have short wings than males are.
#No direct effect of sym_dist
#No host*sex effect
#Being further from the sympatric zone increases your likelihood of having long wings more if you are female.

#visualize
wing_summary<-aggregate(w_morph_b~sex*pophost*sym_dist, FUN=mean, data=data_morph)
plot(w_morph_b~sym_dist, data=wing_summary, col=c(rgb(1,0.5,0.5),rgb(0,1,0.8,0.5))[as.factor(pophost)], pch=c(19,21)[as.factor(sex)])

##########Wing length (L only), must also include body size as a covariate

data_L<-data_morph[data_morph$w_morph_b==1,]

data<-data.frame(R=data_L$wing, A=data_L$host_c, B=data_L$sex_c, C=data_L$sym_dist, D=data_L$thor_c)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-gaussian glm 4-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)
#top 5 models: m32, m39, m51, m48, m65

anova(m32, m39, test="Chisq")##No improvement from sex (?!)
anova(m32, m51, test="Chisq")##No improvement from adding host*thorax
anova(m32, m48, test="Chisq")##No improvement from adding host*sym_dist
anova(m39, m65, test="Chisq")##No improvement from adding sex*sym_dist

model_wing<-glm(wing~host_c + sym_dist*thor_c, data=data_L, family=gaussian)
summary(model_wing)


##
#Bugs from K. elegans have longer wings
#Bugs further from the sympatric zone have shorter wings
#Larger bugs have longer wings
#Closer to the sympatric zone, the effect of thorax width on wing size is stronger

####The correlation between thorax and wing length is incredibly high, so interpret other effect sizes carefully


#visualize
plot(wing~thorax, data=data_L, col=c(rgb(1,0.5,0.5),rgb(0,1,0.8,0.5))[as.factor(pophost)], pch=c(19,21)[as.factor(sex)])
plot(wing~sym_dist, data=data_L, col=c(rgb(1,0.5,0.5),rgb(0,1,0.8,0.5))[as.factor(pophost)], pch=c(19,21)[as.factor(sex)])

wing_length_summary<-aggregate(wing~sex*pophost*sym_dist, data=data_L, FUN=mean)
wing_length_summary
