##2019 data
setwd("~/Dropbox/SBB datasheets 2019/Mate choice/2019 mate choice")
#load functions and metadata
source("age calculator.R")

#Load and prep metadata
#metadata
data_raw<-read.csv("labdata_8_29.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)
metadata<-data_raw[data_raw$eclosion.date!="" & data_raw$eclosion.date!="eclosion.date", c(1,2,3,5,6,9,10)]

morph_dataF<-read.csv('assembled data/morphology_females.csv', header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)

morph_dataM<-read.csv('assembled data/morphology_males.csv', header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)

morph_data<-rbind(morph_dataF[,c(1,3,4,5,6,7,8)], morph_dataM[1:136,c(1,4,5,6,7,8,9)])

morph_data$sex[morph_data$sex=="FALSE"]<-"F"
data_with_reps<-cbind(morph_data, metadata[sapply(1:nrow(morph_data), function(x){which(metadata$ID==morph_data$ID_num[x])}),c(3,4,5,7)])

data<-data.frame(ID=unique(data_with_reps$ID_num), sex=NA, beak=NA, thorax=NA, wing=NA, body=NA, w_morph=NA, pop=NA, nathost=NA, nathost.site=NA)

#drop duplicates
for(ID in 1:nrow(data)){
	data[ID,]<-data_with_reps[data_with_reps$ID_num==data$ID[ID],][1,1:10]
}

data$pophost<-"K.elegans"
data$pophost[data$pop=="PK" | data$pophost=="KL" | data$pophost=="NKL"]<-"C.corindum"

data2019<-data[data$w_morph=="L",]

data2019$wing2body<-data2019$wing/data2019$body
data2019$sex_binom<-1
data2019$sex_binom[data2019$sex=="M"]<--1
data2019$pophost_binom<--1
data2019$pophost_binom[data2019$pophost=="K.elegans"]<-1
data2019$nathost_binom<--1
data2019$nathost_binom[data2019$nathost=="K.elegans"]<-1


data<-data.frame(R=(data2019$wing2body-mean(data2019$wing2body, na.rm=TRUE)), A=data2019$sex_binom, B=data2019$pophost_binom, C=data2019$nathost_binom)


library(lme4)

#run AICprobs script
setwd("~/Dropbox/SBB datasheets 2019/Dispersal Project/analyses of previous data/generic models")
source("AICprobabilities.R")
source("generic models-gaussian glm 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)



####NO nathost effect on wing2body ratio!
####What if look within each sex?
dataF<-data2019[data2019$sex=="F",]

modelF<-glm(wing2body~pophost_binom*nathost_binom, data=dataF)
summary(modelF)

dataF<-data2019[data2019$sex=="F",]

dataM<-data2019[data2019$sex=="M",]
modelM<-glm(wing2body~pophost_binom*nathost_binom, data=dataM)
summary(modelM)
