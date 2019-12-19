#exploring dispersal data
rm(list=ls())
setwd("~/Documents/Florida soapberry project/2019 Dispersal")

data30<-read.csv("data/dispersal_data.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)

data60_raw<-read.csv("data/dispersal_data2.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)
data60<-data60_raw[!is.na(data60_raw$flew),]

data90_raw<-read.csv("data/dispersal3.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)
data90<-data90_raw[!is.na(data90_raw$flew),]

datalong_raw<-read.csv("data/dispersal4.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)
datalong<-datalong_raw[!is.na(datalong_raw$flew),]



data<-data.frame(ID=as.factor(c(data30$ID, data60$ID, data90$ID, datalong$ID)), sex=NA, population=c(data30$population, data60$population, data90$population, datalong$population), chamber=c(data30$chamber, data60$chamber, data90$chamber, datalong$chamber), trial=rep(c("30", "60", "90", "long"), times=c(nrow(data30), nrow(data60), nrow(data90), nrow(datalong))), flew=c(data30$flew, data60$flew, data90$flew, datalong$flew), flight_type=c(data30$flight_type, data60$flight_type, data90$flight_type, datalong$flight_type), mass=c(rep(NA, times=nrow(data30)), data60$mass, data90$mass, datalong$mass), pophost=NA)

for(row in 1:nrow(data)){
	data$sex[row]<-data30$sex[data30$ID==data$ID[row]]
	if(data$population[row]=="North Key Largo"){
		data$pophost[row]<-"C.corindum"
	}
	else if(data$population[row]=="Key Largo") data$pophost[row]<-"C.corindum"
	else if(data$population[row]=="Plantation Key") data$pophost[row]<-"C.corindum"
	else {data$pophost[row]<-"K.elegans"}
}


data<-data[data$flew=="Y" | data$flew=="N",]
data<-na.omit(data)
data$flew_num<-0
data$flew_num[data$flew=="Y"]<-1

library(lme4)

model1<-glmer(flew_num~mass + sex + pophost + (1|chamber), family=binomial, data=data)
summary(model1)
