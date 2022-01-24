#exploring dispersal data
rm(list=ls())
setwd("~/Documents/Florida soapberry project/2019 Dispersal")

data30<-read.csv("data/dispersal_data.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)

data60<-read.csv("data/dispersal_data2.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)

data90<-read.csv("data/dispersal_data3.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)


flew30<-data30$ID[data30$flew=="Y"]
flew30<-flew30[!is.na(flew30)]

flew60<-data60$ID[data60$flew=="Y"]
flew60<-flew60[!is.na(flew60)]

flew90<-data90$ID[data90$flew=="Y"]
flew90<-flew90[!is.na(flew90)]

multifliers<-intersect(flew30,flew60) #intersect(flew60,flew90), intersect(flew30,flew90) #put unique statement before vector of all three intersects

#filter deceased
fly_set<-data60[data60$ID%in%multifliers & data60$died!="Y",c(1,2,8,10)]
fly_set$flight_type_30<-NA
fly_set$flight_type_60<-NA
fly_set$sex<-NA

for(ind in fly_set$ID){
	fly_set$flight_type_30[fly_set$ID==ind]<-data30$flight_type[data30$ID==ind]
	fly_set$flight_type_60[fly_set$ID==ind]<-data60$flight_type[data60$ID==ind]
	fly_set$sex[fly_set$ID==ind]<-data30$sex[data30$ID==ind]	
}


no_fly_list<-intersect(data30$ID[data30$flew=="N"], data60$ID[data60$flew=="N"])

nonfliers<-data60[data60$ID%in%no_fly_list & data60$died!="Y", c(1,8,9,10)]
nonfliers<-nonfliers[!is.na(nonfliers$ID),]

nonfliers$flew30<-NA

for(ind in nonfliers$ID){
	nonfliers$flew30[nonfliers$ID==ind]<-data30$flew[data30$ID==ind]
}

write.csv(nonfliers, "nonfliers.csv")