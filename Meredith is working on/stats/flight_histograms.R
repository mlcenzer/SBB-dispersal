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

dist_summary<-aggregate(distance~ID*host_c, data=data_all, FUN=mean)
dist_summary$n<-aggregate(distance~ID*host_c, data=data_all, FUN=length)$distance


hist(dist_summary$distance[dist_summary$host_c==1], col=rgb(0,1,1,0.5), breaks=seq(0,4200, by=50), ylim=c(0,100), xlab="Average distance (m)", main='Soapberry bug dispersal fall 2019')
hist(dist_summary$distance[dist_summary$host_c==-1], col=rgb(1,0.2,0,0.5), breaks=seq(0,4200, by=50), add=TRUE)
legend(3000, 80, legend=c("Spatially clustered host", "Spatially unclustered host"), pch=15, col=c(rgb(0,1,1,0.5), rgb(1,0.2,0,0.5)))

####With a flipped axis?

dist_summary<-aggregate(distance~ID*host_c, data=data_all, FUN=mean)
dist_summary$n<-aggregate(distance~ID*host_c, data=data_all, FUN=length)$distance

par(mfrow=c(2,1), mar=c(0.1,4.9,4.8,0.1))
hist(dist_summary$distance[dist_summary$host_c==1], col=rgb(0,1,1,0.5), breaks=seq(0,4200, by=50), ylim=c(0,90), xlab="", main='Soapberry bug dispersal fall 2019', xaxt='n')
legend(2800, 80, legend=c("Spatially clustered host", "Spatially unclustered host"), pch=15, col=c(rgb(0,1,1,0.5), rgb(1,0.2,0,0.5)))
par(mar=c(4.8,4.9,0.1,0.1))
hist(dist_summary$distance[dist_summary$host_c==-1], col=rgb(1,0.2,0,0.5), breaks=seq(0,4200, by=50), ylim=c(90,0), xlab="Average distance (m)", main="")


####With a flipped axis, considering a 'bin' to be 1km?

dist_summary<-aggregate(distance~ID*host_c, data=data_all, FUN=mean)
dist_summary$n<-aggregate(distance~ID*host_c, data=data_all, FUN=length)$distance

par(mfrow=c(2,1), mar=c(0.1,4.9,4.8,0.1))
hist((dist_summary$distance[dist_summary$host_c==1]/1000), col=rgb(0,1,1,0.5), breaks=seq(-1,10, by=0.5), ylim=c(0,90), xlab="", main='Soapberry bug dispersal fall 2019', xaxt='n')
legend(5, 80, legend=c("Spatially clustered host", "Spatially unclustered host"), pch=15, col=c(rgb(0,1,1,0.5), rgb(1,0.2,0,0.5)))
par(mar=c(4.8,4.9,0.1,0.1))
hist((dist_summary$distance[dist_summary$host_c==-1]/1000), col=rgb(1,0.2,0,0.5), breaks=seq(-1,10, by=0.5), ylim=c(90,0), xlab="Average distance (km)", main="")


####With unflipped axis, considering a 'bin' to be 1km?
##I like this one, but want to normalize it as a density plot.

dist_summary<-aggregate(distance~ID*host_c, data=data_all, FUN=mean)
dist_summary$n<-aggregate(distance~ID*host_c, data=data_all, FUN=length)$distance

hist((dist_summary$distance[dist_summary$host_c==1]/1000), col=rgb(0,1,1,0.5), breaks=seq(-1,10, by=0.5), ylim=c(0,90), xlab="", main='Soapberry bug dispersal fall 2019', xaxt='n', density=TRUE)
legend(5, 80, legend=c("Spatially clustered host", "Spatially unclustered host"), pch=15, col=c(rgb(0,1,1,0.5), rgb(1,0.2,0,0.5)))
#par(mar=c(4.8,4.9,0.1,0.1))
hist((dist_summary$distance[dist_summary$host_c==-1]/1000), col=rgb(1,0.2,0,0.5), breaks=seq(-1,10, by=0.5), xlab="Average distance (km)", add=TRUE, density=TRUE)


###normalizing
dist_summary<-aggregate(distance~ID*host_c, data=data_all, FUN=max)
dist_summary$n<-aggregate(distance~ID*host_c, data=data_all, FUN=length)$distance

hist((dist_summary$distance[dist_summary$host_c==-1]/1000), col=rgb(1,0.2,0,0.5), main='Soapberry bug dispersal fall 2019', breaks=seq(-0.49,17.01, by=0.5), xlab="Average distance (km)", freq=FALSE)
legend(3, 1, legend=c("Spatially clustered host", "Spatially unclustered host"), pch=15, col=c(rgb(0,1,1,0.5), rgb(1,0.2,0,0.5)))
#par(mar=c(4.8,4.9,0.1,0.1))
hist((dist_summary$distance[dist_summary$host_c==1]/1000), col=rgb(0,1,1, 0.5), breaks=seq(-0.49,17.01, by=0.5), add=TRUE, freq=FALSE)




####As the sum of all distance traveled? This may be problematic as some individuals were preferentially tested additional times.

dist_summary<-aggregate(distance~ID*host_c, data=data_all, FUN=sum)
dist_summary$n<-aggregate(distance~ID*host_c, data=data_all, FUN=length)$distance

par(mfrow=c(2,1), mar=c(0.1,4.9,4.8,0.1))
hist((dist_summary$distance[dist_summary$host_c==1]/1000), col=rgb(0,1,1,0.5), breaks=seq(-1,30, by=0.5), ylim=c(0,90), xlab="", main='Soapberry bug dispersal fall 2019', xaxt='n')
legend(2800, 80, legend=c("Spatially clustered host", "Spatially unclustered host"), pch=15, col=c(rgb(0,1,1,0.5), rgb(1,0.2,0,0.5)))
par(mar=c(4.8,4.9,0.1,0.1))
hist((dist_summary$distance[dist_summary$host_c==-1]/1000), col=rgb(1,0.2,0,0.5), breaks=seq(-1,30, by=0.5), ylim=c(90,0), xlab="Average distance (km)", main="")
