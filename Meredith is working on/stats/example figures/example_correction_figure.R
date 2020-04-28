###############################################################################
######Fig. 5: Plasticity across latitude

setwd("/Users/Meredith/Desktop/Documents/R/soapberry bugs/F2 April 2014/")

library(lme4)

morph1<-read.csv("F2morphology12.3.15a.csv", header=TRUE, sep=",", quote = "")

morph<-data.frame(morph1[morph1$cracked=='y' & morph1$bugpop!="GainesvilleBV",], pophost=NA, nathost=NA, latitude=NA)

for(n in 1:length(morph$beak)){

	if(morph$bugpop[n]=="Key_Largo"){
		morph$pophost[n]<-"C.corindum"
		morph$latitude[n]<-25.1756 }
		else if(morph$bugpop[n]=="Plantation_Key"){
		morph$pophost[n]<-"C.corindum"
		morph$latitude[n]<-24.9913 }	
		else if(morph$bugpop[n]=="HomesteadBV"){
			morph$pophost[n]<-"C.corindum"
			morph$latitude[n]<-25.5708}
			
		else if(morph$bugpop[n]=="HomesteadGRT"){
			morph$pophost[n]<-"K.elegans"
			morph$latitude[n]<-25.5510}
		else if(morph$bugpop[n]=="Ft.Myers"){
			morph$pophost[n]<-"K.elegans"
			morph$latitude[n]<-26.6340}
		else if(morph$bugpop[n]=="Lake_Wales"){
			morph$pophost[n]<-"K.elegans"
			morph$latitude[n]<-27.9347}
		else if(morph$bugpop[n]=="Leesburg"){
			morph$pophost[n]<-"K.elegans"
			morph$latitude[n]<-28.7964}
		else if(morph$bugpop[n]=="GainesvilleGRT"){
			morph$pophost[n]<-"K.elegans"
			morph$latitude[n]<-29.6605}


	if(morph$natpop[n]=="Key_Largo"){
		morph$nathost[n]<-"C.corindum"}
		else if(morph$natpop[n]=="Plantation_Key"){
		morph$nathost[n]<-"C.corindum"}	
	
		else if(morph$natpop[n]=="HomesteadBV"){
			morph$nathost[n]<-"C.corindum"}
	
		else{morph$nathost[n]<-"K.elegans"}	


}
#Sym.lat<-25.56091
Sym.lat<-25.5609

morph$sym.dist<-abs(morph$latitude-Sym.lat)

data<-data.frame(R=log(morph$beak), A=log(morph$thorax), B=morph$pophost, C=morph$nathost, D=morph$sex, E=(morph$sym.dist-mean(morph$sym.dist)), X=droplevels(morph$bugpop))
data$B1<-1
data$B1[data$B=="K.elegans"]<- -1
data$C1<-1
data$C1[data$C=="K.elegans"]<- -1
data$D1<-1
data$D1[data$D=="M"]<- -1

m26a<-glm(R~B1*D1 + A + C1 + E*C1, family=gaussian, data=data)

new.data<-expand.grid(n.host=c(-1,1), sym.dist=unique(data$E))
coefs<-summary(m26a)$coef[,1]
new.data$pred<-coefs[1] + coefs[5]*new.data$n.host + coefs[6]*new.data$sym.dist + coefs[8]*new.data$n.host*new.data$sym.dist

data$maleness<--1
data$maleness[data$D=="F"]<-1
data$p.host<--1
data$p.host[data$B=="C.corindum"]<-1
data$correction<-data$R-coefs[4]*data$A-coefs[3]*data$maleness-coefs[7]*data$maleness*data$p.host-coefs[2]*data$p.host

m.c<-aggregate(correction~E*C1, data=data, FUN=mean)$correction
n.c<-aggregate(correction~E*C1, data=data, FUN=length)$correction
s.c<-aggregate(correction~E*C1, data=data, FUN=sd)$correction
other<-aggregate(correction~E*C1, data=data, FUN=mean)
#2014 F2 thorax confidence intervals
conf.c<-data.frame(sym.dist=other$E, nathost=other$C1, mean=m.c,reps=n.c,sd=s.c, upper=(m.c+s.c/sqrt(n.c)), lower=(m.c-s.c/sqrt(n.c)))

#don't plot rows with <5 datapoints; just plot the data.
#conf.c[2,]<-NA
#conf.c[4,]<-NA
#conf.c[10,]<-NA
#conf.c[12,]<-NA
#conf.c[14,]<-NA

#best fit ys
y1<-max(new.data$pred[new.data$n.host==-1 & round(new.data$sym.dist, digits=2)==-1.92])
y2<-max(new.data$pred[new.data$n.host==-1 & round(new.data$sym.dist, digits=2)==2.17])
y3<-max(new.data$pred[new.data$n.host==1 & round(new.data$sym.dist, digits=2)==-1.92])
y4<-max(new.data$pred[new.data$n.host==1 & round(new.data$sym.dist, digits=2)==2.17])

setwd("/Users/Meredith/Desktop/Documents/publications/Plant defense fitness consequences/American Naturalist submission/Figures")

setEPS()
postscript(file="Cenzer2.fig.5.eps", width=7, height=4.7)

#plot beak length corrected
#to separate sym sites
#conf.c$sym.dist[1]<-conf.c$sym.dist[1]+0.01
#conf.c$sym.dist[9]<-conf.c$sym.dist[9]+0.01

par(mai=c(.5, .5, .2, .1), ps=8)
plot(conf.c$mean~conf.c$sym.dist, pch=c(2,1)[as.factor(conf.c$nathost)], ylab="", xlab="", xaxt='n', yaxt='n', ylim=c(0.65, 0.82), cex=0.7)

#points(data$correction[round(data$E, digits=2)==-1.36]~data$E[round(data$E, digits=2)==-1.36], cex=0.7, pch=c(1,2)[as.factor(data$C)])

#points(data$correction[round(data$E, digits=2)==0.44]~data$E[round(data$E, digits=2)==0.44], cex=0.7, pch=c(2,2,1))

#points(new.data$pred~new.data$sym.dist, pch=c(1,2)[as.factor(new.data$n.hostK)], cex=0.7)

for(n in 1:length(conf.c$sd)){
	lines(x=c(conf.c$sym.dist[n],conf.c$sym.dist[n]), y=c((conf.c$upper[n]),(conf.c$lower[n])))
}

#best fit lines?
lines(x=c(-1.9246718, 2.1650282), y=c(y1, y2), lty=2)
lines(x=c(-1.9246718, 2.1650282), y=c(y3, y4), lty=3)


title(ylab="Corrected log(beak length)", line=1.4)
title(xlab="Distance from sympatric zone (degrees of latitude)", line=1.3)

axis(side=1, at=seq(-2, 2, by=1), labels=c("", "", "", "", ""))
mtext(c("0", "1", "2", "3", "4"), side=1, line=.4, at=seq(-2, 2, by=1))

axis(side=2, at=seq(0.65, 0.80, by=0.05), labels=c( "", "", "", ""))
mtext(c("0.65", "0.70", "0.75", "0.80"), side=2, line=.5, at=seq(0.65, .8, by=0.05))

legend(1.7, 0.825, legend=c(expression(italic("K. elegans")), expression(italic("C. corindum"))), pch=c(2,1), pt.cex=.7, cex=.9)

dev.off()
