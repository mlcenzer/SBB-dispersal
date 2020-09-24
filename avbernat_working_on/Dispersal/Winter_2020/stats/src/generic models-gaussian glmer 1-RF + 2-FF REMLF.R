#generic three factor analysis with gaussian data
m0<-lmer(R~(1|X), data=data, REML=FALSE)

m1<-lmer(R~A + (1|X), data=data, REML=FALSE)

m2<-lmer(R~B + (1|X), data=data, REML=FALSE)

m3<-lmer(R~A+B + (1|X), data=data, REML=FALSE)

m4<-lmer(R~A*B + (1|X), data=data, REML=FALSE)

#identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m0)
sort(summary$AIC, index.return=TRUE) #We want the smallest one of these

#Run AICprobabilities in generic models folder

P<-AICprobs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) #remember, we want the largest one of these

