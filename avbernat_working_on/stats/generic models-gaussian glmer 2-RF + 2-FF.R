#generic three factor analysis with gaussian data
m0<-lmer(R~(1|X), data=data)

m1<-lmer(R~A + (1|X), data=data)
m2<-lmer(R~B + (1|X), data=data)
m3<-lmer(R~A+B + (1|X), data=data)
m4<-lmer(R~A*B + (1|X), data=data)

m5<-lmer(R~(1|Y), data=data)
m6<-lmer(R~A + (1|Y), data=data)
m7<-lmer(R~B + (1|Y), data=data)
m8<-lmer(R~A+B + (1|Y), data=data)
m9<-lmer(R~A*B + (1|Y), data=data)

m10<-lmer(R~(1|X) + (1|Y), data=data)
m11<-lmer(R~A + (1|X) + (1|Y), data=data)
m12<-lmer(R~B + (1|X) + (1|Y), data=data)
m13<-lmer(R~A+B + (1|X) + (1|Y), data=data)
m14<-lmer(R~A*B + (1|X) + (1|Y), data=data)

#identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m0)
sort(summary$AIC, index.return=TRUE) #We want the smallest one of these

#Run AICprobabilities in generic models folder

P<-AICprobs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) #remember, we want the largest one of these

