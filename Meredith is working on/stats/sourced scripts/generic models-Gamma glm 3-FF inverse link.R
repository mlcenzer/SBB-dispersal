library(lme4)
#generic three factor analysis with binomial data

m0<-glm(R~1, family=Gamma(link="inverse"), data=data)

m1<-glm(R~A, family=Gamma(link="inverse"), data=data)

m2<-glm(R~B, family=Gamma(link="inverse"), data=data)

m3<-glm(R~C, family=Gamma(link="inverse"), data=data)

m4<-glm(R~A+B, family=Gamma(link="inverse"), data=data)

m5<-glm(R~A+C, family=Gamma(link="inverse"), data=data)

m6<-glm(R~B+C, family=Gamma(link="inverse"), data=data)

m7<-glm(R~A+B+C, family=Gamma(link="inverse"), data=data)

m8<-glm(R~A*B, family=Gamma(link="inverse"), data=data)

m9<-glm(R~A*C, family=Gamma(link="inverse"), data=data)

m10<-glm(R~B*C, family=Gamma(link="inverse"), data=data)

m11<-glm(R~A*B + C, family=Gamma(link="inverse"), data=data)

m12<-glm(R~A*C + B, family=Gamma(link="inverse"), data=data)

m13<-glm(R~B*C + A, family=Gamma(link="inverse"), data=data)

m14<-glm(R~A*B + A*C, family=Gamma(link="inverse"), data=data)

m15<-glm(R~A*B + B*C, family=Gamma(link="inverse"), data=data)

m16<-glm(R~A*C + B*C, family=Gamma(link="inverse"), data=data)

m17<-glm(R~A*B + A*C + B*C, family=Gamma(link="inverse"), data=data)

#m18<-glm(R~A*B*C, family=Gamma(link="inverse"), data=data)


#identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m0)
sort(summary$AIC, index.return=TRUE)


#Run AICprobabilities in generic models folder

P<-AICprobs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) #remember, we want the largest one of these
