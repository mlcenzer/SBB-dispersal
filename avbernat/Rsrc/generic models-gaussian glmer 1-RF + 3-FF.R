
# generic three factor analysis with gaussian data

m0<-lmer(R~(1|X), data=data)
m1<-lmer(R~A + (1|X), data=data)
m2<-lmer(R~B + (1|X), data=data)
m3<-lmer(R~C + (1|X), data=data)

m4<-lmer(R~A+B + (1|X), data=data)
m5<-lmer(R~A+C + (1|X), data=data)
m6<-lmer(R~B+C + (1|X), data=data)

m7<-lmer(R~A+B+C + (1|X), data=data)

m8<-lmer(R~A*B + (1|X), data=data)
m9<-lmer(R~A*C + (1|X), data=data)
m10<-lmer(R~B*C + (1|X), data=data)

m11<-lmer(R~A*B + C + (1|X), data=data)
m12<-lmer(R~A*C + B + (1|X), data=data)
m13<-lmer(R~B*C + A + (1|X), data=data)

m14<-lmer(R~A*B + A*C + (1|X), data=data)
m15<-lmer(R~A*B + B*C + (1|X), data=data)
m16<-lmer(R~A*C + B*C + (1|X), data=data)

m17<-lmer(R~A*B + A*C + B*C + (1|X), data=data)



# identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m0)
sort(summary$AIC, index.return=TRUE) 

# run get_Akaike_weights script in Rsrc folder
P<-get_model_probs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) # want the largest one of these
library(lmtest) # can use later to test for heteroscedacity in the models
