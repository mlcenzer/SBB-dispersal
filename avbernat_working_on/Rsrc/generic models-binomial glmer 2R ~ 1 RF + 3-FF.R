
# generic three factor analysis with binomial data

m0<-glmer(cbind(R1, R2)~1 + (1|X), family=binomial, data=data)
m1<-glmer(cbind(R1, R2)~A + (1|X), family=binomial, data=data)
m2<-glmer(cbind(R1, R2)~B + (1|X), family=binomial, data=data)
m3<-glmer(cbind(R1, R2)~C + (1|X), family=binomial, data=data)

m4<-glmer(cbind(R1, R2)~A+B + (1|X), family=binomial, data=data)
m5<-glmer(cbind(R1, R2)~A+C + (1|X), family=binomial, data=data)
m6<-glmer(cbind(R1, R2)~B+C + (1|X), family=binomial, data=data)
m7<-glmer(cbind(R1, R2)~A+B+C + (1|X), family=binomial, data=data)

m8<-glmer(cbind(R1, R2)~A*B + (1|X), family=binomial, data=data)
m9<-glmer(cbind(R1, R2)~A*C + (1|X), family=binomial, data=data)
m10<-glmer(cbind(R1, R2)~B*C + (1|X), family=binomial, data=data)
m11<-glmer(cbind(R1, R2)~A*B + C + (1|X), family=binomial, data=data)
m12<-glmer(cbind(R1, R2)~A*C + B + (1|X), family=binomial, data=data)

m13<-glmer(cbind(R1, R2)~B*C + A + (1|X), family=binomial, data=data)
m14<-glmer(cbind(R1, R2)~A*B + A*C + (1|X), family=binomial, data=data)
m15<-glmer(cbind(R1, R2)~A*B + B*C + (1|X), family=binomial, data=data)
m16<-glmer(cbind(R1, R2)~A*C + B*C + (1|X), family=binomial, data=data)

m17<-glmer(cbind(R1, R2)~A*B + A*C + B*C + (1|X), family=binomial, data=data)

#m18<-glmer(cbind(R1, R2)~A*B*C + (1|X), family=binomial, data=data)


# identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, 
             m17, m0)

sort(summary$AIC, index.return=TRUE)


# run get_Akaike_weights script in Rsrc folder
P<-get_model_probs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) # want the largest one of these
library(lmtest) # can use later to test for heteroscedacity in the models
