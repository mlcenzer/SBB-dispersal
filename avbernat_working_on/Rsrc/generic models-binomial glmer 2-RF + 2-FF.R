
# generic two factor analysis with binomial data

m0<-glmer(R~1 + (1|X), family=binomial, data=data)
m1<-glmer(R~A + (1|X), family=binomial, data=data)
m2<-glmer(R~B + (1|X), family=binomial, data=data)
m3<-glmer(R~A+B + (1|X), family=binomial, data=data)
m4<-glmer(R~A*B + (1|X), family=binomial, data=data)

m5<-glmer(R~1 + (1|Y), family=binomial, data=data)
m6<-glmer(R~A + (1|Y), family=binomial, data=data)
m7<-glmer(R~B + (1|Y), family=binomial, data=data)
m8<-glmer(R~A+B + (1|Y), family=binomial, data=data)
m9<-glmer(R~A*B + (1|Y), family=binomial, data=data)

m10<-glmer(R~1 + (1|X) + (1|Y), family=binomial, data=data)
m11<-glmer(R~A + (1|X) + (1|Y), family=binomial, data=data)
m12<-glmer(R~B + (1|X) + (1|Y), family=binomial, data=data)
m13<-glmer(R~A+B + (1|X) + (1|Y), family=binomial, data=data)
m14<-glmer(R~A*B + (1|X) + (1|Y), family=binomial, data=data)


# identify top models using AIC
summary<-AIC(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m0)
sort(summary$AIC, index.return=TRUE)


# run get_Akaike_weights script in Rsrc folder
P<-get_model_probs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) # want the largest one of these
library(lmtest) # can use later to test for heteroscedacity in the models