
# generic three factor analysis with binomial data

m0<-glmer(R~1 + (1|X), family=binomial, data=data)
m1<-glmer(R~A + (1|X), family=binomial, data=data)
m2<-glmer(R~B + (1|X), family=binomial, data=data)
m3<-glmer(R~C + (1|X), family=binomial, data=data)

m4<-glmer(R~A+B + (1|X), family=binomial, data=data)
m5<-glmer(R~A+C + (1|X), family=binomial, data=data)
m6<-glmer(R~B+C + (1|X), family=binomial, data=data)
m7<-glmer(R~A+B+C + (1|X), family=binomial, data=data)

m8<-glmer(R~A*B + (1|X), family=binomial, data=data)
m9<-glmer(R~A*B + C + (1|X), family=binomial, data=data)

m10<-glmer(R~1 + (1|Y), family=binomial, data=data)
m11<-glmer(R~A + (1|Y), family=binomial, data=data)
m12 <-glmer(R~B + (1|Y), family=binomial, data=data)
m13<-glmer(R~C + (1|Y), family=binomial, data=data)

m14<-glmer(R~A+B + (1|Y), family=binomial, data=data)
m15<-glmer(R~A+C + (1|Y), family=binomial, data=data)
m16<-glmer(R~B+C + (1|Y), family=binomial, data=data)
m17<-glmer(R~A+B+C + (1|Y), family=binomial, data=data)

m18<-glmer(R~A*B + C + (1|Y), family=binomial, data=data)

m19<-glmer(R~1 + (1|X) + (1|Y), family=binomial, data=data)
m20<-glmer(R~A + (1|X) + (1|Y), family=binomial, data=data)
m21<-glmer(R~B + (1|X) + (1|Y), family=binomial, data=data)
m22<-glmer(R~C + (1|X)+ (1|Y), family=binomial, data=data)

m23<-glmer(R~A+B + (1|X)+ (1|Y), family=binomial, data=data)
m24<-glmer(R~A+C + (1|X) + (1|Y), family=binomial, data=data)
m25<-glmer(R~B+C + (1|X) + (1|Y), family=binomial, data=data)
m26<-glmer(R~A+B+C + (1|X) + (1|Y), family=binomial, data=data)

m27<-glmer(R~A*B + (1|X) + (1|Y), family=binomial, data=data)
m28<-glmer(R~A*B + C + (1|X) + (1|Y), family=binomial, data=data)


# identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, 
             m17, m18, m19, m20, m21, m22, m23, m24, m25, m26, m27, m28, m0)

sort(summary$AIC, index.return=TRUE)


# run get_Akaike_weights script in Rsrc folder
P<-get_model_probs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) # want the largest one of these
library(lmtest) # can use later to test for heteroscedacity in the models

