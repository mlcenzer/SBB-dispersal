
# generic three factor analysis with binomial data

m0<-glmer(R~1, family=binomial, data=data)
m1<-glm(R~A, family=binomial, data=data)
m2<-glm(R~B, family=binomial, data=data)
m3<-glm(R~C, family=binomial, data=data)

m4<-glm(R~A+B, family=binomial, data=data)
m5<-glm(R~A+C, family=binomial, data=data)
m6<-glm(R~B+C, family=binomial, data=data)

m7<-glm(R~A+B+C, family=binomial, data=data)

m8<-glm(R~A*B, family=binomial, data=data)
m9<-glm(R~A*C, family=binomial, data=data)
m10<-glm(R~B*C, family=binomial, data=data)

m11<-glm(R~A*B + C, family=binomial, data=data)
m12<-glm(R~A*C + B, family=binomial, data=data)
m13<-glm(R~B*C + A, family=binomial, data=data)

m14<-glm(R~A*B + A*C, family=binomial, data=data)
m15<-glm(R~A*B + B*C, family=binomial, data=data)
m16<-glm(R~A*C + B*C, family=binomial, data=data)

m17<-glm(R~A*B + A*C + B*C, family=binomial, data=data)


m18<-glmer(R~1 + (1|X), family=binomial, data=data)
m19<-glmer(R~A + (1|X), family=binomial, data=data)
m20<-glmer(R~B + (1|X), family=binomial, data=data)
m21<-glmer(R~C + (1|X), family=binomial, data=data)

m22<-glmer(R~A+B + (1|X), family=binomial, data=data)
m23<-glmer(R~A+C + (1|X), family=binomial, data=data)
m24<-glmer(R~B+C + (1|X), family=binomial, data=data)

m25<-glmer(R~A+B+C + (1|X), family=binomial, data=data)

m26<-glmer(R~A*B + (1|X), family=binomial, data=data)
m27<-glmer(R~A*C + (1|X), family=binomial, data=data)
m28<-glmer(R~B*C + (1|X), family=binomial, data=data)

m29<-glmer(R~A*B + C + (1|X), family=binomial, data=data)
m30<-glmer(R~A*C + B + (1|X), family=binomial, data=data)
m31<-glmer(R~B*C + A + (1|X), family=binomial, data=data)

m32<-glmer(R~A*B + A*C + (1|X), family=binomial, data=data)
m33<-glmer(R~A*B + B*C + (1|X), family=binomial, data=data)
m34<-glmer(R~A*C + B*C + (1|X), family=binomial, data=data)

m35<-glmer(R~A*B + A*C + B*C + (1|X), family=binomial, data=data)

#m36<-glmer(R~A*B*C + (1|X), family=binomial, data=data)


# identify top models using AIC
summary<-AIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, 
             m13, m14, m15, m16, m17, m18, m19, m20, m21, m22, 
             m23, m24, m25, m26, m27, m28, m29, m30, m31, m32, 
             m33, m34, m35, m0)
sort(summary$AIC, index.return=TRUE)


# run get_Akaike_weights script in Rsrc folder
P<-get_model_probs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) # want the largest one of these
library(lmtest) # can use later to test for heteroscedacity in the models
