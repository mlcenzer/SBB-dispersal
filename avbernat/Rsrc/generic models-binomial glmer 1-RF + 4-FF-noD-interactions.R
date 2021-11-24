
# generic 4-fixed factor and 1 random-factor models

m0<-glmer(R~1 + (1|X), data=data, family=binomial) #this one will be at the end to keep index number and model number the same.
m1<-glmer(R~A + (1|X), data=data, family=binomial)
m2<-glmer(R~B + (1|X), data=data, family=binomial)
m3<-glmer(R~C + (1|X), data=data, family=binomial)
m4<-glmer(R~D + (1|X), data=data, family=binomial)

m5<-glmer(R~A+B + (1|X), data=data, family=binomial)
m6<-glmer(R~A+C + (1|X), data=data, family=binomial)
m7<-glmer(R~A+D + (1|X), data=data, family=binomial)
m8<-glmer(R~B+C + (1|X), data=data, family=binomial)
m9<-glmer(R~B+D + (1|X), data=data, family=binomial)
m10<-glmer(R~C+D + (1|X), data=data, family=binomial)
m11<-glmer(R~A+B+C + (1|X), data=data, family=binomial)
m12<-glmer(R~A+B+D + (1|X), data=data, family=binomial)
m13<-glmer(R~A+C+D + (1|X), data=data, family=binomial)
m14<-glmer(R~B+C+D + (1|X), data=data, family=binomial)
m15<-glmer(R~A+B+C+D + (1|X), data=data, family=binomial)

m16<-glmer(R~A*B + (1|X), data=data, family=binomial)
m17<-glmer(R~A*C + (1|X), data=data, family=binomial)
m18<-glmer(R~B*C + (1|X), data=data, family=binomial)

m19<-glmer(R~A*B + C + (1|X), data=data, family=binomial)
m20<-glmer(R~A*B + D + (1|X), data=data, family=binomial)
m21<-glmer(R~A*C + B + (1|X), data=data, family=binomial)
m22<-glmer(R~A*C + D + (1|X), data=data, family=binomial)
m23<-glmer(R~B*C + A + (1|X), data=data, family=binomial)
m24<-glmer(R~B*C + D + (1|X), data=data, family=binomial)

m25<-glmer(R~A*B + C + D + (1|X), data=data, family=binomial)
m26<-glmer(R~A*C + B + D + (1|X), data=data, family=binomial)
m27<-glmer(R~B*C + A + D + (1|X), data=data, family=binomial)

m28<-glmer(R~A*B + A*C + (1|X), data=data, family=binomial)
m29<-glmer(R~A*B + B*C + (1|X), data=data, family=binomial)
m30<-glmer(R~A*C + B*C + (1|X), data=data, family=binomial)

m31<-glmer(R~A*B + A*C + D + (1|X), data=data, family=binomial)
m32<-glmer(R~A*B + B*C + D + (1|X), data=data, family=binomial)
m33<-glmer(R~A*C + B*C + D + (1|X), data=data, family=binomial)

m34<-glmer(R~A*B + A*C + B*C + (1|X), data=data, family=binomial)

m35<-glmer(R~A*B + A*C + B*C + D + (1|X), data=data, family=binomial)

# identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, 
             m17, m18, m19, m20, m21, m22, m23, m24, m25, m26, m27, m28, m29, m30, 
             m31, m32, m33, m34, m35, m0)

sort(summary$AIC, index.return=TRUE)

# run get_Akaike_weights script in Rsrc folder
P<-get_model_probs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) # want the largest one of these
library(lmtest) # can use later to test for heteroscedacity in the models
