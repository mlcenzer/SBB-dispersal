
# generic three factor analysis with Gamma(link="log") data

m0<-glmer(R~1 + (1|X), data=data, family=Gamma(link="log"))
m1<-glmer(R~A + (1|X), data=data, family=Gamma(link="log"))
m2<-glmer(R~B + (1|X), data=data, family=Gamma(link="log"))
m3<-glmer(R~C + (1|X), data=data, family=Gamma(link="log"))

m4<-glmer(R~A+B + (1|X), data=data, family=Gamma(link="log"))
m5<-glmer(R~A+C + (1|X), data=data, family=Gamma(link="log"))
m6<-glmer(R~B+C + (1|X), data=data, family=Gamma(link="log"))

m7<-glmer(R~A+B+C + (1|X), data=data, family=Gamma(link="log"))

m8<-glmer(R~A*B + (1|X), data=data, family=Gamma(link="log"))
m9<-glmer(R~A*C + (1|X), data=data, family=Gamma(link="log"))
m10<-glmer(R~B*C + (1|X), data=data, family=Gamma(link="log"))

m11<-glmer(R~A*B + C + (1|X), data=data, family=Gamma(link="log"))
m12<-glmer(R~A*C + B + (1|X), data=data, family=Gamma(link="log"))
m13<-glmer(R~B*C + A + (1|X), data=data, family=Gamma(link="log"))

m14<-glmer(R~A*B + A*C + (1|X), data=data, family=Gamma(link="log"))
m15<-glmer(R~A*B + B*C + (1|X), data=data, family=Gamma(link="log"))
m16<-glmer(R~A*C + B*C + (1|X), data=data, family=Gamma(link="log"))

m17<-glmer(R~A*B + A*C + B*C + (1|X), data=data, family=Gamma(link="log"))

#m18<-glmer(R~A*B*C + (1|X), family=Gamma(link="log"), data=data)

# identify top models using AIC
summary<-AIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m0)
sort(summary$AIC, index.return=TRUE)


# run get_Akaike_weights script in Rsrc folder
P<-get_model_probs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) # want the largest one of these
library(lmtest) # can use later to test for heteroscedacity in the models
