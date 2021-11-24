
# generic two factor analysis with Gamma(link="log") data

m0<-glmer(R~1 + (1|X), data=data, family=Gamma(link="log"))
m1<-glmer(R~A + (1|X), data=data, family=Gamma(link="log"))
m2<-glmer(R~B + (1|X), data=data, family=Gamma(link="log"))
m3<-glmer(R~A+B + (1|X), data=data, family=Gamma(link="log"))
m4<-glmer(R~A*B + (1|X), data=data, family=Gamma(link="log"))


# identify top models using AIC
summary<-AIC(m1, m2, m3, m4, m0)
sort(summary$AIC, index.return=TRUE)


# run get_Akaike_weights script in Rsrc folder
P<-get_model_probs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) # want the largest one of these
library(lmtest) # can use later to test for heteroscedacity in the models