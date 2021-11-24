
# generic two factor analysis with gaussian data

m0<-lmer(R~(1|X), data=data)
m1<-lmer(R~A + (1|X), data=data)
m2<-lmer(R~B + (1|X), data=data)
m3<-lmer(R~A+B + (1|X), data=data)
m4<-lmer(R~A*B + (1|X), data=data)

# identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m0)
sort(summary$AIC, index.return=TRUE) #We want the smallest one of these

# run get_Akaike_weights script in Rsrc folder
P<-get_model_probs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) # want the largest one of these
library(lmtest) # can use later to test for heteroscedacity in the models