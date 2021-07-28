
# generic two factor analysis with binomial data

m0<-glm(R~1, family=binomial, data=data)
m1<-glm(R~A, family=binomial, data=data)
m2<-glm(R~B, family=binomial, data=data)
m3<-glm(R~A+B, family=binomial, data=data)
m4<-glm(R~A*B, family=binomial, data=data)


# identify top models using AIC
summary<-AIC(m1,m2,m3,m4,m0)
sort(summary$AIC, index.return=TRUE)


# run get_Akaike_weights script in Rsrc folder
P<-get_model_probs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) # want the largest one of these
library(lmtest) # can use later to test for heteroscedacity in the models
