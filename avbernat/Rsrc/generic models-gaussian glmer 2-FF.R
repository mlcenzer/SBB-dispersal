
# generic three factor analysis with gaussian data

m0<-lm(R~1, data=data)
m1<-lm(R~A, data=data)
m2<-lm(R~B, data=data)
m3<-lm(R~A+B, data=data)
m4<-lm(R~A*B, data=data)

# identify top models using AIC
summary<-AIC(m1, m2, m3, m4, m0)
sort(summary$AIC, index.return=TRUE) #We want the smallest one of these

# run get_Akaike_weights script in Rsrc folder
P<-get_model_probs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) # want the largest one of these
library(lmtest) # can use later to test for heteroscedacity in the models
