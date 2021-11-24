
# generic three factor analysis with multinomial data

m0<-multinom(R ~ 1, data = data, trace=FALSE) 
m1<-multinom(R ~ A, data = data, trace=FALSE)  
m2<-multinom(R ~ B, data = data, trace=FALSE)  
m3<-multinom(R ~ A+B, data = data, trace=FALSE)  
m4<-multinom(R ~ A*B, data = data, trace=FALSE)  


# identify top models using AIC
summary<-AIC(m1,m2,m3,m4,m0)

sort(summary$AIC, index.return=TRUE)

# What's below may not work:
# run get_Akaike_weights script in Rsrc folder
P<-get_model_probs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) # want the largest one of these
library(lmtest) # can use later to test for heteroscedacity in the models
