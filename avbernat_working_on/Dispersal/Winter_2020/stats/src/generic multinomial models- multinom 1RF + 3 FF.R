
#generic three factor analysis with multinomial data

m0<-multinom(R ~ 1, data = data, trace=FALSE) 
m1<-multinom(R ~ A, data = data, trace=FALSE)  
m2<-multinom(R ~ B, data = data, trace=FALSE)  
m3<-multinom(R ~ C, data = data, trace=FALSE) 

m4<-multinom(R ~ A+B, data = data, trace=FALSE)  
m5<-multinom(R ~ A+C, data = data, trace=FALSE) 
m6<-multinom(R ~ B+C, data = data, trace=FALSE)  
m7<-multinom(R ~ A+B+C, data = data, trace=FALSE) 

m8<-multinom(R ~ A*B, data = data, trace=FALSE)  
m9<-multinom(R ~ A*C, data = data, trace=FALSE) 
m10<-multinom(R ~ B*C, data = data, trace=FALSE) 

m11<-multinom(R ~ A*B + C, data = data, trace=FALSE)  
m12<-multinom(R ~ A*C + B, data = data, trace=FALSE) 
m13<-multinom(R ~ B*C + A, data = data, trace=FALSE) 

m14<-multinom(R ~ A*B + A*C, data = data, trace=FALSE)  
m15<-multinom(R ~ A*C + C*B, data = data, trace=FALSE) 
m16<-multinom(R ~ B*C + A*B, data = data, trace=FALSE) 

m17<-multinom(R ~ A*B + A*C + B*C, data = data, trace=FALSE) 

#identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, 
             m17, m0)

sort(summary$AIC, index.return=TRUE)

# What's below may not work:
#Run AICprobabilities in generic models folder

P<-AICprobs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) #remember, we want the largest one of these
library(lmtest) #you might want this later to test for heteroscedacity in your models

# 
# model_list <- list(m1,m2,m3,m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, 
#              m17, m18, m0)
# 
# # what I was trying to do:
# for(m in model_list){
#   
# }