
#generic two factor analysis with binomial data

m0<-glmer(R~1 + (1|X), family=binomial, data=data)
m1<-glmer(R~1 + (1|Y), family=binomial, data=data)
m2<-glmer(R~1 + (1|X) + (1|Y), family=binomial, data=data)

m3<-glmer(R~A + (1|X), family=binomial, data=data)
m4<-glmer(R~A + (1|Y), family=binomial, data=data)
m5<-glmer(R~A + (1|X) + (1|Y), family=binomial, data=data)

#identify top models using AIC
summary<-AIC(m1,m2,m3,m4,m5,m0)
sort(summary$AIC, index.return=TRUE)

#Run AICprobabilities in generic models folder

P<-AICprobs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) #remember, we want the largest one of these
library(lmtest) #you might want this later to test for heteroscedacity in your models
