
#generic three factor analysis with binomial data

# One effect
m0<-glm(R~1, family=binomial, data=data)
m1<-glm(R~A, family=binomial, data=data)
m2<-glm(R~B, family=binomial, data=data)
m3<-glm(R~C, family=binomial, data=data)
m4<-glm(R~D, family=binomial, data=data)

# Two effects
m5<-glm(R~A+B, family=binomial, data=data)
m6<-glm(R~A+C, family=binomial, data=data)
m7<-glm(R~A+D, family=binomial, data=data)
m8<-glm(R~B+C, family=binomial, data=data)
m9<-glm(R~B+D, family=binomial, data=data)
m10<-glm(R~C+D, family=binomial, data=data)

# Three effects
m11<-glm(R~A+B+C, family=binomial, data=data)
m12<-glm(R~A+B+D, family=binomial, data=data)
m13<-glm(R~A+C+D, family=binomial, data=data)
m14<-glm(R~B+C+D, family=binomial, data=data)

# Interaction
m15<-glm(R~A*B, family=binomial, data=data)
m16<-glm(R~A*C, family=binomial, data=data)
m17<-glm(R~A*D, family=binomial, data=data)
m18<-glm(R~B*C, family=binomial, data=data)
m19<-glm(R~B*D, family=binomial, data=data)
m20<-glm(R~C*D, family=binomial, data=data)

# Interaction and Effect
m21<-glm(R~A*B + C, family=binomial, data=data)
m22<-glm(R~A*B + D, family=binomial, data=data)
m23<-glm(R~A*D + B, family=binomial, data=data)
m24<-glm(R~A*D + C, family=binomial, data=data)
m25<-glm(R~A*C + B, family=binomial, data=data)
m26<-glm(R~A*C + D, family=binomial, data=data)

m27<-glm(R~B*C + A, family=binomial, data=data)
m28<-glm(R~B*C + D, family=binomial, data=data)
m29<-glm(R~B*D + A, family=binomial, data=data)
m30<-glm(R~B*D + C, family=binomial, data=data)

m31<-glm(R~D*C + B, family=binomial, data=data)
m32<-glm(R~D*B + A, family=binomial, data=data)

# Two Interactions
m33<-glm(R~A*B + A*C, family=binomial, data=data)
m34<-glm(R~A*B + B*C, family=binomial, data=data)
m35<-glm(R~A*B + B*D, family=binomial, data=data)
m36<-glm(R~A*B + C*D, family=binomial, data=data)
m37<-glm(R~A*B + A*D, family=binomial, data=data)

m38<-glm(R~A*C + B*C, family=binomial, data=data)
m39<-glm(R~A*C + B*D, family=binomial, data=data)
m40<-glm(R~A*C + D*C, family=binomial, data=data)
m41<-glm(R~A*C + D*A, family=binomial, data=data)

m42<-glm(R~A*D + B*C, family=binomial, data=data)
m43<-glm(R~A*D + B*D, family=binomial, data=data)
m44<-glm(R~A*D + C*D, family=binomial, data=data)



# Three Interactions
m17<-glm(R~A*B + A*C + B*C, family=binomial, data=data)

# Saturated model
m18<-glm(R~A*B*C*D, family=binomial, data=data)


#identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m0)
sort(summary$AIC, index.return=TRUE)


#Run AICprobabilities in generic models folder

P<-AICprobs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) #remember, we want the largest one of these
library(lmtest) #you might want this later to test for heteroscedacity in your models
