
#generic three factor analysis with binomial data

m0<-glmer(R~1 + (1|X), family=binomial, data=data)
m1<-glmer(R~A + (1|X), family=binomial, data=data)
m2<-glmer(R~B + (1|X), family=binomial, data=data)
m3<-glmer(R~C + (1|X), family=binomial, data=data)

m4<-glmer(R~A+B + (1|X), family=binomial, data=data)
m5<-glmer(R~A+C + (1|X), family=binomial, data=data)
m6<-glmer(R~B+C + (1|X), family=binomial, data=data)
m7<-glmer(R~A+B+C + (1|X), family=binomial, data=data)

m8<-glmer(R~A*B + (1|X), family=binomial, data=data)
m9<-glmer(R~A*C + (1|X), family=binomial, data=data)
m10<-glmer(R~B*C + (1|X), family=binomial, data=data)
m11<-glmer(R~A*B + C + (1|X), family=binomial, data=data)
m12<-glmer(R~A*C + B + (1|X), family=binomial, data=data)

m13<-glmer(R~B*C + A + (1|X), family=binomial, data=data)
m14<-glmer(R~A*B + A*C + (1|X), family=binomial, data=data)
m15<-glmer(R~A*B + B*C + (1|X), family=binomial, data=data)
m16<-glmer(R~A*C + B*C + (1|X), family=binomial, data=data)

m17<-glmer(R~A*B + A*C + B*C + (1|X), family=binomial, data=data)

m18<-glmer(R~A*B*C + (1|X), family=binomial, data=data)

m19<-glmer(R~1 + (1|Y), family=binomial, data=data)
m20<-glmer(R~A + (1|Y), family=binomial, data=data)
m21<-glmer(R~B + (1|Y), family=binomial, data=data)
m22<-glmer(R~C + (1|Y), family=binomial, data=data)

m23<-glmer(R~A+B + (1|Y), family=binomial, data=data)
m24<-glmer(R~A+C + (1|Y), family=binomial, data=data)
m25<-glmer(R~B+C + (1|Y), family=binomial, data=data)
m26<-glmer(R~A+B+C + (1|Y), family=binomial, data=data)

m27<-glmer(R~A*B + (1|Y), family=binomial, data=data)
m28<-glmer(R~A*C + (1|Y), family=binomial, data=data)
m29<-glmer(R~B*C + (1|Y), family=binomial, data=data)
m30<-glmer(R~A*B + C + (1|Y), family=binomial, data=data)
m31<-glmer(R~A*C + B + (1|Y), family=binomial, data=data)

m32<-glmer(R~B*C + A + (1|Y), family=binomial, data=data)
m33<-glmer(R~A*B + A*C + (1|Y), family=binomial, data=data)
m34<-glmer(R~A*B + B*C + (1|Y), family=binomial, data=data)
m35<-glmer(R~A*C + B*C + (1|Y), family=binomial, data=data)

m36<-glmer(R~A*B + A*C + B*C + (1|Y), family=binomial, data=data)

m37<-glmer(R~A*B*C + (1|Y), family=binomial, data=data)

m38<-glmer(R~1 + (1|X) + (1|Y), family=binomial, data=data)
m39<-glmer(R~A + (1|X) + (1|Y), family=binomial, data=data)
m40<-glmer(R~B + (1|X) + (1|Y), family=binomial, data=data)
m41<-glmer(R~C + (1|X)+ (1|Y), family=binomial, data=data)

m42<-glmer(R~A+B + (1|X)+ (1|Y), family=binomial, data=data)
m43<-glmer(R~A+C + (1|X) + (1|Y), family=binomial, data=data)
m44<-glmer(R~B+C + (1|X) + (1|Y), family=binomial, data=data)
m45<-glmer(R~A+B+C + (1|X) + (1|Y), family=binomial, data=data)

m46<-glmer(R~A*B + (1|X) + (1|Y), family=binomial, data=data)
m47<-glmer(R~A*C + (1|X) + (1|Y), family=binomial, data=data)
m48<-glmer(R~B*C + (1|X) + (1|Y), family=binomial, data=data)
m49<-glmer(R~A*B + C + (1|X) + (1|Y), family=binomial, data=data)
m50<-glmer(R~A*C + B + (1|X) + (1|Y), family=binomial, data=data)

m51<-glmer(R~B*C + A + (1|X) + (1|Y), family=binomial, data=data)
m52<-glmer(R~A*B + A*C + (1|X) + (1|Y), family=binomial, data=data)
m53<-glmer(R~A*B + B*C + (1|X) + (1|Y), family=binomial, data=data)
m54<-glmer(R~A*C + B*C + (1|X) + (1|Y), family=binomial, data=data)

m55<-glmer(R~A*B + A*C + B*C + (1|X) + (1|Y), family=binomial, data=data)

#m56<-glmer(R~A*B*C + (1|X) + (1|Y), family=binomial, data=data)

#identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, 
             m17, m18, m19, m20, m21, m22, m23, m24, m25, m26, m27, m28, m29, m30, 
             m31, m32, m33, m34, m35, m36, m37, m38, m39, m40, m41, m42, m43, m44, 
             m45, m46, m47, m48, m49, m50, m51, m52, m53, m54, m55, m0)

sort(summary$AIC, index.return=TRUE)


#Run AICprobabilities in generic models folder

P<-AICprobs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) #remember, we want the largest one of these
library(lmtest) #you might want this later to test for heteroscedacity in your models
