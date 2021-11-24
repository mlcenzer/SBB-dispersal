
# generic three factor analysis with gaussian data

m0<-lmer(R~(1|X), data=data, REML=FALSE)
m1<-lmer(R~A + (1|X), data=data, REML=FALSE)
m2<-lmer(R~B + (1|X), data=data, REML=FALSE)
m3<-lmer(R~C + (1|X), data=data, REML=FALSE)

m4<-lmer(R~A+B + (1|X), data=data, REML=FALSE)
m5<-lmer(R~A+C + (1|X), data=data, REML=FALSE)
m6<-lmer(R~B+C + (1|X), data=data, REML=FALSE)

m7<-lmer(R~A+B+C + (1|X), data=data, REML=FALSE)

m8<-lmer(R~A*B + (1|X), data=data, REML=FALSE)
m9<-lmer(R~A*C + (1|X), data=data, REML=FALSE)
m10<-lmer(R~B*C + (1|X), data=data, REML=FALSE)

m11<-lmer(R~A*B + C + (1|X), data=data, REML=FALSE)
m12<-lmer(R~A*C + B + (1|X), data=data, REML=FALSE)
m13<-lmer(R~B*C + A + (1|X), data=data, REML=FALSE)

m14<-lmer(R~A*B + A*C + (1|X), data=data, REML=FALSE)
m15<-lmer(R~A*B + B*C + (1|X), data=data, REML=FALSE)
m16<-lmer(R~A*C + B*C + (1|X), data=data, REML=FALSE)

m17<-lmer(R~A*B + A*C + B*C + (1|X), data=data, REML=FALSE)

m18<-lmer(R~(1|Y), data=data, REML=FALSE)
m19<-lmer(R~A + (1|Y), data=data, REML=FALSE)
m20<-lmer(R~B + (1|Y), data=data, REML=FALSE)
m21<-lmer(R~C + (1|Y), data=data, REML=FALSE)

m22<-lmer(R~A+B + (1|Y), data=data, REML=FALSE)
m23<-lmer(R~A+C + (1|Y), data=data, REML=FALSE)
m24<-lmer(R~B+C + (1|Y), data=data, REML=FALSE)

m25<-lmer(R~A+B+C + (1|Y), data=data, REML=FALSE)

m26<-lmer(R~A*B + (1|Y), data=data, REML=FALSE)
m27<-lmer(R~A*C + (1|Y), data=data, REML=FALSE)
m28<-lmer(R~B*C + (1|Y), data=data, REML=FALSE)

m29<-lmer(R~A*B + C + (1|Y), data=data, REML=FALSE)
m30<-lmer(R~A*C + B + (1|Y), data=data, REML=FALSE)
m31<-lmer(R~B*C + A + (1|Y), data=data, REML=FALSE)

m32<-lmer(R~A*B + A*C + (1|Y), data=data, REML=FALSE)
m33<-lmer(R~A*B + B*C + (1|Y), data=data, REML=FALSE)
m34<-lmer(R~A*C + B*C + (1|Y), data=data, REML=FALSE)

m35<-lmer(R~A*B + A*C + B*C + (1|Y), data=data, REML=FALSE)

m36<-lmer(R~(1|X) + (1|Y), data=data, REML=FALSE)
m37<-lmer(R~A + (1|X) + (1|Y), data=data, REML=FALSE)
m38<-lmer(R~B + (1|X) + (1|Y), data=data, REML=FALSE)
m39<-lmer(R~C + (1|X) + (1|Y), data=data, REML=FALSE)

m40<-lmer(R~A+B + (1|X) + (1|Y), data=data, REML=FALSE)
m41<-lmer(R~A+C + (1|X) + (1|Y), data=data, REML=FALSE)
m42<-lmer(R~B+C + (1|X) + (1|Y), data=data, REML=FALSE)

m43<-lmer(R~A+B+C + (1|X) + (1|Y), data=data, REML=FALSE)

m44<-lmer(R~A*B + (1|X) + (1|Y), data=data, REML=FALSE)
m45<-lmer(R~A*C + (1|X) + (1|Y), data=data, REML=FALSE)
m46<-lmer(R~B*C + (1|X) + (1|Y), data=data, REML=FALSE)

m47<-lmer(R~A*B + C + (1|X) + (1|Y), data=data, REML=FALSE)
m48<-lmer(R~A*C + B + (1|X) + (1|Y), data=data, REML=FALSE)
m49<-lmer(R~B*C + A + (1|X) + (1|Y), data=data, REML=FALSE)

m50<-lmer(R~A*B + A*C + (1|X) + (1|Y), data=data, REML=FALSE)
m51<-lmer(R~A*B + B*C + (1|X) + (1|Y), data=data, REML=FALSE)
m52<-lmer(R~A*C + B*C + (1|X) + (1|Y), data=data, REML=FALSE)

m53<-lmer(R~A*B + A*C + B*C + (1|X) + (1|Y), data=data, REML=FALSE)


# identify top models using AIC
summary<-AIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12,
             m13, m14, m15, m16, m17, m18, m19, m20, m21, m22, 
             m23, m24, m25, m26, m27, m28, m29, m30, m31, m32,
             m33, m34, m35, m36, m37, m38, m39, m40, m41, m42, 
             m43, m44, m45, m46, m47, m48, m49, m50, m51, m52, m53, m0)
sort(summary$AIC, index.return=TRUE)

# run get_Akaike_weights script in Rsrc folder
P<-get_model_probs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) # want the largest one of these
library(lmtest) # can use later to test for heteroscedacity in the models

