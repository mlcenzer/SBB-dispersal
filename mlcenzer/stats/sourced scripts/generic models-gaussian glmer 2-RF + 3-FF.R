#generic three factor analysis with gaussian data
m0<-lmer(R~(1|X), data=data)

m1<-lmer(R~A + (1|X), data=data)

m2<-lmer(R~B + (1|X), data=data)

m3<-lmer(R~C + (1|X), data=data)

m4<-lmer(R~A+B + (1|X), data=data)

m5<-lmer(R~A+C + (1|X), data=data)

m6<-lmer(R~B+C + (1|X), data=data)

m7<-lmer(R~A+B+C + (1|X), data=data)

m8<-lmer(R~A*B + (1|X), data=data)

m9<-lmer(R~A*C + (1|X), data=data)

m10<-lmer(R~B*C + (1|X), data=data)

m11<-lmer(R~A*B + C + (1|X), data=data)

m12<-lmer(R~A*C + B + (1|X), data=data)

m13<-lmer(R~B*C + A + (1|X), data=data)

m14<-lmer(R~A*B + A*C + (1|X), data=data)

m15<-lmer(R~A*B + B*C + (1|X), data=data)

m16<-lmer(R~A*C + B*C + (1|X), data=data)

m17<-lmer(R~A*B + A*C + B*C + (1|X), data=data)

m18<-lmer(R~(1|Y), data=data)

m19<-lmer(R~A + (1|Y), data=data)

m20<-lmer(R~B + (1|Y), data=data)

m21<-lmer(R~C + (1|Y), data=data)

m22<-lmer(R~A+B + (1|Y), data=data)

m23<-lmer(R~A+C + (1|Y), data=data)

m24<-lmer(R~B+C + (1|Y), data=data)

m25<-lmer(R~A+B+C + (1|Y), data=data)

m26<-lmer(R~A*B + (1|Y), data=data)

m27<-lmer(R~A*C + (1|Y), data=data)

m28<-lmer(R~B*C + (1|Y), data=data)

m29<-lmer(R~A*B + C + (1|Y), data=data)

m30<-lmer(R~A*C + B + (1|Y), data=data)

m31<-lmer(R~B*C + A + (1|Y), data=data)

m32<-lmer(R~A*B + A*C + (1|Y), data=data)

m33<-lmer(R~A*B + B*C + (1|Y), data=data)

m34<-lmer(R~A*C + B*C + (1|Y), data=data)

m35<-lmer(R~A*B + A*C + B*C + (1|Y), data=data)

m36<-lmer(R~(1|X) + (1|Y), data=data)

m37<-lmer(R~A + (1|X) + (1|Y), data=data)

m38<-lmer(R~B + (1|X) + (1|Y), data=data)

m39<-lmer(R~C + (1|X) + (1|Y), data=data)

m40<-lmer(R~A+B + (1|X) + (1|Y), data=data)

m41<-lmer(R~A+C + (1|X) + (1|Y), data=data)

m42<-lmer(R~B+C + (1|X) + (1|Y), data=data)

m43<-lmer(R~A+B+C + (1|X) + (1|Y), data=data)

m44<-lmer(R~A*B + (1|X) + (1|Y), data=data)

m45<-lmer(R~A*C + (1|X) + (1|Y), data=data)

m46<-lmer(R~B*C + (1|X) + (1|Y), data=data)

m47<-lmer(R~A*B + C + (1|X) + (1|Y), data=data)

m48<-lmer(R~A*C + B + (1|X) + (1|Y), data=data)

m49<-lmer(R~B*C + A + (1|X) + (1|Y), data=data)

m50<-lmer(R~A*B + A*C + (1|X) + (1|Y), data=data)

m51<-lmer(R~A*B + B*C + (1|X) + (1|Y), data=data)

m52<-lmer(R~A*C + B*C + (1|X) + (1|Y), data=data)

m53<-lmer(R~A*B + A*C + B*C + (1|X) + (1|Y), data=data)


#identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20, m21, m22, m23, m24, m25, m26, m27, m28, m29, m30, m31, m32, m33, m34, m35, m36, m37, m38, m39, m40, m41, m42, m43, m44, m45, m46, m47, m48, m49, m50, m51, m52, m53, m0)
sort(summary$AIC, index.return=TRUE) #We want the smallest one of these

#Run AICprobabilities in generic models folder

P<-AICprobs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) #remember, we want the largest one of these

