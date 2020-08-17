library(lme4)
#generic 4-fixed factor and 1 random-factor models

m0<-glmer(R~1 + (1|X), data=data, family=binomial) #this one will be at the end to keep index number and model number the same.
m1<-glmer(R~A + (1|X), data=data, family=binomial)
m2<-glmer(R~B + (1|X), data=data, family=binomial)
m3<-glmer(R~C + (1|X), data=data, family=binomial)
m4<-glmer(R~D + (1|X), data=data, family=binomial)

m5<-glmer(R~A+B + (1|X), data=data, family=binomial)
m6<-glmer(R~A+C + (1|X), data=data, family=binomial)
m7<-glmer(R~A+D + (1|X), data=data, family=binomial)
m8<-glmer(R~B+C + (1|X), data=data, family=binomial)
m9<-glmer(R~B+D + (1|X), data=data, family=binomial)
m10<-glmer(R~C+D + (1|X), data=data, family=binomial)
m11<-glmer(R~A+B+C + (1|X), data=data, family=binomial)
m12<-glmer(R~A+B+D + (1|X), data=data, family=binomial)
m13<-glmer(R~A+C+D + (1|X), data=data, family=binomial)
m14<-glmer(R~B+C+D + (1|X), data=data, family=binomial)
m15<-glmer(R~A+B+C+D + (1|X), data=data, family=binomial)

m16<-glmer(R~A*B + (1|X), data=data, family=binomial)
m17<-glmer(R~A*C + (1|X), data=data, family=binomial)
m18<-glmer(R~A*D + (1|X), data=data, family=binomial)
m19<-glmer(R~B*C + (1|X), data=data, family=binomial)
m20<-glmer(R~B*D + (1|X), data=data, family=binomial)
m21<-glmer(R~C*D + (1|X), data=data, family=binomial)

m22<-glmer(R~A*B + C + (1|X), data=data, family=binomial)
m23<-glmer(R~A*B + D + (1|X), data=data, family=binomial)
m24<-glmer(R~A*C + B + (1|X), data=data, family=binomial)
m25<-glmer(R~A*C + D + (1|X), data=data, family=binomial)
m26<-glmer(R~A*D + B + (1|X), data=data, family=binomial)
m27<-glmer(R~A*D + C + (1|X), data=data, family=binomial)
m28<-glmer(R~B*C + A + (1|X), data=data, family=binomial)
m29<-glmer(R~B*C + D + (1|X), data=data, family=binomial)
m30<-glmer(R~B*D + A + (1|X), data=data, family=binomial)
m31<-glmer(R~B*D + C + (1|X), data=data, family=binomial)
m32<-glmer(R~C*D + A + (1|X), data=data, family=binomial)
m33<-glmer(R~C*D + B + (1|X), data=data, family=binomial)

m34<-glmer(R~A*B + C + D + (1|X), data=data, family=binomial)
m35<-glmer(R~A*C + B + D + (1|X), data=data, family=binomial)
m36<-glmer(R~A*D + B + C + (1|X), data=data, family=binomial)
m37<-glmer(R~B*C + A + D + (1|X), data=data, family=binomial)
m38<-glmer(R~B*D + A + C + (1|X), data=data, family=binomial)
m39<-glmer(R~C*D + A + B + (1|X), data=data, family=binomial)


m40<-glmer(R~A*B + A*C + (1|X), data=data, family=binomial)
m41<-glmer(R~A*B + A*D + (1|X), data=data, family=binomial)
m42<-glmer(R~A*B + B*C + (1|X), data=data, family=binomial)
m43<-glmer(R~A*B + B*D + (1|X), data=data, family=binomial)
m44<-glmer(R~A*B + C*D + (1|X), data=data, family=binomial)
m45<-glmer(R~A*C + A*D + (1|X), data=data, family=binomial)
m46<-glmer(R~A*C + B*C + (1|X), data=data, family=binomial)
m47<-glmer(R~A*C + B*D + (1|X), data=data, family=binomial)
m48<-glmer(R~A*C + C*D + (1|X), data=data, family=binomial)
m49<-glmer(R~A*D + B*C + (1|X), data=data, family=binomial)
m50<-glmer(R~A*D + B*D + (1|X), data=data, family=binomial)
m51<-glmer(R~A*D + C*D + (1|X), data=data, family=binomial)
m52<-glmer(R~B*C + B*D + (1|X), data=data, family=binomial)
m53<-glmer(R~B*C + C*D + (1|X), data=data, family=binomial)
m54<-glmer(R~B*D + C*D + (1|X), data=data, family=binomial)

m55<-glmer(R~A*B + A*C + D + (1|X), data=data, family=binomial)
m56<-glmer(R~A*B + A*D + C + (1|X), data=data, family=binomial)
m57<-glmer(R~A*B + B*C + D + (1|X), data=data, family=binomial)
m58<-glmer(R~A*B + B*D + C + (1|X), data=data, family=binomial)
m59<-glmer(R~A*C + A*D + B + (1|X), data=data, family=binomial)
m60<-glmer(R~A*C + B*C + D + (1|X), data=data, family=binomial)
m61<-glmer(R~A*C + C*D + B + (1|X), data=data, family=binomial)
m62<-glmer(R~A*D + B*D + C + (1|X), data=data, family=binomial)
m63<-glmer(R~A*D + C*D + B + (1|X), data=data, family=binomial)
m64<-glmer(R~B*C + B*D + A + (1|X), data=data, family=binomial)
m65<-glmer(R~B*C + C*D + A + (1|X), data=data, family=binomial)
m66<-glmer(R~B*D + C*D + A + (1|X), data=data, family=binomial)

m67<-glmer(R~A*B + A*C + A*D + (1|X), data=data, family=binomial)
m68<-glmer(R~A*B + A*C + B*C + (1|X), data=data, family=binomial)
m69<-glmer(R~A*B + A*C + B*D + (1|X), data=data, family=binomial)
m70<-glmer(R~A*B + A*C + C*D + (1|X), data=data, family=binomial)
m71<-glmer(R~A*B + A*D + B*C + (1|X), data=data, family=binomial)
m72<-glmer(R~A*B + A*D + B*D + (1|X), data=data, family=binomial)
m73<-glmer(R~A*B + A*D + C*D + (1|X), data=data, family=binomial)
m74<-glmer(R~A*B + B*C + B*D + (1|X), data=data, family=binomial)
m75<-glmer(R~A*B + B*C + C*D + (1|X), data=data, family=binomial)
m76<-glmer(R~A*B + B*D + C*D + (1|X), data=data, family=binomial)
m77<-glmer(R~A*C + A*D + B*C + (1|X), data=data, family=binomial)
m78<-glmer(R~A*C + A*D + B*D + (1|X), data=data, family=binomial)
m79<-glmer(R~A*C + A*D + C*D + (1|X), data=data, family=binomial)
m80<-glmer(R~A*C + B*C + B*D + (1|X), data=data, family=binomial)
m81<-glmer(R~A*C + B*C + C*D + (1|X), data=data, family=binomial)
m82<-glmer(R~A*C + B*D + C*D + (1|X), data=data, family=binomial)
m83<-glmer(R~A*D + B*C + B*D + (1|X), data=data, family=binomial)
m84<-glmer(R~A*D + B*C + C*D + (1|X), data=data, family=binomial)
m85<-glmer(R~A*D + B*D + C*D + (1|X), data=data, family=binomial)
m86<-glmer(R~B*C + B*D + C*D + (1|X), data=data, family=binomial)


m87<-glmer(R~A*B + A*C + B*C + D + (1|X), data=data, family=binomial)
m88<-glmer(R~A*B + A*D + B*D + C + (1|X), data=data, family=binomial)
m89<-glmer(R~A*C + A*D + C*D + B + (1|X), data=data, family=binomial)
m90<-glmer(R~B*C + B*D + C*D + A + (1|X), data=data, family=binomial)


m91<-glmer(R~A*B + A*C + A*D + B*C + (1|X), data=data, family=binomial)
m92<-glmer(R~A*B + A*C + A*D + B*D + (1|X), data=data, family=binomial)
m93<-glmer(R~A*B + A*C + A*D + C*D + (1|X), data=data, family=binomial)
m94<-glmer(R~A*B + A*C + B*C + B*D + (1|X), data=data, family=binomial)
m95<-glmer(R~A*B + A*C + B*C + C*D + (1|X), data=data, family=binomial)
m96<-glmer(R~A*B + A*C + B*D + C*D + (1|X), data=data, family=binomial)
m97<-glmer(R~A*B + A*D + B*C + B*D + (1|X), data=data, family=binomial)
m98<-glmer(R~A*B + A*D + B*C + C*D + (1|X), data=data, family=binomial)
m99<-glmer(R~A*B + A*D + B*D + C*D + (1|X), data=data, family=binomial)
m100<-glmer(R~A*B + B*C + B*D + C*D + (1|X), data=data, family=binomial)
m101<-glmer(R~A*C + A*D + B*C + B*D + (1|X), data=data, family=binomial)
m102<-glmer(R~A*C + A*D + B*C + C*D + (1|X), data=data, family=binomial)
m103<-glmer(R~A*C + A*D + B*D + C*D + (1|X), data=data, family=binomial)
m104<-glmer(R~A*C + B*C + B*D + C*D + (1|X), data=data, family=binomial)
m105<-glmer(R~A*D + B*C + B*D + C*D + (1|X), data=data, family=binomial)


m106<-glmer(R~A*B + A*C + A*D + B*C + B*D + (1|X), data=data, family=binomial)
m107<-glmer(R~A*B + A*C + A*D + B*C + C*D + (1|X), data=data, family=binomial)
m108<-glmer(R~A*B + A*C + A*D + B*D + C*D + (1|X), data=data, family=binomial)
m109<-glmer(R~A*B + A*C + B*C + B*D + C*D + (1|X), data=data, family=binomial)
m110<-glmer(R~A*B + A*D + B*C + B*D + C*D + (1|X), data=data, family=binomial)
m111<-glmer(R~A*C + A*D + B*C + B*D + C*D + (1|X), data=data, family=binomial)

m112<-glmer(R~A*B + A*C + A*D + B*C + B*D + C*D + (1|X), data=data, family=binomial)

#identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, 
             m17, m18, m19, m20, m21, m22, m23, m24, m25, m26, m27, m28, m29, m30, 
             m31, m32, m33, m34, m35, m36, m37, m38, m39, m40, m41, m42, m43, m44, 
             m45, m46, m47, m48, m49, m50, m51, m52, m53, m54, m55, m56, m57, m58, 
             m59, m60, m61, m62, m63, m64, m65, m66, m67, m68, m69, m70, m71, m72, 
             m73, m74, m75, m76, m77, m78, m79, m80, m81, m82, m83, m84, m85, m86, 
             m87, m88, m89, m90, m91, m92, m93, m94, m95, m96, m97, m98, m99, m100, 
             m101, m102, m103, m104, m105, m106, m107, m108, m109, m110, m111, m112, m0)

sort(summary$AIC, index.return=TRUE)

P<-AICprobs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) #remember, we want the largest one of these
