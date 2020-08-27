library(lme4)
#generic 4-fixed factor and 2 random-factor models

## First Random Factor 

m0<-lmer(R~1 + (1|X) + (1|Y), data=data, REML=FALSE) #this one will be at the end to keep index number and model number the same.
m1<-lmer(R~A + (1|X) + (1|Y), data=data, REML=FALSE)
m2<-lmer(R~B + (1|X) + (1|Y), data=data, REML=FALSE)
m3<-lmer(R~C + (1|X) + (1|Y), data=data, REML=FALSE)
m4<-lmer(R~D + (1|X) + (1|Y), data=data, REML=FALSE)

m5<-lmer(R~A+B + (1|X) + (1|Y), data=data, REML=FALSE)
m6<-lmer(R~A+C + (1|X) + (1|Y), data=data, REML=FALSE)
m7<-lmer(R~A+D + (1|X) + (1|Y), data=data, REML=FALSE)
m8<-lmer(R~B+C + (1|X) + (1|Y), data=data, REML=FALSE)
m9<-lmer(R~B+D + (1|X) + (1|Y), data=data, REML=FALSE)
m10<-lmer(R~C+D + (1|X) + (1|Y), data=data, REML=FALSE)
m11<-lmer(R~A+B+C + (1|X) + (1|Y), data=data, REML=FALSE)
m12<-lmer(R~A+B+D + (1|X) + (1|Y), data=data, REML=FALSE)
m13<-lmer(R~A+C+D + (1|X) + (1|Y), data=data, REML=FALSE)
m14<-lmer(R~B+C+D + (1|X) + (1|Y), data=data, REML=FALSE)
m15<-lmer(R~A+B+C+D + (1|X) + (1|Y), data=data, REML=FALSE)

m16<-lmer(R~A*B + (1|X) + (1|Y), data=data, REML=FALSE)
m17<-lmer(R~A*C + (1|X) + (1|Y), data=data, REML=FALSE)
m18<-lmer(R~A*D + (1|X) + (1|Y), data=data, REML=FALSE)
m19<-lmer(R~B*C + (1|X) + (1|Y), data=data, REML=FALSE)
m20<-lmer(R~B*D + (1|X) + (1|Y), data=data, REML=FALSE)
m21<-lmer(R~C*D + (1|X) + (1|Y), data=data, REML=FALSE)

m22<-lmer(R~A*B + C + (1|X) + (1|Y), data=data, REML=FALSE)
m23<-lmer(R~A*B + D + (1|X) + (1|Y), data=data, REML=FALSE)
m24<-lmer(R~A*C + B + (1|X) + (1|Y), data=data, REML=FALSE)
m25<-lmer(R~A*C + D + (1|X) + (1|Y), data=data, REML=FALSE)
m26<-lmer(R~A*D + B + (1|X) + (1|Y), data=data, REML=FALSE)
m27<-lmer(R~A*D + C + (1|X) + (1|Y), data=data, REML=FALSE)
m28<-lmer(R~B*C + A + (1|X) + (1|Y), data=data, REML=FALSE)
m29<-lmer(R~B*C + D + (1|X) + (1|Y), data=data, REML=FALSE)
m30<-lmer(R~B*D + A + (1|X) + (1|Y), data=data, REML=FALSE)
m31<-lmer(R~B*D + C + (1|X) + (1|Y), data=data, REML=FALSE)
m32<-lmer(R~C*D + A + (1|X) + (1|Y), data=data, REML=FALSE)
m33<-lmer(R~C*D + B + (1|X) + (1|Y), data=data, REML=FALSE)

m34<-lmer(R~A*B + C + D + (1|X) + (1|Y), data=data, REML=FALSE)
m35<-lmer(R~A*C + B + D + (1|X) + (1|Y), data=data, REML=FALSE)
m36<-lmer(R~A*D + B + C + (1|X) + (1|Y), data=data, REML=FALSE)
m37<-lmer(R~B*C + A + D + (1|X) + (1|Y), data=data, REML=FALSE)
m38<-lmer(R~B*D + A + C + (1|X) + (1|Y), data=data, REML=FALSE)
m39<-lmer(R~C*D + A + B + (1|X) + (1|Y), data=data, REML=FALSE)


m40<-lmer(R~A*B + A*C + (1|X) + (1|Y), data=data, REML=FALSE)
m41<-lmer(R~A*B + A*D + (1|X) + (1|Y), data=data, REML=FALSE)
m42<-lmer(R~A*B + B*C + (1|X) + (1|Y), data=data, REML=FALSE)
m43<-lmer(R~A*B + B*D + (1|X) + (1|Y), data=data, REML=FALSE)
m44<-lmer(R~A*B + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m45<-lmer(R~A*C + A*D + (1|X) + (1|Y), data=data, REML=FALSE)
m46<-lmer(R~A*C + B*C + (1|X) + (1|Y), data=data, REML=FALSE)
m47<-lmer(R~A*C + B*D + (1|X) + (1|Y), data=data, REML=FALSE)
m48<-lmer(R~A*C + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m49<-lmer(R~A*D + B*C + (1|X) + (1|Y), data=data, REML=FALSE)
m50<-lmer(R~A*D + B*D + (1|X) + (1|Y), data=data, REML=FALSE)
m51<-lmer(R~A*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m52<-lmer(R~B*C + B*D + (1|X) + (1|Y), data=data, REML=FALSE)
m53<-lmer(R~B*C + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m54<-lmer(R~B*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)

m55<-lmer(R~A*B + A*C + D + (1|X) + (1|Y), data=data, REML=FALSE)
m56<-lmer(R~A*B + A*D + C + (1|X) + (1|Y), data=data, REML=FALSE)
m57<-lmer(R~A*B + B*C + D + (1|X) + (1|Y), data=data, REML=FALSE)
m58<-lmer(R~A*B + B*D + C + (1|X) + (1|Y), data=data, REML=FALSE)
m59<-lmer(R~A*C + A*D + B + (1|X) + (1|Y), data=data, REML=FALSE)
m60<-lmer(R~A*C + B*C + D + (1|X) + (1|Y), data=data, REML=FALSE)
m61<-lmer(R~A*C + C*D + B + (1|X) + (1|Y), data=data, REML=FALSE)
m62<-lmer(R~A*D + B*D + C + (1|X) + (1|Y), data=data, REML=FALSE)
m63<-lmer(R~A*D + C*D + B + (1|X) + (1|Y), data=data, REML=FALSE)
m64<-lmer(R~B*C + B*D + A + (1|X) + (1|Y), data=data, REML=FALSE)
m65<-lmer(R~B*C + C*D + A + (1|X) + (1|Y), data=data, REML=FALSE)
m66<-lmer(R~B*D + C*D + A + (1|X) + (1|Y), data=data, REML=FALSE)

m67<-lmer(R~A*B + A*C + A*D + (1|X) + (1|Y), data=data, REML=FALSE)
m68<-lmer(R~A*B + A*C + B*C + (1|X) + (1|Y), data=data, REML=FALSE)
m69<-lmer(R~A*B + A*C + B*D + (1|X) + (1|Y), data=data, REML=FALSE)
m70<-lmer(R~A*B + A*C + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m71<-lmer(R~A*B + A*D + B*C + (1|X) + (1|Y), data=data, REML=FALSE)
m72<-lmer(R~A*B + A*D + B*D + (1|X) + (1|Y), data=data, REML=FALSE)
m73<-lmer(R~A*B + A*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m74<-lmer(R~A*B + B*C + B*D + (1|X) + (1|Y), data=data, REML=FALSE)
m75<-lmer(R~A*B + B*C + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m76<-lmer(R~A*B + B*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m77<-lmer(R~A*C + A*D + B*C + (1|X) + (1|Y), data=data, REML=FALSE)
m78<-lmer(R~A*C + A*D + B*D + (1|X) + (1|Y), data=data, REML=FALSE)
m79<-lmer(R~A*C + A*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m80<-lmer(R~A*C + B*C + B*D + (1|X) + (1|Y), data=data, REML=FALSE)
m81<-lmer(R~A*C + B*C + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m82<-lmer(R~A*C + B*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m83<-lmer(R~A*D + B*C + B*D + (1|X) + (1|Y), data=data, REML=FALSE)
m84<-lmer(R~A*D + B*C + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m85<-lmer(R~A*D + B*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m86<-lmer(R~B*C + B*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)


m87<-lmer(R~A*B + A*C + B*C + D + (1|X) + (1|Y), data=data, REML=FALSE)
m88<-lmer(R~A*B + A*D + B*D + C + (1|X) + (1|Y), data=data, REML=FALSE)
m89<-lmer(R~A*C + A*D + C*D + B + (1|X) + (1|Y), data=data, REML=FALSE)
m90<-lmer(R~B*C + B*D + C*D + A + (1|X) + (1|Y), data=data, REML=FALSE)


m91<-lmer(R~A*B + A*C + A*D + B*C + (1|X) + (1|Y), data=data, REML=FALSE)
m92<-lmer(R~A*B + A*C + A*D + B*D + (1|X) + (1|Y), data=data, REML=FALSE)
m93<-lmer(R~A*B + A*C + A*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m94<-lmer(R~A*B + A*C + B*C + B*D + (1|X) + (1|Y), data=data, REML=FALSE)
m95<-lmer(R~A*B + A*C + B*C + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m96<-lmer(R~A*B + A*C + B*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m97<-lmer(R~A*B + A*D + B*C + B*D + (1|X) + (1|Y), data=data, REML=FALSE)
m98<-lmer(R~A*B + A*D + B*C + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m99<-lmer(R~A*B + A*D + B*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m100<-lmer(R~A*B + B*C + B*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m101<-lmer(R~A*C + A*D + B*C + B*D + (1|X) + (1|Y), data=data, REML=FALSE)
m102<-lmer(R~A*C + A*D + B*C + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m103<-lmer(R~A*C + A*D + B*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m104<-lmer(R~A*C + B*C + B*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m105<-lmer(R~A*D + B*C + B*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)


m106<-lmer(R~A*B + A*C + A*D + B*C + B*D + (1|X) + (1|Y), data=data, REML=FALSE)
m107<-lmer(R~A*B + A*C + A*D + B*C + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m108<-lmer(R~A*B + A*C + A*D + B*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m109<-lmer(R~A*B + A*C + B*C + B*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m110<-lmer(R~A*B + A*D + B*C + B*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)
m111<-lmer(R~A*C + A*D + B*C + B*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)

m112<-lmer(R~A*B + A*C + A*D + B*C + B*D + C*D + (1|X) + (1|Y), data=data, REML=FALSE)


#identify top models using AIC
summary<-AIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, 
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
