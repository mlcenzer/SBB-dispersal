
# generic 4-fixed factor models

m0<-glm(R~1, data=data, family=binomial) # this one will be at the end to keep index number and model number the same.
m1<-glm(R~A, data=data, family=binomial)
m2<-glm(R~B, data=data, family=binomial)
m3<-glm(R~C, data=data, family=binomial)
m4<-glm(R~D, data=data, family=binomial)

m5<-glm(R~A+B, data=data, family=binomial)
m6<-glm(R~A+C, data=data, family=binomial)
m7<-glm(R~A+D, data=data, family=binomial)
m8<-glm(R~B+C, data=data, family=binomial)
m9<-glm(R~B+D, data=data, family=binomial)
m10<-glm(R~C+D, data=data, family=binomial)
m11<-glm(R~A+B+C, data=data, family=binomial)
m12<-glm(R~A+B+D, data=data, family=binomial)
m13<-glm(R~A+C+D, data=data, family=binomial)
m14<-glm(R~B+C+D, data=data, family=binomial)
m15<-glm(R~A+B+C+D, data=data, family=binomial)

m16<-glm(R~A*B, data=data, family=binomial)
m17<-glm(R~A*C, data=data, family=binomial)
m18<-glm(R~A*D, data=data, family=binomial)
m19<-glm(R~B*C, data=data, family=binomial)
m20<-glm(R~B*D, data=data, family=binomial)
m21<-glm(R~C*D, data=data, family=binomial)

m22<-glm(R~A*B + C, data=data, family=binomial)
m23<-glm(R~A*B + D, data=data, family=binomial)
m24<-glm(R~A*C + B, data=data, family=binomial)
m25<-glm(R~A*C + D, data=data, family=binomial)
m26<-glm(R~A*D + B, data=data, family=binomial)
m27<-glm(R~A*D + C, data=data, family=binomial)
m28<-glm(R~B*C + A, data=data, family=binomial)
m29<-glm(R~B*C + D, data=data, family=binomial)
m30<-glm(R~B*D + A, data=data, family=binomial)
m31<-glm(R~B*D + C, data=data, family=binomial)
m32<-glm(R~C*D + A, data=data, family=binomial)
m33<-glm(R~C*D + B, data=data, family=binomial)

m34<-glm(R~A*B + C + D, data=data, family=binomial)
m35<-glm(R~A*C + B + D, data=data, family=binomial)
m36<-glm(R~A*D + B + C, data=data, family=binomial)
m37<-glm(R~B*C + A + D, data=data, family=binomial)
m38<-glm(R~B*D + A + C, data=data, family=binomial)
m39<-glm(R~C*D + A + B, data=data, family=binomial)


m40<-glm(R~A*B + A*C, data=data, family=binomial)
m41<-glm(R~A*B + A*D, data=data, family=binomial)
m42<-glm(R~A*B + B*C, data=data, family=binomial)
m43<-glm(R~A*B + B*D, data=data, family=binomial)
m44<-glm(R~A*B + C*D, data=data, family=binomial)
m45<-glm(R~A*C + A*D, data=data, family=binomial)
m46<-glm(R~A*C + B*C, data=data, family=binomial)
m47<-glm(R~A*C + B*D, data=data, family=binomial)
m48<-glm(R~A*C + C*D, data=data, family=binomial)
m49<-glm(R~A*D + B*C, data=data, family=binomial)
m50<-glm(R~A*D + B*D, data=data, family=binomial)
m51<-glm(R~A*D + C*D, data=data, family=binomial)
m52<-glm(R~B*C + B*D, data=data, family=binomial)
m53<-glm(R~B*C + C*D, data=data, family=binomial)
m54<-glm(R~B*D + C*D, data=data, family=binomial)

m55<-glm(R~A*B + A*C + D, data=data, family=binomial)
m56<-glm(R~A*B + A*D + C, data=data, family=binomial)
m57<-glm(R~A*B + B*C + D, data=data, family=binomial)
m58<-glm(R~A*B + B*D + C, data=data, family=binomial)
m59<-glm(R~A*C + A*D + B, data=data, family=binomial)
m60<-glm(R~A*C + B*C + D, data=data, family=binomial)
m61<-glm(R~A*C + C*D + B, data=data, family=binomial)
m62<-glm(R~A*D + B*D + C, data=data, family=binomial)
m63<-glm(R~A*D + C*D + B, data=data, family=binomial)
m64<-glm(R~B*C + B*D + A, data=data, family=binomial)
m65<-glm(R~B*C + C*D + A, data=data, family=binomial)
m66<-glm(R~B*D + C*D + A, data=data, family=binomial)

m67<-glm(R~A*B + A*C + A*D, data=data, family=binomial)
m68<-glm(R~A*B + A*C + B*C, data=data, family=binomial)
m69<-glm(R~A*B + A*C + B*D, data=data, family=binomial)
m70<-glm(R~A*B + A*C + C*D, data=data, family=binomial)
m71<-glm(R~A*B + A*D + B*C, data=data, family=binomial)
m72<-glm(R~A*B + A*D + B*D, data=data, family=binomial)
m73<-glm(R~A*B + A*D + C*D, data=data, family=binomial)
m74<-glm(R~A*B + B*C + B*D, data=data, family=binomial)
m75<-glm(R~A*B + B*C + C*D, data=data, family=binomial)
m76<-glm(R~A*B + B*D + C*D, data=data, family=binomial)
m77<-glm(R~A*C + A*D + B*C, data=data, family=binomial)
m78<-glm(R~A*C + A*D + B*D, data=data, family=binomial)
m79<-glm(R~A*C + A*D + C*D, data=data, family=binomial)
m80<-glm(R~A*C + B*C + B*D, data=data, family=binomial)
m81<-glm(R~A*C + B*C + C*D, data=data, family=binomial)
m82<-glm(R~A*C + B*D + C*D, data=data, family=binomial)
m83<-glm(R~A*D + B*C + B*D, data=data, family=binomial)
m84<-glm(R~A*D + B*C + C*D, data=data, family=binomial)
m85<-glm(R~A*D + B*D + C*D, data=data, family=binomial)
m86<-glm(R~B*C + B*D + C*D, data=data, family=binomial)


m87<-glm(R~A*B + A*C + B*C + D, data=data, family=binomial)
m88<-glm(R~A*B + A*D + B*D + C, data=data, family=binomial)
m89<-glm(R~A*C + A*D + C*D + B, data=data, family=binomial)
m90<-glm(R~B*C + B*D + C*D + A, data=data, family=binomial)


m91<-glm(R~A*B + A*C + A*D + B*C, data=data, family=binomial)
m92<-glm(R~A*B + A*C + A*D + B*D, data=data, family=binomial)
m93<-glm(R~A*B + A*C + A*D + C*D, data=data, family=binomial)
m94<-glm(R~A*B + A*C + B*C + B*D, data=data, family=binomial)
m95<-glm(R~A*B + A*C + B*C + C*D, data=data, family=binomial)
m96<-glm(R~A*B + A*C + B*D + C*D, data=data, family=binomial)
m97<-glm(R~A*B + A*D + B*C + B*D, data=data, family=binomial)
m98<-glm(R~A*B + A*D + B*C + C*D, data=data, family=binomial)
m99<-glm(R~A*B + A*D + B*D + C*D, data=data, family=binomial)
m100<-glm(R~A*B + B*C + B*D + C*D, data=data, family=binomial)
m101<-glm(R~A*C + A*D + B*C + B*D, data=data, family=binomial)
m102<-glm(R~A*C + A*D + B*C + C*D, data=data, family=binomial)
m103<-glm(R~A*C + A*D + B*D + C*D, data=data, family=binomial)
m104<-glm(R~A*C + B*C + B*D + C*D, data=data, family=binomial)
m105<-glm(R~A*D + B*C + B*D + C*D, data=data, family=binomial)


m106<-glm(R~A*B + A*C + A*D + B*C + B*D, data=data, family=binomial)
m107<-glm(R~A*B + A*C + A*D + B*C + C*D, data=data, family=binomial)
m108<-glm(R~A*B + A*C + A*D + B*D + C*D, data=data, family=binomial)
m109<-glm(R~A*B + A*C + B*C + B*D + C*D, data=data, family=binomial)
m110<-glm(R~A*B + A*D + B*C + B*D + C*D, data=data, family=binomial)
m111<-glm(R~A*C + A*D + B*C + B*D + C*D, data=data, family=binomial)

m112<-glm(R~A*B + A*C + A*D + B*C + B*D + C*D, data=data, family=binomial)

# identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, 
             m17, m18, m19, m20, m21, m22, m23, m24, m25, m26, m27, m28, m29, m30, 
             m31, m32, m33, m34, m35, m36, m37, m38, m39, m40, m41, m42, m43, m44, 
             m45, m46, m47, m48, m49, m50, m51, m52, m53, m54, m55, m56, m57, m58, 
             m59, m60, m61, m62, m63, m64, m65, m66, m67, m68, m69, m70, m71, m72, 
             m73, m74, m75, m76, m77, m78, m79, m80, m81, m82, m83, m84, m85, m86, 
             m87, m88, m89, m90, m91, m92, m93, m94, m95, m96, m97, m98, m99, m100, 
             m101, m102, m103, m104, m105, m106, m107, m108, m109, m110, m111, m112, m0)

sort(summary$AIC, index.return=TRUE)

# run get_Akaike_weights script in Rsrc folder
P<-get_model_probs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) # want the largest one of these
library(lmtest) # can use later to test for heteroscedacity in the models
