
# generic 4-fixed factor

m0<-glm(R~1, data=data, family=gaussian) #this one will be at the end to keep index number and model number the same.
m1<-glm(R~A, data=data, family=gaussian)
m2<-glm(R~B, data=data, family=gaussian)
m3<-glm(R~C, data=data, family=gaussian)
m4<-glm(R~D, data=data, family=gaussian)

m5<-glm(R~A+B, data=data, family=gaussian)
m6<-glm(R~A+C, data=data, family=gaussian)
m7<-glm(R~A+D, data=data, family=gaussian)
m8<-glm(R~B+C, data=data, family=gaussian)
m9<-glm(R~B+D, data=data, family=gaussian)
m10<-glm(R~C+D, data=data, family=gaussian)
m11<-glm(R~A+B+C, data=data, family=gaussian)
m12<-glm(R~A+B+D, data=data, family=gaussian)
m13<-glm(R~A+C+D, data=data, family=gaussian)
m14<-glm(R~B+C+D, data=data, family=gaussian)
m15<-glm(R~A+B+C+D, data=data, family=gaussian)

m16<-glm(R~A*B, data=data, family=gaussian)
m17<-glm(R~A*C, data=data, family=gaussian)
m18<-glm(R~A*D, data=data, family=gaussian)
m19<-glm(R~B*C, data=data, family=gaussian)
m20<-glm(R~B*D, data=data, family=gaussian)
m21<-glm(R~C*D, data=data, family=gaussian)

m22<-glm(R~A*B + C, data=data, family=gaussian)
m23<-glm(R~A*B + D, data=data, family=gaussian)
m24<-glm(R~A*C + B, data=data, family=gaussian)
m25<-glm(R~A*C + D, data=data, family=gaussian)
m26<-glm(R~A*D + B, data=data, family=gaussian)
m27<-glm(R~A*D + C, data=data, family=gaussian)
m28<-glm(R~B*C + A, data=data, family=gaussian)
m29<-glm(R~B*C + D, data=data, family=gaussian)
m30<-glm(R~B*D + A, data=data, family=gaussian)
m31<-glm(R~B*D + C, data=data, family=gaussian)
m32<-glm(R~C*D + A, data=data, family=gaussian)
m33<-glm(R~C*D + B, data=data, family=gaussian)

m34<-glm(R~A*B + C + D, data=data, family=gaussian)
m35<-glm(R~A*C + B + D, data=data, family=gaussian)
m36<-glm(R~A*D + B + C, data=data, family=gaussian)
m37<-glm(R~B*C + A + D, data=data, family=gaussian)
m38<-glm(R~B*D + A + C, data=data, family=gaussian)
m39<-glm(R~C*D + A + B, data=data, family=gaussian)


m40<-glm(R~A*B + A*C, data=data, family=gaussian)
m41<-glm(R~A*B + A*D, data=data, family=gaussian)
m42<-glm(R~A*B + B*C, data=data, family=gaussian)
m43<-glm(R~A*B + B*D, data=data, family=gaussian)
m44<-glm(R~A*B + C*D, data=data, family=gaussian)
m45<-glm(R~A*C + A*D, data=data, family=gaussian)
m46<-glm(R~A*C + B*C, data=data, family=gaussian)
m47<-glm(R~A*C + B*D, data=data, family=gaussian)
m48<-glm(R~A*C + C*D, data=data, family=gaussian)
m49<-glm(R~A*D + B*C, data=data, family=gaussian)
m50<-glm(R~A*D + B*D, data=data, family=gaussian)
m51<-glm(R~A*D + C*D, data=data, family=gaussian)
m52<-glm(R~B*C + B*D, data=data, family=gaussian)
m53<-glm(R~B*C + C*D, data=data, family=gaussian)
m54<-glm(R~B*D + C*D, data=data, family=gaussian)

m55<-glm(R~A*B + A*C + D, data=data, family=gaussian)
m56<-glm(R~A*B + A*D + C, data=data, family=gaussian)
m57<-glm(R~A*B + B*C + D, data=data, family=gaussian)
m58<-glm(R~A*B + B*D + C, data=data, family=gaussian)
m59<-glm(R~A*C + A*D + B, data=data, family=gaussian)
m60<-glm(R~A*C + B*C + D, data=data, family=gaussian)
m61<-glm(R~A*C + C*D + B, data=data, family=gaussian)
m62<-glm(R~A*D + B*D + C, data=data, family=gaussian)
m63<-glm(R~A*D + C*D + B, data=data, family=gaussian)
m64<-glm(R~B*C + B*D + A, data=data, family=gaussian)
m65<-glm(R~B*C + C*D + A, data=data, family=gaussian)
m66<-glm(R~B*D + C*D + A, data=data, family=gaussian)

m67<-glm(R~A*B + A*C + A*D, data=data, family=gaussian)
m68<-glm(R~A*B + A*C + B*C, data=data, family=gaussian)
m69<-glm(R~A*B + A*C + B*D, data=data, family=gaussian)
m70<-glm(R~A*B + A*C + C*D, data=data, family=gaussian)
m71<-glm(R~A*B + A*D + B*C, data=data, family=gaussian)
m72<-glm(R~A*B + A*D + B*D, data=data, family=gaussian)
m73<-glm(R~A*B + A*D + C*D, data=data, family=gaussian)
m74<-glm(R~A*B + B*C + B*D, data=data, family=gaussian)
m75<-glm(R~A*B + B*C + C*D, data=data, family=gaussian)
m76<-glm(R~A*B + B*D + C*D, data=data, family=gaussian)
m77<-glm(R~A*C + A*D + B*C, data=data, family=gaussian)
m78<-glm(R~A*C + A*D + B*D, data=data, family=gaussian)
m79<-glm(R~A*C + A*D + C*D, data=data, family=gaussian)
m80<-glm(R~A*C + B*C + B*D, data=data, family=gaussian)
m81<-glm(R~A*C + B*C + C*D, data=data, family=gaussian)
m82<-glm(R~A*C + B*D + C*D, data=data, family=gaussian)
m83<-glm(R~A*D + B*C + B*D, data=data, family=gaussian)
m84<-glm(R~A*D + B*C + C*D, data=data, family=gaussian)
m85<-glm(R~A*D + B*D + C*D, data=data, family=gaussian)
m86<-glm(R~B*C + B*D + C*D, data=data, family=gaussian)


m87<-glm(R~A*B + A*C + B*C + D, data=data, family=gaussian)
m88<-glm(R~A*B + A*D + B*D + C, data=data, family=gaussian)
m89<-glm(R~A*C + A*D + C*D + B, data=data, family=gaussian)
m90<-glm(R~B*C + B*D + C*D + A, data=data, family=gaussian)


m91<-glm(R~A*B + A*C + A*D + B*C, data=data, family=gaussian)
m92<-glm(R~A*B + A*C + A*D + B*D, data=data, family=gaussian)
m93<-glm(R~A*B + A*C + A*D + C*D, data=data, family=gaussian)
m94<-glm(R~A*B + A*C + B*C + B*D, data=data, family=gaussian)
m95<-glm(R~A*B + A*C + B*C + C*D, data=data, family=gaussian)
m96<-glm(R~A*B + A*C + B*D + C*D, data=data, family=gaussian)
m97<-glm(R~A*B + A*D + B*C + B*D, data=data, family=gaussian)
m98<-glm(R~A*B + A*D + B*C + C*D, data=data, family=gaussian)
m99<-glm(R~A*B + A*D + B*D + C*D, data=data, family=gaussian)
m100<-glm(R~A*B + B*C + B*D + C*D, data=data, family=gaussian)
m101<-glm(R~A*C + A*D + B*C + B*D, data=data, family=gaussian)
m102<-glm(R~A*C + A*D + B*C + C*D, data=data, family=gaussian)
m103<-glm(R~A*C + A*D + B*D + C*D, data=data, family=gaussian)
m104<-glm(R~A*C + B*C + B*D + C*D, data=data, family=gaussian)
m105<-glm(R~A*D + B*C + B*D + C*D, data=data, family=gaussian)


m106<-glm(R~A*B + A*C + A*D + B*C + B*D, data=data, family=gaussian)
m107<-glm(R~A*B + A*C + A*D + B*C + C*D, data=data, family=gaussian)
m108<-glm(R~A*B + A*C + A*D + B*D + C*D, data=data, family=gaussian)
m109<-glm(R~A*B + A*C + B*C + B*D + C*D, data=data, family=gaussian)
m110<-glm(R~A*B + A*D + B*C + B*D + C*D, data=data, family=gaussian)
m111<-glm(R~A*C + A*D + B*C + B*D + C*D, data=data, family=gaussian)

m112<-glm(R~A*B + A*C + A*D + B*C + B*D + C*D, data=data, family=gaussian)


# identify top models using AIC
summary<-AIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, 
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
