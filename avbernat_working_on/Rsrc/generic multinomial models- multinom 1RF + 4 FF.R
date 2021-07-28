
# generic three factor analysis with multinomial data

m0<-multinom(R ~ 1, data = data, trace=FALSE) 

m0<-multinom(R~1, data = data, trace=FALSE)  # this one will be at the end to keep index number and model number the same.
m1<-multinom(R~A, data = data, trace=FALSE) 
m2<-multinom(R~B, data = data, trace=FALSE) 
m3<-multinom(R~C, data = data, trace=FALSE) 
m4<-multinom(R~D, data = data, trace=FALSE) 

m5<-multinom(R~A+B, data = data, trace=FALSE) 
m6<-multinom(R~A+C, data = data, trace=FALSE) 
m7<-multinom(R~A+D, data = data, trace=FALSE) 
m8<-multinom(R~B+C, data = data, trace=FALSE) 
m9<-multinom(R~B+D, data = data, trace=FALSE) 
m10<-multinom(R~C+D, data = data, trace=FALSE) 
m11<-multinom(R~A+B+C, data = data, trace=FALSE) 
m12<-multinom(R~A+B+D, data = data, trace=FALSE) 
m13<-multinom(R~A+C+D, data = data, trace=FALSE) 
m14<-multinom(R~B+C+D, data = data, trace=FALSE) 
m15<-multinom(R~A+B+C+D, data = data, trace=FALSE) 

m16<-multinom(R~A*B, data = data, trace=FALSE) 
m17<-multinom(R~A*C, data = data, trace=FALSE) 
m18<-multinom(R~A*D, data = data, trace=FALSE) 
m19<-multinom(R~B*C, data = data, trace=FALSE) 
m20<-multinom(R~B*D, data = data, trace=FALSE) 
m21<-multinom(R~C*D, data = data, trace=FALSE) 

m22<-multinom(R~A*B + C, data = data, trace=FALSE) 
m23<-multinom(R~A*B + D, data = data, trace=FALSE) 
m24<-multinom(R~A*C + B, data = data, trace=FALSE) 
m25<-multinom(R~A*C + D, data = data, trace=FALSE) 
m26<-multinom(R~A*D + B, data = data, trace=FALSE) 
m27<-multinom(R~A*D + C, data = data, trace=FALSE) 
m28<-multinom(R~B*C + A, data = data, trace=FALSE) 
m29<-multinom(R~B*C + D, data = data, trace=FALSE) 
m30<-multinom(R~B*D + A, data = data, trace=FALSE) 
m31<-multinom(R~B*D + C, data = data, trace=FALSE) 
m32<-multinom(R~C*D + A, data = data, trace=FALSE) 
m33<-multinom(R~C*D + B, data = data, trace=FALSE) 

m34<-multinom(R~A*B + C + D, data = data, trace=FALSE) 
m35<-multinom(R~A*C + B + D, data = data, trace=FALSE) 
m36<-multinom(R~A*D + B + C, data = data, trace=FALSE) 
m37<-multinom(R~B*C + A + D, data = data, trace=FALSE) 
m38<-multinom(R~B*D + A + C, data = data, trace=FALSE) 
m39<-multinom(R~C*D + A + B, data = data, trace=FALSE) 


m40<-multinom(R~A*B + A*C, data = data, trace=FALSE) 
m41<-multinom(R~A*B + A*D, data = data, trace=FALSE) 
m42<-multinom(R~A*B + B*C, data = data, trace=FALSE) 
m43<-multinom(R~A*B + B*D, data = data, trace=FALSE) 
m44<-multinom(R~A*B + C*D, data = data, trace=FALSE) 
m45<-multinom(R~A*C + A*D, data = data, trace=FALSE) 
m46<-multinom(R~A*C + B*C, data = data, trace=FALSE) 
m47<-multinom(R~A*C + B*D, data = data, trace=FALSE) 
m48<-multinom(R~A*C + C*D, data = data, trace=FALSE) 
m49<-multinom(R~A*D + B*C, data = data, trace=FALSE) 
m50<-multinom(R~A*D + B*D, data = data, trace=FALSE) 
m51<-multinom(R~A*D + C*D, data = data, trace=FALSE) 
m52<-multinom(R~B*C + B*D, data = data, trace=FALSE) 
m53<-multinom(R~B*C + C*D, data = data, trace=FALSE) 
m54<-multinom(R~B*D + C*D, data = data, trace=FALSE) 

m55<-multinom(R~A*B + A*C + D, data = data, trace=FALSE) 
m56<-multinom(R~A*B + A*D + C, data = data, trace=FALSE) 
m57<-multinom(R~A*B + B*C + D, data = data, trace=FALSE) 
m58<-multinom(R~A*B + B*D + C, data = data, trace=FALSE) 
m59<-multinom(R~A*C + A*D + B, data = data, trace=FALSE) 
m60<-multinom(R~A*C + B*C + D, data = data, trace=FALSE) 
m61<-multinom(R~A*C + C*D + B, data = data, trace=FALSE) 
m62<-multinom(R~A*D + B*D + C, data = data, trace=FALSE) 
m63<-multinom(R~A*D + C*D + B, data = data, trace=FALSE) 
m64<-multinom(R~B*C + B*D + A, data = data, trace=FALSE) 
m65<-multinom(R~B*C + C*D + A, data = data, trace=FALSE) 
m66<-multinom(R~B*D + C*D + A, data = data, trace=FALSE) 

m67<-multinom(R~A*B + A*C + A*D, data = data, trace=FALSE) 
m68<-multinom(R~A*B + A*C + B*C, data = data, trace=FALSE) 
m69<-multinom(R~A*B + A*C + B*D, data = data, trace=FALSE) 
m70<-multinom(R~A*B + A*C + C*D, data = data, trace=FALSE) 
m71<-multinom(R~A*B + A*D + B*C, data = data, trace=FALSE) 
m72<-multinom(R~A*B + A*D + B*D, data = data, trace=FALSE) 
m73<-multinom(R~A*B + A*D + C*D, data = data, trace=FALSE) 
m74<-multinom(R~A*B + B*C + B*D, data = data, trace=FALSE) 
m75<-multinom(R~A*B + B*C + C*D, data = data, trace=FALSE) 
m76<-multinom(R~A*B + B*D + C*D, data = data, trace=FALSE) 
m77<-multinom(R~A*C + A*D + B*C, data = data, trace=FALSE) 
m78<-multinom(R~A*C + A*D + B*D, data = data, trace=FALSE) 
m79<-multinom(R~A*C + A*D + C*D, data = data, trace=FALSE) 
m80<-multinom(R~A*C + B*C + B*D, data = data, trace=FALSE) 
m81<-multinom(R~A*C + B*C + C*D, data = data, trace=FALSE) 
m82<-multinom(R~A*C + B*D + C*D, data = data, trace=FALSE) 
m83<-multinom(R~A*D + B*C + B*D, data = data, trace=FALSE) 
m84<-multinom(R~A*D + B*C + C*D, data = data, trace=FALSE) 
m85<-multinom(R~A*D + B*D + C*D, data = data, trace=FALSE) 
m86<-multinom(R~B*C + B*D + C*D, data = data, trace=FALSE) 


m87<-multinom(R~A*B + A*C + B*C + D, data = data, trace=FALSE) 
m88<-multinom(R~A*B + A*D + B*D + C, data = data, trace=FALSE) 
m89<-multinom(R~A*C + A*D + C*D + B, data = data, trace=FALSE) 
m90<-multinom(R~B*C + B*D + C*D + A, data = data, trace=FALSE) 


m91<-multinom(R~A*B + A*C + A*D + B*C, data = data, trace=FALSE) 
m92<-multinom(R~A*B + A*C + A*D + B*D, data = data, trace=FALSE) 
m93<-multinom(R~A*B + A*C + A*D + C*D, data = data, trace=FALSE) 
m94<-multinom(R~A*B + A*C + B*C + B*D, data = data, trace=FALSE) 
m95<-multinom(R~A*B + A*C + B*C + C*D, data = data, trace=FALSE) 
m96<-multinom(R~A*B + A*C + B*D + C*D, data = data, trace=FALSE) 
m97<-multinom(R~A*B + A*D + B*C + B*D, data = data, trace=FALSE) 
m98<-multinom(R~A*B + A*D + B*C + C*D, data = data, trace=FALSE) 
m99<-multinom(R~A*B + A*D + B*D + C*D, data = data, trace=FALSE) 
m100<-multinom(R~A*B + B*C + B*D + C*D, data = data, trace=FALSE) 
m101<-multinom(R~A*C + A*D + B*C + B*D, data = data, trace=FALSE) 
m102<-multinom(R~A*C + A*D + B*C + C*D, data = data, trace=FALSE) 
m103<-multinom(R~A*C + A*D + B*D + C*D, data = data, trace=FALSE) 
m104<-multinom(R~A*C + B*C + B*D + C*D, data = data, trace=FALSE) 
m105<-multinom(R~A*D + B*C + B*D + C*D, data = data, trace=FALSE) 


m106<-multinom(R~A*B + A*C + A*D + B*C + B*D, data = data, trace=FALSE) 
m107<-multinom(R~A*B + A*C + A*D + B*C + C*D, data = data, trace=FALSE) 
m108<-multinom(R~A*B + A*C + A*D + B*D + C*D, data = data, trace=FALSE) 
m109<-multinom(R~A*B + A*C + B*C + B*D + C*D, data = data, trace=FALSE) 
m110<-multinom(R~A*B + A*D + B*C + B*D + C*D, data = data, trace=FALSE) 
m111<-multinom(R~A*C + A*D + B*C + B*D + C*D, data = data, trace=FALSE) 

m112<-multinom(R~A*B + A*C + A*D + B*C + B*D + C*D, data = data, trace=FALSE) 


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

# What's below may not work:
# run get_Akaike_weights script in Rsrc folder
P<-get_model_probs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) # want the largest one of these
library(lmtest) # can use later to test for heteroscedacity in the models

