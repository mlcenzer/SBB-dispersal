
# generic 4-fixed factor analysis with Gamma(link="log") data

m0<-glmer(R~1 + (1|X), data=data, family=Gamma (link="log")) #this one will be at the end to keep index number and model number the same.
m1<-glmer(R~A + (1|X), data=data, family=Gamma (link="log"))
m2<-glmer(R~B + (1|X), data=data, family=Gamma (link="log"))
m3<-glmer(R~C + (1|X), data=data, family=Gamma (link="log"))
m4<-glmer(R~D + (1|X), data=data, family=Gamma (link="log"))

m5<-glmer(R~A+B + (1|X), data=data, family=Gamma (link="log"))
m6<-glmer(R~A+C + (1|X), data=data, family=Gamma (link="log"))
m7<-glmer(R~A+D + (1|X), data=data, family=Gamma (link="log"))
m8<-glmer(R~B+C + (1|X), data=data, family=Gamma (link="log"))
m9<-glmer(R~B+D + (1|X), data=data, family=Gamma (link="log"))
m10<-glmer(R~C+D + (1|X), data=data, family=Gamma (link="log"))

m11<-glmer(R~A+B+C + (1|X), data=data, family=Gamma (link="log"))
m12<-glmer(R~A+B+D + (1|X), data=data, family=Gamma (link="log"))
m13<-glmer(R~A+C+D + (1|X), data=data, family=Gamma (link="log"))
m14<-glmer(R~B+C+D + (1|X), data=data, family=Gamma (link="log"))

m15<-glmer(R~A+B+C+D + (1|X), data=data, family=Gamma (link="log"))


m16<-glmer(R~A*B + (1|X), data=data, family=Gamma (link="log"))
m17<-glmer(R~A*C + (1|X), data=data, family=Gamma (link="log"))
m18<-glmer(R~A*D + (1|X), data=data, family=Gamma (link="log"))
m19<-glmer(R~B*C + (1|X), data=data, family=Gamma (link="log"))
m20<-glmer(R~B*D + (1|X), data=data, family=Gamma (link="log"))
m21<-glmer(R~C*D + (1|X), data=data, family=Gamma (link="log"))

m22<-glmer(R~A*B + C + (1|X), data=data, family=Gamma (link="log"))
m23<-glmer(R~A*B + D + (1|X), data=data, family=Gamma (link="log"))
m24<-glmer(R~A*C + B + (1|X), data=data, family=Gamma (link="log"))
m25<-glmer(R~A*C + D + (1|X), data=data, family=Gamma (link="log"))
m26<-glmer(R~A*D + B + (1|X), data=data, family=Gamma (link="log"))
m27<-glmer(R~A*D + C + (1|X), data=data, family=Gamma (link="log"))
m28<-glmer(R~B*C + A + (1|X), data=data, family=Gamma (link="log"))
m29<-glmer(R~B*C + D + (1|X), data=data, family=Gamma (link="log"))
m30<-glmer(R~B*D + A + (1|X), data=data, family=Gamma (link="log"))
m31<-glmer(R~B*D + C + (1|X), data=data, family=Gamma (link="log"))
m32<-glmer(R~C*D + A + (1|X), data=data, family=Gamma (link="log"))
m33<-glmer(R~C*D + B + (1|X), data=data, family=Gamma (link="log"))

m34<-glmer(R~A*B + C + D + (1|X), data=data, family=Gamma (link="log"))
m35<-glmer(R~A*C + B + D + (1|X), data=data, family=Gamma (link="log"))
m36<-glmer(R~A*D + B + C + (1|X), data=data, family=Gamma (link="log"))
m37<-glmer(R~B*C + A + D + (1|X), data=data, family=Gamma (link="log"))
m38<-glmer(R~B*D + A + C + (1|X), data=data, family=Gamma (link="log"))
m39<-glmer(R~C*D + A + B + (1|X), data=data, family=Gamma (link="log"))


m40<-glmer(R~A*B + A*C + (1|X), data=data, family=Gamma (link="log"))
m41<-glmer(R~A*B + A*D + (1|X), data=data, family=Gamma (link="log"))
m42<-glmer(R~A*B + B*C + (1|X), data=data, family=Gamma (link="log"))
m43<-glmer(R~A*B + B*D + (1|X), data=data, family=Gamma (link="log"))
m44<-glmer(R~A*B + C*D + (1|X), data=data, family=Gamma (link="log"))
m45<-glmer(R~A*C + A*D + (1|X), data=data, family=Gamma (link="log"))
m46<-glmer(R~A*C + B*C + (1|X), data=data, family=Gamma (link="log"))
m47<-glmer(R~A*C + B*D + (1|X), data=data, family=Gamma (link="log"))
m48<-glmer(R~A*C + C*D + (1|X), data=data, family=Gamma (link="log"))
m49<-glmer(R~A*D + B*C + (1|X), data=data, family=Gamma (link="log"))
m50<-glmer(R~A*D + B*D + (1|X), data=data, family=Gamma (link="log"))
m51<-glmer(R~A*D + C*D + (1|X), data=data, family=Gamma (link="log"))
m52<-glmer(R~B*C + B*D + (1|X), data=data, family=Gamma (link="log"))
m53<-glmer(R~B*C + C*D + (1|X), data=data, family=Gamma (link="log"))
m54<-glmer(R~B*D + C*D + (1|X), data=data, family=Gamma (link="log"))

m55<-glmer(R~A*B + A*C + D + (1|X), data=data, family=Gamma (link="log"))
m56<-glmer(R~A*B + A*D + C + (1|X), data=data, family=Gamma (link="log"))
m57<-glmer(R~A*B + B*C + D + (1|X), data=data, family=Gamma (link="log"))
m58<-glmer(R~A*B + B*D + C + (1|X), data=data, family=Gamma (link="log"))
m59<-glmer(R~A*C + A*D + B + (1|X), data=data, family=Gamma (link="log"))
m60<-glmer(R~A*C + B*C + D + (1|X), data=data, family=Gamma (link="log"))
m61<-glmer(R~A*C + C*D + B + (1|X), data=data, family=Gamma (link="log"))
m62<-glmer(R~A*D + B*D + C + (1|X), data=data, family=Gamma (link="log"))
m63<-glmer(R~A*D + C*D + B + (1|X), data=data, family=Gamma (link="log"))
m64<-glmer(R~B*C + B*D + A + (1|X), data=data, family=Gamma (link="log"))
m65<-glmer(R~B*C + C*D + A + (1|X), data=data, family=Gamma (link="log"))
m66<-glmer(R~B*D + C*D + A + (1|X), data=data, family=Gamma (link="log"))



m67<-glmer(R~A*B + A*C + A*D + (1|X), data=data, family=Gamma (link="log"))
m68<-glmer(R~A*B + A*C + B*C + (1|X), data=data, family=Gamma (link="log"))
m69<-glmer(R~A*B + A*C + B*D + (1|X), data=data, family=Gamma (link="log"))
m70<-glmer(R~A*B + A*C + C*D + (1|X), data=data, family=Gamma (link="log"))
m71<-glmer(R~A*B + A*D + B*C + (1|X), data=data, family=Gamma (link="log"))
m72<-glmer(R~A*B + A*D + B*D + (1|X), data=data, family=Gamma (link="log"))
m73<-glmer(R~A*B + A*D + C*D + (1|X), data=data, family=Gamma (link="log"))
m74<-glmer(R~A*B + B*C + B*D + (1|X), data=data, family=Gamma (link="log"))
m75<-glmer(R~A*B + B*C + C*D + (1|X), data=data, family=Gamma (link="log"))
m76<-glmer(R~A*B + B*D + C*D + (1|X), data=data, family=Gamma (link="log"))
m77<-glmer(R~A*C + A*D + B*C + (1|X), data=data, family=Gamma (link="log"))
m78<-glmer(R~A*C + A*D + B*D + (1|X), data=data, family=Gamma (link="log"))
m79<-glmer(R~A*C + A*D + C*D + (1|X), data=data, family=Gamma (link="log"))
m80<-glmer(R~A*C + B*C + B*D + (1|X), data=data, family=Gamma (link="log"))
m81<-glmer(R~A*C + B*C + C*D + (1|X), data=data, family=Gamma (link="log"))
m82<-glmer(R~A*C + B*D + C*D + (1|X), data=data, family=Gamma (link="log"))
m83<-glmer(R~A*D + B*C + B*D + (1|X), data=data, family=Gamma (link="log"))
m84<-glmer(R~A*D + B*C + C*D + (1|X), data=data, family=Gamma (link="log"))
m85<-glmer(R~A*D + B*D + C*D + (1|X), data=data, family=Gamma (link="log"))
m86<-glmer(R~B*C + B*D + C*D + (1|X), data=data, family=Gamma (link="log"))


m87<-glmer(R~A*B + A*C + B*C + D + (1|X), data=data, family=Gamma (link="log"))
m88<-glmer(R~A*B + A*D + B*D + C + (1|X), data=data, family=Gamma (link="log"))
m89<-glmer(R~A*C + A*D + C*D + B + (1|X), data=data, family=Gamma (link="log"))
m90<-glmer(R~B*C + B*D + C*D + A + (1|X), data=data, family=Gamma (link="log"))


m91<-glmer(R~A*B + A*C + A*D + B*C + (1|X), data=data, family=Gamma (link="log"))
m92<-glmer(R~A*B + A*C + A*D + B*D + (1|X), data=data, family=Gamma (link="log"))
m93<-glmer(R~A*B + A*C + A*D + C*D + (1|X), data=data, family=Gamma (link="log"))
m94<-glmer(R~A*B + A*C + B*C + B*D + (1|X), data=data, family=Gamma (link="log"))
m95<-glmer(R~A*B + A*C + B*C + C*D + (1|X), data=data, family=Gamma (link="log"))
m96<-glmer(R~A*B + A*C + B*D + C*D + (1|X), data=data, family=Gamma (link="log"))
m97<-glmer(R~A*B + A*D + B*C + B*D + (1|X), data=data, family=Gamma (link="log"))
m98<-glmer(R~A*B + A*D + B*C + C*D + (1|X), data=data, family=Gamma (link="log"))
m99<-glmer(R~A*B + A*D + B*D + C*D + (1|X), data=data, family=Gamma (link="log"))
m100<-glmer(R~A*B + B*C + B*D + C*D + (1|X), data=data, family=Gamma (link="log"))
m101<-glmer(R~A*C + A*D + B*C + B*D + (1|X), data=data, family=Gamma (link="log"))
m102<-glmer(R~A*C + A*D + B*C + C*D + (1|X), data=data, family=Gamma (link="log"))
m103<-glmer(R~A*C + A*D + B*D + C*D + (1|X), data=data, family=Gamma (link="log"))
m104<-glmer(R~A*C + B*C + B*D + C*D + (1|X), data=data, family=Gamma (link="log"))
m105<-glmer(R~A*D + B*C + B*D + C*D + (1|X), data=data, family=Gamma (link="log"))


m106<-glmer(R~A*B + A*C + A*D + B*C + B*D + (1|X), data=data, family=Gamma (link="log"))
m107<-glmer(R~A*B + A*C + A*D + B*C + C*D + (1|X), data=data, family=Gamma (link="log"))
m108<-glmer(R~A*B + A*C + A*D + B*D + C*D + (1|X), data=data, family=Gamma (link="log"))
m109<-glmer(R~A*B + A*C + B*C + B*D + C*D + (1|X), data=data, family=Gamma (link="log"))
m110<-glmer(R~A*B + A*D + B*C + B*D + C*D + (1|X), data=data, family=Gamma (link="log"))
m111<-glmer(R~A*C + A*D + B*C + B*D + C*D + (1|X), data=data, family=Gamma (link="log"))
m112<-glmer(R~A*B + A*C + A*D + B*C + B*D + C*D + (1|X), data=data, family=Gamma (link="log"))


#m113<-glmer(R~A*B*C + (1|X), data=data, family=Gamma (link="log"))
#m114<-glmer(R~A*B*D + (1|X), data=data, family=Gamma (link="log"))
#m115<-glmer(R~A*C*D + (1|X), data=data, family=Gamma (link="log"))
#m116<-glmer(R~B*C*D + (1|X), data=data, family=Gamma (link="log"))


#m117<-glmer(R~A*B*C + D + (1|X), data=data, family=Gamma (link="log"))
#m118<-glmer(R~A*B*D + C + (1|X), data=data, family=Gamma (link="log"))
#m119<-glmer(R~A*C*D + B + (1|X), data=data, family=Gamma (link="log"))
#m120<-glmer(R~B*C*D + A + (1|X), data=data, family=Gamma (link="log"))


#m121<-glmer(R~A*B*C + A*D + (1|X), data=data, family=Gamma (link="log"))
#m122<-glmer(R~A*B*C + B*D + (1|X), data=data, family=Gamma (link="log"))
#m123<-glmer(R~A*B*C + C*D + (1|X), data=data, family=Gamma (link="log"))
#m124<-glmer(R~A*B*D + A*C + (1|X), data=data, family=Gamma (link="log"))
#m125<-glmer(R~A*B*D + B*C + (1|X), data=data, family=Gamma (link="log"))
#m126<-glmer(R~A*B*D + D*C + (1|X), data=data, family=Gamma (link="log"))
#m127<-glmer(R~A*C*D + A*B + (1|X), data=data, family=Gamma (link="log"))
#m128<-glmer(R~A*C*D + C*B + (1|X), data=data, family=Gamma (link="log"))
#m129<-glmer(R~A*C*D + D*B + (1|X), data=data, family=Gamma (link="log"))
#m130<-glmer(R~B*C*D + B*A + (1|X), data=data, family=Gamma (link="log"))
#m131<-glmer(R~B*C*D + C*A + (1|X), data=data, family=Gamma (link="log"))
#m132<-glmer(R~B*C*D + D*A + (1|X), data=data, family=Gamma (link="log"))


#m133<-glmer(R~A*B*C + A*D + B*D + (1|X), data=data, family=Gamma (link="log"))
#m134<-glmer(R~A*B*C + A*D + C*D + (1|X), data=data, family=Gamma (link="log"))
#m135<-glmer(R~A*B*C + B*D + C*D + (1|X), data=data, family=Gamma (link="log"))
#m136<-glmer(R~A*B*D + A*C + B*C + (1|X), data=data, family=Gamma (link="log"))
#m137<-glmer(R~A*B*D + A*C + D*C + (1|X), data=data, family=Gamma (link="log"))
#m138<-glmer(R~A*B*D + B*C + D*C + (1|X), data=data, family=Gamma (link="log"))
#m139<-glmer(R~A*C*D + A*B + C*B + (1|X), data=data, family=Gamma (link="log"))
#m140<-glmer(R~A*C*D + A*B + D*B + (1|X), data=data, family=Gamma (link="log"))
#m141<-glmer(R~A*C*D + C*B + D*B + (1|X), data=data, family=Gamma (link="log"))
#m142<-glmer(R~B*C*D + B*A + C*A + (1|X), data=data, family=Gamma (link="log"))
#m143<-glmer(R~B*C*D + B*A + D*A + (1|X), data=data, family=Gamma (link="log"))
#m144<-glmer(R~B*C*D + C*A + D*A + (1|X), data=data, family=Gamma (link="log"))


#m145<-glmer(R~A*B*C + A*D + B*D + C*D + (1|X), data=data, family=Gamma (link="log"))
#m146<-glmer(R~A*B*D + A*C + B*C + D*C + (1|X), data=data, family=Gamma (link="log"))
#m147<-glmer(R~A*C*D + A*B + C*B + D*B + (1|X), data=data, family=Gamma (link="log"))
#m148<-glmer(R~B*C*D + B*A + C*A + D*A + (1|X), data=data, family=Gamma (link="log"))


#m149<-glmer(R~A*B*C + A*B*D + (1|X), data=data, family=Gamma (link="log"))
#m150<-glmer(R~A*B*C + A*C*D + (1|X), data=data, family=Gamma (link="log"))
#m151<-glmer(R~A*B*C + B*C*D + (1|X), data=data, family=Gamma (link="log"))
#m152<-glmer(R~A*B*D + A*C*D + (1|X), data=data, family=Gamma (link="log"))
#m153<-glmer(R~A*B*D + B*C*D + (1|X), data=data, family=Gamma (link="log"))
#m154<-glmer(R~A*C*D + B*C*D + (1|X), data=data, family=Gamma (link="log"))


#m155<-glmer(R~A*B*C + A*B*D + C*D + (1|X), data=data, family=Gamma (link="log"))
#m156<-glmer(R~A*B*C + A*C*D + B*D + (1|X), data=data, family=Gamma (link="log"))
#m157<-glmer(R~A*B*C + B*C*D + A*D + (1|X), data=data, family=Gamma (link="log"))
#m158<-glmer(R~A*B*D + A*C*D + B*C + (1|X), data=data, family=Gamma (link="log"))
#m159<-glmer(R~A*B*D + B*C*D + A*C + (1|X), data=data, family=Gamma (link="log"))
#m160<-glmer(R~A*C*D + B*C*D + A*B + (1|X), data=data, family=Gamma (link="log"))


#m161<-glmer(R~A*B*C + A*B*D + A*C*D + (1|X), data=data, family=Gamma (link="log"))
#m162<-glmer(R~A*B*C + A*B*D + B*C*D + (1|X), data=data, family=Gamma (link="log"))
#m163<-glmer(R~A*B*C + A*C*D + B*C*D + (1|X), data=data, family=Gamma (link="log"))
#m164<-glmer(R~A*B*D + A*C*D + B*C*D + (1|X), data=data, family=Gamma (link="log"))


#m165<-glmer(R~A*B*C + A*B*D + A*C*D + B*C*D + (1|X), data=data, family=Gamma (link="log"))

#m166<-glmer(R~A*B*C*D + (1|X), data=data, family=Gamma (link="log"))


#identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, 
             m19, m20, m21, m22, m23, m24, m25, m26, m27, m28, m29, m30, m31, m32, m33, m34, 
             m35, m36, m37, m38, m39, m40, m41, m42, m43, m44, m45, m46, m47, m48, m49, m50, 
             m51, m52, m53, m54, m55, m56, m57, m58, m59, m60, m61, m62, m63, m64, m65, m66, 
             m67, m68, m69, m70, m71, m72, m73, m74, m75, m76, m77, m78, m79, m80, m81, m82, 
             m83, m84, m85, m86, m87, m88, m89, m90, m91, m92, m93, m94, m95, m96, m97, m98, 
             m99, m100, m101, m102, m103, m104, m105, m106, m107, m108, m109, m110, m111, m112, m0)
sort(summary$AIC, index.return=TRUE)

# run get_Akaike_weights script in Rsrc folder
P<-get_model_probs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) # want the largest one of these
library(lmtest) # can use later to test for heteroscedacity in the models
