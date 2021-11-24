
# generic 4-fixed factor and 1 fixed covariate models

m0<-glm(cbind(R1, R2)~1, data=data, family=binomial) # this one will be at the end to keep index number and model number the same.
m1<-glm(cbind(R1, R2)~A, data=data, family=binomial)
m2<-glm(cbind(R1, R2)~B, data=data, family=binomial)
m3<-glm(cbind(R1, R2)~C, data=data, family=binomial)
m4<-glm(cbind(R1, R2)~D, data=data, family=binomial)

m5<-glm(cbind(R1, R2)~A+B, data=data, family=binomial)
m6<-glm(cbind(R1, R2)~A+C, data=data, family=binomial)
m7<-glm(cbind(R1, R2)~A+D, data=data, family=binomial)
m8<-glm(cbind(R1, R2)~B+C, data=data, family=binomial)
m9<-glm(cbind(R1, R2)~B+D, data=data, family=binomial)
m10<-glm(cbind(R1, R2)~C+D, data=data, family=binomial)
m11<-glm(cbind(R1, R2)~A+B+C, data=data, family=binomial)
m12<-glm(cbind(R1, R2)~A+B+D, data=data, family=binomial)
m13<-glm(cbind(R1, R2)~A+C+D, data=data, family=binomial)
m14<-glm(cbind(R1, R2)~B+C+D, data=data, family=binomial)
m15<-glm(cbind(R1, R2)~A+B+C+D, data=data, family=binomial)

m16<-glm(cbind(R1, R2)~A*B, data=data, family=binomial)
m17<-glm(cbind(R1, R2)~A*C, data=data, family=binomial)
m18<-glm(cbind(R1, R2)~A*D, data=data, family=binomial)
m19<-glm(cbind(R1, R2)~B*C, data=data, family=binomial)
m20<-glm(cbind(R1, R2)~B*D, data=data, family=binomial)
m21<-glm(cbind(R1, R2)~C*D, data=data, family=binomial)

m22<-glm(cbind(R1, R2)~A*B + C, data=data, family=binomial)
m23<-glm(cbind(R1, R2)~A*B + D, data=data, family=binomial)
m24<-glm(cbind(R1, R2)~A*C + B, data=data, family=binomial)
m25<-glm(cbind(R1, R2)~A*C + D, data=data, family=binomial)
m26<-glm(cbind(R1, R2)~A*D + B, data=data, family=binomial)
m27<-glm(cbind(R1, R2)~A*D + C, data=data, family=binomial)
m28<-glm(cbind(R1, R2)~B*C + A, data=data, family=binomial)
m29<-glm(cbind(R1, R2)~B*C + D, data=data, family=binomial)
m30<-glm(cbind(R1, R2)~B*D + A, data=data, family=binomial)
m31<-glm(cbind(R1, R2)~B*D + C, data=data, family=binomial)
m32<-glm(cbind(R1, R2)~C*D + A, data=data, family=binomial)
m33<-glm(cbind(R1, R2)~C*D + B, data=data, family=binomial)

m34<-glm(cbind(R1, R2)~A*B + C + D, data=data, family=binomial)
m35<-glm(cbind(R1, R2)~A*C + B + D, data=data, family=binomial)
m36<-glm(cbind(R1, R2)~A*D + B + C, data=data, family=binomial)
m37<-glm(cbind(R1, R2)~B*C + A + D, data=data, family=binomial)
m38<-glm(cbind(R1, R2)~B*D + A + C, data=data, family=binomial)
m39<-glm(cbind(R1, R2)~C*D + A + B, data=data, family=binomial)


m40<-glm(cbind(R1, R2)~A*B + A*C, data=data, family=binomial)
m41<-glm(cbind(R1, R2)~A*B + A*D, data=data, family=binomial)
m42<-glm(cbind(R1, R2)~A*B + B*C, data=data, family=binomial)
m43<-glm(cbind(R1, R2)~A*B + B*D, data=data, family=binomial)
m44<-glm(cbind(R1, R2)~A*B + C*D, data=data, family=binomial)
m45<-glm(cbind(R1, R2)~A*C + A*D, data=data, family=binomial)
m46<-glm(cbind(R1, R2)~A*C + B*C, data=data, family=binomial)
m47<-glm(cbind(R1, R2)~A*C + B*D, data=data, family=binomial)
m48<-glm(cbind(R1, R2)~A*C + C*D, data=data, family=binomial)
m49<-glm(cbind(R1, R2)~A*D + B*C, data=data, family=binomial)
m50<-glm(cbind(R1, R2)~A*D + B*D, data=data, family=binomial)
m51<-glm(cbind(R1, R2)~A*D + C*D, data=data, family=binomial)
m52<-glm(cbind(R1, R2)~B*C + B*D, data=data, family=binomial)
m53<-glm(cbind(R1, R2)~B*C + C*D, data=data, family=binomial)
m54<-glm(cbind(R1, R2)~B*D + C*D, data=data, family=binomial)

m55<-glm(cbind(R1, R2)~A*B + A*C + D, data=data, family=binomial)
m56<-glm(cbind(R1, R2)~A*B + A*D + C, data=data, family=binomial)
m57<-glm(cbind(R1, R2)~A*B + B*C + D, data=data, family=binomial)
m58<-glm(cbind(R1, R2)~A*B + B*D + C, data=data, family=binomial)
m59<-glm(cbind(R1, R2)~A*C + A*D + B, data=data, family=binomial)
m60<-glm(cbind(R1, R2)~A*C + B*C + D, data=data, family=binomial)
m61<-glm(cbind(R1, R2)~A*C + C*D + B, data=data, family=binomial)
m62<-glm(cbind(R1, R2)~A*D + B*D + C, data=data, family=binomial)
m63<-glm(cbind(R1, R2)~A*D + C*D + B, data=data, family=binomial)
m64<-glm(cbind(R1, R2)~B*C + B*D + A, data=data, family=binomial)
m65<-glm(cbind(R1, R2)~B*C + C*D + A, data=data, family=binomial)
m66<-glm(cbind(R1, R2)~B*D + C*D + A, data=data, family=binomial)

m67<-glm(cbind(R1, R2)~A*B + A*C + A*D, data=data, family=binomial)
m68<-glm(cbind(R1, R2)~A*B + A*C + B*C, data=data, family=binomial)
m69<-glm(cbind(R1, R2)~A*B + A*C + B*D, data=data, family=binomial)
m70<-glm(cbind(R1, R2)~A*B + A*C + C*D, data=data, family=binomial)
m71<-glm(cbind(R1, R2)~A*B + A*D + B*C, data=data, family=binomial)
m72<-glm(cbind(R1, R2)~A*B + A*D + B*D, data=data, family=binomial)
m73<-glm(cbind(R1, R2)~A*B + A*D + C*D, data=data, family=binomial)
m74<-glm(cbind(R1, R2)~A*B + B*C + B*D, data=data, family=binomial)
m75<-glm(cbind(R1, R2)~A*B + B*C + C*D, data=data, family=binomial)
m76<-glm(cbind(R1, R2)~A*B + B*D + C*D, data=data, family=binomial)
m77<-glm(cbind(R1, R2)~A*C + A*D + B*C, data=data, family=binomial)
m78<-glm(cbind(R1, R2)~A*C + A*D + B*D, data=data, family=binomial)
m79<-glm(cbind(R1, R2)~A*C + A*D + C*D, data=data, family=binomial)
m80<-glm(cbind(R1, R2)~A*C + B*C + B*D, data=data, family=binomial)
m81<-glm(cbind(R1, R2)~A*C + B*C + C*D, data=data, family=binomial)
m82<-glm(cbind(R1, R2)~A*C + B*D + C*D, data=data, family=binomial)
m83<-glm(cbind(R1, R2)~A*D + B*C + B*D, data=data, family=binomial)
m84<-glm(cbind(R1, R2)~A*D + B*C + C*D, data=data, family=binomial)
m85<-glm(cbind(R1, R2)~A*D + B*D + C*D, data=data, family=binomial)
m86<-glm(cbind(R1, R2)~B*C + B*D + C*D, data=data, family=binomial)


m87<-glm(cbind(R1, R2)~A*B + A*C + B*C + D, data=data, family=binomial)
m88<-glm(cbind(R1, R2)~A*B + A*D + B*D + C, data=data, family=binomial)
m89<-glm(cbind(R1, R2)~A*C + A*D + C*D + B, data=data, family=binomial)
m90<-glm(cbind(R1, R2)~B*C + B*D + C*D + A, data=data, family=binomial)


m91<-glm(cbind(R1, R2)~A*B + A*C + A*D + B*C, data=data, family=binomial)
m92<-glm(cbind(R1, R2)~A*B + A*C + A*D + B*D, data=data, family=binomial)
m93<-glm(cbind(R1, R2)~A*B + A*C + A*D + C*D, data=data, family=binomial)
m94<-glm(cbind(R1, R2)~A*B + A*C + B*C + B*D, data=data, family=binomial)
m95<-glm(cbind(R1, R2)~A*B + A*C + B*C + C*D, data=data, family=binomial)
m96<-glm(cbind(R1, R2)~A*B + A*C + B*D + C*D, data=data, family=binomial)
m97<-glm(cbind(R1, R2)~A*B + A*D + B*C + B*D, data=data, family=binomial)
m98<-glm(cbind(R1, R2)~A*B + A*D + B*C + C*D, data=data, family=binomial)
m99<-glm(cbind(R1, R2)~A*B + A*D + B*D + C*D, data=data, family=binomial)
m100<-glm(cbind(R1, R2)~A*B + B*C + B*D + C*D, data=data, family=binomial)
m101<-glm(cbind(R1, R2)~A*C + A*D + B*C + B*D, data=data, family=binomial)
m102<-glm(cbind(R1, R2)~A*C + A*D + B*C + C*D, data=data, family=binomial)
m103<-glm(cbind(R1, R2)~A*C + A*D + B*D + C*D, data=data, family=binomial)
m104<-glm(cbind(R1, R2)~A*C + B*C + B*D + C*D, data=data, family=binomial)
m105<-glm(cbind(R1, R2)~A*D + B*C + B*D + C*D, data=data, family=binomial)


m106<-glm(cbind(R1, R2)~A*B + A*C + A*D + B*C + B*D, data=data, family=binomial)
m107<-glm(cbind(R1, R2)~A*B + A*C + A*D + B*C + C*D, data=data, family=binomial)
m108<-glm(cbind(R1, R2)~A*B + A*C + A*D + B*D + C*D, data=data, family=binomial)
m109<-glm(cbind(R1, R2)~A*B + A*C + B*C + B*D + C*D, data=data, family=binomial)
m110<-glm(cbind(R1, R2)~A*B + A*D + B*C + B*D + C*D, data=data, family=binomial)
m111<-glm(cbind(R1, R2)~A*C + A*D + B*C + B*D + C*D, data=data, family=binomial)

m112<-glm(cbind(R1, R2)~A*B + A*C + A*D + B*C + B*D + C*D, data=data, family=binomial)

# Adding E 

m113<-glm(cbind(R1, R2)~1 + E, data=data, family=binomial)
m114<-glm(cbind(R1, R2)~A + E, data=data, family=binomial)
m115<-glm(cbind(R1, R2)~B + E, data=data, family=binomial)
m116<-glm(cbind(R1, R2)~C + E, data=data, family=binomial)
m117<-glm(cbind(R1, R2)~D + E, data=data, family=binomial)

m118<-glm(cbind(R1, R2)~A+B + E, data=data, family=binomial)
m119<-glm(cbind(R1, R2)~A+C + E, data=data, family=binomial)
m120<-glm(cbind(R1, R2)~A+D + E, data=data, family=binomial)
m121<-glm(cbind(R1, R2)~B+C + E, data=data, family=binomial)
m122<-glm(cbind(R1, R2)~B+D + E, data=data, family=binomial)
m123<-glm(cbind(R1, R2)~C+D + E, data=data, family=binomial)
m124<-glm(cbind(R1, R2)~A+B+C + E, data=data, family=binomial)
m125<-glm(cbind(R1, R2)~A+B+D + E, data=data, family=binomial)
m126<-glm(cbind(R1, R2)~A+C+D + E, data=data, family=binomial)
m127<-glm(cbind(R1, R2)~B+C+D + E, data=data, family=binomial)
m128<-glm(cbind(R1, R2)~A+B+C+D + E, data=data, family=binomial)

m129<-glm(cbind(R1, R2)~A*B + E, data=data, family=binomial)
m130<-glm(cbind(R1, R2)~A*C + E, data=data, family=binomial)
m131<-glm(cbind(R1, R2)~A*D + E, data=data, family=binomial)
m132<-glm(cbind(R1, R2)~B*C + E, data=data, family=binomial)
m133<-glm(cbind(R1, R2)~B*D + E, data=data, family=binomial)
m134<-glm(cbind(R1, R2)~C*D + E, data=data, family=binomial)

m135<-glm(cbind(R1, R2)~A*B + C + E, data=data, family=binomial)
m136<-glm(cbind(R1, R2)~A*B + D + E, data=data, family=binomial)
m137<-glm(cbind(R1, R2)~A*C + B + E, data=data, family=binomial)
m138<-glm(cbind(R1, R2)~A*C + D + E, data=data, family=binomial)
m139<-glm(cbind(R1, R2)~A*D + B + E, data=data, family=binomial)
m140<-glm(cbind(R1, R2)~A*D + C + E, data=data, family=binomial)
m141<-glm(cbind(R1, R2)~B*C + A + E, data=data, family=binomial)
m142<-glm(cbind(R1, R2)~B*C + D + E, data=data, family=binomial)
m143<-glm(cbind(R1, R2)~B*D + A + E, data=data, family=binomial)
m144<-glm(cbind(R1, R2)~B*D + C + E, data=data, family=binomial)
m145<-glm(cbind(R1, R2)~C*D + A + E, data=data, family=binomial)
m146<-glm(cbind(R1, R2)~C*D + B + E, data=data, family=binomial)

m147<-glm(cbind(R1, R2)~A*B + C + D + E, data=data, family=binomial)
m148<-glm(cbind(R1, R2)~A*C + B + D + E, data=data, family=binomial)
m149<-glm(cbind(R1, R2)~A*D + B + C + E, data=data, family=binomial)
m150<-glm(cbind(R1, R2)~B*C + A + D + E, data=data, family=binomial)
m151<-glm(cbind(R1, R2)~B*D + A + C + E, data=data, family=binomial)
m152<-glm(cbind(R1, R2)~C*D + A + B + E, data=data, family=binomial)


m153<-glm(cbind(R1, R2)~A*B + A*C + E, data=data, family=binomial)
m154<-glm(cbind(R1, R2)~A*B + A*D + E, data=data, family=binomial)
m155<-glm(cbind(R1, R2)~A*B + B*C + E, data=data, family=binomial)
m156<-glm(cbind(R1, R2)~A*B + B*D + E, data=data, family=binomial)
m157<-glm(cbind(R1, R2)~A*B + C*D + E, data=data, family=binomial)
m158<-glm(cbind(R1, R2)~A*C + A*D + E, data=data, family=binomial)
m159<-glm(cbind(R1, R2)~A*C + B*C + E, data=data, family=binomial)
m160<-glm(cbind(R1, R2)~A*C + B*D + E, data=data, family=binomial)
m161<-glm(cbind(R1, R2)~A*C + C*D + E, data=data, family=binomial)
m162<-glm(cbind(R1, R2)~A*D + B*C + E, data=data, family=binomial)
m163<-glm(cbind(R1, R2)~A*D + B*D + E, data=data, family=binomial)
m164<-glm(cbind(R1, R2)~A*D + C*D + E, data=data, family=binomial)
m165<-glm(cbind(R1, R2)~B*C + B*D + E, data=data, family=binomial)
m166<-glm(cbind(R1, R2)~B*C + C*D + E, data=data, family=binomial)
m167<-glm(cbind(R1, R2)~B*D + C*D + E, data=data, family=binomial)

m168<-glm(cbind(R1, R2)~A*B + A*C + D + E, data=data, family=binomial)
m169<-glm(cbind(R1, R2)~A*B + A*D + C + E, data=data, family=binomial)
m170<-glm(cbind(R1, R2)~A*B + B*C + D + E, data=data, family=binomial)
m171<-glm(cbind(R1, R2)~A*B + B*D + C + E, data=data, family=binomial)
m172<-glm(cbind(R1, R2)~A*C + A*D + B + E, data=data, family=binomial)
m173<-glm(cbind(R1, R2)~A*C + B*C + D + E, data=data, family=binomial)
m174<-glm(cbind(R1, R2)~A*C + C*D + B + E, data=data, family=binomial)
m175<-glm(cbind(R1, R2)~A*D + B*D + C + E, data=data, family=binomial)
m176<-glm(cbind(R1, R2)~A*D + C*D + B + E, data=data, family=binomial)
m177<-glm(cbind(R1, R2)~B*C + B*D + A + E, data=data, family=binomial)
m178<-glm(cbind(R1, R2)~B*C + C*D + A + E, data=data, family=binomial)
m179<-glm(cbind(R1, R2)~B*D + C*D + A + E, data=data, family=binomial)

m180<-glm(cbind(R1, R2)~A*B + A*C + A*D + E, data=data, family=binomial)
m181<-glm(cbind(R1, R2)~A*B + A*C + B*C + E, data=data, family=binomial)
m182<-glm(cbind(R1, R2)~A*B + A*C + B*D + E, data=data, family=binomial)
m183<-glm(cbind(R1, R2)~A*B + A*C + C*D + E, data=data, family=binomial)
m184<-glm(cbind(R1, R2)~A*B + A*D + B*C + E, data=data, family=binomial)
m185<-glm(cbind(R1, R2)~A*B + A*D + B*D + E, data=data, family=binomial)
m186<-glm(cbind(R1, R2)~A*B + A*D + C*D + E, data=data, family=binomial)
m187<-glm(cbind(R1, R2)~A*B + B*C + B*D + E, data=data, family=binomial)
m188<-glm(cbind(R1, R2)~A*B + B*C + C*D + E, data=data, family=binomial)
m189<-glm(cbind(R1, R2)~A*B + B*D + C*D + E, data=data, family=binomial)
m190<-glm(cbind(R1, R2)~A*C + A*D + B*C + E, data=data, family=binomial)
m191<-glm(cbind(R1, R2)~A*C + A*D + B*D + E, data=data, family=binomial)
m192<-glm(cbind(R1, R2)~A*C + A*D + C*D + E, data=data, family=binomial)
m193<-glm(cbind(R1, R2)~A*C + B*C + B*D + E, data=data, family=binomial)
m194<-glm(cbind(R1, R2)~A*C + B*C + C*D + E, data=data, family=binomial)
m195<-glm(cbind(R1, R2)~A*C + B*D + C*D + E, data=data, family=binomial)
m196<-glm(cbind(R1, R2)~A*D + B*C + B*D + E, data=data, family=binomial)
m197<-glm(cbind(R1, R2)~A*D + B*C + C*D + E, data=data, family=binomial)
m198<-glm(cbind(R1, R2)~A*D + B*D + C*D + E, data=data, family=binomial)
m199<-glm(cbind(R1, R2)~B*C + B*D + C*D + E, data=data, family=binomial)


m200<-glm(cbind(R1, R2)~A*B + A*C + B*C + D + E, data=data, family=binomial)
m201<-glm(cbind(R1, R2)~A*B + A*D + B*D + C + E, data=data, family=binomial)
m202<-glm(cbind(R1, R2)~A*C + A*D + C*D + B + E, data=data, family=binomial)
m203<-glm(cbind(R1, R2)~B*C + B*D + C*D + A + E, data=data, family=binomial)


m204<-glm(cbind(R1, R2)~A*B + A*C + A*D + B*C + E, data=data, family=binomial)
m205<-glm(cbind(R1, R2)~A*B + A*C + A*D + B*D + E, data=data, family=binomial)
m206<-glm(cbind(R1, R2)~A*B + A*C + A*D + C*D + E, data=data, family=binomial)
m207<-glm(cbind(R1, R2)~A*B + A*C + B*C + B*D + E, data=data, family=binomial)
m208<-glm(cbind(R1, R2)~A*B + A*C + B*C + C*D + E, data=data, family=binomial)
m209<-glm(cbind(R1, R2)~A*B + A*C + B*D + C*D + E, data=data, family=binomial)
m210<-glm(cbind(R1, R2)~A*B + A*D + B*C + B*D + E, data=data, family=binomial)
m211<-glm(cbind(R1, R2)~A*B + A*D + B*C + C*D + E, data=data, family=binomial)
m212<-glm(cbind(R1, R2)~A*B + A*D + B*D + C*D + E, data=data, family=binomial)
m213<-glm(cbind(R1, R2)~A*B + B*C + B*D + C*D + E, data=data, family=binomial)
m214<-glm(cbind(R1, R2)~A*C + A*D + B*C + B*D + E, data=data, family=binomial)
m215<-glm(cbind(R1, R2)~A*C + A*D + B*C + C*D + E, data=data, family=binomial)
m216<-glm(cbind(R1, R2)~A*C + A*D + B*D + C*D + E, data=data, family=binomial)
m217<-glm(cbind(R1, R2)~A*C + B*C + B*D + C*D + E, data=data, family=binomial)
m218<-glm(cbind(R1, R2)~A*D + B*C + B*D + C*D + E, data=data, family=binomial)


m219<-glm(cbind(R1, R2)~A*B + A*C + A*D + B*C + B*D + E, data=data, family=binomial)
m220<-glm(cbind(R1, R2)~A*B + A*C + A*D + B*C + C*D + E, data=data, family=binomial)
m221<-glm(cbind(R1, R2)~A*B + A*C + A*D + B*D + C*D + E, data=data, family=binomial)
m222<-glm(cbind(R1, R2)~A*B + A*C + B*C + B*D + C*D + E, data=data, family=binomial)
m223<-glm(cbind(R1, R2)~A*B + A*D + B*C + B*D + C*D + E, data=data, family=binomial)
m224<-glm(cbind(R1, R2)~A*C + A*D + B*C + B*D + C*D + E, data=data, family=binomial)

# identify top models using AIC
summary<-AIC(m1,m2,m3,m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, 
             m17, m18, m19, m20, m21, m22, m23, m24, m25, m26, m27, m28, m29, m30, 
             m31, m32, m33, m34, m35, m36, m37, m38, m39, m40, m41, m42, m43, m44, 
             m45, m46, m47, m48, m49, m50, m51, m52, m53, m54, m55, m56, m57, m58, 
             m59, m60, m61, m62, m63, m64, m65, m66, m67, m68, m69, m70, m71, m72, 
             m73, m74, m75, m76, m77, m78, m79, m80, m81, m82, m83, m84, m85, m86, 
             m87, m88, m89, m90, m91, m92, m93, m94, m95, m96, m97, m98, m99, m100, 
             m101, m102, m103, m104, m105, m106, m107, m108, m109, m110, m111, m112, 
             m113, m114, m115, m116, m117, m118, m119, m120, m121, m122, m123, m124,
             m125, m126, m127, m128, m129, m130, m131, m132, m133, m134, m135, m136,
             m137, m138, m139, m140, m141, m142, m143, m144, m145, m146, m147, m148, 
             m149, m150, m151, m152, m153, m154, m155, m156, m157, m158, m159, m160,
             m161, m162, m163, m164, m165, m166, m167, m168, m169, m170, m171, m172, 
             m173, m174, m175, m176, m177, m178, m179, m180, m181, m182, m183, m184,
             m185, m186, m187, m188, m189, m190, m191, m192, m193, m194, m195, m196, 
             m197, m198, m199, m200, m201, m202, m203, m204, m205, m206, m207, m208,
             m209, m210, m211, m212, m213, m214, m215, m216, m217, m218, m219, m220,
             m221, m222, m223, m224, m0)

sort(summary$AIC, index.return=TRUE)

# run get_Akaike_weights script in Rsrc folder
P<-get_model_probs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) # want the largest one of these
library(lmtest) # can use later to test for heteroscedacity in the models


