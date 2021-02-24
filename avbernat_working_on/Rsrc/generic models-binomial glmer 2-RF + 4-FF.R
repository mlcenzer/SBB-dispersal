library(lme4)
#generic 4-fixed factor and 2 random-factor models

## First Random Factor 

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

## Second Random Factor 

m113<-glmer(R~1 + (1|Y), data=data, family=binomial) 
m114<-glmer(R~A + (1|Y), data=data, family=binomial)
m115<-glmer(R~B + (1|Y), data=data, family=binomial)
m116<-glmer(R~C + (1|Y), data=data, family=binomial)
m117<-glmer(R~D + (1|Y), data=data, family=binomial)

m118<-glmer(R~A+B + (1|Y), data=data, family=binomial)
m119<-glmer(R~A+C + (1|Y), data=data, family=binomial)
m120<-glmer(R~A+D + (1|Y), data=data, family=binomial)
m121<-glmer(R~B+C + (1|Y), data=data, family=binomial)
m122<-glmer(R~B+D + (1|Y), data=data, family=binomial)
m123<-glmer(R~C+D + (1|Y), data=data, family=binomial)
m124<-glmer(R~A+B+C + (1|Y), data=data, family=binomial)
m125<-glmer(R~A+B+D + (1|Y), data=data, family=binomial)
m126<-glmer(R~A+C+D + (1|Y), data=data, family=binomial)
m127<-glmer(R~B+C+D + (1|Y), data=data, family=binomial)
m128<-glmer(R~A+B+C+D + (1|Y), data=data, family=binomial)

m129<-glmer(R~A*B + (1|Y), data=data, family=binomial)
m130<-glmer(R~A*C + (1|Y), data=data, family=binomial)
m131<-glmer(R~A*D + (1|Y), data=data, family=binomial)
m132<-glmer(R~B*C + (1|Y), data=data, family=binomial)
m133<-glmer(R~B*D + (1|Y), data=data, family=binomial)
m134<-glmer(R~C*D + (1|Y), data=data, family=binomial)

m135<-glmer(R~A*B + C + (1|Y), data=data, family=binomial)
m136<-glmer(R~A*B + D + (1|Y), data=data, family=binomial)
m137<-glmer(R~A*C + B + (1|Y), data=data, family=binomial)
m138<-glmer(R~A*C + D + (1|Y), data=data, family=binomial)
m139<-glmer(R~A*D + B + (1|Y), data=data, family=binomial)
m140<-glmer(R~A*D + C + (1|Y), data=data, family=binomial)
m141<-glmer(R~B*C + A + (1|Y), data=data, family=binomial)
m142<-glmer(R~B*C + D + (1|Y), data=data, family=binomial)
m143<-glmer(R~B*D + A + (1|Y), data=data, family=binomial)
m144<-glmer(R~B*D + C + (1|Y), data=data, family=binomial)
m145<-glmer(R~C*D + A + (1|Y), data=data, family=binomial)
m146<-glmer(R~C*D + B + (1|Y), data=data, family=binomial)

m147<-glmer(R~A*B + C + D + (1|Y), data=data, family=binomial)
m148<-glmer(R~A*C + B + D + (1|Y), data=data, family=binomial)
m149<-glmer(R~A*D + B + C + (1|Y), data=data, family=binomial)
m150<-glmer(R~B*C + A + D + (1|Y), data=data, family=binomial)
m151<-glmer(R~B*D + A + C + (1|Y), data=data, family=binomial)
m152<-glmer(R~C*D + A + B + (1|Y), data=data, family=binomial)


m153<-glmer(R~A*B + A*C + (1|Y), data=data, family=binomial)
m154<-glmer(R~A*B + A*D + (1|Y), data=data, family=binomial)
m155<-glmer(R~A*B + B*C + (1|Y), data=data, family=binomial)
m156<-glmer(R~A*B + B*D + (1|Y), data=data, family=binomial)
m157<-glmer(R~A*B + C*D + (1|Y), data=data, family=binomial)
m158<-glmer(R~A*C + A*D + (1|Y), data=data, family=binomial)
m159<-glmer(R~A*C + B*C + (1|Y), data=data, family=binomial)
m160<-glmer(R~A*C + B*D + (1|Y), data=data, family=binomial)
m161<-glmer(R~A*C + C*D + (1|Y), data=data, family=binomial)
m162<-glmer(R~A*D + B*C + (1|Y), data=data, family=binomial)
m163<-glmer(R~A*D + B*D + (1|Y), data=data, family=binomial)
m164<-glmer(R~A*D + C*D + (1|Y), data=data, family=binomial)
m165<-glmer(R~B*C + B*D + (1|Y), data=data, family=binomial)
m166<-glmer(R~B*C + C*D + (1|Y), data=data, family=binomial)
m167<-glmer(R~B*D + C*D + (1|Y), data=data, family=binomial)

m168<-glmer(R~A*B + A*C + D + (1|Y), data=data, family=binomial)
m169<-glmer(R~A*B + A*D + C + (1|Y), data=data, family=binomial)
m170<-glmer(R~A*B + B*C + D + (1|Y), data=data, family=binomial)
m171<-glmer(R~A*B + B*D + C + (1|Y), data=data, family=binomial)
m172<-glmer(R~A*C + A*D + B + (1|Y), data=data, family=binomial)
m173<-glmer(R~A*C + B*C + D + (1|Y), data=data, family=binomial)
m174<-glmer(R~A*C + C*D + B + (1|Y), data=data, family=binomial)
m175<-glmer(R~A*D + B*D + C + (1|Y), data=data, family=binomial)
m176<-glmer(R~A*D + C*D + B + (1|Y), data=data, family=binomial)
m177<-glmer(R~B*C + B*D + A + (1|Y), data=data, family=binomial)
m178<-glmer(R~B*C + C*D + A + (1|Y), data=data, family=binomial)
m179<-glmer(R~B*D + C*D + A + (1|Y), data=data, family=binomial)

m180<-glmer(R~A*B + A*C + A*D + (1|Y), data=data, family=binomial)
m181<-glmer(R~A*B + A*C + B*C + (1|Y), data=data, family=binomial)
m182<-glmer(R~A*B + A*C + B*D + (1|Y), data=data, family=binomial)
m183<-glmer(R~A*B + A*C + C*D + (1|Y), data=data, family=binomial)
m184<-glmer(R~A*B + A*D + B*C + (1|Y), data=data, family=binomial)
m185<-glmer(R~A*B + A*D + B*D + (1|Y), data=data, family=binomial)
m186<-glmer(R~A*B + A*D + C*D + (1|Y), data=data, family=binomial)
m187<-glmer(R~A*B + B*C + B*D + (1|Y), data=data, family=binomial)
m188<-glmer(R~A*B + B*C + C*D + (1|Y), data=data, family=binomial)
m189<-glmer(R~A*B + B*D + C*D + (1|Y), data=data, family=binomial)
m190<-glmer(R~A*C + A*D + B*C + (1|Y), data=data, family=binomial)
m191<-glmer(R~A*C + A*D + B*D + (1|Y), data=data, family=binomial)
m192<-glmer(R~A*C + A*D + C*D + (1|Y), data=data, family=binomial)
m193<-glmer(R~A*C + B*C + B*D + (1|Y), data=data, family=binomial)
m194<-glmer(R~A*C + B*C + C*D + (1|Y), data=data, family=binomial)
m195<-glmer(R~A*C + B*D + C*D + (1|Y), data=data, family=binomial)
m196<-glmer(R~A*D + B*C + B*D + (1|Y), data=data, family=binomial)
m197<-glmer(R~A*D + B*C + C*D + (1|Y), data=data, family=binomial)
m198<-glmer(R~A*D + B*D + C*D + (1|Y), data=data, family=binomial)
m199<-glmer(R~B*C + B*D + C*D + (1|Y), data=data, family=binomial)


m200<-glmer(R~A*B + A*C + B*C + D + (1|Y), data=data, family=binomial)
m201<-glmer(R~A*B + A*D + B*D + C + (1|Y), data=data, family=binomial)
m202<-glmer(R~A*C + A*D + C*D + B + (1|Y), data=data, family=binomial)
m203<-glmer(R~B*C + B*D + C*D + A + (1|Y), data=data, family=binomial)


m204<-glmer(R~A*B + A*C + A*D + B*C + (1|Y), data=data, family=binomial)
m205<-glmer(R~A*B + A*C + A*D + B*D + (1|Y), data=data, family=binomial)
m206<-glmer(R~A*B + A*C + A*D + C*D + (1|Y), data=data, family=binomial)
m207<-glmer(R~A*B + A*C + B*C + B*D + (1|Y), data=data, family=binomial)
m208<-glmer(R~A*B + A*C + B*C + C*D + (1|Y), data=data, family=binomial)
m209<-glmer(R~A*B + A*C + B*D + C*D + (1|Y), data=data, family=binomial)
m210<-glmer(R~A*B + A*D + B*C + B*D + (1|Y), data=data, family=binomial)
m211<-glmer(R~A*B + A*D + B*C + C*D + (1|Y), data=data, family=binomial)
m212<-glmer(R~A*B + A*D + B*D + C*D + (1|Y), data=data, family=binomial)
m213<-glmer(R~A*B + B*C + B*D + C*D + (1|Y), data=data, family=binomial)
m214<-glmer(R~A*C + A*D + B*C + B*D + (1|Y), data=data, family=binomial)
m215<-glmer(R~A*C + A*D + B*C + C*D + (1|Y), data=data, family=binomial)
m216<-glmer(R~A*C + A*D + B*D + C*D + (1|Y), data=data, family=binomial)
m217<-glmer(R~A*C + B*C + B*D + C*D + (1|Y), data=data, family=binomial)
m218<-glmer(R~A*D + B*C + B*D + C*D + (1|Y), data=data, family=binomial)


m219<-glmer(R~A*B + A*C + A*D + B*C + B*D + (1|Y), data=data, family=binomial)
m220<-glmer(R~A*B + A*C + A*D + B*C + C*D + (1|Y), data=data, family=binomial)
m221<-glmer(R~A*B + A*C + A*D + B*D + C*D + (1|Y), data=data, family=binomial)
m222<-glmer(R~A*B + A*C + B*C + B*D + C*D + (1|Y), data=data, family=binomial)
m223<-glmer(R~A*B + A*D + B*C + B*D + C*D + (1|Y), data=data, family=binomial)
m224<-glmer(R~A*C + A*D + B*C + B*D + C*D + (1|Y), data=data, family=binomial)

m225<-glmer(R~A*B + A*C + A*D + B*C + B*D + C*D + (1|Y), data=data, family=binomial)

## Both Random Factors 

m226<-glmer(R~1 + (1|X) + (1|Y), data=data, family=binomial) 
m227<-glmer(R~A + (1|X) + (1|Y), data=data, family=binomial)
m228<-glmer(R~B + (1|X) + (1|Y), data=data, family=binomial)
m229<-glmer(R~C + (1|X) + (1|Y), data=data, family=binomial)
m230<-glmer(R~D + (1|X) + (1|Y), data=data, family=binomial)

m231<-glmer(R~A+B + (1|X) + (1|Y), data=data, family=binomial)
m232<-glmer(R~A+C + (1|X) + (1|Y), data=data, family=binomial)
m233<-glmer(R~A+D + (1|X) + (1|Y), data=data, family=binomial)
m234<-glmer(R~B+C + (1|X) + (1|Y), data=data, family=binomial)
m235<-glmer(R~B+D + (1|X) + (1|Y), data=data, family=binomial)
m236<-glmer(R~C+D + (1|X) + (1|Y), data=data, family=binomial)
m237<-glmer(R~A+B+C + (1|X) + (1|Y), data=data, family=binomial)
m238<-glmer(R~A+B+D + (1|X) + (1|Y), data=data, family=binomial)
m239<-glmer(R~A+C+D + (1|X) + (1|Y), data=data, family=binomial)
m240<-glmer(R~B+C+D + (1|X) + (1|Y), data=data, family=binomial)
m241<-glmer(R~A+B+C+D + (1|X) + (1|Y), data=data, family=binomial)

m242<-glmer(R~A*B + (1|X) + (1|Y), data=data, family=binomial)
m243<-glmer(R~A*C + (1|X) + (1|Y), data=data, family=binomial)
m244<-glmer(R~A*D + (1|X) + (1|Y), data=data, family=binomial)
m245<-glmer(R~B*C + (1|X) + (1|Y), data=data, family=binomial)
m246<-glmer(R~B*D + (1|X) + (1|Y), data=data, family=binomial)
m247<-glmer(R~C*D + (1|X) + (1|Y), data=data, family=binomial)

m248<-glmer(R~A*B + C + (1|X) + (1|Y), data=data, family=binomial)
m249<-glmer(R~A*B + D + (1|X) + (1|Y), data=data, family=binomial)
m250<-glmer(R~A*C + B + (1|X) + (1|Y), data=data, family=binomial)
m251<-glmer(R~A*C + D + (1|X) + (1|Y), data=data, family=binomial)
m252<-glmer(R~A*D + B + (1|X) + (1|Y), data=data, family=binomial)
m253<-glmer(R~A*D + C + (1|X) + (1|Y), data=data, family=binomial)
m254<-glmer(R~B*C + A + (1|X) + (1|Y), data=data, family=binomial)
m255<-glmer(R~B*C + D + (1|X) + (1|Y), data=data, family=binomial)
m256<-glmer(R~B*D + A + (1|X) + (1|Y), data=data, family=binomial)
m257<-glmer(R~B*D + C + (1|X) + (1|Y), data=data, family=binomial)
m258<-glmer(R~C*D + A + (1|X) + (1|Y), data=data, family=binomial)
m259<-glmer(R~C*D + B + (1|X) + (1|Y), data=data, family=binomial)

m260<-glmer(R~A*B + C + D + (1|X) + (1|Y), data=data, family=binomial)
m261<-glmer(R~A*C + B + D + (1|X) + (1|Y), data=data, family=binomial)
m262<-glmer(R~A*D + B + C + (1|X) + (1|Y), data=data, family=binomial)
m263<-glmer(R~B*C + A + D + (1|X) + (1|Y), data=data, family=binomial)
m264<-glmer(R~B*D + A + C + (1|X) + (1|Y), data=data, family=binomial)
m265<-glmer(R~C*D + A + B + (1|X) + (1|Y), data=data, family=binomial)


m266<-glmer(R~A*B + A*C + (1|X) + (1|Y), data=data, family=binomial)
m267<-glmer(R~A*B + A*D + (1|X) + (1|Y), data=data, family=binomial)
m268<-glmer(R~A*B + B*C + (1|X) + (1|Y), data=data, family=binomial)
m269<-glmer(R~A*B + B*D + (1|X) + (1|Y), data=data, family=binomial)
m270<-glmer(R~A*B + C*D + (1|X) + (1|Y), data=data, family=binomial)
m271<-glmer(R~A*C + A*D + (1|X) + (1|Y), data=data, family=binomial)
m272<-glmer(R~A*C + B*C + (1|X) + (1|Y), data=data, family=binomial)
m273<-glmer(R~A*C + B*D + (1|X) + (1|Y), data=data, family=binomial)
m274<-glmer(R~A*C + C*D + (1|X) + (1|Y), data=data, family=binomial)
m275<-glmer(R~A*D + B*C + (1|X) + (1|Y), data=data, family=binomial)
m276<-glmer(R~A*D + B*D + (1|X) + (1|Y), data=data, family=binomial)
m277<-glmer(R~A*D + C*D + (1|X) + (1|Y), data=data, family=binomial)
m278<-glmer(R~B*C + B*D + (1|X) + (1|Y), data=data, family=binomial)
m279<-glmer(R~B*C + C*D + (1|X) + (1|Y), data=data, family=binomial)
m280<-glmer(R~B*D + C*D + (1|X) + (1|Y), data=data, family=binomial)

m281<-glmer(R~A*B + A*C + D + (1|X) + (1|Y), data=data, family=binomial)
m282<-glmer(R~A*B + A*D + C + (1|X) + (1|Y), data=data, family=binomial)
m283<-glmer(R~A*B + B*C + D + (1|X) + (1|Y), data=data, family=binomial)
m284<-glmer(R~A*B + B*D + C + (1|X) + (1|Y), data=data, family=binomial)
m285<-glmer(R~A*C + A*D + B + (1|X) + (1|Y), data=data, family=binomial)
m286<-glmer(R~A*C + B*C + D + (1|X) + (1|Y), data=data, family=binomial)
m287<-glmer(R~A*C + C*D + B + (1|X) + (1|Y), data=data, family=binomial)
m288<-glmer(R~A*D + B*D + C + (1|X) + (1|Y), data=data, family=binomial)
m289<-glmer(R~A*D + C*D + B + (1|X) + (1|Y), data=data, family=binomial)
m290<-glmer(R~B*C + B*D + A + (1|X) + (1|Y), data=data, family=binomial)
m291<-glmer(R~B*C + C*D + A + (1|X) + (1|Y), data=data, family=binomial)
m292<-glmer(R~B*D + C*D + A + (1|X) + (1|Y), data=data, family=binomial)

m293<-glmer(R~A*B + A*C + A*D + (1|X) + (1|Y), data=data, family=binomial)
m294<-glmer(R~A*B + A*C + B*C + (1|X) + (1|Y), data=data, family=binomial)
m295<-glmer(R~A*B + A*C + B*D + (1|X) + (1|Y), data=data, family=binomial)
m296<-glmer(R~A*B + A*C + C*D + (1|X) + (1|Y), data=data, family=binomial)
m297<-glmer(R~A*B + A*D + B*C + (1|X) + (1|Y), data=data, family=binomial)
m298<-glmer(R~A*B + A*D + B*D + (1|X) + (1|Y), data=data, family=binomial)
m299<-glmer(R~A*B + A*D + C*D + (1|X) + (1|Y), data=data, family=binomial)
m300<-glmer(R~A*B + B*C + B*D + (1|X) + (1|Y), data=data, family=binomial)
m301<-glmer(R~A*B + B*C + C*D + (1|X) + (1|Y), data=data, family=binomial)
m302<-glmer(R~A*B + B*D + C*D + (1|X) + (1|Y), data=data, family=binomial)
m303<-glmer(R~A*C + A*D + B*C + (1|X) + (1|Y), data=data, family=binomial)
m304<-glmer(R~A*C + A*D + B*D + (1|X) + (1|Y), data=data, family=binomial)
m305<-glmer(R~A*C + A*D + C*D + (1|X) + (1|Y), data=data, family=binomial)
m306<-glmer(R~A*C + B*C + B*D + (1|X) + (1|Y), data=data, family=binomial)
m307<-glmer(R~A*C + B*C + C*D + (1|X) + (1|Y), data=data, family=binomial)
m308<-glmer(R~A*C + B*D + C*D + (1|X) + (1|Y), data=data, family=binomial)
m309<-glmer(R~A*D + B*C + B*D + (1|X) + (1|Y), data=data, family=binomial)
m310<-glmer(R~A*D + B*C + C*D + (1|X) + (1|Y), data=data, family=binomial)
m311<-glmer(R~A*D + B*D + C*D + (1|X) + (1|Y), data=data, family=binomial)
m312<-glmer(R~B*C + B*D + C*D + (1|X) + (1|Y), data=data, family=binomial)


m313<-glmer(R~A*B + A*C + B*C + D + (1|X) + (1|Y), data=data, family=binomial)
m314<-glmer(R~A*B + A*D + B*D + C + (1|X) + (1|Y), data=data, family=binomial)
m315<-glmer(R~A*C + A*D + C*D + B + (1|X) + (1|Y), data=data, family=binomial)
m316<-glmer(R~B*C + B*D + C*D + A + (1|X) + (1|Y), data=data, family=binomial)


m317<-glmer(R~A*B + A*C + A*D + B*C + (1|X) + (1|Y), data=data, family=binomial)
m318<-glmer(R~A*B + A*C + A*D + B*D + (1|X) + (1|Y), data=data, family=binomial)
m319<-glmer(R~A*B + A*C + A*D + C*D + (1|X) + (1|Y), data=data, family=binomial)
m320<-glmer(R~A*B + A*C + B*C + B*D + (1|X) + (1|Y), data=data, family=binomial)
m321<-glmer(R~A*B + A*C + B*C + C*D + (1|X) + (1|Y), data=data, family=binomial)
m322<-glmer(R~A*B + A*C + B*D + C*D + (1|X) + (1|Y), data=data, family=binomial)
m323<-glmer(R~A*B + A*D + B*C + B*D + (1|X) + (1|Y), data=data, family=binomial)
m324<-glmer(R~A*B + A*D + B*C + C*D + (1|X) + (1|Y), data=data, family=binomial)
m325<-glmer(R~A*B + A*D + B*D + C*D + (1|X) + (1|Y), data=data, family=binomial)
m326<-glmer(R~A*B + B*C + B*D + C*D + (1|X) + (1|Y), data=data, family=binomial)
m327<-glmer(R~A*C + A*D + B*C + B*D + (1|X) + (1|Y), data=data, family=binomial)
m328<-glmer(R~A*C + A*D + B*C + C*D + (1|X) + (1|Y), data=data, family=binomial)
m329<-glmer(R~A*C + A*D + B*D + C*D + (1|X) + (1|Y), data=data, family=binomial)
m330<-glmer(R~A*C + B*C + B*D + C*D + (1|X) + (1|Y), data=data, family=binomial)
m331<-glmer(R~A*D + B*C + B*D + C*D + (1|X) + (1|Y), data=data, family=binomial)


m332<-glmer(R~A*B + A*C + A*D + B*C + B*D + (1|X) + (1|Y), data=data, family=binomial)
m333<-glmer(R~A*B + A*C + A*D + B*C + C*D + (1|X) + (1|Y), data=data, family=binomial)
m334<-glmer(R~A*B + A*C + A*D + B*D + C*D + (1|X) + (1|Y), data=data, family=binomial)
m335<-glmer(R~A*B + A*C + B*C + B*D + C*D + (1|X) + (1|Y), data=data, family=binomial)
m336<-glmer(R~A*B + A*D + B*C + B*D + C*D + (1|X) + (1|Y), data=data, family=binomial)
m337<-glmer(R~A*C + A*D + B*C + B*D + C*D + (1|X) + (1|Y), data=data, family=binomial)

m338<-glmer(R~A*B + A*C + A*D + B*C + B*D + C*D + (1|X) + (1|Y), data=data, family=binomial)


#identify top models using AIC
summary<-AIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, 
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
             m221, m222, m223, m224, m225, m226, m227, m228, m229, m230, m231, m232,
             m233, m234, m235, m236, m237, m238, m239, m240, m241, m242, m243, m244,
             m245, m246, m247, m248, m249, m250, m251, m252, m253, m254, m255, m256, 
             m257, m258, m259, m260, m261, m262, m263, m264, m265, m266, m267, m268,
             m269, m270, m271, m272, m273, m274, m275, m276, m277, m278, m279, m280,
             m281, m282, m283, m284, m285, m286, m287, m288, m289, m290, m291, m292,
             m293, m294, m295, m296, m297, m298, m299, m300, m301, m302, m303, m304,
             m305, m306, m307, m308, m309, m310, m311, m312, m313, m314, m315, m316,
             m317, m318, m319, m320, m321, m322, m323, m324, m325, m326, m327, m328,
             m329, m330, m331, m332, m333, m334, m335, m336, m337, m338, m0) 

sort(summary$AIC, index.return=TRUE)

P<-AICprobs(summary$AIC)
sort(P, index.return=TRUE, decreasing=TRUE) #remember, we want the largest one of these
