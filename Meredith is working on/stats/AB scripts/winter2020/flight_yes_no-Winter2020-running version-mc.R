#---
#title: "Binomial Modeling: flight_yes_no"
#author: "Anastasia Bernat"
#date: "3/30/2020"
#output: html_document
#---

# Winter 2020 Flight Trials 

rm(list=ls())
setwd("~/Documents/Florida soapberry project/2019 Dispersal/SBB-dispersal git/Meredith is working on/stats/AB scripts/winter2020/")

library(lme4)
library(lubridate)
library(dplyr)
library(glmnet)
library(chron)
library(ggplotify)
library(gridExtra)
library(ggformula)
library(tidyselect)


# Reading the data
source('cleandata-Winter2020-mc-baseR.R')
data_all<-read_data("data/complete_flight_data-Winter2020-edited.csv")

source('flight_yes_no-cat_data_function-mc.R')

model<-glmer(flew_b~host_c*sex_c + (1|ID) + (1|trial_type), data=data_all, family=binomial)
summary(model)
getME(model, "lower")

## All trials

# Experimental Set-Up Covariates:

####### No effect of chamber
summary(glm(flew_b~chamber, data=data_all, family=binomial))

####### Effect of test date (but no effect of test date when you split between T1 and T2); this could actually end up being confounded with host, since we had higher mortality in our GRT populations

summary(glm(flew_b~days_from_start_c, data=data_all, family=binomial))

####### No effect of test time
summary(glm(flew_b~min_from_IncStart_c, data=data_all, family=binomial))


# Biological Effects:

####### (Strong) Effect of mass
summary(glm(flew_b~mass_c, data=data_all, family=binomial))

####### Effect of number of eggs laid, which of course will only matter if you're female
summary(glm(flew_b~total_eggs, data=data_all, family=binomial))

####### Effect of whether eggs were laid or not, again which only matters if you're female
summary(glm(flew_b~eggs_b, data=data_all, family=binomial))


# Morphology Effects:

####### Effect of beak length - but don't run it without thorax!
summary(glm(flew_b~beak_c + thorax_c, data=data_all, family=binomial))

####### Effect of thorax length, again could be confounded with sex
summary(glm(flew_b~thorax_c, data=data_all, family=binomial))

####### Effect of body length, again could be confounded with sex
summary(glm(flew_b~body_c, data=data_all, family=binomial))

####### No effect of wing length, this is the weirdest one!!! But does actually pop out if you include thorax too??
summary(glm(flew_b~wing_c + thorax_c, data=data_all, family=binomial))


## T1: GLM Binomial Modeling | Mass as a covariate 


data_T1 <-data_all[data_all$trial_type=="T1",]

####### No effect of chamber
summary(glm(flew_b~chamber, data=data_T1, family=binomial))

####### No effect of test date
summary(glm(flew_b~days_from_start_c, data=data_T1, family=binomial))

####### No effect of test time
summary(glm(flew_b~min_from_IncStart, data=data_T1, family=binomial))


# Missing mass for some (3 NA)
missing_mass <- subset(data_T1, is.na(data_T1$mass))
data_T1 <- data_T1 %>%
  filter(!is.na(mass))

# Recentering
data_T1$lat_c<-data_T1$latitude-mean(data_T1$latitude)
data_T1$sym_dist<-abs(data_T1$latitude-25.49197)
data_T1$mass_c <- data_T1$mass-mean(data_T1$mass, na.rm=TRUE) 

R = data_T1$flew_b
A = data_T1$host_c
B = data_T1$sex_c
C = data_T1$sym_dist
##I would not compete these against each other - because mass is determined in part by A, B, and C, I would not add it until after we've tested the other three.
D = data_T1$mass_c # as a covariate 

data<-data.frame(R, A, B, C, D)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-binomial glm 3-FF.R")
AICs <- sort(summary$AIC)
models_init <- sort(P, decreasing=TRUE, index.return=TRUE)
top <- length(models_init$x[which(models_init$x>0.05)])

AICs <- round(AICs[1:top], digits=2)
models <- sapply(models_init$ix[1:top], FUN=function(x) sprintf("m%d", x)) 
probs <- round(models_init$x[1:top], digits=2)
rbind(AICs, models, probs) # top models and their AIC values & probabilities


m8 <- glm(formula = R ~ A * B, family = binomial, data = data)
m11 <- glm(formula = R ~ A * B + C, family = binomial, data = data)
m15 <- glm(formula = R ~ A * B + B * C, family = binomial, data = data)
m2 <- glm(formula = R ~ B, family = binomial, data = data)
m14 <- glm(formula = R ~ A * B + A * C, family = binomial, data = data)

####MLC: discuss the limitations of anova comparisons; can only compare directly cases where you are only changing one thing (ie, adding a single factor or a single interaction). The order doesn't matter and will always tell you whether the thing you ADDED improved fit, not the thing you removed.
####So, we could compare R~A to R~A+B, and R~A+B to R~A*B, but not R~A to R~A*B. The first comparison would tell us if adding B helped, the second if adding the interaction helped. So, among our top models, we actually have nothing to compare m2 to (since m4, R~A+B, didn't make the cut)

anova(m8, m11, test="Chisq") ## Adding C does not improve fit
anova(m11, m15, test="Chisq") ## Adding B*C interaction does not improve fit
anova(m11, m14, test="Chisq") ## Adding A*C interaciton does not improve fit

#Comparing models: Because most changes in deviance are positive, meaning that Model 2 had a higher deviance value, this suggests that the Model 1 fits better Model 2.

model_T1<-glm(flew_b~host_c*sex_c, family=binomial, data=data_T1)
summary(model_T1)

###Before adding mass, we can see that:
#Being female strongly reduces your chances of flying
#Being from K. elegans reduces the negative effects of being female on flight
#No direct effect of host plant


model_T1_cov<-glm(flew_b~host_c*sex_c + mass_c, family=binomial, data=data_T1)
summary(model_T1_cov)

#* no longer a strong negative effect of being far from the sympatric zone; 
#* strong negative effect of mass that subsumes the sex effect
#* no direct effect of host plant
#* host*sex interaction shows a strong positive effect, such that if from GRT and are female then more likely to be dispersive
#* Marginal negative effect of being female


# TRIAL 1 WITH MASS AS A COVARIATE

#run AICprobs script
source("AICprobabilities.R")
source("generic models-binomial glm 4-FF.R")
AICs <- sort(summary$AIC)
models_init <- sort(P, decreasing=TRUE, index.return=TRUE)
top <- length(models_init$x[which(models_init$x>0.05)])

AICs <- round(AICs[1:top], digits=2)
models <- sapply(models_init$ix[1:top], FUN=function(x) sprintf("m%d", x)) 
probs <- round(models_init$x[1:top], digits=2)
rbind(AICs, models, probs) # top models and their AIC values & probabilities

m50 <- glm(formula = R ~ A * D + B * D, family = binomial, data = data)
m26 <- glm(formula = R ~ A * D + B, family = binomial, data = data)
m43 <- glm(formula = R ~ A * B + B * D, family = binomial, data = data)
m72 <- glm(formula = R ~ A * B + A * D + B * D, family = binomial, data = data)
m23 <- glm(formula = R ~ A * B + D, family = binomial, data = data)


anova(m43, m23, test="Chisq") #No improvement from adding B*D
anova(m50, m26, test="Chisq") #No improvement from adding B*D
anova(m72, m50, test="Chisq") #No improvement from A*B
anova(m72, m43, test="Chisq") #No improvement from A*D

###What this basically looks like is if you have sex*host, you don't need mass*host, and vice-versa, but you need one.

model_T1_m<-glm(flew_b~host_c*mass_c + sex_c, family=binomial, data=data_T1)
summary(model_T1_m)





########Plots being moved to separate scripts


## T2: GLM Binomial Modeling | Mass as a covariate 
source("clean_flight_data.R")
data_all <- read_flight_data("complete_flight_data-Winter2020.csv")

data_T2 <-data_all[data_all$trial_type=="T2",]

####### No effect of chamber
summary(glm(flew_b~chamber, data=data_T2, family=binomial))

####### No effect of test date
summary(glm(flew_b~days_from_start_c, data=data_T2, family=binomial))

####### No effect of test time
summary(glm(flew_b~min_from_IncStart, data=data_T2, family=binomial))

#Recentering
data_T2$lat_c<-data_T2$latitude-mean(data_T2$latitude)
data_T2$mass_c <- data_T2$mass-mean(data_T2$mass, na.rm=TRUE) 

R = data_T2$flew_b
A = data_T2$host_c
B = data_T2$sex_c
C = data_T2$sym_dist
D = data_T2$mass_c # as a covariate 

data<-data.frame(R, A, B, C, D)
#data<-data.frame(R, A, B, C)

# Automatic stepwise functions using AIC to decide between models
model.null <- glm(R~1, family=binomial, data = data_T2)
model.large <-glm(R~A*B*C*D, family=binomial, data=data_T2)


#run AICprobs script
source("AICprobabilities.R")
source("generic models-binomial glm 3-FF.R")
AICs <- sort(summary$AIC)
models_init <- sort(P, decreasing=TRUE, index.return=TRUE)
top <- length(models_init$x[which(models_init$x>0.05)])

AICs <- AICs[1:top]
models <- round(models_init$ix[1:top])
probs <- models_init$x[1:top]
rbind(AICs, models, probs)


m2 <- glm(formula = R ~ B, family = binomial, data = data)
m4 <- glm(formula = R ~ A + B, family = binomial, data = data)
m6 <- glm(formula = R ~ B + C, family = binomial, data = data)
m7 <- glm(formula = R ~ A + B + C, family = binomial, data = data)
m8 <- glm(formula = R ~ A * B, family = binomial, data = data)
m10 <- glm(formula = R ~ B * C, family = binomial, data = data)


anova(m2, m4, test="Chisq") # Adding A does not improve fit
anova(m2, m6, test="Chisq") # Adding C does not improve fit
anova(m4, m7, test="Chisq") # Adding C does not improve fit
anova(m6, m7, test="Chisq") # Adding A does not improve fit
anova(m4, m8, test="Chisq") # Adding a A*B interaction term does not improve fit
anova(m6, m10, test="Chisq") # Adding a B*C interaction term does not improve fit

model_T2 <-glm(flew_b~sex_c, family=binomial, data=data_T2)
summary(model_T2)


# sex is the only significant and strong effect, so that if you are female you are much less likely to disperse/fly

# TRIAL 2 WITHOUT MASS AS COVARIATE
#run AICprobs script
source("AICprobabilities.R")
source("generic models-binomial glm 4-FF.R")
AICs <- sort(summary$AIC)
models_init <- sort(P, decreasing=TRUE, index.return=TRUE)
top <- length(models_init$x[which(models_init$x>0.05)])

AICs <- AICs[1:top]
models <- models_init$ix[1:top]
probs <- models_init$x[1:top]
rbind(AICs, models, probs)

m7 <- glm(formula = R ~ A + D, family = binomial, data = data)
m4 <- glm(formula = R ~ D, family = binomial, data = data)
m9 <- glm(formula = R ~ B + D, family = binomial, data = data)
m12 <- glm(formula = R ~ A + B + D, family = binomial, data = data)

anova(m7, m4, test="Chisq") # Adding A doesn't improve fit
anova(m7, m9, test="Chisq") # REplacing A with B does improve fit (neg Dev)
anova(m4, m9, test="Chisq") # Adding B does not improve fit
anova(m7, m12, test="Chisq") # Adding B does not improve fit


model_T2_mass <-glm(flew_b~mass_c, family=binomial, data=data_T2)
summary(model_T2_mass)

#* mass is the only significant and extremely strong effect, so that if you are heavy you are much less likely to disperse/fly





# Spliting by Sex (due to strong effect of mass)

data_all <- read_flight_data("complete_flight_data-Winter2020.csv")
data_fem <- data_all[data_all$sex=="F",]

## Female: GLM Binomial Modeling 


####### Marginal effect of chamber B-4 (ah this again!)
summary(glm(flew_b~chamber, data=data_fem, family=binomial))

####### No effect of test date
summary(glm(flew_b~days_from_start, data=data_fem, family=binomial))

####### No effect of test time
summary(glm(flew_b~min_from_IncStart, data=data_fem, family=binomial))

# Recentering
data_fem$lat_c<-data_fem$latitude-mean(data_fem$latitude)
data_fem$sym_dist<-abs(data_fem$latitude-25.49197)
data_fem$mass_c <- data_fem$mass-mean(data_fem$mass, na.rm=TRUE) 

R = data_fem$flew_b
A = data_fem$host_c
B = data_fem$sym_dist

data<-data.frame(R, A, B)
head(data) 

# Automatic stepwise functions using AIC to decide between models
model.null <- glm(R~1, family=binomial, data = data_fem)
model.large <-glm(R~A*B, family=binomial, data = data_fem)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-binomial glm 2-FF.R")
AICs <- sort(summary$AIC)
models_init <- sort(P, decreasing=TRUE, index.return=TRUE)
top <- length(models_init$x[which(models_init$x>0.05)])

AICs <- AICs[1:top]
models <- models_init$ix[1:top]
probs <- models_init$x[1:top]
rbind(AICs, models, probs)

m0 <- glm(formula = R ~ 1, family = binomial, data = data) 
m1 <- glm(formula = R ~ A, family = binomial, data = data) #*host*
m2 <- glm(formula = R ~ B, family = binomial, data = data) #*sym_dist*
m3 <- glm(formula = R ~ A + B, family = binomial, data = data) #*host and sym_dist*

anova(m0, m1, test="Chisq") #The null model is the best.
anova(m1, m3, test="Chisq") # Replacing A with B improves fit 
anova(m2, m3, test="Chisq") # Adding B does not improve fit

model_fem <-glm(flew_b~sym_dist, family=binomial, data=data_fem)
summary(model_fem) 


#* but effect of sym_dist not significant...
##MLC: This is because the best model is the null.


## Consider covariates
model_fem_final <-glmer(flew_b~sym_dist + (1|population) + (1|trial_type), family=binomial, data=data_fem) # boundary (singular) fit: see ?isSingular
summary(model_fem_final)
## Barely changed effect estimates but does not improve fit



# FEM DATA: MASS AS COVARIATE 
setwd("~/Documents/Florida soapberry project/2019 Dispersal/SBB-dispersal git/Meredith is working on/stats/AB scripts/winter2020/")
source('cleandata-Winter2020-mc-baseR.R')
data_all<-read_data("data/complete_flight_data-Winter2020-edited.csv")
data_fem <- data_all[data_all$sex=="F" & !is.na(data_all$mass),]
#drop missing masses###


# Recentering
data_fem$lat_c<-data_fem$latitude-mean(data_fem$latitude)
data_fem$sym_dist<-abs(data_fem$latitude-25.49197)
data_fem$mass_c <- data_fem$mass-mean(data_fem$mass, na.rm=TRUE) 

R = data_fem$flew_b
A = data_fem$host_c
B = data_fem$sym_dist
C = data_fem$mass_c
####MLC: Should also add chamber as a random covariate

data<-data.frame(R, A, B, C)
head(data)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-binomial glm 3-FF.R")
AICs <- sort(summary$AIC)
models_init <- sort(P, decreasing=TRUE, index.return=TRUE)
top <- length(models_init$x[which(models_init$x>0.05)])

AICs <- AICs[1:top]
models <- models_init$ix[1:top]
probs <- models_init$x[1:top]
rbind(AICs, models, probs)

m3 <- glm(formula = R ~ C, family = binomial, data = data)
m6 <- glm(formula = R ~ B + C, family = binomial, data = data)
m7 <- glm(formula = R ~ A + B + C, family = binomial, data = data)
m9 <- glm(formula = R ~ A * C, family = binomial, data = data)
m5 <- glm(formula = R ~ A + C, family = binomial, data = data)
m12 <- glm(formula = R ~ A * C + B, family = binomial, data = data)
m10 <- glm(formula = R ~ B * C, family = binomial, data = data)

anova(m3, m6, test="Chisq") # Adding B does not improve fit
anova(m3, m5, test="Chisq") # Adding A does not improve fit
anova(m6, m7, test="Chisq") # Adding A does not improve fit
anova(m5, m7, test="Chisq") # Adding B does not improve fit
anova(m5, m9, test="Chisq") # Adding A*C interaction does not improve fit
anova(m9, m12, test="Chisq") # Adding B does not improve fit


model_fem_mass <-glm(flew_b~mass_c, family=binomial, data=data_fem)
summary(model_fem_mass) 




## MALE: GLM Binomial Modeling 
setwd("~/Documents/Florida soapberry project/2019 Dispersal/SBB-dispersal git/Meredith is working on/stats/AB scripts/winter2020/")
source('cleandata-Winter2020-mc-baseR.R')
data_all<-read_data("data/complete_flight_data-Winter2020-edited.csv")
data_male <- data_all[data_all$sex=="M" & !is.na(data_all$mass),]


####### No effect of chamber B-4 
summary(glm(flew_b~chamber, data=data_male, family=binomial))

####### Effect of test date (as seen in the data_all)
summary(glmer(flew_b~days_from_start_c + (1|trial_type), data=data_male, family=binomial))

####### No effect of test time (but close p = 0.09443)
summary(glm(flew_b~min_from_IncStart, data=data_male, family=binomial))

# seems like males are very time sensitive

R = data_male$flew_b
A = data_male$host_c
B = data_male$sym_dist

data<-data.frame(R, A, B)
head(data)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-binomial glm 2-FF.R")
AICs <- sort(summary$AIC)
models_init <- sort(P, decreasing=TRUE, index.return=TRUE)
top <- length(models_init$x[which(models_init$x>0.05)])

AICs <- AICs[1:top]
models <- models_init$ix[1:top]
probs <- models_init$x[1:top]
rbind(AICs, models, probs)

m1 <- glm(formula = R ~ A, family = binomial, data = data) #*host*
m3 <- glm(formula = R ~ A + B, family = binomial, data = data) #*host and sym_dist*
m0 <- glm(R~1, family=binomial, data=data) #null
m4 <- glm(formula = R ~ A * B, family = binomial, data = data) #*host and sym_dist interaction - most complex model*


anova(m1, m3, test="Chisq") # Adding B does not improve fit 
anova(m3, m4, test="Chisq") # Adding A*B does not improve fit
anova(m0, m1, test="Chisq") # Adding A DOES improve fit


model_male<-glm(flew_b~host_c, family=binomial, data=data_male)
summary(model_male)


#* Strong negative effect if from GRT


## Consider covariates
model_male_final <-glmer(flew_b~host_c + (1|population) + (1|trial_type), family=binomial, data=data_male) # no error
summary(model_male_final)

## Changed the effect slightly and improved the model and in turn made host notsignificant...





# MALE DATA WITH MASS AS COVARIATE

# Recentering
data_male$lat_c<-data_male$latitude-mean(data_male$latitude)
data_male$sym_dist<-abs(data_male$latitude-25.49197)
data_male$mass_c <- data_male$mass-mean(data_male$mass, na.rm=TRUE) 

R = data_male$flew_b
A = data_male$host_c
B = data_male$sym_dist
C = data_male$mass_c # as a covariate 
##I would not compete these against each other - because mass is determined in part by A, B, and C, I would not add it until after we've tested the other three.

data<-data.frame(R, A, B, C)


#run AICprobs script
source("AICprobabilities.R")
source("generic models-binomial glm 3-FF.R")
AICs <- sort(summary$AIC)
models_init <- sort(P, decreasing=TRUE, index.return=TRUE)
top <- length(models_init$x[which(models_init$x>0.05)])

AICs <- AICs[1:top]
models <- models_init$ix[1:top]
probs <- models_init$x[1:top]
rbind(AICs, models, probs)

m5 <- glm(formula = R ~ A + C, family = binomial, data = data) *host + mass*
m7 <- glm(formula = R ~ A + B + C, family = binomial, data = data)
m13 <- glm(formula = R ~ B * C + A, family = binomial, data = data)
m9 <- glm(formula = R ~ A * C, family = binomial, data = data)
m16 <- glm(formula = R ~ A * C + B * C, family = binomial, data = data)
m11 <- glm(formula = R ~ A * B + C, family = binomial, data = data)
m12 <- glm(formula = R ~ A * C + B, family = binomial, data = data)
m15 <- glm(formula = R ~ A * B + B * C, family = binomial, data = data)

anova(m5, m7, test="Chisq") # Adding B does not improve fit 
anova(m7, m13, test="Chisq") # Adding B*C does not improve fit
anova(m5, m9, test="Chisq") # Adding A*C does not improve fit
anova(m13, m16, test="Chisq") # Adding A*C does not improve fit
anova(m7, m11, test="Chisq") # Adding A*B does not improve fit
anova(m7, m12, test="Chisq") # Adding A*C does not improve fit
anova(m11, m15, test="Chisq") # Adding B*C does not irmpve fit


model_male_mass <-glm(flew_b~host_c + mass_c, family=binomial, data=data_male)
summary(model_male_mass)


#* Strong negative effect if from GRT
#* Strong negative effect of mass, that if weigh more then less likely to disperse




######################Come back in here

# Biological Effects:

####### MASS 
summary(glm(flew_b~mass_c, data=data_fem, family=binomial)) # F | (Strong) Effect of mass 
summary(glm(flew_b~mass_c, data=data_male, family=binomial)) # M | (Strong) Effect of mass 

####### EGGS
summary(glm(flew_b~total_eggs_c, data=data_fem, family=binomial)) # F | Effect of number of eggs laid
summary(glm(flew_b~eggs_b, data=data_fem, family=binomial))  #F | Effect of whether eggs were laid or not
```

```{r}
# Morphology Effects:

####### BEAK LENGTH
summary(glm(flew_b~beak_c, data=data_fem, family=binomial)) # F | Effect of beak length
summary(glm(flew_b~beak_c, data=data_male, family=binomial)) # M | Effect of beak length

####### THORAX LENGTH
summary(glm(flew_b~thorax_c, data=data_fem, family=binomial)) # F | No effect of thorax length
summary(glm(flew_b~thorax_c, data=data_male, family=binomial)) #  M | No effect of thorax length

####### BODY LENGTH
summary(glm(flew_b~body_c, data=data_fem, family=binomial)) # F |Effect of body length
summary(glm(flew_b~body_c, data=data_male, family=binomial)) # N |No effect of body length

####### WING LENGTH
summary(glm(flew_b~wing_c, data=data_fem, family=binomial)) # F | Effect of wing length
summary(glm(flew_b~wing_c, data=data_male, family=binomial)) # M | Effect of wing length

####### No effect of wing morph (check how annotated the wing morph) - don't include it
#summary(glm(flew_b~w_morph_c, data=data_all, family=binomial)) # but close p val = 0.0512

```

# Female Data | Remodeling with new covariates 

```{r}
missing_mass <- subset(data_fem, is.na(data_fem$mass))
data_fem <- setdiff(data_fem, missing_mass)

R = data_fem$flew_b
A = data_fem$host_c
B = data_fem$sym_dist
C = data_fem$mass_c
D = data_fem$eggs_b
  
data<-data.frame(R, A, B, C, D)
head(data) #kable()

# Automatic stepwise functions using AIC to decide between models
model.null <- glm(R~1, family=binomial, data = data)
model.large <-glm(R~A*B*C*D, family=binomial, data = data)

# Backwards
#step(model.large, direction = "backward")
```

For backwards:
best_fit <- glm(formula = R ~ A + B + C + D + A:C + B:C + B:D, family = binomial, 
    data = data_male)
AIC: 222.7

```{r}
## Forwards
#step(model.null, scope = ~ (A*B*C*D), direction = "forward")
```

For forwards:

best_fit <- glm(formula = R ~ D, family = binomial, data = data)
AIC: 222.4

```{r}
#run AICprobs script
source("AICprobabilities.R")
source("generic models-binomial glm 4-FF.R")
AICs <- sort(summary$AIC)
models <- sort(P, decreasing=TRUE, index.return=TRUE)

AICs <- AICs[1:4]
models <- models$ix[1:4]
rbind(AICs, models) # top models and their AIC values
```

m4 <-  glm(formula = R ~ D, family = binomial, data = data) *eggs_b*
m20 <- glm(formula = R ~ B * D, family = binomial, data = data) *sym_dist x eggs_b*
m9 <- glm(formula = R ~ B + D, family = binomial, data = data) *sym_dist and eggs_b*
m10 <-  glm(formula = R ~ C + D, family = binomial, data = data) *mass and eggs_b*

```{r}
anova(m4, m20, test="Chisq") # Replacing B with B*D does not improve fit
anova(m4, m9, test="Chisq") # Adding B does not improve fit
anova(m4, m10, test="Chisq") # Adding C does not improve fit
```

```{r}
fem_model<-glm(flew_b~eggs_b, family=binomial, data=data_fem)
summary(fem_model)
```

* Strong negative effect if laid eggs that day

```{r}
ffem_model<-glmer(flew_b~eggs_b + (1|population) + (1|trial_type), family=binomial, data=data_male) # no error
summary(ffem_model) # still a strong negative effect if laid eggs that day
```

#Host seems to not be effecing female flight...let's remove it

```{r}
missing_mass <- subset(data_fem, is.na(data_fem$mass))
data_fem <- setdiff(data_fem, missing_mass)

R = data_fem$flew_b
A = data_fem$total_eggs_c
B = data_fem$sym_dist
C = data_fem$mass_c
D = data_fem$eggs_b
  
data<-data.frame(R, A, B, C, D)
head(data) #kable()

# Automatic stepwise functions using AIC to decide between models
model.null <- glm(R~1, family=binomial, data = data)
model.large <-glm(R~A*B*C*D, family=binomial, data = data)

# Backwards
#step(model.large, direction = "backward")
```

```{r}
## Forwards
#step(model.null, scope = ~ (A*B*C*D), direction = "forward")
```

```{r}
#run AICprobs script
source("AICprobabilities.R")
source("generic models-binomial glm 4-FF.R")
AICs <- sort(summary$AIC)
models <- sort(P, decreasing=TRUE, index.return=TRUE)

AICs <- AICs[1:4]
models <- models$ix[1:4]
rbind(AICs, models) # top models and their AIC values
```

m43 <-  glm(formula = R ~ A * B + B * D, family = binomial, data = data) *total_eggs_c, sym_dist, eggs_b*
m72 <- glm(formula = R ~ A * B + A * D + B * D, family = binomial, data = data) *total_eggs_c, sym_dist, eggs_b*
m50 <- glm(formula = R ~ A * D + B * D, family = binomial, data = data) *total_eggs_c, sym_dist, eggs_b*
m18 <-   glm(formula = R ~ A * D, family = binomial, data = data) *total_eggs_c and eggs_b*

```{r}
anova(m43, m72, test="Chisq") # Replacing B*D with A*D does not improve fit
anova(m43, m50, test="Chisq") # Replacing A*B with A*D does improve fit
anova(m50, m18, test="Chisq") # Removing B*D does improve fit
```

```{r}
fem_model<-glm(flew_b~total_eggs_c*eggs_b, family=binomial, data=data_fem) # strong negative effect from eggs_b, no other effects (although these two interactions are basically the same thing, not sure which to use)
summary(fem_model)
```

```{r}
ffem_model<-glmer(flew_b~eggs_b*total_eggs_c + (1|population) + (1|trial_type), family=binomial, data=data_fem) # no error
summary(ffem_model) 
```


```{r fig.width=7, fig.height=2.5}
pf1 <- as.grob(expression(
  plot(data_fem$flew_b, data_fem$total_eggs)))
# seems like if you generally laid less eggs you flew, but a lot of females laid few and also didn't fly
pf2 <- as.grob(expression(
  plot(data_fem$mass, data_fem$total_eggs)))
pf3 <- as.grob(expression(
  plot(data_fem$sym_dist,data_fem$total_eggs)))
grid.arrange(pf1,pf2,pf3, ncol=3)
```

# Male Data | Remodeling with new covariates 


```{r}
# recap of covariates with significant effects
summary(glm(flew_b~days_from_start_c, data=data_male, family=binomial))
summary(glm(flew_b~mass_c, data=data_male, family=binomial)) 
```

```{r}
missing_mass <- subset(data_male, is.na(data_male$mass))
data_male <- setdiff(data_male, missing_mass)

R = data_male$flew_b
A = data_male$host_c
B = data_male$sym_dist
C = data_male$mass_c # would I need to recenter based on data split?
D = data_male$days_from_start_c

data<-data.frame(R, A, B, C, D)
head(data) #kable()

# Automatic stepwise functions using AIC to decide between models
model.null <- glm(R~1, family=binomial, data = data_male)
model.large <-glm(R~A*B*C*D, family=binomial, data = data_male)

# Backwards
#step(model.large, direction = "backward")
```

```{r}
## Forwards
#step(model.null, scope = ~ (A*B), direction = "forward")
```

```{r}
#run AICprobs script
source("AICprobabilities.R")
source("generic models-binomial glm 4-FF.R")
AICs <- sort(summary$AIC)
models <- sort(P, decreasing=TRUE, index.return=TRUE)

AICs <- AICs[1:4]
models <- models$ix[1:4]
rbind(AICs, models) # top models and their AIC values
```

m13 <-  glm(formula = R ~ A + C + D, family = binomial, data = data) *host, mass, days_from_start*
m27 <- glm(formula = R ~ A * D + C, family = binomial, data = data)*host, sym_dist, days_from_start, mass*
m15 <- glm(formula = R ~ A + B + C + D, family = binomial, data = data) *host, sym_dist, days_from_start, mass*

```{r}
anova(m13, m27, test="Chisq") # Adding A*D does not improve fit
anova(m13, m15, test="Chisq") # Adding B effect does not improve fit
```

```{r}
male_model1<-glm(flew_b~host_c + mass_c + days_from_start_c, family=binomial, data=data_male) 
male_model2<-glmer(flew_b~host_c + mass_c + days_from_start_c + (1|population) + (1|trial_type), family=binomial,
                   data=data_male) 
summary(male_model1) # strong negative effect from mass, strong negative effect of host_c, strong negative effect of days_from_start_c
summary(male_model2) # strong negative effect from mass and days_from_start but now marginal effect from host

# less likely to fly if you are from GRT, have more mass, and fly later (older bug..?)
```

```{r}
plot(data_male$mass, data_male$flew_b) # only a few large ones...really driving this but need to go back and check in the morphology if it's a typo or a a big male, but beside that looks pretty 50-50
```


