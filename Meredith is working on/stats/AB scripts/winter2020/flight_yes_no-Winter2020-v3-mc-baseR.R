#---
#title: "Binomial Modeling: flight_yes_no"
#author: "Anastasia Bernat"
#date: "3/30/2020"
#output: html_document
#---

# Winter 2020 Flight Trials 

#```{r setup, include=FALSE}
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

#knitr::opts_chunk$set(echo = TRUE)
#```

# Reading the data
source('cleandata-Winter2020-mc-baseR.R')
data_all<-read_data("data/complete_flight_data-Winter2020-edited.csv")

source('flight_yes_no-cat_data_function-mc.R')

model<-glmer(flew_b~host_c*sex_c + (1|ID) + (1|trial_type), data=data_all, family=binomial)
summary(model)
getME(model, "lower")
#```

## All trials

#```{r}
# Experimental Set-Up Covariates:

####### No effect of chamber
summary(glm(flew_b~chamber, data=data_all, family=binomial))

####### Effect of test date (but no neffect of test date when you split between T1 and T2); this could actually end up being confounded with host, since we had higher mortality in our GRT populations

summary(glm(flew_b~days_from_start_c, data=data_all, family=binomial))

####### No effect of test time
summary(glm(flew_b~min_from_IncStart_c, data=data_all, family=binomial))

#```

#```{r}
# Biological Effects:

####### (Strong) Effect of mass
summary(glm(flew_b~mass_c, data=data_all, family=binomial))

####### Effect of number of eggs laid, which of course will only matter if you're female
summary(glm(flew_b~total_eggs, data=data_all, family=binomial))

####### Effect of whether eggs were laid or not, again which only matters if you're female
summary(glm(flew_b~eggs_b, data=data_all, family=binomial))
#```

#```{r}
# Morphology Effects:

####### Effect of beak length - but don't run it without thorax!
summary(glm(flew_b~beak_c + thorax_c, data=data_all, family=binomial))

####### Effect of thorax length, again could be confounded with sex
summary(glm(flew_b~thorax_c, data=data_all, family=binomial))

####### Effect of body length, again could be confounded with sex
summary(glm(flew_b~body_c, data=data_all, family=binomial))

####### No effect of wing length, this is the weirdest one!!! But does actually pop out if you include thorax too??
summary(glm(flew_b~wing_c + thorax_c, data=data_all, family=binomial))

####### No effect of wing morph (check how annotated the wing morph) - don't include it
#summary(glm(flew_b~w_morph_c, data=data_all, family=binomial)) # but close p val = 0.0512

#```

## T1: GLM Binomial Modeling | Mass as a covariate 

#```{r}
data_T1 <-data_all[data_all$trial_type=="T1",]

####### No effect of chamber
summary(glm(flew_b~chamber, data=data_T1, family=binomial))

####### No effect of test date
summary(glm(flew_b~days_from_start_c, data=data_T1, family=binomial))

####### No effect of test time
summary(glm(flew_b~min_from_IncStart, data=data_T1, family=binomial))
#```

#```{r}
# Missing mass for some (3 NA)
missing_mass <- subset(data_T1, is.na(data_T1$mass))
missing_mass
# 339, 48, and 342 have no mass and were tested on the same date
data_T1 <- setdiff(data_T1, missing_mass)

R = data_T1$flew_b
A = data_T1$host_c
B = data_T1$sex_c
C = data_T1$sym_dist
##I would not compete these against each other - because mass is determined in part by A, B, and C, I would not add it until after we've tested the other three.
D = data_T1$mass_c # as a covariate 

data<-data.frame(R, A, B, C, D)

# Automatic stepwise functions using AIC to decide between models
model.null <- glm(R~1, family=binomial, data=data)
model.large <-glm(R~A*B*C*D, family=binomial, data=data)

# Backwards
#m_backwards <- step(model.large, direction = "backward")
#```
#For backwards selection:

#- Minimum AIC value from the backwards selection function:  m_backwards <- glm(formula = R ~ A + B + C + D + A:D + C:D, family = binomial, data = data), AIC: 400.48

#```{r}
# Forwards
#m_forwards <- step(model.null, scope = ~ A*B*C*D, direction = "forward")
#```

#For forwards selection:

#- Minimum AIC value from the forwards selection function:  m_forwards <- glm(formula = R ~ D + B, family = binomial, data = data_T1), AIC: 404.83

#```{r}
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
rbind(AICs, models, probs) # top models and their AIC values & probabilities
#```

m26 <- glm(formula = R ~ A * D + B, family = binomial, data = data)
m63 <- glm(formula = R ~ A * D + C * D + B, family = binomial, data = data)
m23 <- glm(formula = R ~ A * B + D, family = binomial, data = data)
m44 <- glm(formula = R ~ A * B + C * D, family = binomial, data = data)

#```{r}
anova(m26, m63, test="Chisq") ## Adding the C*D interaction does not improve fit
anova(m26, m23, test="Chisq") ## Replacing the A*D + B with A*B + D does improve fit (neg Dev)
anova(m23, m63, test="Chisq") ## More complex model does not improve fit
anova(m23, m44, test="Chisq") ## Replacing D with C*D does not improve fit
#```

#Comparing models: Because most changes in deviance are positive, meaning that Model 2 had a higher deviance value, this suggests that the Model 1 fits better Model 2.

#```{r}
model_T1<-glm(flew_b~host_c*sex_c + mass_c, family=binomial, data=data_T1)
summary(model_T1)
#```

#* no longer a strong negative effect of being far from the sympatric zone; 
#* strong negative effect of mass and has a large coefficient;
#* no effect of host plant
#* host*sex interaction shows a strong positive effect, such that if from GRT and are female then more likely to be dispersive
#* no effect of sex



########
summary <-aggregate(flew_b~sex*host_plant*sym_dist, data=data_all, FUN=mean)
summary

par(mar=c(6.5, 5.5, 5.5, 9.5), xpd=TRUE) # Add extra space to right of plot area; change clipping to figure
plot(summary$flew_b~summary$sym_dist, 
     col=c(1,2)[as.factor(summary$sex)], # Female = Black, Male = Red
     pch=c(19,22)[as.factor(summary$host_plant)],
     main="Observed Data",
     xlab = "Distance from Sympatric Zone (°)",
     ylab= "Proportion Flew", # K. elegans = Squares C.corindum = circles
     #sub=eq_glmer
     ) 
legend("topright",
       legend = c("C.corindum and F","K.elegans and M"),
       inset=c(-0.23,0.2),
       col=c(1,2),
       pch = c(19,22),
       title="Groups")

#```

#```{r}
# ...would this graph make sense?
###Because sym_dist didn't end up having an effect, it would make more sense to plot based on sex, host, and mass; maybe swap out sym_dist for mass here?
##### Observed proportions of yes flew by mass #############

# Missing mass for some (3 NA)
missing_mass <- subset(data_all, is.na(data_all$mass))
missing_mass
# 339, 48, and 342 have no mass and were tested on the same date
data_all <- setdiff(data_all, missing_mass)

# group x values into categories and calculate the sample of proportion of flight for each category
df <- select(data_all, mass, flew_b, sex_c, host_c)
df <- df[order(df$mass),]

mass_interval <- 0.010
bins <- round(max(df$mass) / mass_interval)

iterations = bins
variables = + length(df) + 1
matrix <- matrix(ncol=variables, nrow=iterations)

i <- 0.015
f <- 0.025
for (b in 1:bins) {
  matrix[b,1] <- f
  binned_df <- filter(df, mass < f & mass > i)

  flew_n <- binned_df$flew_b
  successes <- sum(flew_n)
  n_cases <- nrow(binned_df)
  sample_prop <- successes / n_cases
  if (is.na(sample_prop)) {
    next
  }
  #sex_group_mean <- mean(binned_df$sex_c)
  #host_group_mean <- mean(binned_df$host_c)
  
  sex_group_mean <- mean(binned_df$sex_c)
  host_group_mean <- mean(binned_df$host_c)
  print(host_group_mean)
  
  if (sex_group_mean < 0) {
    sex_group_mean <- floor(sex_group_mean)
    sex <- "M"
  }
  if (sex_group_mean > 0) {
    sex_group_mean <- ceiling(sex_group_mean)
    sex <- "F"
  }
  if (host_group_mean < 0) {
    host <- "K.elegans"
    host_group_mean <- floor(host_group_mean)

  }
  if (host_group_mean > 0) {
    host <- "C.corindum"
    host_group_mean <- ceiling(host_group_mean)
  }

  matrix[b,2] <- n_cases
  matrix[b,3] <- round(sample_prop,3)
  matrix[b,4] <- sex_group_mean
  matrix[b,5] <- host_group_mean
  
  cat("Cases:", n_cases, end="\t")
  cat("Sample Proportion:", sample_prop, end="\n")
  
  i <- i + 0.010
  f <- f + 0.010
}

categorized_data4 <- as.data.frame(matrix)
colnames(categorized_data4) <- c("mass", "n_cases", 
                                     "sample_prop", "sex", "host_plant")

NaN_rows <- subset(categorized_data4, is.na(categorized_data4$sample_prop))
categorized_data4 <- setdiff(categorized_data4, NaN_rows)

categorized_data4

###Compare to aggregate
data_all$mass_bin<-round(data_all$mass/0.01)*0.01

summary_4<-aggregate(flew_b~mass_bin*sex*host_plant, data=data_all, FUN=mean)
summary_4$N<-aggregate(flew_b~mass_bin*sex*host_plant, data=data_all, FUN=length)$flew_b
summary_4

#```

#```{r fig.width=5, fig.height=3}

gf_point(sample_prop ~ mass, data=categorized_data4, col=~sex, size=~host_plant,
         xlab = "Mass (g)", ylab= "Sample Proportion that Flew",
         title= "Observed Data")

par(mar=c(6.5, 5.5, 5.5, 9.5), xpd=TRUE) 
plot(categorized_data4$sample_prop~categorized_data4$mass, 
     col=c(1,2)[as.factor(categorized_data4$sex)], # Female = Black, Male = Red
     pch=c(19,22)[as.factor(categorized_data4$host_plant)],
     main="Observed Data",
     xlab = "Mass (g)",
     ylab= "Proportion Flew", # K. elegans = Squares, C.corindum = circles
     #sub=eq_glmer
     ) 
legend("topright",
       legend = c("C.corindum and F","K.elegans and M"),
       inset=c(-0.23,0.2),
       col=c(1,2),
       pch = c(19,22),
       title="Groups")
#```

#Mass is really dominating everything, so let's split by sex after looking at Trial 2.

## T2: GLM Binomial Modeling | Mass as a covariate 

#```{r}
data_T2 <-data_all[data_all$trial_type=="T2",]

####### No effect of chamber
summary(glm(flew_b~chamber, data=data_T2, family=binomial))

####### No effect of test date
summary(glm(flew_b~days_from_start_c, data=data_T2, family=binomial))

####### No effect of test time
summary(glm(flew_b~min_from_IncStart, data=data_T2, family=binomial))
#```

#```{r}
R = data_T2$flew_b
A = data_T2$host_c
B = data_T2$sex_c
C = data_T2$sym_dist
D = data_T2$mass_c # as a covariate 

data<-data.frame(R, A, B, C, D)
#data<-data.frame(R, A, B, C)
head(data) #kable()

# Automatic stepwise functions using AIC to decide between models
model.null <- glm(R~1, family=binomial, data = data_T2)
model.large <-glm(R~A*B*C*D, family=binomial, data=data_T2)

# Backwards
#step(model.large, direction = "backward")
#```

#For Backwards:

best_fit <- glm(formula = R ~ A + B + C + D + A:B + A:C + B:C + A:B:C
AIC: 364.7

#```{r}
# Forwards
#step(model.null, scope = ~ (A*B*C*D), direction = "forward")
#```
#For forwards:

#best_fit <- glm(formula = R ~ D + A
#AIC:356.7

#```{r}
#run AICprobs script
source("AICprobabilities.R")
source("generic models-binomial glm 4-FF.R")
AICs <- sort(summary$AIC)
models <- sort(P, decreasing=TRUE, index.return=TRUE)

AICs <- AICs[1:4]
models <- models$ix[1:4]
rbind(AICs, models) # top models and their AIC values
#```

m7 <- glm(formula = R ~ A + D, family = binomial, data = data)
m4 <- glm(formula = R ~ D, family = binomial, data = data)
m9 <- glm(formula = R ~ B + D, family = binomial, data = data)
m12 <- glm(formula = R ~ A + B + D, family = binomial, data = data)

#```{r}
anova(m7, m4, test="Chisq") # Removing A does not improve fit
anova(m7, m9, test="Chisq") # Replacing A with B does improve fit
anova(m9, m12, test="Chisq") #Adding A does not improve fit
#```

#```{r}
model_T2 <-glm(flew_b~sex_c + mass, family=binomial, data=data_T2)
summary(model_T2)
#```

#* mass is the only significant and strong effect

#```{r}
## Consider covariates
model_T2_final <-glmer(flew_b~sex_c + mass + (1|population), family=binomial, data=data_T2)
summary(model_T2_final)
## Did not change effect estimates or improve fit
#```

## Plotting T2 

#```{r}
plot(data_T2$mass, data_T2$flew_b)
#```

#```{r}
# no missing mass during T2 trials
cat_data4 <- categorize_data(data_T2, all_of("mass"), all_of("flew_b"),  0.010, 0.015, 0.025) # not working so well right now so wrote it all out below for now:

#```
#```{r}
##### TRIAL 2 DATA: Observed proportions of yes flew by mass #############

# group x values into categories and calculate the sample of proportion of flight for each category
df <- select(data_T2, mass, flew_b)
df <- df[order(df$mass),]

mass_interval <- 0.010
bins <- round(max(df$mass) / mass_interval)

iterations = bins
variables = + 3
matrix <- matrix(ncol=variables, nrow=iterations)

#here2
i <- 0.015
f <- 0.025
for (b in 1:bins) {
  matrix[b,1] <- f
  
  binned_df <- filter(df, mass < f & mass > i)
  flew_n <- binned_df$flew_b
  successes <- sum(flew_n)
  n_cases <- nrow(binned_df)
  sample_prop <- successes / n_cases
  matrix[b,2] <- n_cases
  matrix[b,3] <-sample_prop
  
  cat("Cases:", n_cases, end="\t")
  cat("Sample Proportion:", sample_prop, end="\n")
  
  i <- i + 0.010
  f <- f + 0.010
}

cat_data4 <- as.data.frame(matrix)
colnames(cat_data4) <- c("Mass", "Number of Cases", "Sample Proportion")
cat_data4

NaN_rows <- subset(cat_data4, is.na(cat_data4$`Sample Proportion`))
NaN_rows
# 339, 48, and 342 have no mass and were tested on the same date
cat_data4 <- setdiff(cat_data4, NaN_rows)

cat_data4
#```

#```{r}
fit<-lm(flew_b~mass, data=data_T2) 
coeff <- coefficients(summary(fit))

eq <- paste0("portion_flew = ", round(coeff[1],3),
             ifelse(sign(coeff[2])==1, " + ", " - "), 
             abs(round(coeff[2],3)), "*mass")

plot(as.matrix(cat_data4[1]), 
     as.matrix(cat_data4[3]),
     ylab="Sample Proportion of Yes Flew", 
     xlab="Mass (g)", 
     main="Observed proportions of yes flew by mass")
abline(coeff[1], coeff[2], col="blue")
mtext(eq, side=4)
#```

# Spliting by Sex (due to strong effect of mass)

#```{r}
data_fem <- data_all[data_all$sex=="F",]
data_male <- data_all[data_all$sex=="M",]
#```

## Female: GLM Binomial Modeling 

#```{r}
####### Marginal effect of chamber B-4 (ah this again!)
summary(glm(flew_b~chamber, data=data_fem, family=binomial))

####### No effect of test date
summary(glm(flew_b~days_from_start, data=data_fem, family=binomial))

####### No effect of test time
summary(glm(flew_b~min_from_IncStart, data=data_fem, family=binomial))
#```

#```{r}
R = data_fem$flew_b
A = data_fem$host_c
B = data_fem$sym_dist

data<-data.frame(R, A, B)
head(data) #kable()

# Automatic stepwise functions using AIC to decide between models
model.null <- glm(R~1, family=binomial, data = data_fem)
model.large <-glm(R~A*B, family=binomial, data = data_fem)

# Backwards
#step(model.large, direction = "backward")
#```

#```{r}
## Forwards
#step(model.null, scope = ~ (A*B*C), direction = "forward")
#```
#For forwards and backwards:

best_fit <- glm(formula = R ~ 1, family = binomial, data = data_fem)
#AIC:266.3

#```{r}
#run AICprobs script
source("AICprobabilities.R")
source("generic models-binomial glm 2-FF.R")
AICs <- sort(summary$AIC)
models <- sort(P, decreasing=TRUE, index.return=TRUE)

AICs <- AICs[1:4]
models <- models$ix[1:4]
rbind(AICs, models) # top models and their AIC values
#```

m1 <- glm(formula = R ~ A, family = binomial, data = data) *host*
m2 <- glm(formula = R ~ B, family = binomial, data = data) *sym_dist*
m3 <- glm(formula = R ~ A + B, family = binomial, data = data) *host and sym_dist*

#```{r}
anova(m1, m2, test="Chisq") # Replacing A with B improves fit 
anova(m1, m3, test="Chisq") # Adding B does not improve fit
anova(m2, m3, test="Chisq") # Adding A does not improve fit
#```

#```{r}
model_fem <-glm(flew_b~sym_dist, family=binomial, data=data_fem)
summary(model_fem) 
#```

#* but effect of sym_dist not significant...

#```{r}
## Consider covariates
model_fem_final <-glmer(flew_b~sym_dist + (1|population) + (1|trial_type), family=binomial, data=data_fem) # boundary (singular) fit: see ?isSingular
summary(model_fem_final)
## Barely changed effect estimates but does not improve fit
#```

## Plotting Female Data

#```{r fig.width=5, fig.height=3}
plot(data_fem$sym_dist, data_fem$flew_b)

fem_sum <-aggregate(flew_b~host_plant*sym_dist, data=data_fem, FUN=mean)
fem_sum

par(mar=c(6.5, 5.5, 5.5, 9.5), xpd=TRUE) # Add extra space to right of plot area; change clipping to figure
plot(agg$flew_b~fem_sum$sym_dist, 
     pch=c(19,22)[as.factor(fem_sum$host_plant)],
     main="Observed Data",
     xlab = "Distance from Sympatric Zone (°)",
     ylab= "Proportion Flew", # K. elegans = Squares C.corindum = circles
     #sub=eq_glmer
     ) 
legend("topright",
       legend = c("C.corindum","K.elegans"),
       inset=c(-0.23,0.2),
       pch = c(19,22),
       title="Groups")

#```


#```{r}
cat_data5 <- categorize_data(data_fem, all_of("sym_dist"), all_of("flew_b"),  0.1, 0, 0.05) 

fit_fem <- lm(flew_b~sym_dist, data=data_fem) 
coefff <- coefficients(summary(fit_fem))

eq <- paste0("portion_flew = ", round(coefff[1],3),
             ifelse(sign(coefff[2])==1, " + ", " - "), 
             abs(round(coefff[2],3)), "*sym_dist")

plot(as.matrix(cat_data5[1]), 
     as.matrix(cat_data5[3]),
     ylab="Sample Proportion of Yes Flew", 
     xlab="Distance From Sympatric Zone (°)", 
     main="Observed proportions of yes flew by distance from sympatric zone")
abline(coefff[1], coefff[2], col="blue")
mtext(eq, side=4)
#```

## Male: GLM Binomial Modeling 

#```{r}
data_male
####### No effect of chamber B-4 
summary(glm(flew_b~chamber, data=data_male, family=binomial))

####### Effect of test date (as seen in the data_all)
summary(glm(flew_b~days_from_start_c, data=data_male, family=binomial))

####### No effect of test time (but close p = 0.09443)
summary(glm(flew_b~min_from_IncStart, data=data_male, family=binomial))

# seems like males are very time sensitive
#```

#```{r}
R = data_male$flew_b
A = data_male$host_c
B = data_male$sym_dist

data<-data.frame(R, A, B)
head(data) #kable()

# Automatic stepwise functions using AIC to decide between models
model.null <- glm(R~1, family=binomial, data = data_male)
model.large <-glm(R~A*B, family=binomial, data = data_male)

# Backwards
#step(model.large, direction = "backward")
#```

#```{r}
## Forwards
#step(model.null, scope = ~ (A*B), direction = "forward")
#```

#For forwards and backwards:

best_fit <- glm(formula = R ~ A, family = binomial, data = data_male)
#AIC: 511.9

#```{r}
#run AICprobs script
source("AICprobabilities.R")
source("generic models-binomial glm 2-FF.R")
AICs <- sort(summary$AIC)
models <- sort(P, decreasing=TRUE, index.return=TRUE)

AICs <- AICs[1:4]
models <- models$ix[1:4]
rbind(AICs, models) # top models and their AIC values
#```

m1 <- glm(formula = R ~ A, family = binomial, data = data) *host*
m3 <- glm(formula = R ~ A + B, family = binomial, data = data) *host and sym_dist*
m4 <- glm(formula = R ~ A * B, family = binomial, data = data) *host and sym_dist interaction - most complex model*

#```{r}
anova(m1, m3, test="Chisq") # Adding B does not improve fit 
anova(m1, m4, test="Chisq") # Adding A*B does not improve fit
anova(m3, m4, test="Chisq") # Adding A*B does not improve fit
#```

#```{r}
model_male<-glm(flew_b~host_c, family=binomial, data=data_male)
summary(model_male)
```

* Strong negative effect if from GRT

```{r}
## Consider covariates
model_male_final <-glmer(flew_b~host_c + (1|population) + (1|trial_type), family=binomial, data=data_male) # no error
summary(model_male_final)
## Changed the effect slightly and improved the model and in turn made host notsignificant...
```

## Plotting Male Data

```{r fig.width=5, fig.height=3}
plot(data_male$sym_dist, data_male$flew_b)

male_sum <-aggregate(flew_b~host_plant*sym_dist, data=data_male, FUN=mean)
male_sum

par(mar=c(6.5, 5.5, 5.5, 9.5), xpd=TRUE) # Add extra space to right of plot area; change clipping to figure
plot(agg$flew_b~male_sum$sym_dist, 
     pch=c(19,22)[as.factor(male_sum$host_plant)],
     main="Observed Data",
     xlab = "Distance from Sympatric Zone (°)",
     ylab= "Proportion Flew", # K. elegans = Squares C.corindum = circles
     #sub=eq_glmer
     ) 
legend("topright",
       legend = c("C.corindum","K.elegans"),
       inset=c(-0.23,0.2),
       pch = c(19,22),
       title="Groups")

```


```{r}
cat_data6 <- categorize_data(data_male, all_of("sym_dist"), all_of("flew_b"),  0.1, 0, 0.05) 

fit_male <- lm(flew_b~sym_dist, data=data_male) 
coeffm <- coefficients(summary(fit_male))
coeffm

eq <- paste0("portion_flew = ", round(coeffm[1],3),
             ifelse(sign(coeffm[2])==1, " + ", " - "), 
             abs(round(coeffm[2],3)), "*sym_dist")

plot(as.matrix(cat_data6[1]), 
     as.matrix(cat_data6[3]),
     ylab="Sample Proportion of Yes Flew", 
     xlab="Distance From Sympatric Zone (°)", 
     main="Observed proportions of yes flew by distance from sympatric zone")
abline(coeffm[1], coeffm[2], col="blue")
mtext(eq, side=4)
```


## Other factors I have not addressed - egg laying which is specific to females, males have an effect of test date, and mass effects for males and females. 

Let's add these as covariates...

```{r}
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

Host seems to not be effecing female flight...let's remove it

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


