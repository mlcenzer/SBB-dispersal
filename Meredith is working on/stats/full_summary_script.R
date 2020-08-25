rm(list=ls())
setwd("~/Documents/Florida soapberry project/2019 Dispersal/SBB-dispersal git/avbernat_working_on/stats") ####MLC: changed to my working directory


##load libraries

library(lme4)
library(dplyr)
library(ggplotify)
library(gridExtra)
library(ggformula)
library(tidyselect)
library(zoo)

##source scripts

output_col = TRUE # Recommend changing this to TRUE if working in Base R or RStudio, and FALSE if generating an html
source("src/clean_flight_data.R") # Script that loads and cleans up the data
source("src/regression_output.R") # A script that cleans up regression outputs and prints in color or black and white
source("src/center_flight_data.R")
source("src/get_warnings.R")

##read in and clean up data

data <- read_flight_data("data/all_flight_data-Winter2020.csv")
data_all <- data[[1]]
data_tested <- data[[2]]

d <- data_tested %>%
   group_by(ID, sex,population, site, host_plant, latitude, longitude, total_eggs, 
            beak, thorax, wing, body, w_morph, morph_notes, tested,
            host_c, sex_c, w_morph_c, lat_c, sym_dist, sym_dist_s, total_eggs_c, 
            beak_c, thorax_c, thorax_s, body_c, wing_c, wing2body, wing2body_c, wing2body_s) %>%
   summarise_all(funs(list(na.omit(.))))

d$num_flew <- 0
d$num_notflew <- 0
d$average_mass <- 0
d$avg_days <- 0

for(row in 1:length(d$flew_b)){
  
  n_flew <- sum(d$flew_b[[row]] == 1) # total number of times did fly among trails
  d$num_flew[[row]] <- n_flew 
  
  n_notflew <- sum(d$flew_b[[row]] == 0) # total number of times did not fly among trails
  d$num_notflew[[row]] <- n_notflew
  
  avg_mass <- mean(d$mass[[row]])
  d$average_mass[[row]] <- avg_mass
  
 avg_days <- mean(d$days_from_start[[row]]) ##MC: use average days as covariate
  d$avg_days[[row]] <- avg_days

}

d_all <- select(d, -filename, -channel_letter, -set_number) ##MC: changed the name here to avoid re-loading all data to generate male and female only centered datasets
d_all$avg_days_c<-d_all$avg_days-mean(d_all$avg_days, na.rm=TRUE)
d_all$average_mass_trans<-log(sqrt(d_all$average_mass))-mean(log(sqrt(d_all$average_mass)), na.rm=TRUE)
d <- center_data(d_all, is_not_binded = FALSE)

######################All data for y/n flight response

##test total or average days:
days_model<-glm(cbind(num_flew,num_notflew)~avg_days_c, data=d, family=binomial)
summary(days_model) ##while average days does not show the same strong effect as days_from_start, this should still control for the fact that some individuals were only tested early, and some by chance had two earlier or later days within a trial. This allows us to retain the structure with R1 and R2, which controls for ID, but still converges.

## model testing
R1 = d$num_flew
R2 = d$num_notflew
A = d$host_c
B = d$sex_c
C = d$sym_dist_s
D = d$average_mass_trans 
E = d$avg_days_c

data<-data.frame(R1, R2, A, B, C, D, E)

source("src/compare_models.R")
errors <- withWarnings(model_comparisonsAIC("src/generic models-binomial glm 2R ~ 4-FF + E.R"))
length(errors$warnings)

anova(m63, m85, test="Chisq") # Adding B*D does not improve fit
anova(m26, m36, test="Chisq") # Adding C does not improve fit 
anova(m12, m26, test="Chisq") # Adding A*D does improve fit
anova(m63, m36, test="Chisq") # Adding C*D does improve fit

mass_model_all <- glm(cbind(num_flew, num_notflew) ~ host_c * average_mass_trans + sym_dist_s * average_mass_trans + sex + avg_days_c, data=d, family=binomial)

summary(mass_model_all)


#### MLC: Because mass and morphology are so dimorphic between sexs, and sex itself has a strong direct effect, let's go now to the split-by-sex results. It may be that in wing2body and mass, we have pinpointed the reasons that sexes are different; but they differ in so many ways, that is a strong inference ot try and make.


##################### Females only
data_fem <- d_all[d_all$sex=="F",]
data_fem <- center_data(data_fem, is_not_binded = FALSE)

R1 = data_fem$num_flew
R2 = data_fem$num_notflew
A = data_fem$host_c
B = data_fem$sym_dist
C = log(sqrt(data_fem$average_mass))-mean(log(sqrt(data_fem$average_mass)), na.rm=TRUE)
D = data_fem$wing2body_s
E = data_fem$avg_days_c

data<-data.frame(R1, R2, A, B, C, D, E)

source("src/compare_models.R")
errors <- withWarnings(model_comparisonsAIC("src/generic models-binomial glm 2R ~ 4-FF + E.R"))
length(errors$warnings)

anova(m25, m13, test='Chisq') #adding A*C improves fit

anova(m25, m17, test="Chisq") #adding D improves fit

anova(m10, m13, test="Chisq") #adding A alone does not improve fit

anova(m10, m3, test="Chisq") #adding D improves fit

anova(m10, m4, test="Chisq") #adding C improves fit, like a lot

anova(m25, m45, test='Chisq') #adding A*D does not improve fit


mass_model_fem <- glm(cbind(num_flew, num_notflew) ~ host_c * average_mass_c +  wing2body_s + avg_days_c, data=data_fem, family=binomial)

summary(mass_model_fem)



##MC: Average days pops out!





#######################Males only
data_male <- d_all[d_all$sex=="M",]
data_male <- center_data(data_male, is_not_binded = FALSE)

R1 = data_male$num_flew
R2 = data_male$num_notflew
A = data_male$host_c
B = data_male$sym_dist
C = data_male$average_mass_c 
D = data_male$wing2body_c
E = data_male$avg_days_c

data<-data.frame(R1, R2, A, B, C, D, E)

source("src/compare_models.R")
errors <- withWarnings(model_comparisonsAIC("src/generic models-binomial glm 2R ~ 4-FF + E.R"))
length(errors$warnings)

anova(m83, m105, test="Chisq") #no improvement from adding C*D

anova(m50, m62, test="Chisq") #no improvement from adding C

anova(m83, m62, test="Chisq") #marginal improvement from B*C

mass_model_male<-glm(cbind(num_flew, num_notflew)~host_c*wing2body_c + sym_dist*average_mass_c + sym_dist*wing2body_c + avg_days_c, family=binomial, data=data_male)
summary(mass_model_male)

##quick plots

par(mfrow=c(2,3), mai=c(0.6, 0.6, 0.02, 0.02))
##mass by sex
d$mass_block<-round(d$average_mass/0.005)*0.005
d$f_prob<-d$num_flew/(d$num_flew+d$num_notflew)
data_temp<-aggregate(f_prob~sex*mass_block, data=d, FUN=mean)
data_temp$n<-aggregate(f_prob~sex*mass_block, data=d, FUN=length)$f_prob
plot(data_temp$f_prob~data_temp$mass_block, pch=c(2,19)[as.factor(data_temp$sex)], ylab="Flight probability", xlab="Mass")
##Indeed, we can see that the effect of mass is very pronounced in females but essentially absent in males.


##wing2body by sex
d$wing2body_block<-round(d$wing2body, digits=2)
data_temp<-aggregate(f_prob~sex*wing2body_block, data=d, FUN=mean)
data_temp$n<-aggregate(f_prob~sex*wing2body_block, data=d, FUN=length)$f_prob
plot(data_temp$f_prob~data_temp$wing2body_block, pch=c(2,19)[as.factor(data_temp$sex)], ylab="Flight probability", xlab="wing-to-body ratio")
##Here we can see that as wing2body ratio increases, flight probability increases, but that the effect is much more pronounced in males.


##average days from start by sex
d$days_block<-round(d$avg_days, digits=0)
data_temp<-aggregate(f_prob~sex*days_block, data=d, FUN=mean)
data_temp$n<-aggregate(f_prob~sex*days_block, data=d, FUN=length)$f_prob
plot(data_temp$f_prob~data_temp$days_block, pch=c(2,19)[as.factor(data_temp$sex)], ylab="Flight probability", xlab="Days from start")
##Here we can see that as the average days from start increases, flight probability increases, but that the effect is really only visible in females. This does not mean days from start didn't impact males - only that our experimental design successfully stopped that from being confounding (eg, males may have been less likely to die, or had less biased mortality by host).


##wing2body by host
data_temp<-aggregate(f_prob~host_c*wing2body_block, data=d, FUN=mean)
data_temp$n<-aggregate(f_prob~host_c*wing2body_block, data=d, FUN=length)$f_prob
plot(data_temp$f_prob~data_temp$wing2body_block, pch=19, col=c(rgb(1,0.1,0,0.8),rgb(0,1,0.8,0.8))[as.factor(data_temp$host_c)], , ylab="Flight probability", xlab="wing-to-body ratio")
##Here, we can see that the positive effect of wing2body ratio is clear on GRT (red) and on BV (blue)


##mass by host
data_temp<-aggregate(f_prob~host_c*mass_block, data=d, FUN=mean)
data_temp$n<-aggregate(f_prob~host_c*mass_block, data=d, FUN=length)$f_prob
plot(data_temp$f_prob~data_temp$mass_block, pch=19, col=c(rgb(1,0.1,0,0.8),rgb(0,1,0.8,0.8))[as.factor(data_temp$host_c)], ylab="Flight probability", xlab="Mass")
##Here, we can see that the effect of mass is clear on GRT (red) but weak on BV (blue)

##average days from start by host
data_temp<-aggregate(f_prob~host_c*days_block, data=d, FUN=mean)
data_temp$n<-aggregate(f_prob~host_c*days_block, data=d, FUN=length)$f_prob
plot(data_temp$f_prob~data_temp$days_block, pch=19, col=c(rgb(1,0.1,0,0.8),rgb(0,1,0.8,0.8))[as.factor(data_temp$host_c)], , ylab="Flight probability", xlab="Days from start")
##Here we can see no clear impact of days from start when separated by host