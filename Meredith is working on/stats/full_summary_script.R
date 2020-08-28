
####################################################################
######Flight yes-no stats
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

##added 3 additional variables:
#average days from start; this deals with the problem of individuals who were only tested early, who will have a low value here, and with any chance individuals who happened to get in early or late in both trials.
d_all$avg_days_c<-d_all$avg_days-mean(d_all$avg_days, na.rm=TRUE)

##this transformation gets average_mass to act normal and not give haywire effect estimates
d_all$average_mass_trans<-log(sqrt(d_all$average_mass))-mean(log(sqrt(d_all$average_mass)), na.rm=TRUE)

##this transformation gets wing2body ratio to act normal and not give haywire effect estimates; note that this has an inverse relationship with wing2body ratio itself, so effect estimate directions need to be flipped for interpretation.

d_all$wing2body_trans<-log(sqrt(0.85-d_all$wing2body))-mean(log(sqrt(0.85-d_all$wing2body)), na.rm=TRUE)

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
C = data_fem$average_mass_trans
D = data_fem$wing2body_trans
E = data_fem$avg_days_c

data<-data.frame(R1, R2, A, B, C, D, E)

source("src/compare_models.R")
errors <- withWarnings(model_comparisonsAIC("src/generic models-binomial glm 2R ~ 4-FF + E.R"))
length(errors$warnings)

anova(m25, m13, test='Chisq') #adding A*C improves fit

anova(m25, m17, test="Chisq") #adding D improves fit

anova(m10, m13, test="Chisq") #adding A alone does not improve fit

anova(m10, m3, test="Chisq") #adding D improves fit

anova(m10, m4, test="Chisq") #adding C improves fit

anova(m25, m45, test='Chisq') #adding A*D does not improve fit


mass_model_fem <- glm(cbind(num_flew, num_notflew) ~ host_c * average_mass_trans +  wing2body_trans + avg_days_c, data=data_fem, family=binomial)

summary(mass_model_fem)


#######################Males only
data_male <- d_all[d_all$sex=="M",]
data_male <- center_data(data_male, is_not_binded = FALSE)

R1 = data_male$num_flew
R2 = data_male$num_notflew
A = data_male$host_c
B = data_male$sym_dist_s
C = data_male$average_mass_trans 
D = data_male$wing2body_trans
E = data_male$avg_days_c

data<-data.frame(R1, R2, A, B, C, D, E)

source("src/compare_models.R")
errors <- withWarnings(model_comparisonsAIC("src/generic models-binomial glm 2R ~ 4-FF + E.R"))
length(errors$warnings)

anova(m83, m105, test="Chisq") #marginal improvement from adding C*D

anova(m50, m62, test="Chisq") #no improvement from adding C

anova(m83, m62, test="Chisq") #marginal improvement from B*C

mass_model_male<-glm(cbind(num_flew, num_notflew)~host_c*wing2body_trans + sym_dist_s*wing2body_trans + avg_days_c, family=binomial, data=data_male)
summary(mass_model_male)


#I just made this a function so it's easy to collapse. Probably won't stay in the final summary script, but I found it helpful for looking at these interactions. In making the data_temp summaries, data=d can be swapped for data=d[d$sex=="M",] (or F) to look at host effects within one sex only.

six_plots<-function(){
##quick plots for y/n flight
##plot-specific grouping variables
d$mass_block<-round(d$average_mass/0.005)*0.005
d$f_prob<-d$num_flew/(d$num_flew+d$num_notflew)
d$wing2body_block<-round(d$wing2body, digits=2)
d$days_block<-round(d$avg_days, digits=0)


par(mfrow=c(2,3), mai=c(0.6, 0.6, 0.02, 0.02))
##mass by sex
data_temp<-aggregate(f_prob~sex*mass_block, data=d, FUN=mean)
data_temp$n<-aggregate(f_prob~sex*mass_block, data=d, FUN=length)$f_prob
plot(data_temp$f_prob~data_temp$mass_block, pch=c(2,19)[as.factor(data_temp$sex)], ylab="Flight probability", xlab="Mass")
##Indeed, we can see that the effect of mass is very pronounced in females but essentially absent in males.


##wing2body by sex
data_temp<-aggregate(f_prob~sex*wing2body_block, data=d, FUN=mean)
data_temp$n<-aggregate(f_prob~sex*wing2body_block, data=d, FUN=length)$f_prob
plot(data_temp$f_prob~data_temp$wing2body_block, pch=c(2,19)[as.factor(data_temp$sex)], ylab="Flight probability", xlab="wing-to-body ratio")
##Here we can see that as wing2body ratio increases, flight probability increases, but that the effect is much more pronounced in males.


##average days from start by sex
data_temp<-aggregate(f_prob~sex*days_block, data=d, FUN=mean)
data_temp$n<-aggregate(f_prob~sex*days_block, data=d, FUN=length)$f_prob
plot(data_temp$f_prob~data_temp$days_block, pch=c(2,19)[as.factor(data_temp$sex)], ylab="Flight probability", xlab="Days from start")
##Here we can see that as the average days from start increases, flight probability increases, but that the effect is really only visible in females. This does not mean days from start didn't impact males - only that our experimental design successfully stopped that from being confounding (eg, males may have been less likely to die, or had less biased mortality by host).


##mass by host
data_temp<-aggregate(f_prob~host_c*mass_block, data=d, FUN=mean)
data_temp$n<-aggregate(f_prob~host_c*mass_block, data=d, FUN=length)$f_prob
plot(data_temp$f_prob~data_temp$mass_block, pch=19, col=c(rgb(1,0.1,0,0.8),rgb(0,1,0.8,0.8))[as.factor(data_temp$host_c)], ylab="Flight probability", xlab="Mass")
##Here, we can see that the effect of mass is clear on GRT (red) but weak on BV (blue)


##wing2body by host
data_temp<-aggregate(f_prob~host_c*wing2body_block, data=d, FUN=mean)
data_temp$n<-aggregate(f_prob~host_c*wing2body_block, data=d, FUN=length)$f_prob
plot(data_temp$f_prob~data_temp$wing2body_block, pch=19, col=c(rgb(1,0.1,0,0.8),rgb(0,1,0.8,0.8))[as.factor(data_temp$host_c)], , ylab="Flight probability", xlab="wing-to-body ratio")
##Here, we can see that the positive effect of wing2body ratio is clear on GRT (red) and on BV (blue)


##average days from start by host
data_temp<-aggregate(f_prob~host_c*days_block, data=d, FUN=mean)
data_temp$n<-aggregate(f_prob~host_c*days_block, data=d, FUN=length)$f_prob
plot(data_temp$f_prob~data_temp$days_block, pch=19, col=c(rgb(1,0.1,0,0.8),rgb(0,1,0.8,0.8))[as.factor(data_temp$host_c)], , ylab="Flight probability", xlab="Days from start")
##Here we can see no clear impact of days from start when separated by host
}
six_plots()


#################################################################
#### Flight speed stats

rm(list=ls())
setwd("~/Documents/Florida soapberry project/2019 Dispersal/SBB-dispersal git/avbernat_working_on/stats")


library(lme4)

library(dplyr)
library(tidyselect)
library(stringr)

library(glmnet)
library(ggplotify)
library(gridExtra)
library(ggformula)
library(randomcoloR)

output_col = FALSE # Recommend changing this to TRUE if working in Base R or RStudio, and FALSE if generating an html
source("src/clean_flight_data.R") # Script that loads and cleans up the data
source("src/regression_output.R") # A script that cleans up regression outputs and prints in color or black and white
source("src/center_flight_data.R")
source("src/get_warnings.R")

data <- read_flight_data("data/all_flight_data-Winter2020.csv")
data_all <- data[[1]]
data_tested <- data[[2]]

### Remove everyone who didn't fly (then remove distances = 0, if that didn't work fully)
data_flew_all <- data_tested[data_tested$flew_b == 1, ] 

### Check for low speeds
low_speeds <- data_flew_all %>%
  filter(average_speed <0.05)

### Check for high speeds
high_speeds <- data_flew_all %>%
  filter(average_speed >0.65)

low_speeds$flight_type# have 7 bugs with average_speed = 0 but were marked as bursters (this could be just something very short (second burst) - not enough to grant a calculation) - I decided to remove them. But one bug was continuous and had 0 distance and 0 speeds - that was bug 196 T2 set011-3-03-2020-A3_196.txt
high_speeds$flight_type # 3 bugs - also bursters. Could also be short explosive bursts but not true to the biology of these bugs (more like us blowing on them).

### Remove outliers
data_flew <- data_flew_all %>%
  filter(average_speed > 0.05) %>%
  filter(average_speed < 0.65)
  
data_flew <- center_data(data_flew)

##transform mass & speed
data_flew$mass_trans<-log(data_flew$mass)-mean(log(data_flew$mass), na.rm=TRUE)

data_flew$speed_trans<-log(data_flew$average_speed)-mean(log(data_flew$average_speed), na.rm=TRUE)


#######do flight types differ?
data_flew$flight_type <- relevel(data_flew$flight_type, ref="B")

summary(lmer(speed_trans~flight_type + (1|chamber) + (1|ID), data=data_flew)) #yes, B and C differ distinctly in average speed; BC and CB are not different from C, so let's keep those in the continuous flight analyses.

#######testing some covariates:
data_flew$chamber <- relevel(data_flew$chamber, ref="A-4")

####### Effect of chamber B-2 and B-4
summary(lmer(speed_trans~chamber + (1|ID), data=data_flew))###Possibly reductions in speed 

####### No effect of test date
tidy_regression(lmer(speed_trans~days_from_start + (1|chamber), data=data_flew), is_color=output_col)

####### No effect of test time
tidy_regression(lmer(average_speed~min_from_IncStart + (1|chamber), data=data_flew), is_color=output_col)


##bursters are likely to be much less reliable than continuous flyers; so, let's exclude them.

########C, BC, and CB data for speed

data_flew <- data_flew %>%
  filter(!is.na(mass))
data_flew <- center_data(data_flew)

dC<-data_flew[data_flew$flight_type=="C" | data_flew$flight_type=="BC" | data_flew$flight_type=="CB" ,] 
dC <- dC %>%
  filter(!is.na(body))
dC <- center_data(dC)

data<-data.frame(R=dC$speed_trans,
                 A=dC$host_c, 
                 B=dC$sex_c, 
                 C=dC$mass_trans,
                 D=dC$sym_dist_s, #I note it does not matter whether this is sym_dist or wing2body 
                 X=dC$chamber,
                 Y=dC$ID) 

source("src/compare_models.R")
model_comparisonsAIC("src/generic models-gaussian glmer 2-RF + 4-FF REMLF.R")

anova(m19, m28, test="Chisq") #adding A does not improve fit
anova(m19, m29, test="Chisq") #adding D does not improve fit
anova(m19, m8, test="Chisq") #adding B*C interaction does improve fit

continuous_model<-lmer(speed_trans~sex*mass_trans + (1|ID) + (1|chamber), data=dC)

summary(continuous_model) 

s.test <- paste("pval: ", shapiro.test(residuals(continuous_model))$p.value)
s.test
qqnorm(resid(continuous_model))
qqline(resid(continuous_model))
###Gorgeously normal post-transform

speed_summary<-aggregate(average_speed~sex, data=dC, FUN=mean)
speed_summary$se<-aggregate(average_speed~sex, data=dC, FUN=function(x) sd(x)/sqrt(length(x)))$average_speed

###There is not enough replication to break this down by sex, so we'll leave it as it is.

#this could be generalized, although it's probably not worth the time?
four_plots<-function(){
##quick plots for speed
##plot-specific grouping variables
data_flew$mass_block<-round(data_flew$mass/0.005)*0.005
data_flew$wing2body_block<-round(data_flew$wing2body, digits=2)


par(mfrow=c(2,2), mai=c(0.8, 0.8, 0.02, 0.02))
##mass by sex
data_temp<-aggregate(average_speed~sex*mass_block, data=data_flew, FUN=mean)
data_temp$n<-aggregate(average_speed~sex*mass_block, data=data_flew, FUN=length)$average_speed
plot(data_temp$average_speed~data_temp$mass_block, pch=c(2,19)[as.factor(data_temp$sex)], ylab="Flight speed", xlab="Mass")
##Unlike for flight probability, here we see a clear effect of mass on male flight speed, but not female flight speed.


##wing2body by sex
data_temp<-aggregate(average_speed~sex*wing2body_block, data=data_flew, FUN=mean)
data_temp$n<-aggregate(average_speed~sex*wing2body_block, data=data_flew, FUN=length)$average_speed
plot(data_temp$average_speed~data_temp$wing2body_block, pch=c(2,19)[as.factor(data_temp$sex)], ylab="Flight speed", xlab="wing-to-body ratio")
##Here we can see that as wing2body ratio increases, flight speed increases marginally in males, but not noticeably in females

#mass by host
data_temp<-aggregate(average_speed~host_c*mass_block, data=data_flew, FUN=mean)
data_temp$n<-aggregate(average_speed~host_c*mass_block, data=data_flew, FUN=length)$average_speed
plot(data_temp$average_speed~data_temp$mass_block, pch=19, col=c(rgb(1,0.1,0,0.8),rgb(0,1,0.8,0.8))[as.factor(data_temp$host_c)], ylab="Flight speed", xlab="Mass")
##Here we can see a weak positive effect of mass on flight speed, that does not appear to differ between hosts


##wing2body by host
data_temp<-aggregate(average_speed~host_c*wing2body_block, data=data_flew, FUN=mean)
data_temp$n<-aggregate(average_speed~host_c*wing2body_block, data=data_flew, FUN=length)$average_speed
plot(data_temp$average_speed~data_temp$wing2body_block, pch=19, col=c(rgb(1,0.1,0,0.8),rgb(0,1,0.8,0.8))[as.factor(data_temp$host_c)], ylab="Flight speed", xlab="wing-to-body ratio")
##Here we can see a potential weak positive effect of wing2body ratio on flight speed that does not differ between hosts.

}
four_plots()









#####################################################################
##### distance

rm(list=ls())
setwd("~/Documents/Florida soapberry project/2019 Dispersal/SBB-dispersal git/avbernat_working_on/stats")

library(lme4)

library(dplyr)
library(tidyselect)

library(ggplot2)
library(glmnet)
library(ggplotify)
library(gridExtra)
library(ggformula)
library(randomcoloR)


output_col = FALSE # Recommend changing this to TRUE if working in Base R or RStudio, and FALSE if generating an html
source("src/clean_flight_data.R") # Script that loads and cleans up the data
source("src/regression_output.R") # A script that cleans up regression outputs and prints in color or black and white
source("src/center_flight_data.R")
source("src/get_warnings.R")

data <- read_flight_data("data/all_flight_data-Winter2020.csv")
data_all <- data[[1]]
data_tested <- data[[2]]

### Remove everyone who didn't fly (then remove distances = 0, if that didn't work fully)
data_flew <- data_tested[data_tested$flew_b == 1, ]
data_flew <- data_flew[data_flew$distance > 0, ]
data_flew <- center_data(data_flew)


##bursters are likely to be much less reliable than continuous flyers; so, let's exclude them.

########C, BC, and CB data for distance

data_flew <- data_flew %>%
  filter(!is.na(mass))
data_flew <- center_data(data_flew)

dC<-data_flew[data_flew$flight_type=="C" | data_flew$flight_type=="BC" | data_flew$flight_type=="CB" ,] 
dC <- dC %>%
  filter(!is.na(body))
dC <- center_data(dC)

###This is intensely influenced by flight_type - similar to speed, I think these are much more meaningful for continuous fliers.
##transform mass
dC$mass_trans<-log(dC$mass)-mean(log(dC$mass), na.rm=TRUE)

##transform distance
dC$distance_trans<-log(sqrt(100+dC$distance))-mean(log(sqrt(100+dC$distance)), na.rm=TRUE)

#######testing some covariates:
dC$chamber <- relevel(dC$chamber, ref="B-4")

####### Effect of chamber A-4, B-2 and B-4
summary(lmer(distance_trans~chamber + (1|ID), data=dC)) ###yes, this one's an issue 

####### No effect of test date
tidy_regression(lmer(distance_trans~days_from_start + (1|chamber), data=dC), is_color=output_col)

####### marginal effect of start time
tidy_regression(lmer(distance_trans~min_from_IncStart + (1|chamber), data=data_flew), is_color=output_col)



### model transformed distance
data<-data.frame(R=dC$distance_trans,
                 A=dC$host_c, 
                 B=dC$sex_c, 
                 C=dC$mass_trans,
                 D=dC$wing2body, #Sym_dist showed up in nothing, reran with wing2body
                 X=dC$chamber,
                 Y=dC$ID) 

source("src/compare_models.R")
model_comparisonsAIC("src/generic models-gaussian glmer 2-RF + 4-FF REMLF.R")

anova(m2, m9, test="Chisq") #adding D does not improve things
anova(m3, m8, test="Chisq") #adding B does not improve things
anova(m2, m8, test="Chisq") #adding C does not improve things

#Once again, unclear if it's mass or sex driving the increase in distance traveled. Unfortunately, we still don't have the replication to split this by sex (at least, not for females). So, we just have to accept the uncertainty here - but we can use the delta stats to help dig into this for mass!

distance_model_all<-lmer(distance_trans~mass_trans + (1|chamber) + (1|ID), data=dC)
summary(distance_model_all)

alt_distance_model<-lmer(distance_trans~sex + (1|chamber) + (1|ID), data=dC)
summary(alt_distance_model)
        
s.test <- paste("pval: ", shapiro.test(residuals(distance_model_all))$p.value)
s.test #This isn't passing, but it's not awful. I'm willing to accept this.
qqnorm(resid(distance_model_all))
qqline(resid(distance_model_all))
 




four_plots<-function(){
##quick plots for distance
##plot-specific grouping variables
dC$mass_block<-round(dC$mass/0.005)*0.005
dC$wing2body_block<-round(dC$wing2body, digits=2)


par(mfrow=c(2,2), mai=c(0.8, 0.8, 0.02, 0.02))
##mass by sex
data_temp<-aggregate(distance~sex*mass_block, data=dC, FUN=mean)
data_temp$n<-aggregate(distance~sex*mass_block, data=dC, FUN=length)$distance
plot(data_temp$distance~data_temp$mass_block, pch=c(2,19)[as.factor(data_temp$sex)], ylab="Flight distance", xlab="Mass")
##Like flight speed, here we again see what looks like a positive effect of mass for males, but not for females


##wing2body by sex
data_temp<-aggregate(distance~sex*wing2body_block, data=dC, FUN=mean)
data_temp$n<-aggregate(distance~sex*wing2body_block, data=dC, FUN=length)$distance
plot(data_temp$distance~data_temp$wing2body_block, pch=c(2,19)[as.factor(data_temp$sex)], ylab="Flight distance", xlab="wing-to-body ratio")
##There's not much going on here.

#mass by host
data_temp<-aggregate(distance~host_c*mass_block, data=dC, FUN=mean)
data_temp$n<-aggregate(distance~host_c*mass_block, data=dC, FUN=length)$distance
plot(data_temp$distance~data_temp$mass_block, pch=19, col=c(rgb(1,0.1,0,0.8),rgb(0,1,0.8,0.8))[as.factor(data_temp$host_c)], ylab="Flight distance", xlab="Mass")
##Not much here either


##wing2body by host
data_temp<-aggregate(distance~host_c*wing2body_block, data=dC, FUN=mean)
data_temp$n<-aggregate(distance~host_c*wing2body_block, data=dC FUN=length)$distance
plot(data_temp$distance~data_temp$wing2body_block, pch=19, col=c(rgb(1,0.1,0,0.8),rgb(0,1,0.8,0.8))[as.factor(data_temp$host_c)], ylab="Flight distance", xlab="wing-to-body ratio")
##Not much here either

}
four_plots()