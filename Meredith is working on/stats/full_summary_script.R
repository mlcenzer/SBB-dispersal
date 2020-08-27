
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

data<-data.frame(R=data_flew$speed_trans,
                 A=data_flew$host_c, 
                 B=data_flew$sex_c, 
                 C=data_flew$sym_dist_s, 
                 D=data_flew$mass_trans,
                 X=data_flew$chamber,
                 Y=data_flew$ID) 

source("src/compare_models.R")
model_comparisonsAIC("src/generic models-gaussian glmer 2-RF + 4-FF REMLF.R")

#one model, m52, did not converge; I'm ok excluding that model as a possibility.

anova(m4, m7, test="Chisq") #adding A does not improve fit
anova(m4, m9, test="Chisq") #adding B does not improve fit
anova(m2, m9, test="Chisq") #adding D does not improve fit - basically it seems we can't tell here if it's sex or mass.
anova(m9, m20, test="Chisq") #adding B*D interaction does not improve fit
anova(m0, m4, test="Chisq") # Adding D does improve fit
anova(m0, m2, test="Chisq") # Adding B does improve fit




#######Since we can't tell if it's sex or mass, let's look within each sex, as in y/n; and try adding wing2body

data_fem <- data_flew[data_flew$sex=="F",]
data_fem <- center_data(data_fem, is_not_binded = FALSE)

data<-data.frame(R=data_fem$speed_trans,
                 A=data_fem$host_c, 
                 B=data_fem$sym_dist_s, 
                 C=data_fem$mass_trans,
                 D=data_fem$wing2body,
                 X=data_fem$chamber,
                 Y=data_fem$ID) 

source("src/compare_models.R")
model_comparisonsAIC("src/generic models-gaussian glmer 2-RF + 4-FF REMLF.R")

anova(m0, m4, test="Chisq") #null model is the best model
anova(m0, m1, test="Chisq") #null model is the best model



###males

data_male <- data_flew[data_flew$sex=="M",]
data_male <- center_data(data_male, is_not_binded = FALSE)

data<-data.frame(R=data_male$speed_trans,
                 A=data_male$host_c, 
                 B=data_male$sym_dist_s, 
                 C=data_male$mass_trans,
                 D=data_male$wing2body,
                 X=data_male$chamber,
                 Y=data_male$ID) 

source("src/compare_models.R")
model_comparisonsAIC("src/generic models-gaussian glmer 2-RF + 4-FF REMLF.R")

anova(m10, m4, test="Chisq") #no improvement from adding C
anova(m10, m3, test="Chisq") #marginal improvement from adding D
anova(m4, m0, test="Chisq") #improvement from adding D
anova(m3, m0, test="Chisq") #marginal improvement from adding C

male_speed_model<-lmer(speed_trans~wing2body + (1|ID) + (1|chamber), data=data_male)
summary(male_speed_model)

s.test <- paste("pval: ", shapiro.test(residuals(male_speed_model))$p.value)
qqnorm(resid(male_speed_model))
qqline(resid(male_speed_model))
###Gorgeously normal post-transform




####I'm actually quite happy with this, but will take a quick peak here at the continuous fliers only:

dC<-data_flew[data_flew$flight_type=="C",] 
dC <- dC %>%
  filter(!is.na(body))
dC <- center_data(dC)

data<-data.frame(R=dC$speed_trans,
                 A=dC$host_c, 
                 B=dC$sex_c, 
                 C=dC$mass_trans,
                 D=dC$wing2body,
                 X=dC$chamber,
                 Y=dC$ID) 

source("src/compare_models.R")
model_comparisonsAIC("src/generic models-gaussian glmer 2-RF + 4-FF REMLF.R")

#mass and wing2body ratio both potentially interact with sex; by sex:

######females

data_fem <- dC[dC$sex=="F",]
data_fem <- center_data(data_fem, is_not_binded = FALSE)

data<-data.frame(R=data_fem$speed_trans,
                 A=data_fem$host_c, 
                 B=data_fem$sym_dist_s, 
                 C=data_fem$mass_trans,
                 D=data_fem$wing2body,
                 X=data_fem$chamber,
                 Y=data_fem$ID) 

source("src/compare_models.R")
model_comparisonsAIC("src/generic models-gaussian glmer 2-RF + 4-FF REMLF.R")

anova(m0, m4, test="Chisq") #null model is the best model
anova(m0, m1, test="Chisq") #null model is the best model



###males

data_male <- dC[dC$sex=="M",]
data_male <- center_data(data_male, is_not_binded = FALSE)

data<-data.frame(R=data_male$speed_trans,
                 A=data_male$host_c, 
                 B=data_male$sym_dist_s, 
                 C=data_male$mass_trans,
                 D=data_male$wing2body,
                 X=data_male$chamber,
                 Y=data_male$ID) 

source("src/compare_models.R")
model_comparisonsAIC("src/generic models-gaussian glmer 2-RF + 4-FF REMLF.R")

anova(m10, m4, test="Chisq") #no improvement from adding C
anova(m10, m3, test="Chisq") #marginal improvement from adding D
anova(m4, m0, test="Chisq") #improvement from adding D
anova(m3, m0, test="Chisq") #marginal improvement from adding C

male_speed_model<-lmer(speed_trans~wing2body + (1|ID) + (1|chamber), data=data_male)
summary(male_speed_model)

s.test <- paste("pval: ", shapiro.test(residuals(male_speed_model))$p.value)
qqnorm(resid(male_speed_model))
qqline(resid(male_speed_model))
###Gorgeously normal post-transform


