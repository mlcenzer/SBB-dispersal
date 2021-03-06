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
d$total_days <- 0

for(row in 1:length(d$flew_b)){
  
  n_flew <- sum(d$flew_b[[row]] == 1) # total number of times did fly among trails
  d$num_flew[[row]] <- n_flew 
  
  n_notflew <- sum(d$flew_b[[row]] == 0) # total number of times did not fly among trails
  d$num_notflew[[row]] <- n_notflew
  
  avg_mass <- mean(d$mass[[row]])
  d$average_mass[[row]] <- avg_mass
  
  total_days <- sum(d$days_from_start[[row]]) ##MC: didn't work, or as average
  d$total_days[[row]] <- total_days

}

d_all <- select(d, -filename, -channel_letter, -set_number) ##MC: changed the name here to avoid re-loading all data to generate male and female only centered datasets
d <- center_data(d_all, is_not_binded = FALSE)


days_model<-glm(flew_b~days_from_start_c, data=data_tested, family=binomial)
summary(days_model)

days_model_ID<-glmer(flew_b~days_from_start_c + (1|ID), data=data_tested, family=binomial)
summary(days_model_ID)

####MLC: This issue is really frustrating, but I don't think we can ignore it. Unfortunately, I think this means going back to the old way - we can't include it, averaging it doesn't work, totalling it doesn't work, truncating the dataset leads to really different results and still has differences in average days_from_start between host populations - we just have to use the days_from_start covariate, which means going back to the old way and dealing with convergence issues a different way.



############## truncating dataset to only those who flew in both trials
repeat_IDs<-aggregate(flew_b~ID, data=data_tested, FUN=length)
data_repeated<-data_tested[data_tested$ID%in%repeat_IDs$ID[repeat_IDs$flew_b==2],]

days_model<-glm(flew_b~days_from_start_c, family=binomial, data=data_repeated)
summary(days_model) ###VERY strong, indeed, even stronger than before exclusion; however, we now have equal representation of all groups in both trials. Does it influence the results of other models?

d_repeated_all<-d[(d$num_flew+d$num_notflew)==2,]

d_repeated <- center_data(d_repeated_all, is_not_binded = FALSE)


######################All data for y/n flight response:: same response as with full dataset!

##test average days:
days_model<-glm(cbind(num_flew,num_notflew)~total_days, data=d_repeated, family=binomial)
summary(days_model) ##no detectabe effect

## model testing; no random factors.
R1 = d_repeated$num_flew
R2 = d_repeated$num_notflew
A = d_repeated$host_c
B = d_repeated$sex_c
C = d_repeated$sym_dist_s
D = d_repeated$average_mass_c 

data<-data.frame(R1, R2, A, B, C, D)

source("src/compare_models.R")
errors <- withWarnings(model_comparisonsAIC("src/generic models-binomial glm 2R ~ 4-FF.R"))
length(errors$warnings)

anova(m26, m18, test="Chisq") # Adding B*D does not improve fit
anova(m26, m36, test="Chisq") # Adding C does not improve fit
anova(m12, m26, test="Chisq") # Adding A*D does improve fit


mass_model_all <- glm(cbind(num_flew, num_notflew) ~ host_c * average_mass_c +  sex, data=d_repeated, family=binomial)

summary(mass_model_all)




##################### Females only:: same response as with full dataset!


data_fem <- d_repeated_all[d_repeated_all$sex=="F",]
data_fem <- center_data(data_fem, is_not_binded = FALSE)

R1 = data_fem$num_flew
R2 = data_fem$num_notflew
A = data_fem$host_c
B = data_fem$sym_dist
C = data_fem$average_mass_c 
D = data_fem$wing2body_s

data<-data.frame(R1, R2, A, B, C, D)

source("src/compare_models.R")
errors <- withWarnings(model_comparisonsAIC("src/generic models-binomial glm 2R ~ 4-FF.R"))
length(errors$warnings)

anova(m25, m13, test='Chisq') #adding A*C improves fit

anova(m25, m17, test="Chisq") #adding D improves fit

anova(m10, m13, test="Chisq") #adding A alone does not improve fit

anova(m10, m3, test="Chisq") #adding D improves fit

anova(m10, m4, test="Chisq") #adding C improves fit


mass_model_fem <- glm(cbind(num_flew, num_notflew) ~ host_c * average_mass_c +  wing2body_s, data=data_fem, family=binomial)

summary(mass_model_fem)

##MC: wing2body recapitulates the effect of wing, although less strongly; however, wing2body is a better predictor: it is not correlated with mass, and it shows the effect of wing length while controlling for overall body size. Furthermore, it's the predictor we use in the full dataset. So, while the effect is less satisfyingly strong, it is still the right predictor to use.





#######################Males only :: NOT same as with full dataset.

data_male <- d_repeated_all[d_repeated_all$sex=="M",]
data_male <- center_data(data_male, is_not_binded = FALSE)

R1 = data_male$num_flew
R2 = data_male$num_notflew
A = data_male$host_c
B = data_male$sym_dist
C = data_male$average_mass_c 
D = data_male$wing2body_c

data<-data.frame(R1, R2, A, B, C, D)

source("src/compare_models.R")
errors <- withWarnings(model_comparisonsAIC("src/generic models-binomial glm 2R ~ 4-FF.R"))
length(errors$warnings)

anova(m83, m105, test="Chisq") #no improvement from adding C*D

anova(m50, m62, test="Chisq") #no improvement from adding C

anova(m83, m62, test="Chisq") #marginal improvement from B*C

mass_model_male<-glm(cbind(num_flew, num_notflew)~host_c*wing2body_c + sym_dist*average_mass_c + sym_dist*wing2body_c, family=binomial, data=data_male)
summary(mass_model_male)








##quick plots

par(mfrow=c(2,2))
##mass by sex
d$mass_block<-round(d$average_mass/0.005)*0.005
d$f_prob<-d$num_flew/(d$num_flew+d$num_notflew)
data_temp<-aggregate(f_prob~sex*mass_block, data=d, FUN=mean)
data_temp$n<-aggregate(f_prob~sex*mass_block, data=d, FUN=length)$f_prob
plot(data_temp$f_prob~data_temp$mass_block, pch=c(10,19)[as.factor(data_temp$sex)], ylab="Flight probability", xlab="Mass")
##Indeed, we can see that the effect of mass is very pronounced in females but essentially absent in males.

##mass by host

data_temp<-aggregate(f_prob~host_c*mass_block, data=d, FUN=mean)
data_temp$n<-aggregate(f_prob~host_c*mass_block, data=d, FUN=length)$f_prob
plot(data_temp$f_prob~data_temp$mass_block, pch=19, col=c(rgb(1,0.1,0,0.8),rgb(0,1,0.8,0.8))[as.factor(data_temp$host_c)], ylab="Flight probability", xlab="Mass")
##Here, we can see that the effect of mass is clear on GRT (red) but weak on BV (blue)

##wing2body
d$wing2body_block<-round(d$wing2body, digits=2)
data_temp<-aggregate(f_prob~sex*wing2body_block, data=d, FUN=mean)
data_temp$n<-aggregate(f_prob~sex*wing2body_block, data=d, FUN=length)$f_prob
plot(data_temp$f_prob~data_temp$wing2body_block, pch=c(10,19)[as.factor(data_temp$sex)], ylab="Flight probability", xlab="wing-to-body ratio")
##Here we can see that as wing2body ratio increases, flight probability increases, but that the effect is much more pronounced in males.

##wing2body by host
data_temp<-aggregate(f_prob~host_c*wing2body_block, data=d, FUN=mean)
data_temp$n<-aggregate(f_prob~host_c*wing2body_block, data=d, FUN=length)$f_prob
plot(data_temp$f_prob~data_temp$wing2body_block, pch=19, col=c(rgb(1,0.1,0,0.8),rgb(0,1,0.8,0.8))[as.factor(data_temp$host_c)], , ylab="Flight probability", xlab="wing-to-body ratio")
##Here, we can see that the positive effect of wing2body ratio is clear on GRT (red) and on BV (blue)

