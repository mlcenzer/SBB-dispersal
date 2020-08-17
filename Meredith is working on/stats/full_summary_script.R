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

for(row in 1:length(d$flew_b)){
  
  n_flew <- sum(d$flew_b[[row]] == 1) # total number of times did fly among trails
  d$num_flew[[row]] <- n_flew 
  
  n_notflew <- sum(d$flew_b[[row]] == 0) # total number of times did not fly among trails
  d$num_notflew[[row]] <- n_notflew
  
  avg_mass <- mean(d$mass[[row]])
  d$average_mass[[row]] <- avg_mass

}

d <- select(d, -filename, -channel_letter, -set_number)
d <- center_data(d, is_not_binded = FALSE)


####model testing; no random factors.
R1 = d$num_flew
R2 = d$num_notflew
A = d$host_c
B = d$sex_c
C = d$sym_dist_s
D = d$average_mass_c 

data<-data.frame(R1, R2, A, B, C, D)

source("src/compare_models.R")
errors <- withWarnings(model_comparisonsAIC("src/generic models-binomial glm 2R ~ 4-FF.R"))
length(errors$warnings)

anova(m63, m85, test="Chisq") # Adding B*D does not improve fit
anova(m26, m36, test="Chisq") # Adding C does not improve fit | is sym dist important?
anova(m12, m26, test="Chisq") # Adding A*D does improve fit

mass_model_all <- glm(cbind(num_flew, num_notflew) ~ host_c * average_mass_c +  sex_c, data=d, family=binomial)

tidy_regression(mass_model_all, is_color=output_col)
summary(mass_model_all)



#model with wing2body, but no sym_dist
R1 = d$num_flew
R2 = d$num_notflew
A = d$host_c
B = d$sex_c
C = d$wing2body_c
D = d$average_mass_c 

data<-data.frame(R1, R2, A, B, C, D)

source("src/compare_models.R")
errors <- withWarnings(model_comparisonsAIC("src/generic models-binomial glm 2R ~ 4-FF.R"))
length(errors$warnings)


###########MLC: Because mass and morphology are so dimorphic between sexs, and sex itself has a strong direct effect, let's go now to the split-by-sex results.



