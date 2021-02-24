
####################################################################
######Flight yes-no stats
rm(list=ls())
setwd("~/Documents/Florida soapberry project/2019 Dispersal/SBB-dispersal git/avbernat_working_on/Dispersal/Winter_2020/stats") ####MLC: changed to my working directory


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

d_all$wing2body_trans<-log(sqrt(0.85-d_all$wing2body))-mean(log(sqrt(0.85-d_all$wing2body)), na.rm=TRUE)

d <- center_data(d_all, is_not_binded = FALSE)



data<-data.frame(R=d_all$wing2body_trans, A=d_all$host_c, B=d_all$sex_c, C=d_all$sym_dist)

library(lme4)

#run AICprobs script
setwd("/Users/Meredith/Desktop/Documents/R/generic models/")
source("AICprobabilities.R")
source("generic models-gaussian glm 3-FF.R")
sort(summary$AIC)
sort(P, decreasing=TRUE, index.return=TRUE)

