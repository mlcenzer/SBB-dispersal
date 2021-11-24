## ----setup, include=FALSE-------------------------------------------------------------------------------------------------
rm(list=ls())
dir = "~/Desktop/git_repositories/SBB-dispersal/avbernat_working_on/Dispersal/Winter_2020/stats/"
setwd(dir) 

library(cvms)
library(binom)
library(ggimage)
library(ggnewscale)
library(rsvg)
library(dplyr)

knitr::opts_chunk$set(echo = TRUE)


## ----warning=FALSE, message=FALSE-----------------------------------------------------------------------------------------
source_path = "~/Desktop/git_repositories/SBB-dispersal/avbernat_working_on/Rsrc/"

script_names = c("center_flight_data.R", # Re-centers data 
                 "clean_flight_data-Fall.R", # Loads and cleans data
                 "unique_flight_data-Fall.R",
                 "clean_flight_data.R",
                 "unique_flight_data.R")

for (script in script_names) { 
  path = paste0(source_path, script)
  source(path) 
}

morph_d = read.csv("data/bug_morphology_flight-trials-Autumn2019.csv", header=TRUE, sep=",",
                      stringsAsFactors=TRUE)
data = clean_flight_data.Fall("data/all_flight_data-Fall2019.csv", morph_d)

morph_d[morph_d$ID == 146,]$sex = data[data$ID == 146,]$sex # this bug broke apart before morph measurements taken so using flight sex identification


## -------------------------------------------------------------------------------------------------------------------------
data_mass = data %>%
  filter(!is.na(mass))

# 60 min trial
data60 = data_mass %>%
  filter(set_number < 53)

# 90 min trial
data90 = data_mass %>%
  filter(set_number < 72 & set_number > 52)

# ongoing trial
ongoing_data = data_mass %>%
  filter(set_number > 71)


## -------------------------------------------------------------------------------------------------------------------------
d = create_delta_data.Fall(ongoing_data)


## -------------------------------------------------------------------------------------------------------------------------
d <- d[with(d, order(mass_per)),]


## -------------------------------------------------------------------------------------------------------------------------
neither = c()
T1_rather_than_none = c()
T2_rather_than_none = c()
both_rather_than_none = c()
for (i in 1:nrow(d)) {
  mp = d$mass_per[[i]]
  s = d$sex_c[[i]]
  top0 = exp(0) # just equals 1
  top1 = exp(-1.02 + 0.043*mp - 0.69*s)
  top2 = exp(-6.82 - 0.009*mp - 5.63*s)
  top3 = exp(0.12 + 0.019*mp- 0.90*s)
  bottom = top0 + top1 + top2 + top3
  neither = c(neither, top0/bottom)
  T1_rather_than_none = c(T1_rather_than_none, top1/bottom)
  T2_rather_than_none = c(T2_rather_than_none, top2/bottom)
  both_rather_than_none = c(both_rather_than_none, top3/bottom)
}


## -------------------------------------------------------------------------------------------------------------------------
d$index = 1:nrow(d)
females = d %>%
  filter(sex=="F")
males = d %>%
  filter(sex=="M")
Frows = females$index
Mrows = males$index


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------
plot(d$mass_per[Frows], T1_rather_than_none[Frows], ylim=c(0,1), xlim=c(-40,104), col="blue", type="l",
     ylab="Flight Case Probability", xlab="Percent Change in Mass From T1 to T2 (%)", lty=1) #T1 only
points(d$mass_per[Mrows], T1_rather_than_none[Mrows], col="blue", type="l", lty=2)
points(d$mass_per[Frows], T2_rather_than_none[Frows], col="darkgreen", type="l", lty=1)
points(d$mass_per[Mrows], T2_rather_than_none[Mrows], col="darkgreen", type="l", lty=2)
points(d$mass_per[Frows], both_rather_than_none[Frows], col="maroon", type="l", lty=1)
points(d$mass_per[Mrows], both_rather_than_none[Mrows], col="maroon", type="l", lty=2)
points(d$mass_per[Frows], neither[Frows], col="red", type="l", lty=1)
points(d$mass_per[Mrows], neither[Mrows], col="red", type="l", lty=2)

text(-20,0.85, labels="Did Not Fly", col="red")
text(0,0.5, labels="Flew Twice", col="maroon")
text(40,0.25, labels="Flew in T1 only", col="blue")
text(30,0.1, labels="Flew in T2 only", col="darkgreen")

legend(84, 1.02,
       legend = c("female","male"),
       lty=1:2,
       col="black",
       cex=0.8)


## -------------------------------------------------------------------------------------------------------------------------
probs = round(cbind(neither, T1_rather_than_none, T2_rather_than_none, both_rather_than_none),2)
summary_probs = cbind(as.character(d$flight_case), as.character(d$sex), probs)
colnames(summary_probs) = c("event", "sex", "none", "T1", "T2", "both")
dataframe = as.data.frame(summary_probs)
nrow(dataframe)


## -------------------------------------------------------------------------------------------------------------------------
calculate_accuracy = function(data, cfirst, clast) {
  
  probs = data[cfirst:clast]
  most_likely_events = colnames(probs)[apply(probs,1,which.max)]
  actual_events = c()
  
  for (i in 1:nrow(data)) {
    if (data[i,1] == "0") {
      actual_event = "none"
    }
    if (data[i,1] == "-1") {
      actual_event = "T1"
    }
    if (data[i,1] == "1") {
      actual_event = "T2"
    }
    if (data[i,1] == "2") {
      actual_event = "both"
    }
    actual_events = c(actual_events, actual_event)
  }
  
  events = actual_events == most_likely_events
  accurate_events = sum(events)
  total_events = length(events)
  accuracy = accurate_events / total_events
  
  return(accuracy)
}


## -------------------------------------------------------------------------------------------------------------------------
# overall
acc = calculate_accuracy(dataframe,3,6)
paste("Overall prediction accuracy, ", round(acc,2))

# by sex
femdata = dataframe %>%
  filter(sex=="F")
maledata = dataframe %>%
  filter(sex=="M")

accF = calculate_accuracy(femdata,3,6)
paste("Female prediction accuracy, ", round(accF,2))
accM = calculate_accuracy(maledata,3,6)
paste("Male prediction accuracy, ", round(accM,2))


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------
get_confusion_matrix = function(data, cfirst, clast) {
  
  probs = data[cfirst:clast]
  most_likely_events = colnames(probs)[apply(probs,1,which.max)]
  actual_events = c()
  
  for (i in 1:nrow(data)) {
    if (data[i,1] == "0") {
      actual_event = "none"
    }
    if (data[i,1] == "-1") {
      actual_event = "T1"
    }
    if (data[i,1] == "1") {
      actual_event = "T2"
    }
    if (data[i,1] == "2") {
      actual_event = "both"
    }
    actual_events = c(actual_events, actual_event)
  }
  
  df = as.data.frame(cbind(actual_events, most_likely_events))
  #final_df = dplyr::count_(df, vars = c('actual_events','most_likely_events'))
  
  eval <- evaluate(df,
                 target_col = "actual_events",
                 prediction_cols = "most_likely_events",
                 type = "multinomial")
  
  return(eval)
}



## -------------------------------------------------------------------------------------------------------------------------
acc_table = get_confusion_matrix(dataframe,3,6)
acc_table
confusion_matrix <- acc_table$'Confusion Matrix'[[1]]
confusion_matrix
plot_confusion_matrix(confusion_matrix, add_sums=TRUE)


## -------------------------------------------------------------------------------------------------------------------------
d$wing2body = 0
for (i in 1:nrow(d)) {
  d$wing2body[i] = d$wing[[i]][1] / d$body[[i]][1]
}
d$wing2body_c = 0
d$wing2body_c = d$wing2body - mean(d$wing2body)


## -------------------------------------------------------------------------------------------------------------------------
neither = c()
T1_rather_than_none = c()
T2_rather_than_none = c()
both_rather_than_none = c()

for (i in 1:nrow(d)) {
  m = d$mass_per[[i]]
  s = d$sex_c[[i]]
  #w = d$wing[[i]][1] / d$body[[i]][1]
  w = d$wing2body_c[i]
  top0 = exp(0) # just equals 1
  #top1 = exp(-17.862 + 0.041*m - 0.571*s + 23.558*w )
  #top2 = exp(-4.395 - 0.005*m - 9.580*s - 8.937*w)
  #top3 = exp(-19.931 + 0.018*m - 0.760*s + 28.019*w)
  top1 = exp(-0.935 + 0.041*m - 0.571*s + 23.739*w)
  top2 = exp(-8.177 - 0.005*m - 6.954*s - 6.595*w)
  top3 = exp(0.201 + 0.018*m - 0.760*s + 28.094*w)
  bottom = top0 + top1 + top2 + top3
  neither = c(neither, top0/bottom)
  T1_rather_than_none = c(T1_rather_than_none, top1/bottom)
  T2_rather_than_none = c(T2_rather_than_none, top2/bottom)
  both_rather_than_none = c(both_rather_than_none, top3/bottom)
}


## -------------------------------------------------------------------------------------------------------------------------
probs = round(cbind(neither, T1_rather_than_none, T2_rather_than_none, both_rather_than_none),2)
summary_probs = cbind(as.character(d$flight_case), as.character(d$sex), probs)
colnames(summary_probs) = c("event", "sex", "none", "T1", "T2", "both")
dataframe = as.data.frame(summary_probs)
nrow(dataframe)


## -------------------------------------------------------------------------------------------------------------------------
# overall
acc = calculate_accuracy(dataframe,3,6)
paste("Overall prediction accuracy, ", round(acc,2))

# by sex
femdata = dataframe %>%
  filter(sex=="F")
maledata = dataframe %>%
  filter(sex=="M")

accF = calculate_accuracy(femdata,3,6)
paste("Female prediction accuracy, ", round(accF,2))
accM = calculate_accuracy(maledata,3,6)
paste("Male prediction accuracy, ", round(accM,2))


## -------------------------------------------------------------------------------------------------------------------------
acc_table = get_confusion_matrix(dataframe,3,6)
acc_table
confusion_matrix <- acc_table$'Confusion Matrix'[[1]]
confusion_matrix
plot_confusion_matrix(confusion_matrix, add_sums=TRUE)


## -------------------------------------------------------------------------------------------------------------------------
d = d %>%
  filter(sex=="F")


## -------------------------------------------------------------------------------------------------------------------------
d <- d[with(d, order(mass_diff)),]


## -------------------------------------------------------------------------------------------------------------------------
neither = c()
T1_rather_than_none = c()
both_rather_than_none = c()
for (i in 1:nrow(d)) {
  md = d$mass_diff[[i]]
  ed = d$egg_diff[[i]]
  top0 = exp(0) # just equals 1
  top1 = exp(-0.88 - 0.53*ed + 57.43*md )
  top2 = exp(-0.53 - 1.09*ed + 18.67*md)
  bottom = top0 + top1 + top2
  neither = c(neither, top0/bottom)
  T1_rather_than_none = c(T1_rather_than_none, top1/bottom)
  both_rather_than_none = c(both_rather_than_none, top2/bottom)
}


## -------------------------------------------------------------------------------------------------------------------------
probs = round(cbind(neither, T1_rather_than_none, both_rather_than_none),2)
summary_probs = cbind(as.character(d$flight_case), as.character(d$egg_diff), probs)
colnames(summary_probs) = c("event", "egg_diff", "none", "T1", "both")

egg2 = c(1,2,3,5,6,7,9,10,11,13)
noegg = c(4,8,12)

dataframe = as.data.frame(summary_probs)
dataframe$egg_cat = c(2,2,2,0,2,2,2,0,2,2,2,0,2)


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------
plot(d$mass_diff[egg2], T1_rather_than_none[egg2], ylim=c(0,1), col="blue", type="l",
     ylab="Flight Case Probability", xlab="Change in Mass From T1 to T2 (g)", main="Females Only", lty=2) #T1 only
points(d$mass_diff[noegg], T1_rather_than_none[noegg], col="blue", type="l", lty=1)
points(d$mass_diff[egg2], both_rather_than_none[egg2], col="maroon", type="l", lty=2)
points(d$mass_diff[noegg], both_rather_than_none[noegg], col="maroon", type="l", lty=1)
points(d$mass_diff[egg2], neither[egg2], col="red", type="l", lty=2)
points(d$mass_diff[noegg], neither[noegg], col="red", type="l", lty=1)

text(0,0.75, labels="Did Not Fly", col="red")
text(0,0.35, labels="Flew Twice", col="maroon")
text(0,0.15, labels="Flew in T1 only", col="blue")
legend(0.01, 0.98,
       legend = c("no eggs","2 eggs"),
       lty=1:2,
       col="black",
       cex=0.8)


## -------------------------------------------------------------------------------------------------------------------------
accF_eggs = calculate_accuracy(dataframe,3,5)
paste("Female prediction accuracy for mass diff and egg model, ", round(accF_eggs,2))


## -------------------------------------------------------------------------------------------------------------------------
acc_table = get_confusion_matrix(dataframe,3,5)
acc_table
confusion_matrix <- acc_table$'Confusion Matrix'[[1]]
confusion_matrix
plot_confusion_matrix(confusion_matrix, add_sums=TRUE)

