## ----setup, include=FALSE-------------------------------------------------------------------------------------------------
rm(list=ls())
library(lme4)
library(zoo)
library(data.table)

library(dplyr)
library(ggplotify)
library(gridExtra)
library(ggplot2)
library(ggformula)
library(tidyselect)
library(tidyverse)

# time series libraries
library(forecast)
library(tseries)
library(xts)
library(fma)

dir = "~/Desktop/git_repositories/SBB-dispersal/avbernat_working_on/All_Morphology/stats/"
setwd(dir)

knitr::opts_chunk$set(echo = TRUE)


## -------------------------------------------------------------------------------------------------------------------------
source_path = "~/Desktop/git_repositories/SBB-dispersal/avbernat_working_on/Rsrc/"

script_names = c("compare_models.R",
                 "regression_output.R",
                 "clean_morph_data2.R", # two functions: read_morph_data and remove_torn_wings
                 "AICprobabilities.R")

for (script in script_names) { 
  path = paste0(source_path, script)
  source(path) 
}

source("~/Desktop/git_repositories/SBB-dispersal/avbernat_working_on/RTsrc/ts_auxiliary_funs.R")


## -------------------------------------------------------------------------------------------------------------------------
data_list <- read_morph_data("data/allmorphology04.26.21-coors2.csv")
raw_data = data_list[[1]]
all_bugs = nrow(raw_data)
data_long = data_list[[2]] 

# Remove individuals with torn wings first
raw_data = remove_torn_wings(raw_data) # **don't need to remove torn wings for wing morph analysis
data_long = remove_torn_wings(data_long) # **don't need to remove torn wings for wing morph analysis
clean_bugs = nrow(raw_data)

cat("number of bugs with torn wings:", all_bugs - clean_bugs, "\n\n")


## -------------------------------------------------------------------------------------------------------------------------
na.omit(unique(raw_data$datetime))
# equation: +-2/sqrt(T) where T is the number of obs
n=length(na.omit(unique(raw_data$datetime)))
2/sqrt(n)


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates
d = raw_data %>%
  filter(!is.na(wing), !is.na(datetime))

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing", cat_var="datetime", func="mean")
wing_avg = ts_cols[[1]]
date = ts_cols[[2]]

# events
FL_major_hurr = c("Sep 2017 10", "Oct 2018 10") # Irma (s), Michael (n)
hurr_dates = as.Date(FL_major_hurr, "%b %Y %d")

events <- xts(c("Irma (S. FL)", "Michael (N. Fl)"), 
              hurr_dates)


## -------------------------------------------------------------------------------------------------------------------------
plot_consecutive_yrs(wing_avg, "wing length (mm)") # not strong; the points are very dispersed


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------
wing_mm = xts(wing_avg, date)
colnames(wing_mm) <- "wing"
check_stationarity(wing_mm) # this is stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
detrend(wing_mm) # this is not stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
dedrift(wing_mm) # this is not stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
dedriftrend(wing_mm) # this is not stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates
d = data_long %>%
  filter(!is.na(wing2body), !is.na(datetime))

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing2body", cat_var="datetime", func="mean")
ratio_avg = ts_cols[[1]]
ratio_avg = ratio_avg[!is.na(ratio_avg)]
date = ts_cols[[2]]


## -------------------------------------------------------------------------------------------------------------------------
plot_consecutive_yrs(ratio_avg, "wing2body") # weak 


## -------------------------------------------------------------------------------------------------------------------------
ratio = xts(ratio_avg, date)
colnames(ratio) <- "wing2body"
check_stationarity(ratio) # this is not stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
#dedrift(ratio) # this is not stationary
detrend(ratio) # this is stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
dedriftrend(ratio) # this is stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates and wing morph (S=0, L=1)
d = raw_data %>%
  filter(!is.na(wing_morph_binom), !is.na(datetime))

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing_morph_binom", cat_var="datetime", func="mean")
freq_avg = ts_cols[[1]]
date = ts_cols[[2]]


## -------------------------------------------------------------------------------------------------------------------------
plot_consecutive_yrs(freq_avg, "morph freq") # weak


## -------------------------------------------------------------------------------------------------------------------------
morph = xts(freq_avg, date)
colnames(morph) <- "wing_morph_freq"
check_stationarity(morph) # this is not stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
#detrend(morph) # this is not stationary
#dedrift(morph) # this is not stationary
dedriftrend(morph) # this is stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
females = raw_data[raw_data$sex=="F",]
males = raw_data[raw_data$sex=="M",]


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates
d = females %>%
  filter(!is.na(wing), !is.na(datetime))

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing", cat_var="datetime", func="mean")
wing_avg = ts_cols[[1]]
date = ts_cols[[2]]


## -------------------------------------------------------------------------------------------------------------------------
plot_consecutive_yrs(wing_avg, "wing length (mm)") 


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------
wing_mmf = xts(wing_avg, date)
colnames(wing_mmf) <- "wing"

check_stationarity(wing_mmf) # this is stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
#detrend(wing_mmf) # this is not stationary
#dedrift(wing_mmf) # this is not stationary


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates
d = data_long %>%
  filter(!is.na(wing2body), !is.na(datetime))

d = d[d$sex=="F",]

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing2body", cat_var="datetime", func="mean")
ratio_avg = ts_cols[[1]]
ratio_avg = ratio_avg[!is.na(ratio_avg)]
date = ts_cols[[2]]


## -------------------------------------------------------------------------------------------------------------------------
ratiof = xts(ratio_avg, date)
colnames(ratiof) <- "wing2body"

check_stationarity(ratiof) # this is not stationary
addEventLines(events, pos=2, srt=90, col="red")
auto.arima(ratiof, stepwise=FALSE, approximation=FALSE)


## -------------------------------------------------------------------------------------------------------------------------
#dedrift(ratiof) # not stationary
detrend(ratiof) # stationary
addEventLines(events, pos=2, srt=90, col="red") 
dedriftrend(ratiof) # stationary
addEventLines(events, pos=2, srt=90, col="red") 


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates and wing morph (S=0, L=1)
d = raw_data %>%
  filter(!is.na(wing_morph_binom), !is.na(datetime))

d = d[d$sex=="F",]

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing_morph_binom", cat_var="datetime", func="mean")
freq_avg = ts_cols[[1]]
date = ts_cols[[2]]


## -------------------------------------------------------------------------------------------------------------------------
morphf = xts(freq_avg, date)
colnames(morphf) <- "wing_morph_freq"

check_stationarity(morphf) # not stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
detrend(morphf) # stationary
addEventLines(events, pos=2, srt=90, col="red")
dedriftrend(morphf) # stationary
addEventLines(events, pos=2, srt=90, col="red")
dedrift(morphf) # stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates
d = males %>%
  filter(!is.na(wing), !is.na(datetime))

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing", cat_var="datetime", func="mean")
wing_avg = ts_cols[[1]]
date = ts_cols[[2]]


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------
wing_mmm = xts(wing_avg, date)
colnames(wing_mmm) <- "wing"
check_stationarity(wing_mmm) # not stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
dedrift(wing_mmm) # stationary
addEventLines(events, pos=2, srt=90, col="red")
#detrend(wing_mmm) # not stationary
#addEventLines(events, pos=2, srt=90, col="red")
#dedriftrend(wing_mm) # not stationary
#addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates
d = data_long %>%
  filter(!is.na(wing2body), !is.na(datetime))

d = d[d$sex=="M",]

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing2body", cat_var="datetime", func="mean")
ratio_avg = ts_cols[[1]]
ratio_avg = ratio_avg[!is.na(ratio_avg)]
date = ts_cols[[2]]


## -------------------------------------------------------------------------------------------------------------------------
ratiom = xts(ratio_avg, date)
colnames(ratiom) <- "wing2body"
check_stationarity(ratiom) # not stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
dedrift(ratiom) # stationary 
addEventLines(events, pos=2, srt=90, col="red")
#dedriftrend(ratiom) # not stationary
#addEventLines(events, pos=2, srt=90, col="red") # the same  as detrend
#detrend(ratiom) # not stationary


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates and wing morph (S=0, L=1)
d = raw_data %>%
  filter(!is.na(wing_morph_binom), !is.na(datetime))

d = d[d$sex=="M",]

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing_morph_binom", cat_var="datetime", func="mean")
freq_avg = ts_cols[[1]]
date = ts_cols[[2]]


## -------------------------------------------------------------------------------------------------------------------------
morphm = xts(freq_avg, date)
colnames(morphm) <- "wing_morph_freq"
check_stationarity(morphm) # not stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
#dedrift(morphm) # not stationary
#detrend(morphm) # not stationary
#dedriftrend(morphm) # not stationary
#addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
BV = raw_data[raw_data$pophost == "C.corindum",]
GRT = raw_data[raw_data$pophost == "K.elegans",]


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates
d = BV %>%
  filter(!is.na(wing), !is.na(datetime))

ts_cols = clean_for_ts(d, contin_var="wing", cat_var="datetime", func="mean")
wing_avg = ts_cols[[1]]
date = ts_cols[[2]]


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------
wing_BV = xts(wing_avg, date)
colnames(wing_BV) <- "wing"
check_stationarity(wing_BV) # stationary | looks like the ACF and PACF are significant?
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
#detrend(wing_BV) # stationary
#dedriftrend(wing_BV) # stationary
#dedrift(wing_BV) # stationary
#addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates
d = data_long %>%
  filter(!is.na(wing2body), !is.na(datetime))

d = d[d$pophost=="C.corindum",]

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing2body", cat_var="datetime", func="mean")
ratio_avg = ts_cols[[1]]
ratio_avg = ratio_avg[!is.na(ratio_avg)]
date = ts_cols[[2]]


## -------------------------------------------------------------------------------------------------------------------------
ratioBV = xts(ratio_avg, date)
colnames(ratioBV) <- "wing2body"
check_stationarity(ratioBV) # not stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
#dedrift(ratioBV) # not stationary
detrend(ratioBV) # stationary
addEventLines(events, pos=2, srt=90, col="red")
#dedriftrend(ratioBV) # stationary
#addEventLines(events, pos=2, srt=90, col="red") # same as detrend


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates and wing morph (S=0, L=1)
d = raw_data %>%
  filter(!is.na(wing_morph_binom), !is.na(datetime))

d = d[d$pophost=="C.corindum",]

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing_morph_binom", cat_var="datetime", func="mean")
freq_avg = ts_cols[[1]]
date = ts_cols[[2]]


## -------------------------------------------------------------------------------------------------------------------------
morphBV = xts(freq_avg, date)
colnames(morphBV) <- "wing_morph_freq"
check_stationarity(morphBV) # stationary | ACF and PACF are significant
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
#detrend(morphBV) # not stationary
#dedrift(morphBV) # stationary
#addEventLines(events, pos=2, srt=90, col="red")
#dedriftrend(morphBV) # not stationary


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates
d = GRT %>%
  filter(!is.na(wing), !is.na(datetime))

ts_cols = clean_for_ts(d, contin_var="wing", cat_var="datetime", func="mean")
wing_avg = ts_cols[[1]]
date = ts_cols[[2]]


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------
wing_GRT = xts(wing_avg, date)
colnames(wing_GRT) <- "wing"
check_stationarity(wing_GRT) # not stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
#detrend(wing_GRT) # not stationary
#dedrift(wing_GRT) # not stationary
#dedriftrend(wing_GRT) # not stationary
#addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates
d = data_long %>%
  filter(!is.na(wing2body), !is.na(datetime))

d = d[d$pophost=="K.elegans",]

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing2body", cat_var="datetime", func="mean")
ratio_avg = ts_cols[[1]]
ratio_avg = ratio_avg[!is.na(ratio_avg)]
date = ts_cols[[2]]


## -------------------------------------------------------------------------------------------------------------------------
ratioGRT = xts(ratio_avg, date)
colnames(ratioGRT) <- "wing2body"
check_stationarity(ratioGRT) # not stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
detrend(ratioGRT) # stationary
addEventLines(events, pos=2, srt=90, col="red")
dedrift(ratioGRT) # stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates and wing morph (S=0, L=1)
d = raw_data %>%
  filter(!is.na(wing_morph_binom), !is.na(datetime))

d = d[d$pophost=="K.elegans",]

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing_morph_binom", cat_var="datetime", func="mean")
freq_avg = ts_cols[[1]]
date = ts_cols[[2]]


## -------------------------------------------------------------------------------------------------------------------------
morphGRT = xts(freq_avg, date)
colnames(morphGRT) <- "wing_morph_freq"
check_stationarity(morphGRT) # not stationary
addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
#detrend(morphGRT) # not stationary
#dedriftrend(morphGRT) # not stationary
#dedrift(morphGRT) # not stationary
#addEventLines(events, pos=2, srt=90, col="red")


## -------------------------------------------------------------------------------------------------------------------------
a = 2 # drift / intercept - shifted
m = 100 # length of time series
x.wn.drift = a + rnorm(m, 0, 1) 
plot(x.wn.drift, type='l', col='steelblue2', lwd=2) 
abline(h=a, lty=2)

adf.test(x.wn.drift) 
Acf(x.wn.drift, main='')
auto.arima(x.wn.drift)


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates
d = raw_data %>%
  filter(!is.na(wing), !is.na(datetime))

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing", cat_var="datetime", func="mean")
wing_avg = ts_cols[[1]]
date = ts_cols[[2]]

# determine your dates
newdates = c("Dec 2020 01", "April 2021 01", "June 2021 01", "Oct 2021 01", "Jan 2022 01")
dates = as.Date(newdates, "%b %Y %d")
sim_dates = c(date, dates)

# simulate randomly your new winglengths
set.seed(120) # e.g. set.seed = 120, 
wl_sim = runif(length(newdates), min(wing_avg), max(wing_avg))
wings = c(wing_avg, wl_sim)

# create xts time series object
wing_sim = xts(wings, sim_dates)
colnames(wing_sim) <- "wing"
wing_sim

check_stationarity(wing_sim)
addEventLines(events, pos=2, srt=90, col="red")

