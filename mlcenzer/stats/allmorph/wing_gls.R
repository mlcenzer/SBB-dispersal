## ----setup, include=FALSE-------------------------------------------------------------------------------------------------
rm(list=ls())
dir = "~/Desktop/git_repositories/SBB-dispersal/avbernat_working_on/All_Morphology/stats/"
setwd(dir)

library(dplyr)
library(nlme)
library(zoo)
library(xts)
library(forecast)

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
# remove NA dates
d = raw_data %>%
  filter(!is.na(wing), !is.na(datetime))

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing", cat_var="datetime", func="mean")
wing_avg = ts_cols[[1]]
date = ts_cols[[2]]

beak_avg = tapply(X=raw_data[,"beak"], INDEX=raw_data[,"datetime"], FUN=mean, na.rm=T)

df = as.data.frame(cbind(wing_avg, beak_avg, date))


## -------------------------------------------------------------------------------------------------------------------------
m1 <- gls(wing_avg ~ beak_avg, 
          data = df,
          na.action = na.omit,
          method = "REML") 
summary(m1)  


## -------------------------------------------------------------------------------------------------------------------------
# Model with an AR1 correlation
# Figure 3.7
m2 <- gls(wing_avg ~ beak_avg, 
          correlation = corAR1(form=~date),
          data = df,
          na.action = na.omit)
summary(m2)


## -------------------------------------------------------------------------------------------------------------------------
# Create a grid of covariate values
x = range(df$beak_avg, na.rm = TRUE)
data <- data.frame(beak_avg = seq(from = min(x), 
                               to = max(x), 
                               length = 10))
# Model with an AR1 correlation
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = df$beak_avg,
     y = df$wing_avg,
     xlab = "wing_avg (mm)",
     ylab = "beak_avg (mm)",
     pch = 16,
     type = "n")
         
#Add the fitted values     
pred1 <- predict(m1, newdata = data)
pred2 <- predict(m2, newdata = data)

lines(x = data$beak_avg, # without a correlation
      y = pred1,
      lwd = 1,
      lty=2,
      col = "black")
lines(x = data$beak_avg, # with a correlation
      y = pred2,
      col = "red",
      lty=2,
      lwd = 3)
text(x = df$beak_avg,y = df$wing_avg, df$date, cex = 1)


## -------------------------------------------------------------------------------------------------------------------------
# Get only bugs with long wings
data_long<-raw_data[raw_data$w_morph=="L",]

# Calculate wing2body ratio for bugs with long wings 
data_long$wing2body <- data_long$wing/as.numeric(data_long$body)


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates
d = data_long %>%
  filter(!is.na(wing2body), !is.na(datetime))

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing2body", cat_var="datetime", func="mean")
ratio_avg = ts_cols[[1]]
ratio_avg = ratio_avg[!is.na(ratio_avg)]
date = ts_cols[[2]]

beak_avg = beak_avg[c(1,3:10)]

df = as.data.frame(cbind(ratio_avg, beak_avg, date))


## -------------------------------------------------------------------------------------------------------------------------
m3 <- gls(ratio_avg ~ beak_avg, 
          data = df,
          na.action = na.omit,
          method = "REML") 
summary(m3)  

# Model with an AR1 correlation
# Figure 3.7
m4 <- gls(ratio_avg ~ beak_avg, 
          correlation = corAR1(form=~date),
          data = df,
          na.action = na.omit)

summary(m4)


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates and wing morph (S=0, L=1)
d = raw_data %>%
  filter(!is.na(wing_morph_binom), !is.na(datetime))

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing_morph_binom", cat_var="datetime", func="mean")
freq_avg = ts_cols[[1]]
date = ts_cols[[2]]

beak_avg = tapply(X=raw_data[,"beak"], INDEX=raw_data[,"datetime"], FUN=mean, na.rm=T)

df = as.data.frame(cbind(freq_avg, beak_avg, date))


## -------------------------------------------------------------------------------------------------------------------------
m5 <- gls(freq_avg ~ beak_avg, 
          data = df,
          na.action = na.omit,
          method = "REML") 
summary(m5)  

# Model with an AR1 correlation
# Figure 3.7
m6 <- gls(freq_avg ~ beak_avg, 
          correlation = corAR1(form=~date),
          data = df,
          na.action = na.omit)

summary(m6) 


## -------------------------------------------------------------------------------------------------------------------------
females = raw_data[raw_data$sex=="F",]

# remove NA dates
d = females %>%
  filter(!is.na(wing), !is.na(datetime))

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing", cat_var="datetime", func="mean")
wing_avg = ts_cols[[1]]
date = ts_cols[[2]]

beak_avg = tapply(X=d[,"beak"], INDEX=d[,"datetime"], FUN=mean, na.rm=T)

df = as.data.frame(cbind(wing_avg, beak_avg, date))


## -------------------------------------------------------------------------------------------------------------------------
m7 <- gls(wing_avg ~ beak_avg, 
          data = df,
          na.action = na.omit,
          method = "REML") 
summary(m7)  

# Model with an AR1 correlation
# Figure 3.7
m8 <- gls(wing_avg ~ beak_avg, 
          correlation = corAR1(form=~date),
          data = df,
          na.action = na.omit)

summary(m8) 


## -------------------------------------------------------------------------------------------------------------------------
# Create a grid of covariate values
x = range(df$beak_avg, na.rm = TRUE)
data <- data.frame(beak_avg = seq(from = min(x), 
                               to = max(x), 
                               length = 10))
# Model with an AR1 correlation
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = df$beak_avg,
     y = df$wing_avg,
     xlab = "wing_avg (mm)",
     ylab = "beak_avg (mm)",
     pch = 16,
     type = "n")
         
#Add the fitted values     
pred1 <- predict(m7, newdata = data)
pred2 <- predict(m8, newdata = data)

lines(x = data$beak_avg, # without a correlation
      y = pred1,
      lwd = 1,
      lty=2,
      col = "black")
lines(x = data$beak_avg, # with a correlation
      y = pred2,
      col = "red",
      lty=2,
      lwd = 3)
text(x = df$beak_avg, y = df$wing_avg, df$date, cex = 1)


## -------------------------------------------------------------------------------------------------------------------------
# remove NA dates
d = raw_data %>%
  filter(!is.na(wing), !is.na(datetime))

# prep dataset for xts()
ts_cols = clean_for_ts(d, contin_var="wing", cat_var="datetime", func="mean")
wing_avg = ts_cols[[1]]
date = ts_cols[[2]]

wing_mm = xts(wing_avg, date)
colnames(wing_mm) <- "wing"
wing_mm


## -------------------------------------------------------------------------------------------------------------------------
# Example AR(1) model
# x has to be a "ts" object
mod.ar1 <- arima(x=wing_mm, order=c(1,0,0)) # change the 1 to other numbers to get it to be white noise 
Acf(residuals(mod.ar1), main='')


## -------------------------------------------------------------------------------------------------------------------------
#auto.arima(wing_mm)
auto.arima(wing_mm, stepwise=FALSE, approximation=FALSE) # ours yield the same result

