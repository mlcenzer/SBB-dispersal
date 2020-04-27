##Visualizing relationship with start time

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

# Reading the data
source('cleandata-Winter2020-mc-baseR.R')
data_all<-read_data("data/complete_flight_data-Winter2020-edited.csv")

# Plotting Data

#```{r}
# Categorizing Binomial Data Function

#########################################################################################################
# FUNCTION: categorize_data | Group explanatory values into categories and calculate the sample of proportion of flight for each category
# INPUT: (data as a Data Frame, explanatory variable as a character, response variable as a character, interval as an integer for which to group the explanatory variable (e.g. 30 minute intervals), starting value of the first interval (e.g. starts at 0 min), final value of the first interval (e.g. ends at 30 min))
# OUTPUT: return a Data Frame with the sample proportion of the binomial response variable by the grouped explanatory variable
#########################################################################################################
###It sounds a lot like aggregate.
categorize_data <- function(data, explanatory_var, response_var, interval, initial_val, final_val) {
  #browser()
  df <- select(data, explanatory_var, response_var)
  df <- df[order(df[explanatory_var]),]

  bins <- round(max(df[explanatory_var]) / interval)

  n_col= length(df)
  iterations = bins
  variables = n_col + 1
  matrix <- matrix(ncol=variables, nrow=iterations)

  i <- initial_val
  f <- final_val
  
  for (b in 1:bins) {
    
    binned_df <- as.data.frame(matrix(df[df[explanatory_var] <f & df[explanatory_var] > i], ncol=n_col))
    if (nrow(binned_df) == 0) {
      i <- i + 0.1
      f <- f + 0.1
      next
    }
    flew_n <- binned_df[-1]
    successes <- sum(flew_n)
    n_cases <- nrow(binned_df)
    sample_prop <- successes / n_cases

    
    matrix[b,1] <- f
    matrix[b,2] <- n_cases
    matrix[b,3] <- sample_prop
    
    cat(explanatory_var, f, end="\t")
    cat("Cases:", n_cases, end="\t")
    cat("Sample Proportion:", sample_prop, end="\n")
    
    i <- i + interval
    f <- f + interval
  }
  
  categorized_data <- as.data.frame(matrix)
  colnames(categorized_data) <- c(explanatory_var, "Number of Cases", "Sample Proportion")
  
  return(categorized_data)
}
#```

#```{r}
# ALL DATA: Observed proportions of yes flew by minutes from incubation start 
cat_data1 <- categorize_data(data_all, "min_from_IncStart", "flew_b", 30, 0, 30)

##compare to aggregate

####some of these times are not entered in military time, I don't think these bugs started flying at 3am? Edited into new edited data file.

data_all$bins<-ceiling(data_all$min_from_IncStart/30)*30
summary_1<-aggregate(flew_b~bins, data=data_all, FUN=mean)
summary_1$N<-aggregate(flew_b~bins, data=data_all, FUN=length)$flew_b
###I see, this has a very specific kind of rounding scheme that cannot be reproduced exactly with round, ceiling, or floor

# ALL DATA: Observed proportions of yes flew by minutes from trial start
cat_data2 <- categorize_data(data_all, "minute_duration", "flew_b", 30, 0, 30)
#```

#```{r fig.width=7, fig.height=2.5}
# Plotting the categorized data

#p1 <- as.grob(expression(
plot(as.matrix(cat_data1[1]), 
     as.matrix(cat_data1[3]),
     ylab="Sample Proportion of Yes Flew", xlab="Minutes From Incubtation Start (m) ", main="Observed proportions of yes flew by minutes from incubation start")
#))

#p2 <- as.grob(expression(
  plot(as.matrix(cat_data2[1]), 
     as.matrix(cat_data2[3]),
     ylab="Sample Proportion of Yes Flew", xlab="Minutes From Trial Start (m)", main="Observed proportions of yes flew by minutes from trial start") %>%
  abline(v=120, col="red")
#))

#grid.arrange(p1,p2, ncol=2)
#```

#```{r}
# Filter out those that didn't fly.
all_flew <- filter(data_all, flew_b == 1)

# What is happening for those who flew?
x = chron(times = all_flew$time_start)
y = all_flew$total_duration # seconds 
y = y / (24*60*60) # proportion of the day they flew

fit <- lm(y~x, data=all_flew)
summary(fit) # significant effect

cf <- round(coef(fit), 2)  ## rounded coefficients for better output
eq <- paste0("portion_flew = ", cf[1],
             ifelse(sign(cf[2])==1, " + ", " - "), 
             abs(cf[2]), "*time_start ") ## sign check to avoid having plus followed by minus for negative coefficients

plot(x,y, xlab= "Starting Time", ylab="Proportion of 24-H Day Spent Flying")
  abline(coef(fit)[1:2], col="blue") # Alternative: lines(x, fitted(fit), col="blue")
mtext(eq, 3, line=-2, side=3)







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



########This next section won't work because all_of() doesn't exist in the version of this package I'm able to run.
## Plotting for Trial 1 

#```{r}
plot(data_T1$flew_b, data_T1$mass)
#```

#```{r}
# Missing mass for some (3 NA)
missing_mass <- subset(data_all, is.na(data_all$mass))
# 339, 48, and 342 have no mass and were tested on the same date
data_mass <- setdiff(data_all, missing_mass)

## ALL DATA: Observed proportions of yes flew by mass
cat_data3 <- categorize_data(data_mass, all_of("mass"), all_of("flew_b"),  0.010, 0.015, 0.025) 

## TRIAL 1 DATA: Observed proportions of yes flew by mass
missing_mass <- subset(data_T1, is.na(data_all$mass))
data_mass_T1 <- setdiff(data_T1, missing_mass)
cat_data4 <- categorize_data(data_mass_T1, all_of("mass"), all_of("flew_b"),  0.010, 0.015, 0.025) 

#```

#```{r}
# ALL DATA
all_fit <-lm(flew_b~mass, data=data_mass) 
coeff <- coefficients(summary(all_fit))

eq <- paste0("portion_flew = ", round(coeff[1],3),
             ifelse(sign(coeff[2])==1, " + ", " - "), 
             abs(round(coeff[2],3)), "*mass")

plot(as.matrix(cat_data3[1]), 
     as.matrix(cat_data3[3]),
     ylab="Sample Proportion of Yes Flew", 
     xlab="Mass (g)", 
     main="All Data: Observed proportions of yes flew by mass")
abline(coeff[1], coeff[2], col="blue")
mtext(eq, side=4)

# TRIAL 1 
fit1<-lm(flew_b~mass, data=data_mass_T1) 
coeff <- coefficients(summary(fit1))

eq <- paste0("portion_flew = ", round(coeff[1],3),
             ifelse(sign(coeff[2])==1, " + ", " - "), 
             abs(round(coeff[2],3)), "*mass")

plot(as.matrix(cat_data4[1]), 
     as.matrix(cat_data4[3]),
     ylab="Sample Proportion of Yes Flew", 
     xlab="Mass (g)", 
     main="Trial 1: Observed proportions of yes flew by mass")
abline(coeff[1], coeff[2], col="blue")
mtext(eq, side=4)

