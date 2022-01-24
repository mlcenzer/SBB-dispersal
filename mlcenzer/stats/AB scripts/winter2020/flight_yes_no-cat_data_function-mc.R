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