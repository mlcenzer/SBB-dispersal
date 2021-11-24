# Categorizing Binomial Data Function w/ 1 Continuous Factor

#########################################################################################################
# FUNCTION: categorize_data | Group explanatory values into categories and calculate the sample 
# of proportion of flight for each category
# INPUT: (data as a Data Frame, explanatory variable as a character, response variable as a character, 
# bin interval as a numeric for which to create your number of bins,
# interval as a numeric for which to group the explanatory variable (e.g. 30 minute intervals), 
# starting value of the first interval (e.g. starts at 0 min)
# OUTPUT: return a Data Frame with the sample proportion of the binomial response variable by the grouped 
# explanatory variable
#########################################################################################################

categorize_data3 <- function(data, explanatory_var, response_var, bin_interval, interval, initial_val) {
  df<- data[,c(explanatory_var, response_var)]
  df <- df[order(df[explanatory_var]),]
  
  bins <- round((max(df[explanatory_var]) - min(df[explanatory_var])) / bin_interval)

  n_col= length(df)
  iterations = bins
  variables = n_col + 1
  matrix <- matrix(ncol=variables, nrow=iterations)
  
  i <- initial_val
  f <- (initial_val + interval)
  
  for (b in 1:bins) {

    binned_df <- as.data.frame(matrix(df[df[explanatory_var] < f & df[explanatory_var] >= i], ncol=n_col))
    if (nrow(binned_df) == 0) {
      i <- i + interval
      f <- f + interval
      next
    }
    
    flew_n <- binned_df[2] # response variable
    successes <- sum(flew_n)
    n_cases <- nrow(binned_df)
    sample_prop <- successes / n_cases
    if (is.na(sample_prop)) {
      next
    }
    
    matrix[b,1] <- f
    matrix[b,2] <- n_cases
    matrix[b,3] <- sample_prop
    
    cat(explanatory_var, format(round(f, 2), nsmall = 2), end="\t")
    cat("Cases:", n_cases, end="\t")
    cat("Sample Proportion:", sample_prop, end="\n")
    
    i <- i + interval
    f <- f + interval
  }
  
  categorized_data <- as.data.frame(matrix)
  colnames(categorized_data) <- c(explanatory_var, "n_cases", "sample_prop")
  categorized_data <- categorized_data[rowSums(is.na(categorized_data)) != ncol(categorized_data),]
  
  return(categorized_data)
}