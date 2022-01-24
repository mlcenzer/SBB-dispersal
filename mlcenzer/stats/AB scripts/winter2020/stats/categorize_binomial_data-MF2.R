# Categorizing Binomial Data Function w/ Multiple Factors (Continuous and Binomial Factors)

#########################################################################################################
# FUNCTION: group_calculations | For which explanatory variable you want to group into categories, the 
# number of cases and sample proportion of flight for each category is calculated
# INPUT: binned data as a Data Frame.
# OUTPUT: returns a vector with the number of cases (integer) and sample proportion of binomial sucesses (numeric).
#########################################################################################################

group_calculations <- function(df_binned) {
  successes <- sum(df_binned[2], na.rm = TRUE)
  n_cases <- nrow(df_binned)
  sample_prop <- successes / n_cases
  # if (n_cases == 0) {
  #   sample_prop <- NA
  # }
  return(c(n_cases, sample_prop))
}

#########################################################################################################
# FUNCTION: categorize_data_MF | Group explanatory values into categories and calculate the sample 
# of proportion of flight for each category
# INPUT: (data as a Data Frame, continuous explanatory variable as a character, first binomial explanatory 
# variable as a character, second binomial explanatory as a character, response variable as a character, 
# interval as an integer for which to group the explanatory variable (e.g. 30 minute intervals), 
# starting value of the first interval (e.g. starts at 0 min), final value of the first interval 
# (e.g. ends at 30 min))
# OUTPUT: return a Data Frame with the sample proportion of the binomial response variable by the grouped 
# explanatory variables
#########################################################################################################

categorize_data_MF <- function(data, c_exp_var, b_exp_var1, response_var, bin_interval, interval, initial_val) {
  
  df <- select(data, c_exp_var, response_var, b_exp_var1)
  df <- df[order(df[c_exp_var]),]
  
  bins <- round((max(df[c_exp_var]) - min(df[c_exp_var])) / bin_interval)
  
  bin_df_cats <- sort(unique(df[b_exp_var1])[1:3,1]) # ascending order 
  n_combinations= length(bin_df_cats)
  n_col= length(df)  
  iterations = bins * n_combinations
  variables = + length(df) + 1
  matrix <- matrix(ncol=variables, nrow=iterations)
  
  i <- initial_val
  f <- (initial_val + interval)
  row <- 0
  for (b in 1:bins) {

    bin_df <- as.data.frame(matrix(df[df[c_exp_var] < f & df[c_exp_var] >= i], ncol=n_col))
    colnames(bin_df) <- c(c_exp_var, response_var, b_exp_var1)
    bin_len <- length(bin_df)

    if (nrow(bin_df) == 0) {
      i <- i + interval
      f <- f + interval
      next
    }
    
    bin_df_cats <- sort(unique(df[b_exp_var1])[1:3,1]) # ascending order 
    
    binned_df_comb1 <- as.data.frame(matrix(bin_df[bin_df[b_exp_var1] == bin_df_cats[1] | bin_df[b_exp_var1] == bin_df_cats[2]], ncol=bin_len))
    binned_df_comb2 <- as.data.frame(matrix(bin_df[bin_df[b_exp_var1] == bin_df_cats[3]], ncol=bin_len))

    binned_df_combs <- list(binned_df_comb1, binned_df_comb2)
  
    for (c in seq(1:length(binned_df_combs))) {
      
      row <- row + 1
      
      binned_df_comb <- binned_df_combs[[c]]
      colnames(binned_df_comb) <- c(c_exp_var, response_var, b_exp_var1)
      if (nrow(binned_df_comb) == 0) {
        next
      }

      calculations <- group_calculations(binned_df_comb)
      n_cases <- calculations[1]
      sample_prop <- calculations[2]
      # if (is.na(sample_prop)) { # Skips combinations where there were no cases in a given group. Comment out section to see all cases.
      #   next
      # }      
      
      exp_var1 <- unique(binned_df_comb[b_exp_var1])[1,1] # this could definitely be rewritten for more legible code sake

      cat("Mass", format(round(f, 2), nsmall = 2), end="\t")
      cat("Flight Count:", exp_var1, end="\t")
      cat("Cases:", n_cases, end="\t")
      cat("Sample Proportion:", sample_prop, end="\n")

      matrix[row,1] <- f
      matrix[row,2] <- n_cases
      matrix[row,3] <- round(sample_prop,3)
      matrix[row,4] <- exp_var1

    }
    i <- i + interval
    f <- f + interval
  }

  categorized_data <- as.data.frame(matrix)
  colnames(categorized_data) <- c(c_exp_var, "n_cases", 
                                  "sample_prop", b_exp_var1)
  cat_data <- categorized_data[rowSums(is.na(categorized_data)) != ncol(categorized_data),]
  
  return(cat_data)
}
#cat_data_final <- cat_data$value # spits out a dataframe, otherwise you get a list
