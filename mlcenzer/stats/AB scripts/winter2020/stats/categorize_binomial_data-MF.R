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
  if (n_cases == 0) {
    sample_prop <- NA
  }
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

categorize_data_MF <- function(data, c_exp_var, b_exp_var1, b_exp_var2, response_var, interval, initial_val, final_val) {
  
  df <- select(data, c_exp_var, response_var, b_exp_var1, b_exp_var2)
  df <- df[order(df[c_exp_var]),]
  
  bins <- round(max(df[c_exp_var]) / interval)
  
  binomial_exp_vars <- df[3:4]
  n_combinations = nrow(unique(binomial_exp_vars))
  n_col= length(df)  
  iterations = bins * n_combinations
  variables = + length(df) + 1
  matrix <- matrix(ncol=variables, nrow=iterations)
  
  i <- initial_val
  f <- final_val
  row <- 0
  for (b in 1:bins) {
    
    bin_df <- as.data.frame(matrix(df[df[c_exp_var] < f & df[c_exp_var] >= i], ncol=n_col))
    colnames(bin_df) <- c(c_exp_var, response_var, b_exp_var1, b_exp_var2)
    bin_len <- length(bin_df)

    if (nrow(bin_df) == 0) {
      i <- i + interval
      f <- f + interval
      next
    }
  
    binned_df_comb1 <- as.data.frame(matrix(bin_df[bin_df[b_exp_var1] == 1 & bin_df[b_exp_var2] == 1], ncol=bin_len))
    binned_df_comb2 <- as.data.frame(matrix(bin_df[bin_df[b_exp_var1] == -1 & bin_df[b_exp_var2] == 1], ncol=bin_len))
    binned_df_comb3 <- as.data.frame(matrix(bin_df[bin_df[b_exp_var1] == 1 & bin_df[b_exp_var2] == -1], ncol=bin_len))
    binned_df_comb4 <- as.data.frame(matrix(bin_df[bin_df[b_exp_var1] == -1 & bin_df[b_exp_var2] == -1], ncol=bin_len))
    
    binned_df_combs <- list(binned_df_comb1, binned_df_comb2, binned_df_comb3, binned_df_comb4)

    for (c in seq(1:n_combinations)) {
      
      row <- row + 1
      
      binned_df_comb <- binned_df_combs[[c]]
      colnames(binned_df_comb) <- c(c_exp_var, response_var, b_exp_var1, b_exp_var2)
      
      if (nrow(binned_df_comb) == 0) {
        i <- i + interval
        f <- f + interval
        next
      }
      calculations <- group_calculations(binned_df_comb)
      n_cases <- calculations[1]
      sample_prop <- calculations[2]
      if (is.na(sample_prop)) { # Skips combinations where there were no cases in a given group. Comment out section to see all cases.
        next
      }      
      
      sex <- unique(binned_df_comb[b_exp_var1])[1,1] # this could definitely be rewritten for more legible code sake
      if (sex == -1) {
        sex_b <- "F"}
      else if (sex == 1) {
        sex_b <- "M"}
      else if (is.na(sex)) {
        sex_b <- NA}
      host_plant <- as.matrix(unique(binned_df_comb[b_exp_var2]))[1,1] 
      
      cat("Mass", f, end="\t")
      cat("Sex:", sex_b, end="\t")
      cat("Host Plant:", host_plant, end="\t")
      cat("Cases:", n_cases, end="\t")
      cat("Sample Proportion:", sample_prop, end="\n")

      matrix[row,1] <- f
      matrix[row,2] <- n_cases
      matrix[row,3] <- round(sample_prop,3)
      matrix[row,4] <- sex
      matrix[row,5] <- host_plant
    }
    i <- i + interval
    f <- f + interval
  }

  categorized_data <- as.data.frame(matrix)
  colnames(categorized_data) <- c(c_exp_var, "n_cases", 
                                  "sample_prop", b_exp_var1, b_exp_var2)
  cat_data <- categorized_data[rowSums(is.na(categorized_data)) != ncol(categorized_data),]
  
  return(cat_data)
}
#cat_data_final <- cat_data$value # spits out a dataframe, otherwise you get a list
