# Categorizing Binomial Data Function w/ Multiple Factors (Continuous and Binomial Factors)

#########################################################################################################
# FUNCTION: group_calculations | For which explanatory variable you want to group into categories, the 
# number of cases and sample proportion of flight for each category is calculated
# INPUT: binned data as a Data Frame and the response variable whose sucess proportions you want to calculate.
# OUTPUT: returns a vector with the number of cases (integer) and sample proportion of binomial sucesses (numeric).
#########################################################################################################

group_calculations <- function(df_binned) {
  successes <- sum(df_binned$flew_b, na.rm = TRUE)
  n_cases <- nrow(df_binned)
  sample_prop <- successes / n_cases
  if (n_cases == 0) {
    sample_prop <- NA
  }
  return(c(n_cases, sample_prop))
}

#########################################################################################################
# CODE: categorizes data | Group explanatory values into categories and calculate the sample 
# of proportion of flight for each category
# INPUT: (data as a Data Frame, explanatory variable as a character, response variable as a character, 
# interval as an integer for which to group the explanatory variable (e.g. 30 minute intervals), 
# starting value of the first interval (e.g. starts at 0 min), final value of the first interval 
# (e.g. ends at 30 min))
# OUTPUT: return a Data Frame with the sample proportion of the binomial response variable by the grouped 
# explanatory variable
#########################################################################################################

df <- select(data_mass, mass, flew_b, sex_c, host_c)
df <- df[order(df$mass),]

#mass_interval <- 0.010
bins <- round(max(df$mass) / mass_interval)

binomial_exp_vars <- df[3:4]
n_combinations = nrow(unique(binomial_exp_vars))

iterations = bins * n_combinations
variables = + length(df) + 1
matrix <- matrix(ncol=variables, nrow=iterations)

interval <- 0.010
i <- 0.015
f <- 0.025
row <- 0
for (b in 1:bins) {
  
  binned_df <- filter(df, mass < f & mass >= i)
  if (nrow(binned_df) == 0) {
    i <- i + interval
    f <- f + interval
    next
  }
  
  binned_df_comb1 <- filter(binned_df, sex_c == 1 & host_c == 1 )
  binned_df_comb2 <- filter(binned_df, sex_c == -1 & host_c == 1 )
  binned_df_comb3 <- filter(binned_df, sex_c == 1 & host_c == -1 )  
  binned_df_comb4 <- filter(binned_df, sex_c == -1 & host_c == -1 )
  
  binned_df_combs <- list(binned_df_comb1, binned_df_comb2, binned_df_comb3, binned_df_comb4)
  
  for (c in seq(1:n_combinations)) {
    
    row <- row + 1
    
    binned_df_comb <- binned_df_combs[[c]]
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
    
    sex <- unique(binned_df_comb$sex_c)
    if (sex == -1) {
      sex_b <- "F"}
    else if (sex == 1) {
      sex_b <- "M"}
    else if (is.na(sex)) {
      sex_b <- NA}
    host_plant <- unique(binned_df_comb$host_c)
    
    cat("Mass", f, end="\t")
    cat("Sex:", sex_b, end="\t")
    cat("Host Plant:", host_plant, end="\t")
    cat("Cases:", n_cases, end="\t")
    cat("Sample Proportion:", sample_prop, end="\n")
    
    #print(binned_df_comb)
    matrix[row,1] <- f
    matrix[row,2] <- n_cases
    matrix[row,3] <- round(sample_prop,3)
    matrix[row,4] <- sex
    matrix[row,5] <- host_plant
  }
  i <- i + interval
  f <- f + interval
}

matrix
categorized_data <- as.data.frame(matrix)
colnames(categorized_data) <- c("mass", "n_cases", 
                                "sample_prop", "sex", "host_plant")
cat_data <- categorized_data[rowSums(is.na(categorized_data)) != ncol(categorized_data),]
#cat_data_final <- cat_data$value # spits out a dataframe, otherwise you get a list
