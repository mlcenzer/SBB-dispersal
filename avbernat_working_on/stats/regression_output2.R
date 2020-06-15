# Formats the output of regressions into a quickly legible output 

tidy_regression_bw <- function(fit) {
  
  n_col <- length(as.data.frame(summary(fit)$coefficients))
  pvals <- summary(fit)$coefficients[,n_col]
  coeffs <- summary(fit)$coefficients[,1] 
  AIC <- summary(fit)$aic # for multivariate modeling
  aic <- summary(fit)$AICtab[1] # for mixed effect modeling
  name <- as.character(summary(fit)$call)
  cat(name, end="\n")
  cat("AIC: ", AIC, aic, end="\n")
  
  table <- as.data.frame(cbind(coeffs, pvals))
  table <- cbind(fixed_effect = rownames(table), table)
  rownames(table) <- 1:nrow(table)
  as.character(table$fixed_effect[1])
  
  # Make all elements of a character vector the same length (for tidy printing purposes)
  table$fixed_effect <- as.character(table$fixed_effect)
  table$fixed_effect <- gsub("\\s", " ", format(table$fixed_effect , width=max(nchar(table$fixed_effect))))
  
  for (i in 1:nrow(table)) {
    
    coeff <- round(table$coeffs[i], 7)
    
    if (coeff < 0) {
      cat(table$fixed_effect[i], "\t")
      cat("coeff: ", paste0(coeff,"\t"))
      if (n_col < 4) {
        cat("tval: ", table$pvals[i])
        cat('\n')
      }
      else {
        if (table$pvals[i] < 0.05){
          cat("Pr(>|t|): ", table$pvals[i], "*")} 
        else {cat("Pr(>|t|): ", table$pvals[i])}
        cat('\n')
      }
    }
    
    if (coeff > 0) {
      cat(table$fixed_effect[i], "\t")
      cat("coeff: ", paste0(coeff,"\t"))
      if (n_col < 4) {
        cat("tval: ", table$pvals[i])
        cat('\n')
      }
      else {
        if (table$pvals[i] < 0.05){
          cat("Pr(>|t|): ", table$pvals[i], "*")} 
        else {cat("Pr(>|t|): ", table$pvals[i])}
        cat('\n')
      }
    }
  }
  
}