# functions for multinomial logit modeling

calculate_P2 = function(model, var1, var2, print_table=TRUE) {
  # compute p-values using Wald tests (i.e. z-tests)
  s <- summary(model) 
  z <- s$coefficients/s$standard.errors 
  wald <- z^2
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  
  # summary table (similar to lm, glm, or lmer function)
  model_table <- cbind(s$coefficients, c(s$edf, s$edf, s$edf), s$standard.errors[,1:3], z[,1:3], wald[,1:3], p[,1:3])
  colnames(model_table) <- c("(Intercept)", var1, var2,"DF", 
                             "SEi", "SE1", "SE2", "zi", "z1", "z2", 
                             "waldi", "wald1","wald2","Pi > |z|", "P1 > |z|", "P2 > |z|")
  if (print_table) {
    cat("\n", "AIC: ", s$AIC, "\n") 
    print(round(model_table,3))
  }
  return(model_table)
}

calculate_P3 = function(m, print_table=TRUE) {
  
  # compute p-values using Wald tests (i.e. z-tests)
  s <- summary(m) 
  z <- s$coefficients/s$standard.errors 
  wald <- z^2
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  
  # summary table (similar to lm, glm, or lmer function)
  model_table <- cbind(s$coefficients, c(s$edf, s$edf, s$edf), 
                       s$standard.errors[,1:4], z[,1:4], wald[,1:4], p[,1:4])
  colnames(model_table) <- c("(Intercept)", "mass %", "sex","wing2body","DF", "SEi",
                             "SE1", "SE2", "SE3","zi", "z1", "z2", "z3",
                             "waldi", "wald1","wald2", "wald3", "Pi>|z|", "P1>|z|", "P2>|z|", "P3>|z|")
  if (print_table){
    cat("\n", "AIC: ", s$AIC, "\n") 
    print(round(model_table,3))    
  }
  return(model_table)
}




get_prediction_eq = function(tb, table_rowA, table_rowB, var_lab1, var_lab2, var_lab3, 
                             log_lab, title) { 
  I = (tb[table_rowA,1] - tb[table_rowB,1])
  M = (tb[table_rowA,2] - tb[table_rowB,2])
  S = (tb[table_rowA,3] - tb[table_rowB,3])
  W = (tb[table_rowA,4] - tb[table_rowB,4])
  EQ = paste0("      ", log_lab, round(I, 2), " + ", round(M,2), var_lab1, " + ", 
              round(S, 2), var_lab2, " + ", round(W, 2), var_lab3, "\n")
  
  print(title)
  cat(EQ)
  
  return(EQ)
}


get_prediction_eqf = function(tb, table_rowA, table_rowB, var_lab1, var_lab2, 
                             log_lab, title) { 
  I = (tb[table_rowA,1] - tb[table_rowB,1])
  M = (tb[table_rowA,2] - tb[table_rowB,2])
  E = (tb[table_rowA,3] - tb[table_rowB,3])
  EQ = paste0(log_lab, round(I, 2), " + ", round(M,2), var_lab1, " + ", round(E, 2), 
              var_lab2)
  print(title)
  cat(EQ)
  
  return(EQ)
}


get_significant_models = function(val_num, effect_cat=2){
  baselines = -1:2
  pvalues = matrix(NA, nrow = 4, ncol = 4)
  effects = matrix(NA, nrow = 4, ncol = 4)
  r = 0
  model_list = list()
  for (i in baselines) {
    r = r + 1
    df$flight_case <- relevel(as.factor(df$flight_case), ref = as.character(i))
    model_table = run_multinom_model(df)
    model_list[[r]] = as.data.frame(model_table)
    
    eqs = rownames(model_table)
    c = 0
    for (e in eqs){
      c = c + 1
      e = as.integer(e) + 2 
      pvalues[r,e] = model_table[c,val_num]
      effects[r,e] = model_table[c, effect_cat]
    } 
  }
  colnames(pvalues) = baselines
  rownames(pvalues) = baselines 
  
  #plot
  par(mar=c(5.1, 4.1, 3.1, 4.1))
  plot(pvalues, ylab="Baselines", xlab="Equation", main="p-values", 
       na.col="white", col=rev(grey.colors(7)), digits=4, 
       breaks=c(0.05,0.10, 0.5, 0.8, 1),
       text.cell=list(col="white", cex=1), max.col=170, border=NA, fmt.cell='%.2f', fmt.key='%.2f')
  print(effects)
  return(model_list)
}

get_significant_modelsf = function(num_val, effect_cat=2){
  baselines = c(-1, 0, 2)
  pvalues = matrix(NA, nrow = 4, ncol = 4)
  effects = matrix(NA, nrow = 4, ncol = 4)
  r = 0
  model_list = list()
  for (b in baselines) {
    r = r + 1
    df$flight_case <- relevel(as.factor(df$flight_case), ref = as.character(b))
    model_table = run_multinom_model(df)
    model_list[[r]] = as.data.frame(model_table)
    
    eqs = rownames(model_table)
    c = 0
    for (e in eqs){
      if (r == 3) {
        r =4
      }
      c = c + 1
      e = as.integer(e) + 2 
      pvalues[r,e] = model_table[c,num_val]
      effects[r,e] = model_table[c, effect_cat]
    } 
  }
  colnames(pvalues) = c("T1 only", "neither trial", NA, "both trials")
  rownames(pvalues) = c("T1 only", "neither trial", NA, "both trials") 
  colnames(effects) = c("T1 only", "neither trial", NA, "both trials")
  rownames(effects) = c("T1 only", "neither trial", NA, "both trials") 
  #browser()
  #plot
  par(mar=c(5.1, 4.1, 3.1, 4.1))
  plot(pvalues, ylab="Baselines", xlab="Equation", main="p-values", 
       na.col="white", col=rev(grey.colors(7)), digits=4, 
       breaks=c(0.05,0.10, 0.5, 0.8, 1),
       text.cell=list(col="white", cex=1), max.col=170, border=NA, fmt.cell='%.2f', fmt.key='%.2f')#can repress these printing with fmt.cell=''
  
  return(model_list)
}

