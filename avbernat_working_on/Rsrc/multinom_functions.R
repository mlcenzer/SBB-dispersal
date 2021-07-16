# Functions for multinomial logit modeling

#### Calculate estimated parameters, standard errors, Wald test statistics, and p values.

calculate_P = function(model, print_table=TRUE) {
  # compute p-values using Wald tests (i.e. z-tests)
  s <- summary(model) 
  z <- s$coefficients/s$standard.errors 
  wald <- z^2
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  
  # summary table (similar to lm, glm, or lmer function)
  model_table <- cbind(s$coefficients, c(s$edf, s$edf, s$edf), s$standard.errors[,2], z[,2], wald[,2], p[,2])
  colnames(model_table) <- c("(Intercept)"," Estimate","DF", "Std. Err.", "z", "wald", "P > |z|")
  
  if (print_table) {
    cat("\n", "AIC: ", s$AIC, "\n")
    print(round(model_table,3))
  }
  return(model_table)
}

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

#### Get ML prediction equations.

prediction_equations = function(tb) {
  # input model_table
  # -1 / 1 | Flew in T1, not T2
  I = (tb[1,1] - tb[2,1])
  M = (tb[1,2] - tb[2,2])
  EQ1 = paste0("log(pi_-1 / pi_1) = ", round(I, 2), " + ", 
               round(M,2), " Mass Change     Flew in T1, rather than T2" )
  
  # 2 / -1 | Flew in both, not T1
  I = (tb[3,1] - tb[1,1])
  M = (tb[3,2] - tb[1,2])
  EQ2 = paste0("log(pi_2 / pi_-1) = ", round(I, 2), " + ", 
               round(M,2), " Mass Change     Flew in both, rather than T1" )
  
  # 2 / 1 | Flew in both, not T2
  I = (tb[3,1] - tb[2,1])
  M = (tb[3,2] - tb[2,2])
  EQ3 = paste0("log(pi_2 / pi_1) = ", round(I, 2), " + ", 
               round(M,2),  " Mass Change     Flew in both, rather than T2")
  
  eqs = c(EQ1, EQ2, EQ3)
  return(eqs)
}

prediction_equations2 = function(tb, var1, var2, third_eq = TRUE) {
  # -1 / 1 | Flew in T1, not T2
  I = (tb[1,1] - tb[2,1])
  M = (tb[1,2] - tb[2,2])
  S = (tb[1,3] - tb[2,3])
  EQ1 = paste0("log(pi_-1 / pi_1) = ", round(I, 2), " + ", round(M,2), var1, " + ", round(S, 2), var2, "   Flew in T1, not T2" )
  
  if (third_eq) {
    # 2 / -1 | Flew in both, not T1
    I = (tb[3,1] - tb[1,1])
    M = (tb[3,2] - tb[1,2])
    S = (tb[3,3] - tb[1,3])
    EQ2 = paste0("log(pi_2 / pi_-1) = ", round(I, 2), " + ", round(M,2), var1, " + ", round(S, 2), var2, "   Flew in both, not T1" )
  }
  
  if (third_eq) {
    # 2 / 1 | Flew in both, not T2
    I = (tb[3,1] - tb[2,1])
    M = (tb[3,2] - tb[2,2])
    S = (tb[3,3] - tb[2,3])
    EQ3 = paste0("log(pi_2 / pi_1) = ", round(I, 2), " + ", round(M,2),  var1, " + ", round(S, 2), var2, "    Flew in both, not T2")
  }
  
  eqs = EQ1
  
  if (third_eq) {
    eqs = c(EQ1, EQ2, EQ3)
  }
  
  print("Where F = 1")
  return(eqs)
}

#### Determine which multinomial model equations are significant.

get_significant_models = function(val_num, effect_cat=1){
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
  plot(effects, ylab="Baselines", xlab="Equation", main="p-values", 
       na.col="white", col=rev(grey.colors(7)), digits=4, 
       breaks=c(0.05,0.10, 0.5, 0.8, 1),
       text.cell=list(col="white", cex=1), max.col=170, border=NA, fmt.cell='%.2f', fmt.key='%.2f')
  print(effects)
  return(model_list)
}

get_significant_modelsf = function(num_val, effect_cat){
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

### Getting Odds for Summary Table

getting_odds = function(Odds, effect, pars, odds, r) {
  # Odds:     Equation of the odds written out as vector of strings.
  # effect:   Main effects written out as a vector of strings.
  # pars:     Parameters written out as a vector of strings
  # odds:     Model table ML equations for the given outcome and baseline.
  # r:        Row in which the outcome is indexed. 
  estimate = round(unlist(odds[r,1:4]),2)
  SE = round(unlist(odds[r,6:9]),2)
  estimates = format(estimate, nsmall = 2)
  SEs = format(SE, nsmall = 2)
  
  for (i in 2:4) {
    if (i == 2) {
      estimate[i] = round(estimate[i]*20,2)
    }
    if (i == 4) {
      estimate[i] = round(estimate[i]/100,2)
    }
  }
  exp.b = c(round(exp(estimate[1:4]),2))
  wald = c(round(unlist(odds[r,14:17]),2))
  pvals = c(round(unlist(odds[r,18:21]),3))
  
  exps = format(exp.b, nsmall = 2)
  for (i in 1:length(pvals)){
    if (pvals[i] == "0") {
      pvals[i] = "<0.001"
    }
  }
  
  key = cbind(Odds, effect, pars, estimates, SEs, exps, wald, pvals) 
  
  return(key)
}


getting_oddsf = function(Odds, effect, pars, odds, r) {
  # Odds:     Equation of the odds written out as vector of strings.
  # effect:   Main effects written out as a vector of strings.
  # pars:     Parameters written out as a vector of strings
  # odds:     Model table ML equations for the given outcome and baseline.
  # r:        Row in which the outcome is indexed. 
  estimate = round(unlist(odds[r,1:3]),2)
  estimates = format(estimate, nsmall = 2)
  SE = c(round(unlist(odds[r,5:7]),2))
  SEs = format(SE, nsmall = 2)
  
  for (i in 2:4) {
    if (i == 2) {
      estimate[i] = estimate[i]*20
    }
  }
  
  exp.b = c(round(exp(estimate),2))
  wald = c(round(unlist(odds[r,11:13]),2))
  pvals = c(round(unlist(odds[r,14:16]),3))
  
  exps = format(exp.b, nsmall = 2)
  for (i in 1:length(pvals)){
    if (pvals[i] == "0") {
      pvals[i] = "<0.001"
    }
  }
  key = cbind(Odds, effect, pars, estimates, SEs, exps, wald, pvals) 
  
  return(key)
}
