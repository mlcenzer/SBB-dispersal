time_var_testsSEX = function(d, print_test=FALSE) {
  
  months = sort(unique(d$month_of_year))
  month_labs <- c("Feb", "Apr", "May", "Aug", "Sept", "Oct", "Dec")
  
  table = matrix(nrow=length(months), ncol=9)
  i = 0
  for (m in months) {
    i = i + 1
    data = d[d$month_of_year==m, ]
    
    VAR = var.test(wing2body ~ sex, data = data) # F test to compare the variances of two samples from normal pops
    TTEST= t.test(wing2body ~ sex, data=data) # t.test to find significant difference between the means of two groups
    AOV = aov(wing2body ~ sex, data=data) # used to analyze the differences among means. 
    
    p = TukeyHSD(AOV)$sex[,"p adj"]
    diff = TukeyHSD(AOV)$sex[,"diff"]
    
    if (print_test) {
      print(month_labs[i])
      print("t.test")
      print(TTEST)
      print("anova")
      print(summary(AOV))
      print(TukeyHSD(AOV))
      cat("-------------------------------------------------")
    }
    
    # plot histograms
    h = data %>%
      ggplot( aes(x=wing2body, fill=sex)) +
      geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
      scale_fill_manual(values=c("#69b3a2", "#404080")) +
      labs(fill="")  + labs(title=month_labs[i])
    print(h)
    
    # populate matrix
    table[i,1] = month_labs[i]
    table[i,2] = "M-F"
    table[i,3] = round(diff,4)
    table[i,4] = round(p,5)
    table[i,5] = "M=-1 | F=1"
    table[i,6] = round(TTEST$statistic,4)
    table[i,7] = round(TTEST$p.value,5)
    table[i,8] = round(VAR$statistic,2)
    table[i,9] = round(VAR$p.value, 3)
  }
  colnames(table) = c("month","sex", "Tukey-diff", "p-val", "sex", "ttest", "p-val", "F-test", "p-val")
  return(table)
} 

time_var_testsHOST = function(d, print_test=FALSE) {
  
  months = sort(unique(d$month_of_year))
  month_labs <- c("Feb", "Apr", "May", "Aug", "Sept", "Oct", "Dec")
  
  table = matrix(nrow=length(months), ncol=9)
  i = 0
  for (m in months) {
    i = i + 1
    data = d[d$month_of_year==m, ]
    
    VAR = var.test(wing2body ~ pophost, data = data) # F test to compare the variances of two samples from normal pops
    TTEST= t.test(wing2body ~ pophost, data=data) # t.test to find significant difference between the means of two groups
    AOV = aov(wing2body ~ pophost, data=data) # used to analyze the differences among means. 
    
    p = TukeyHSD(AOV)$pophost[,"p adj"]
    diff = TukeyHSD(AOV)$pophost[,"diff"]
    
    if (print_test) {
      print(month_labs[i])
      print("t.test")
      print(TTEST)
      print("anova")
      print(summary(AOV))
      print(TukeyHSD(AOV))
      cat("-------------------------------------------------")
    }
    
    # plot histograms
    h = data %>%
      ggplot( aes(x=wing2body, fill=pophost)) +
      geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
      scale_fill_manual(values=c("#69b3a2", "#404080")) +
      labs(fill="")  + labs(title=month_labs[i])
    print(h)
    
    # populate matrix
    table[i,1] = month_labs[i]
    table[i,2] = "GRT-BV"
    table[i,3] = round(diff,4)
    table[i,4] = round(p,5)
    table[i,5] = "GRT=1 | BV=-1"
    table[i,6] = round(TTEST$statistic,4)
    table[i,7] = round(TTEST$p.value,5)
    table[i,8] = round(VAR$statistic,2)
    table[i,9] = round(VAR$p.value, 3)
  }
  colnames(table) = c("month","sex", "Tukey-diff", "p-val", "sex", "ttest", "p-val", "F-test", "p-val")
  
  return(table)
} 