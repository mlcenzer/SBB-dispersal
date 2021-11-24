library(cvms) # cross-validating regressions

get_confusion_matrix = function(data, cfirst, clast) {
  
  probs = data[cfirst:clast]
  most_likely_events = colnames(probs)[apply(probs,1,which.max)]
  actual_events = c()
  
  for (i in 1:nrow(data)) {
    if (data[i,1] == "0") {
      actual_event = "none"
    }
    if (data[i,1] == "-1") {
      actual_event = "T1"
    }
    if (data[i,1] == "1") {
      actual_event = "T2"
    }
    if (data[i,1] == "2") {
      actual_event = "both"
    }
    actual_events = c(actual_events, actual_event)
  }
  
  df = as.data.frame(cbind(actual_events, most_likely_events))
  #final_df = dplyr::count_(df, vars = c('actual_events','most_likely_events'))
  
  eval <- evaluate(df,
                   target_col = "actual_events",
                   prediction_cols = "most_likely_events",
                   type = "multinomial")
  
  return(eval)
}