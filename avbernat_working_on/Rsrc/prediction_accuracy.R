calculate_accuracy = function(data, cfirst, clast) {
  
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
  
  events = actual_events == most_likely_events
  accurate_events = sum(events)
  total_events = length(events)
  accuracy = accurate_events / total_events
  
  return(accuracy)
}