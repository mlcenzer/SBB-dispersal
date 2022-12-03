remove_torn_wings = function(d) {
  
  rows2remove = c()
  
  for (i in 1:nrow(d)) {
    note_str = d$notes[i]
    if (grepl("torn", note_str) & grepl("wing", note_str)) {
      rows2remove = c(rows2remove, i)
    }
  }
  
  d2 = d[-rows2remove,]
  
  cat("number of bugs with torn wings:", length(rows2remove), "\n")
  
  return(d2)
}