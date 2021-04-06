# Time Series Auxiliary Functions

clean_for_ts = function(contin_var, cat_var, func) {
  # get wing length averages using vectorization
  summary = tapply(X=d[,contin_var], INDEX=d[,cat_var], FUN=func, na.rm=T)
  monyear = unique(d$datetime)
  
  # generate datetime object | datetime object needs a date, 
  # which I initialized at 01 for each month
  monyeardate <- paste(monyear," 01",sep="")
  dates = as.Date(monyeardate, "%b %Y %d")
  
  return(list(summary, dates))
}

plot_consecutive_yrs = function(v, var){
  d = cbind(v[1:length(v)-1],
            v[2:length(v)])
  colnames(d) = c("t", "t1")
  plot(d, ylab="t+1", 
       main="Relationship Between Consecutive Years")
  m = lm(t1 ~ t, data=as.data.frame(d)) # not sig
  abline(m)
  mtext(var, side=3, adj=1, line=-1)
}

check_stationarity = function(dx) {
  #  dx is your xts zoo time series object
  plot(dx)
  #addEventLines(events, pos=2, srt=90, col="red")
  
  par(mfrow=c(1,2))
  print(adf.test(dx[,1])) 
  Acf(dx[,1], main='') # ACF
  Acf(dx[,1], type="partial", main='') #PACF  
  par(mfrow=c(1,1))
}

detrend = function(dx) {
  dx$diff <- diff(dx[,1]) # "regressing out"
  dx <- dx[-1,]
  plot(dx$diff)
  
  par(mfrow=c(1,2))
  print(adf.test(dx$diff)) 
  Acf(dx$diff, main='') # ACF
  Acf(dx$diff, type="partial", main='') #PACF 
  par(mfrow=c(1,1))
}

dedrift = function(dx) {
  dx$logv <- log(dx[,1])
  dx <- dx[-1,]
  plot(dx$logv)
  
  par(mfrow=c(1,2))
  print(adf.test(dx$logv))
  Acf(dx$logv, main='') # ACT
  Acf(dx$logv, type="partial", main='') #PACF 
  par(mfrow=c(1,1))
}

dedriftrend = function(dx) {
  dx$logdiff <- diff(log(dx[,1]))
  dx <- dx[-1,]
  plot(dx$logdiff)
  
  par(mfrow=c(1,2))
  print(adf.test(dx$logdiff)) 
  Acf(dx$logdiff, main='') # ACT
  Acf(dx$logdiff, type="partial", main='') #PACF 
  par(mfrow=c(1,1))
}
