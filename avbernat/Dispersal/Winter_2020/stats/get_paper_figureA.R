# Plot BV paper figure plot

HP = c(1,-1)
SYM = unique(dc$sym_dist_s)
M = seq(min(dc$avg_mass), max(dc$avg_mass), by = 0.015)
c_bt = expand.grid(HP,SYM,M)

eq_bt = function(combo_matrix) {
  effects_col = c()
  for (i in 1:nrow(combo_matrix)) {
    hp=combo_matrix[i,1]
    sym=combo_matrix[i,2]
    ma=combo_matrix[i,3]
    bih = 1.85594
    bis = -1.41367
    total_effect = (exp(bis)^2 * sym * ma) + (exp(bih)^2 * hp * ma)
    perchange =  (exp(total_effect) - 1) * 100
    effects_col = c(effects_col, perchange)
  }
  
  return(effects_col)
}

filter_for_real_events = function(combo) {
  combo$index = seq(1,nrow(combo), by=1)
  
  # remove BV in Homestead
  temp = combo[combo$sym_dist == unique(combo$sym_dist)[9],]
  rows_to_remove = temp[temp$host == unique(combo$host)[2],]$index 
  
  # remove BV in mainland
  temp = combo[combo$sym_dist > unique(combo$sym_dist)[2],]
  rows_to_remove3 = temp[temp$host == unique(combo$host)[2],]$index 
  
  # remove GRT in the keys
  temp = combo[combo$sym_dist > unique(combo$sym_dist)[9],]
  temp2 = temp[temp$sym_dist < unique(combo$sym_dist)[10],] 
  temp3 = temp2[temp2$host == unique(combo$host)[1],] 
  rows_to_remove2 = temp3$index
  
  remove_rows = c(rows_to_remove, rows_to_remove2, rows_to_remove3)
  cf = combo[-remove_rows,]
  cf[order(cf$sym_dist),]
  
  return(cf)
}


MODEL = eq(c)
MODEL_bt = eq_bt(c_bt) # back transformation

c$model = MODEL
c_bt$model_bt = MODEL_bt
colnames(c) = c("host", "sym_dist", "avg_mass_logsqrt", "model")
colnames(c_bt) = c("host", "sym_dist", "avg_mass", "model_bt")

cf = filter_for_real_events(c)
cf_bt = filter_for_real_events(c_bt)


colorfunc = colorRamp(bv_cols)
x = sort(runif(length(SYM), min = 0, max = 1))
mycolors_sim = rgb(colorfunc(x), maxColorValue=255)
mycolors_sim_a = unlist(lapply(mycolors_sim, col.alpha))


cft_bt = cf_bt[cf_bt$host == -1,] # -1 is C.corindum

# Plot back tansformations
plot_back_transformation = function() {
  x = 0.4
  y = 0.55 
  h = y + 0.25
  v = c(x-0.20, x, y, h)
  par( fig=v, new=TRUE, mar=c(0,0,0,0) )
  plot(cft_bt$avg_mass, cft_bt$model_bt, ylab="", xlab="mass (g)",
       col=mycolors_sim[as.factor(cft_bt$sym_dist)],
       pch=c(17,17)[as.factor(cft_bt$host)],
       xlim = c(0.01, 0.19),
       ylim = c(-105, -60),
       cex=1.5, cex.lab=scale, cex.axis=scale)
}

# Function to plot color bar
color.bar = function(lut, min, max=77, nticks=3, 
                     ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut)-1)/(max-min)
  
  final_ticks=seq(min/100, max/100, len=nticks)
  final_ticks = c("-0.54", "-0.46", "-0.24")
  
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', 
       xlab='', yaxt='n', ylab='', main=title, cex=1.6)
  axis(2, ticks, las=1, labels=final_ticks)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}

plot_color_scale = function() {
  x = 0.87
  y = 0.52 # .58
  h = y + 0.21
  v = c(x-0.025, x, y, h)
  par( fig=v, new=TRUE, mar=c(0,0,0,0) )
  colors = rev(c("#d9f7ff", "skyblue","blue", "#152b7a"))
  color.bar(colorRampPalette(colors)(1000), 63)
}

# Figure A

cft = cf[cf$host == -1,] # -1 is C.corindum

xlabel = expression(log(sqrt("mass (g)")))
plot(cft$avg_mass_logsqrt, cft$model, ylab="Percent Change in Flight Potential", xlab=xlabel,
     col=mycolors_sim[as.factor(cft$sym_dist)],
     pch=c(17,17)[as.factor(cft$host)],
     ylim=c(-100,550),
     cex=scale, cex.lab=scale, cex.axis=scale)
grid()
abline(v=0, col="black")
abline(v=70, col="black")

GRT = expression(italic("K. elegans"))
BV = expression(italic("C. corindum"))

LAT = expression(italic("Latitudinal\nDifference"))
text(0.55,440, labels=LAT, cex=c7)
text(-0.15,130, labels="mass (g)", cex=c7)
text(-0.18,525, labels=BV, cex=c_ax, font=fsize)
plot_back_transformation()
plot_color_scale()