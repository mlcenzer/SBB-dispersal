# Plot GRT paper figure plot


color.bar = function(lut, min, max=77, nticks=3, 
                     ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut)-1)/(max-min)
  
  final_ticks=seq(min/100, max/100, len=nticks)
  final_ticks = c("-0.73", "1.49", "3.11")
  
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', 
       xlab='', yaxt='n', ylab='', main=title, cex=1.6)
  axis(2, ticks, las=1, labels=final_ticks)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}

cft_bt = cf_bt[cf_bt$host == 1,]

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
       ylim = c(-500, 135000),
       cex=1.5, cex.lab=scale, cex.axis=scale)
}

plot_color_scale = function() {
  x = 0.67
  y = 0.58 # .58
  h = y + 0.21
  v = c(x-0.025, x, y, h)
  par( fig=v, new=TRUE, mar=c(0,0,0,0) )
  colors = rev(c("green","#15a315", "darkgreen", "#114007", "#0a2e02"))
  color.bar(colorRampPalette(colors)(1000), 63)
}


cft = cf[cf$host == 1,] # 1 is K. elegans

colorfunc = colorRamp(grt_cols)
x = sort(runif(length(SYM), min = 0, max = 1))
mycolors_sim = rgb(colorfunc(x), maxColorValue=255)
mycolors_sim_a = unlist(lapply(mycolors_sim, col.alpha))

xlabel = expression(log(sqrt("mass (g)")))
plot(cft$avg_mass_logsqrt, cft$model, ylab="Percent Change in Flight Potential", xlab=xlabel,
     col=mycolors_sim[as.factor(cft$sym_dist)],
     pch=c(17,17)[as.factor(cft$host)],
     ylim=c(-100,550),
     cex=scale, cex.lab=scale, cex.axis=scale)
grid()
abline(v=0, col="black")
abline(v=70, col="black")
LAT = expression(italic("Latitudinal\nDifference"))
text(0.30,500, labels=LAT, cex=c7) # 500
text(-0.15,130, labels="mass (g)", cex=c7)
text(-0.18,525, labels=GRT, cex=c_ax, font=fsize)
plot_back_transformation()
plot_color_scale()