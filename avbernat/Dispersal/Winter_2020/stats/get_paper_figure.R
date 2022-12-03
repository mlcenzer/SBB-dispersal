
# Get Figure 5
df_all = df4

panelA = "a"
panelB = "b"
panelC = "c"
panelD = "d"
panelE = "e"
panelF = "f"

font_face = 2 # 1 is plain and 2 is boldface

plot2 = function(df, pp, PP, gradient=TRUE, circles=TRUE, stochasticity=TRUE, points=TRUE, model_wo_w2b=FALSE) {
  
  c1 = 0.65
  c2 = 1.2
  df$w2b_col = 0
  
  if (stochasticity) {
    plot(df$mass_per[Frows], pp[Frows,1], ylim=c(0,1), xlim=c(-35,104), col=nf,type="l", 
         ylab="Flight Case Probability", xlab="% Mass Change From T1 to T2", 
         lty=1, cex.axis=c_ax, cex.lab=c_lab, lwd=lweight) 
    points(df$mass_per[Frows], pp[Frows,2], col=f1t1col, type="l", lwd=lweight)
    points(df$mass_per[Frows], pp[Frows,4], col=f2col, type="l", lwd=lweight) 
    mtext(expression(italic("Large Mass Ranges,\nMost Likely to Not Fly or Fly Once")), side=3, adj=0.45, line=1, cex=1.3, font=2)
    
    
  }
  
  if (model_wo_w2b) {
    points(df$mass_per[Frows], PP[Frows,1], col=nf, type="l", lty=2)   
    points(df$mass_per[Frows], PP[Frows,2], col=f1t1col, type="l", lty=2) 
    points(df$mass_per[Frows], PP[Frows,4], col=f2col, type="l", lty=2) 
  }
  if (points){
    plot(df$mass_per[Frows], PP[Frows,1], ylim=c(0,1), xlim=c(-35,104), col=nf,type="l", 
         ylab="Flight Case Probability", xlab="% Mass Change From T1 to T2", 
         lty=1, cex.axis=c_ax, cex.lab=c_lab) 
    points(df$mass_per[Frows], PP[Frows,2], col=f1t1col, type="l")
    points(df$mass_per[Frows], PP[Frows,4], col=f2col, type="l") #darkred   
  }
  mtext(expression(italic("Females")), side=3, adj=0.05, line=-2, cex=1.5, font=2)
  mtext(panelA, side=3, adj=0.01, line=0.5, cex=1.8, font=font_face)
  text(64,0.65, labels="Flew in T1", col=f1t1col, cex=c2)
  text(20,0.70, labels="Did Not Fly", col=nf_dark, cex=c2) 
  text(82,0.37, labels="Flew Twice", col=f2col_dark, cex=c2) #maroon
  
  if (gradient) {
    rbPal = colorRampPalette(c('black', nf))
    df$w2b_col = rbPal(15)[as.numeric(cut(df$wing2body, breaks = 15))]
    points(df$mass_per[Frows], pp[Frows, 1], pch=20, col=df$w2b_col[Frows])
    
    rbPal = colorRampPalette(c('black',f1t1col_norm))
    df$w2b_col = rbPal(15)[as.numeric(cut(df$wing2body,breaks = 15))]
    points(df$mass_per[Frows], pp[Frows, 2], pch=20, col=df$w2b_col[Frows])
    
    rbPal = colorRampPalette(c('black', f2col_norm)) # violetred1
    df$w2b_col = rbPal(15)[as.numeric(cut(df$wing2body,breaks = 15))]
    points(df$mass_per[Frows], pp[Frows, 4], pch=20, col=df$w2b_col[Frows])
  }
  
  if (circles) { # here: maybe should just color the line in a gradient based on w2b
    # Mark points in the graph with high wing2body ratio vs. points with low wing2body ratio.
    test = df[with(df, order(wing2body)),] # ascending order
    
    small = test %>%
      filter(sex =="F", wing2body < 0.7184934)
    large = test %>%
      filter(sex == "F", wing2body > 0.7184934)
    srows = small$index
    lrows = large$index
    
    points(df$mass_per[lrows], pp[lrows,1], col=nf, type="p", cex=c1, pch=16)
    points(df$mass_per[srows], pp[srows,1], col=nf, type="p", cex=c1)
    # those with smaller wing2body ratio were more likely to NOT fly
    points(df$mass_per[lrows], pp[lrows,2], col=f1t1col, type="p", cex=c1, pch=16)
    points(df$mass_per[srows], pp[srows,2], col=f1t1col, type="p", cex=c1)
    points(df$mass_per[lrows], pp[lrows,4], col=nf_dark, type="p", cex=c1, pch=16)
    points(df$mass_per[srows], pp[srows,4], col=nf_dark, type="p", cex=c1)
  }
  
}



plot3 = function(df, pp, PP, gradient=TRUE, circles=TRUE, stochasticity=TRUE, points=TRUE, model_wo_w2b=FALSE) {
  
  c1 = 0.65
  c2 = 1.2
  df$w2b_col = 0
  
  if (stochasticity) {
    plot(df$mass_per[Mrows], pp[Mrows,1], ylim=c(-0.00,1), xlim=c(-25,60), col=nf,type="l", 
         ylab=" ", xlab="% Mass Change From T1 to T2", 
         lty=1, cex.axis=c_ax, cex.lab=c_lab, lwd=lweight)
    points(df$mass_per[Mrows], pp[Mrows,2], col=f1t1col, type="l", cex=0.45, lty=1, lwd=lweight)
    points(df$mass_per[Mrows], pp[Mrows,3], col=f1t2col, type="l", cex=0.45, lty=1, lwd=lweight) 
    points(df$mass_per[Mrows], pp[Mrows,4], col=f2col, type="l", cex=0.45, lty=1, lwd=lweight) # darkred
    # title("Short Mass Ranges,\nMost Likely to Fly Twice")
    mtext(expression(italic("Short Mass Ranges,\nMost Likely to Fly Twice")), side=3, adj=0.75, line=1, cex=1.3, font=2)
  }
  
  if (model_wo_w2b){
    points(df$mass_per[Mrows], PP[Mrows,1], col=nf, type="l", cex=0.45, lty=2)
    points(df$mass_per[Mrows], PP[Mrows,2], col=f1t1col, type="l", cex=0.45, lty=2)
    points(df$mass_per[Mrows], PP[Mrows,3], col=f1t2col, type="l", cex=0.45, lty=2) 
    points(df$mass_per[Mrows], PP[Mrows,4], col=f2col, type="l", cex=0.45, lty=2) # darkred
  }
  if (points) {
    plot(df$mass_per[Mrows], PP[Mrows,1], ylim=c(-0.00,0.8), xlim=c(-25,58), col=nf,type="l", 
         ylab=" ", xlab="% Mass Change From T1 to T2", 
         lty=1, cex.axis=c_ax, cex.lab=c_lab)
    points(df$mass_per[Mrows], PP[Mrows,2], col=f1t1col, type="l", cex=0.45, lty=1)
    points(df$mass_per[Mrows], PP[Mrows,3], col=f1t2col, type="l", cex=0.45, lty=1) 
    points(df$mass_per[Mrows], PP[Mrows,4], col=f2col, type="l", cex=0.45, lty=1) # darkred
  }
  mtext(expression(italic("Males")), side=3, adj=0.05, line=-2, cex=1.5, font=2)
  mtext(panelB, side=3, adj=0.01, line=0.5, cex=1.8, font=font_face)
  text(22,0.35, labels="Flew in T1", col=f1t1col, cex=c2)
  text(52.5,0.18, labels="Did Not Fly", col=nf_dark, cex=c2)
  text(-14, -0.01, labels="Flew in T2", col=f1t2col_dark, cex=c2)
  text(-17,0.69, labels="Flew Twice", col=f2col_dark, cex=c2) # 13,0.77, maroon, 13,0.77
  
  if (circles) {
    legend(39, .81+0.05, 
           pch=c(16,1), 
           title="Wing-to-body", 
           legend=c("> mean", "< mean"), 
           cex=1.1)
  }
  if (gradient) {
    text(48,0.98, labels="Wing-to-body", cex=c2)
  }
  
  if (gradient) {
    rbPal = colorRampPalette(c('black',nf))
    df$w2b_col = rbPal(15)[as.numeric(cut(df$wing2body,breaks = 15))]
    points(df$mass_per[Mrows], pp[Mrows, 1], pch=20, col=df$w2b_col[Mrows])
    
    rbPal = colorRampPalette(c('black',f1t1col_norm))
    df$w2b_col = rbPal(15)[as.numeric(cut(df$wing2body,breaks = 15))]
    points(df$mass_per[Mrows], pp[Mrows, 2], pch=20, col=df$w2b_col[Mrows])
    
    rbPal = colorRampPalette(c('black',f1t2col_norm))
    df$w2b_col = rbPal(15)[as.numeric(cut(df$wing2body,breaks = 15))]
    points(df$mass_per[Mrows], pp[Mrows, 3], pch=20, col=df$w2b_col[Mrows])
    
    rbPal = colorRampPalette(c('black',f2col_norm)) #violetred1
    df$w2b_col = rbPal(15)[as.numeric(cut(df$wing2body,breaks = 15))]
    points(df$mass_per[Mrows], pp[Mrows, 4], pch=20, col=df$w2b_col[Mrows])
  }
  
  if (circles) {
    # Mark points in the graph with high wing2body ratio vs. points with low wing2body ratio. 
    test = df[with(df, order(wing2body)),] # ascending order
    
    small = test %>%
      filter(sex =="M", wing2body < 0.7184934)
    large = test %>%
      filter(sex == "M", wing2body > 0.7184934)
    srows = small$index
    lrows = large$index
    
    points(df$mass_per[lrows], pp[lrows,1], col=nf, type="p", cex=c1, pch=16)
    points(df$mass_per[srows], pp[srows,1], col=nf, type="p", cex=c1)
    # those with smaller wing2body ratio were more likely to NOT fly
    points(df$mass_per[lrows], pp[lrows,2], col=f1t1col, type="p", cex=c1, pch=16)
    points(df$mass_per[srows], pp[srows,2], col=f1t1col, type="p", cex=c1)
    points(df$mass_per[lrows], pp[lrows,3], col=f1t2col, type="p", cex=c1, pch=16)
    points(df$mass_per[srows], pp[srows,3], col=f1t2col, type="p", cex=c1)
    points(df$mass_per[lrows], pp[lrows,4], col=nf_dark, type="p", cex=c1, pch=16)
    points(df$mass_per[srows], pp[srows,4], col=nf_dark, type="p", cex=c1)
  }
}


plot12 = function(df, pp, points=FALSE) {
  # eggs twice
  plot(df$mass_per[egg_2rows], pp[egg_2rows,1], ylim=c(0,1.05), xlim=c(-35,106), # -45,110 (need 140 span)
       col=nf, type="l", lty=1,
       ylab="", 
       xlab="",
       cex.axis=c_ax, cex.lab=c_lab, lwd=lweight_s)
  
  # eggs twice
  points(df$mass_per[egg_2rows], pp[egg_2rows,2], col=f1t1col, type="l", lty=1, lwd=lweight_s) # flew in T1 only
  points(df$mass_per[egg_2rows], pp[egg_2rows,3], col=f2col, type="l", lty=1, lwd=lweight_s) # flew in both
  
  
  if (points){
    points(df$mass_per[egg_2rows], pp[egg_2rows,1], col=nf, type="p", lty=1, pch=16)
    points(df$mass_per[egg_2rows], pp[egg_2rows,2], col=f1t1col, type="p", lty=1, pch=16)
    points(df$mass_per[egg_2rows], pp[egg_2rows,3], col=f2col, type="p", lty=1, pch=16)
    
  }
  mtext(expression(italic("Laid Eggs Twice")), side=3, adj=0.05, line=-2, cex=c0, font=2)
  
  mtext(panelC, side=3, adj=0, line=0.5, cex=c5, font=font_face)
  
  text(89,0.60, labels="Flew in T1", col=f1t1col, cex=c6)
  text(89,0.25, labels="Did Not Fly", col=nf_dark, cex=c6)
  text(89,0.15, labels="Flew Twice", col=f2col_dark, cex=c6)
}

plot10 = function(df, pp, points=FALSE) {
  # only laid eggs in T1
  plot(df$mass_per[eggT1_rows], pp[eggT1_rows,1], ylim=c(0,1.05), xlim=c(-45,40), # -11,57 (need a 70 span)
       col=nf, type="l", lty=1,
       ylab="", 
       xlab="",
       cex.axis=c_ax, cex.lab=c_lab, lwd=lweight_s)
  points(df$mass_per[eggT1_rows], pp[eggT1_rows,2], col=f1t1col, type="l", lty=1, cex=0.45, lwd=lweight_s) 
  points(df$mass_per[eggT1_rows], pp[eggT1_rows,3], col=f2col, type="l", lty=1, cex=0.45, lwd=lweight_s) 
  
  if (points) {
    points(df$mass_per[eggT1_rows], pp[eggT1_rows,1], col=nf, type="p", lty=2, pch=16)
    points(df$mass_per[eggT1_rows], pp[eggT1_rows,2], col=f1t1col, type="p", lty=1, pch=16)
    points(df$mass_per[eggT1_rows], pp[eggT1_rows,3], col=f2col, type="p", lty=1, pch=16)
    
  }
  mtext(expression(italic("Laid Eggs in T1")), side=3, adj=0.05, line=-2, cex=c0, font=2)
  
  mtext(panelD, side=3, adj=0, line=0.5, cex=c5, font=font_face)
  
  text(25,0.75, labels="Flew Twice", col=f2col_dark, cex=c6)
  text(25,0.20, labels="Did Not Fly", col=nf_dark, cex=c6)
  text(25,0.10, labels="Flew in T1", col=f1t1col, cex=c6)
  
}
plot11 = function(df, pp, points=FALSE) {
  # no egg change
  plot(df$mass_per[egg_0rows], pp[egg_0rows,1], ylim=c(0,1.05), xlim=c(-25,60), 
       col=nf, type="l", lty=1,
       ylab="", 
       xlab="% Mass Change From T1 to T2",
       cex.axis=c_ax, cex.lab=c_lab, lwd=lweight_s)
  points(df$mass_per[egg_0rows], pp[egg_0rows,2], col=f1t1col, type="l", lty=1, lwd=lweight_s) # flew in T1 only
  points(df$mass_per[egg_0rows], pp[egg_0rows,3], col=f2col, type="l", lty=1, lwd=lweight_s) # flew in both
  
  if (points) {
    points(df$mass_per[egg_0rows], pp[egg_0rows,1], col=nf, type="p", lty=1, pch=16)
    points(df$mass_per[egg_0rows], pp[egg_0rows,2], col=f1t1col, type="p", lty=1, pch=16)
    points(df$mass_per[egg_0rows], pp[egg_0rows,3], col=f2col, type="p", lty=1, pch=16)
  }
  
  mtext(expression(italic("Laid No Eggs")), side=3, adj=0.05, line=-2, cex=c0, font=2)
  
  mtext(panelF, side=3, adj=0, line=0.5, cex=c5, font=font_face)
  
  text(45,0.65, labels="Flew Twice", col=f2col_dark, cex=c6)
  text(45,0.44, labels="Flew in T1", col=f1t1col, cex=c6)
  text(45,0.21, labels="Did Not Fly", col=nf_dark, cex=c6)
  
}
plot13 = function(df, pp, points=FALSE) {
  # only laid eggs in T2
  plot(df$mass_per[eggT2_rows], pp[eggT2_rows,1], ylim=c(0,1.05), xlim=c(-35,106), 
       col=nf, type="l", lty=1,
       ylab="", 
       xlab="% Mass Change From T1 to T2",
       cex.axis=c_ax, cex.lab=c_lab, lwd=lweight_s)
  
  points(df$mass_per[eggT2_rows], pp[eggT2_rows,2], col=f1t1col, type="l", lty=1, lwd=lweight_s) # flew in T1 only
  points(df$mass_per[eggT2_rows], pp[eggT2_rows,3], col=f2col, type="l", lty=1, lwd=lweight_s) # flew in both
  
  if (points) {
    points(df$mass_per[eggT2_rows], pp[eggT2_rows,1], col=nf, type="p", lty=1, pch=16)
    points(df$mass_per[eggT2_rows], pp[eggT2_rows,2], col=f1t1col, type="p", lty=1, pch=16)
    points(df$mass_per[eggT2_rows], pp[eggT2_rows,3], col=f2col, type="p", lty=1, pch=16)
  }
  mtext(expression(italic("Laid Eggs in T2")), side=3, adj=0.05, line=-2, cex=c0, font=2)
  
  mtext(panelE, side=3, adj=0, line=0.5, cex=c5, font=font_face)
  
  text(93,0.84, labels="Flew in T1", col=f1t1col, cex=c6)
  text(93,0.16, labels="Did Not Fly", col=nf_dark, cex=c6)
  text(93,0.37, labels="Flew Twice", col=f2col_dark, cex=c6)
  mtext("Flight Case Probability",side=2,line=-1.3,outer=TRUE,cex=1.2,las=0, adj=.25)
}


# figure code 
# pdf(file = "plots/figure5-newestS2.pdf",
#     width = 10,
#     height = 10)

layout(matrix(c(1,1,1,1,1,2,2,2,
                1,1,1,1,1,2,2,2,
                3,3,3,3,3,4,4,4,
                5,5,5,5,5,6,6,6), nrow = 4,byrow = TRUE))

par(mar = c(4.1, 4.5, 6, 0.2), bg="white") # bottom, right, top, left
plot2(df5,pp5, pp4, gradient=TRUE, circles=FALSE, stochasticity=TRUE, points=FALSE)
par(xpd=NA)
plot3(df5,pp5, pp4, gradient=TRUE, circles=FALSE, stochasticity=TRUE, points=FALSE)
rect(-40, -6.229, 80, 2.0, col = rgb(0.5,0.5,0.5,alpha=0.075), border=FALSE) 
# xleft, ybottom, xright, ytop
par(mar = c(3.2, 4.5, 2.5, 0.2)) # +1.2
plot12(df6,pp6,points=TRUE) 
plot10(df6,pp6,points=TRUE) 
par(mar = c(4.2, 4.5, 1.7, 0.2)) #-1.2
plot13(df6,pp6,points=TRUE) 
plot11(df6,pp6,points=TRUE) 

par(xpd=TRUE)
plot_histograms = function() {
  x = 0.61
  y = 0.865
  h = y + 0.04
  v = c(x-0.10,x, y, h)
  par( fig=v, new=TRUE, mar=c(0,0,0,0) )
  hist(df_all$wing2body[Frows], col="white", main="", cex.axis=0.9, 
       ylim=c(0,50), breaks=10) 
  text(0.66,40, labels="w2b", cex=0.9)
  
  x = 0.90
  y = 0.865
  h = y + 0.04
  v = c(x-0.10,x, y, h)
  par( fig=v, new=TRUE, mar=c(0,0,0,0) )
  hist(df_all$wing2body[Mrows], col="white", main="", cex.axis=0.9, 
       ylim=c(0,50), xlim=c(0.64,0.78), breaks=10) 
  text(0.66,40, labels="w2b", cex=0.9)
}

plot_color_scale = function() {
  x = 0.98
  y = 0.80
  h = y + 0.08
  v = c(x-0.025, x, y, h)
  par( fig=v, new=TRUE, mar=c(0,0,0,0) )
  color.bar(colorRampPalette(c("black", "grey"))(1000), 63)
}

plot_histograms()
plot_color_scale()
# plot_model_scale()

# dev.off()


###### extra code

# keep = function() {
#   par(mar = c(4, 4, 2.5, 0.2), bg="#f2f2f2")
#   # par(bg = "#f2f2f2")
#   plot3(df5,pp5, pp4, gradient=TRUE, circles=FALSE, stochasticity=TRUE, points=FALSE)
#   rect(par("usr")[1], par("usr")[3],
#        par("usr")[2], par("usr")[4],
#        col = "#ffffff") # Color
#   par(new = TRUE)
#   plot3(df5,pp5, pp4, gradient=TRUE, circles=FALSE, stochasticity=TRUE, points=FALSE)
#   
# }

