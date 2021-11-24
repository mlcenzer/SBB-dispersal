lm_plot <- function(d, xvar, yvar, txpos, typos) {
  
  x=d[,xvar]
  y=d[,yvar] 
  
  m <- lm(y ~ x, data=d)
  x.seq = seq(min(x) - sd(x), max(x) + sd(x), length.out=100)
  
  prd <- data.frame(x=x.seq) # newdata
  err <- predict(m, newdata = prd, se.fit = TRUE)
  prd$lci <- err$fit - 1.96 * err$se.fit
  prd$fit <- err$fit
  prd$uci <- err$fit + 1.96 * err$se.fit
  mu_ci <- t(matrix(c(prd$lci,prd$uci), ncol=2))
  
  #par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  plot(ocos_baj$logNB ~ ocos_baj$NN_c, 
       pch=(16),
       #col=colfunc(2)[as.factor(ocos$site)], 
       #pch = c(17:16)[as.factor(ocos$site)],
       #main="log(Height) ~ site*Elevation",
       xlab = xvar,
       ylab= yvar,
       cex=1.3,
       cex.lab=1.5,
       cex.axis=1.3,
       bty="n") 
  # legend(1.6, 2.9,
  #        legend = c("0-65", "65-130", "130-195", "195-260", "260-325"),
  #        col= c(colfunc(27)[1],colfunc(27)[7],colfunc(27)[12],colfunc(27)[18],colfunc(27)[27] ),
  #        #bty="n",
  #        pch = c(16),
  #        title="Watershed_Dis (m)")
  abline(m, lty=2)
  shade(mu_ci, lim = prd$x)
  pval <- paste0("p = ", round(summary(m)$coefficients[8],3), "*")
  text(txpos, typos, pval, cex=1.2)
  
}