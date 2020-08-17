
# Plotting First Order and Second Order Polynomial Regression Models


lm_polyplot <- function(d, explanatory_var, response_var, xl, yl, title){
  
  x=d[,explanatory_var]
  y=d[,response_var] 
  
  m <- lm(y ~ x, data=d)
  m2 <- lm(y ~ x + I(x^2), data=d)
  
  x.seq = seq(min(x) - sd(x), max(x) + sd(x), length.out=100)
  prd <- data.frame(x=x.seq) # newdata

  err <- predict(m, newdata = prd, se.fit = TRUE)
  prd$lci <- err$fit - 1.96 * err$se.fit
  prd$fit <- err$fit
  prd$uci <- err$fit + 1.96 * err$se.fit
  mu_ci <- t(matrix(c(prd$lci,prd$uci), ncol=2))
  
  err <- predict(m2, newdata = prd, se.fit = TRUE)
  prd$lci2<- err$fit - 1.96 * err$se.fit
  prd$fit2 <- err$fit
  prd$uci2 <- err$fit + 1.96 * err$se.fit
  mu_ci2 <- t(matrix(c(prd$lci2,prd$uci2), ncol=2))

  plot(x, y , col=col.alpha(rangi2,0.7), ylab=yl, xlab=xl, main=title)
  abline(m, col="blue")
  lines(prd$x, prd$fit, col="blue") 
  lines(prd$x, prd$fit2, col="orange") 
  shade(mu_ci, lim = prd$x)
  shade(mu_ci2, lim = prd$x)
  # lines(prd$x, prd$lci, col="blue", lty=2)
  # lines(prd$x, prd$uci, col="blue", lty=2)
  legend("topleft", 
         legend = c("First Order", "Second Order"), 
         col = c("blue", "orange"), 
         pch = c(15), 
         bty = "n", 
         pt.cex = 2, 
         cex = .9, 
         text.col = "black", 
         horiz = F , 
         inset = c(0.05, 0.05))
}


