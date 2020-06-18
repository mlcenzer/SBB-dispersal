
# Plotting Linear Regression Models

#########################################################################################################
# FUNCTION: lm_plot | Generates a linear regression plot by inputting a few arguments.
#
# INPUT: data as a Data Frame, explanatory variable as a character, response variable as a character, 
#        and then, the x-axis label, the y-axis label, and the main title as characters.
#
# OUTPUT: generates a plot fully labeled, with a p-value, R corr value, lm equation line, and 95% confidence 
#         interval of the lm line (shaded).
#
#########################################################################################################

library(rethinking)
library(stringr)

lm_plot <- function(d, explanatory_var, response_var, xl, yl, title) {
  
  x=d[,explanatory_var]
  y=d[,response_var] 
  
  fit <- lm(y ~ x, data=d)
  coef <- coefficients(summary(fit))
  pval <- coefficients(summary(fit))[8]
  corr <- paste0("R = ", round(cor(x,y),3))
  
  x.seq = seq(min(x) - sd(x), max(x) + sd(x), length.out=30)
  #x.seq = seq(min(x) - .2, max(x) + .2, length.out=30)
  conf_interval <- predict(fit, newdata=data.frame(x=x.seq), interval="confidence", level = 0.95)
  mu <- link(fit, data=data.frame(x=x.seq))
  mu.PI <- apply(mu,2,PI, prob=0.95)
  
  alpha <- round(coef[1],3)
  beta <- round(coef[2],3)
  
  if (str_detect(as.character(coef[2]), "e") == TRUE) {
    beta <- as.numeric(formatC(coef[2], format = "e", digits = 2))
  }
  if (str_detect(as.character(coef[1]), "e") == TRUE) {
    alpha <- as.numeric(formatC(coef[1], format = "e", digits = 2))
  }
  
  eq <- paste0(response_var, " = ", alpha,
               ifelse(sign(coef[2])==1, " + ", " - "),
               abs(beta), "*", explanatory_var)
  
  if (pval <= 0.05) {
    p <- c(paste0("p = ", formatC(pval, format = "e", digits = 2), "*"))
    color <- "slategray4"
  }
  else {
    p <- c(paste0("p = ", round(pval,3)))
    color <- "red"
  }
  
  plot(x, y, xlab=xl, ylab=yl, main=title, cex.main=0.95) # rangi2
  abline(coef[1], coef[2], col="blue")
  #mtext(eq, side=4, cex=0.85)
  shade(mu.PI, x.seq, col=col.alpha("blue"))
  text((max(x) -  sd(x)) / 2, max(y) -  0.3*sd(y), eq, col= color, cex=0.9)
  text((max(x) -  sd(x)) / 2, max(y) -  0.7*sd(y), p, col= color, cex=0.9)
  text((max(x) -  sd(x)) / 2, max(y) -  1.1*sd(y), corr, col= color, cex=0.9)
  #text(400, 14000, corr, col= color, cex=0.9)
  lines(x.seq, conf_interval[,2], col="blue", lty=2)
  lines(x.seq, conf_interval[,3], col="blue", lty=2)
  
}


