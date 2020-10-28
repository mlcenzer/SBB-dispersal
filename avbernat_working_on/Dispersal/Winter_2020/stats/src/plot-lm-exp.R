
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

exp_lm_plot <- function(d, explanatory_var, response_var, xl, yl, title, linear) {
  
  x=d[,explanatory_var]
  y=d[,response_var] 
  log.x = d[, length(d)]
  
  fit <- lm(y ~ x, data=d)
  
  if (linear == FALSE) {
    fit <- lm(y ~ log(x), data=d)
  }
  
  coef <- coefficients(summary(fit))
  pval <- coefficients(summary(fit))[8]
  corr <- paste0("R = ", round(cor(x,y),3))
  
  if (linear == TRUE) {
    x.seq = seq(min(x) - sd(x), max(x) + sd(x), length.out=30)
    conf_interval <- predict(fit, newdata=data.frame(x=x.seq), interval="confidence", level = 0.95)
    mu <- link(fit, data=data.frame(x=x.seq))
    mu.PI <- apply(mu,2,PI, prob=0.95)
  }
  if (linear == FALSE) {
    x.seq = seq(0.1, max(x), length.out=30)
    exponential <- predict(fit, list(x = x.seq))
    mu <- link(fit, data=data.frame(log.x=x.seq)) 
    #mu.mean <- apply(mu,2,mean) 
    mu.PI <- apply(mu,2,PI,prob=0.95)
  }
  
  alpha <- round(coef[1],2)
  beta <- round(coef[2],2)
  
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
    color <- "black"
  }
  else {
    p <- c(paste0("p = ", round(pval,3)))
    color <- "red"
  }
  
  if (linear == TRUE) {
    plot(x, y, xlab=xl, ylab=yl, main=title) # rangi2
    abline(coef[1], coef[2], col="blue")
    mtext(eq, side=4, cex=0.85)
    shade(mu.PI, x.seq, col=col.alpha("blue"))
    text(mean(x) - 1.15*sd(x), mean(y) - 1.8*sd(y), p, col= color, cex=0.9)
    text(mean(x) - 1.15*sd(x), mean(y) - 2.1*sd(y), corr, col= color, cex=0.9)
    lines(x.seq, conf_interval[,2], col="blue", lty=2)
    lines(x.seq, conf_interval[,3], col="blue", lty=2)
  }
  if (linear == FALSE) {
    plot(x, y, xlab=xl, ylab=yl, main=title, log="y") # rangi2
    lines(x.seq, exponential, lwd = 2, col = "black")
    shade(mu.PI, lim=x.seq)
    text(mean(x) - sd(x), mean(y) - 1.6*sd(y), p, col= color, cex=0.9)
    #lines(exp(x.seq), conf_interval[,2], col="blue", lty=2)
    #lines(exp(x.seq), conf_interval[,3], col="blue", lty=2)
    #lines( exp(x.seq), mu.mean) # not working
    #shade(mu.PI, lim = exp(x.seq)) # not working
    #abline(v=120, col="red", lty=2)
    #text(mean(x) - 1.25*sd(x), mean(y) - 1.6*sd(y), p, col= color, cex=0.9)
  }
}


