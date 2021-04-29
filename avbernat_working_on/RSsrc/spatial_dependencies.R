library(sp)
library(gstat)
library(lattice)

check_spatial_dependencies = function(m, d, xlong, ylat, zone = 16, cutoff_m, is_glm=FALSE, is_inla=FALSE) {
  
  # lat long to utm
  xy <- data.frame(ID = 1:length(xlong), X = xlong, Y = ylat)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  
  d$X.utm <- res$X
  d$Y.utm <- res$Y
  
  # Get Pearson and fitted values
  if (is_glm) {
    E = m$residuals
    mu = m$fitted.values
  }
  
  if (is_inla) {
    N <- nrow(d)
    mu <- m$summary.fitted.values[1:N,"mean"]
    E  <- (d$wing2body - mu) / sqrt(mu)
  }
  # Plot the Pearson vs. fitted
  plot(mu, E,
       xlab = "Fitted values",
       ylab = "Pearson residuals")
  abline(h = 0, lty = 2) 
  
  # What are the distances between the points? Plot it.
  Loc <- cbind(d$X.utm, d$Y.utm)
  D <- dist(Loc)
  #par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
  hist(D, 
       freq = TRUE,
       main = "", 
       xlab = "Distance between sites (m)",
       ylab = "Frequency")
  
  # Sample-variogram with distances up to ____ m
  MyData  <- data.frame(E = E, 
                        X  = d$X.utm, 
                        Y  = d$Y.utm)
  coordinates(MyData) <- c("X", "Y")
  
  V <- variogram(E ~ 1, MyData, cressie = TRUE, cutoff = (cutoff_m))
  plot(V, 
       main = "", 
       xlab = list(label = "Distance (m)", cex = 1.5), 
       ylab = list(label = "Semi-variogram", cex = 1.5),
       pch = 16,
       col = 1,
       cex = 1.5
  )
  with(V, scatter.smooth(x = dist, 
                         y = gamma,
                         xlab = "Distance",
                         ylab = "Sample variogram"))
  
  # Plot the Pearson vs. their spatial coordinates.
  MyData <- data.frame(E = E,
                       Xkm = d$X.utm,
                       Ykm = d$Y.utm)
  
  MyData$MySize <- 3 * abs(MyData$E) / max( MyData$E)
  MyData$MyCol <- ifelse(MyData$E> 0, 1, 2) 
  plot(Ykm ~ Xkm,
         data = MyData,
         cex = MyData$MySize,
         col = MyData$MyCol,
         pch = 1) 
  # black is positive residual; red negative.
  # larger it is the more positive or negative
  
  return(d)

}