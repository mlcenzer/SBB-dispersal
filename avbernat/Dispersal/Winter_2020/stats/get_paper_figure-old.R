# Old paper figure

# ```{r eval=FALSE, echo=FALSE, warning=FALSE, fig.width=4.8, fig.height=4.6, warning=FALSE, fig.show=FALSE}
# figure code 
# pdf(file = "plots/figure6.pdf",
#     width = 10,
#     height = 10)

layout(matrix(c(1,1,2,3), nrow = 2, byrow = TRUE))

par(mai=c(0.8, 0.69, 0.45, 0.04)) # bottom, right, top, left
plot1(df4,pp4)
par(mar = c(4, 4, 1.8, 0.1)) 
plot2(df5,pp5, pp4, gradient=TRUE, circles=FALSE, stochasticity=TRUE, points=FALSE)
plot3(df5,pp5, pp4, gradient=TRUE, circles=FALSE, stochasticity=TRUE, points=FALSE)

plot_histograms = function() {
  x = 0.48
  y = 0.415
  h = y + 0.04
  v = c(x-0.13,x, y, h)
  par( fig=v, new=TRUE, mar=c(0,0,0,0) )
  hist(df$wing2body[Frows], col="white", main="", cex.axis=0.9, 
       ylim=c(0,50), breaks=10) 
  text(0.66,40, labels="w2b", cex=0.9)
  
  x = 0.90
  y = 0.415
  h = y + 0.04
  v = c(x-0.13,x, y, h)
  par( fig=v, new=TRUE, mar=c(0,0,0,0) )
  hist(df$wing2body[Mrows], col="white", main="", cex.axis=0.9, 
       ylim=c(0,50), xlim=c(0.64,0.78), breaks=10) 
  text(0.66,40, labels="w2b", cex=0.9)
}

plot_color_scale = function() {
  x = 0.98
  y = 0.35
  h = y + 0.08
  v = c(x-0.025, x, y, h)
  par( fig=v, new=TRUE, mar=c(0,0,0,0) )
  color.bar(colorRampPalette(c("black", "grey"))(1000), 63)
}

plot_histograms()
plot_color_scale()

# dev.off()