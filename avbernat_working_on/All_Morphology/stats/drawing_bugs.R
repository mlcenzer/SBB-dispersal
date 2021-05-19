## Drawing Bugs

par(mfrow=c(2,1))
n=4
plot(c(0,short$wing[1]), c(1,1), xlim=c(0,15))
lines(c(0,short$body[1]), c(1,1), col="red")
lines(c(0+n,short$wing[1]+n), c(1,1))

plot(c(0,short$wing[2]), c(1,1), xlim=c(0,15))
lines(c(0,short$body[2]), c(1,1), col="red")
lines(c(0+n,short$wing[2]+n), c(1,1))