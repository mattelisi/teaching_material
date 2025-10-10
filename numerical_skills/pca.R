
set.seed(2)
Sigma <- matrix(c(1,0.7*sqrt(2),0.7*sqrt(2),1.5),nrow=2, ncol=2)
xy <- MASS::mvrnorm(250, mu=c(0,0),Sigma)

par(pty="s")
plot(xy[,1], xy[,2],axes=F,xlab=expression(x[1]),ylab=expression(x[2]))
axis(1)
axis(2)

m <- matrix(c(cov(xy[,1], xy[,1]), cov(xy[,1], xy[,2]), cov(xy[,2], xy[,1]),cov(xy[,2], xy[,2])),
            nrow=2,
            ncol=2,
            byrow=TRUE,
            dimnames=list(c("x","y"),c("x","y")))

PC <- eigen(m)

abline(h=0,lty=2)
abline(v=0,lty=2)

arrows(0, 0,PC$vectors[1,1], PC$vectors[2,1], lwd=5, length=0.1, col="blue")
arrows(0, 0,PC$vectors[1,2], PC$vectors[2,2], lwd=5, length=0.1, col="red")

legend("bottomright",c(expression(v[1]),expression(v[2])),lwd=5, col=c("blue","red"),bty="n")


arrows(0, 0,PC$vectors[1,1]*PC$values[1], PC$vectors[2,1]*PC$values[1], lwd=5, length=0.1, col="blue")
arrows(0, 0,PC$vectors[1,2]*PC$values[2], PC$vectors[2,2]*PC$values[2], lwd=5, length=0.1, col="red")


legend("bottomright",c(expression(lambda[1]~v[1]),expression(lambda[2]~v[2])),lwd=5, col=c("blue","red"),bty="n")
