

x <- runif(50, min=-1, max=1)
y <- runif(50, min=-1, max=1)

delta <- 2
S <- matrix(c(1+delta, 0, 0, 1), nrow=2, ncol=2)

end_dots <- S %*% rbind(x,y)

par(lwd=1,mar=(c(3, 3, 2, 2) + 0.1),mgp=c(1.7,0.5,0),bg = background_plot)
plot(end_dots[1,], end_dots[2,], xlab=" ",ylab=" ",pch=19,cex.lab=1.2,cex=2,col="blue")
points(x,y, pch=19)
arrows(x, y,end_dots[1,], end_dots[2,], length=0.1)