TIEplot2 <- function(x, y, xname, yname){
	means<-tapply(y, x, mean)
	s.dev<-tapply(y, x, sd)
	xlevels<-1:nlevels(x)

	plot(xlevels, means, ylim=c(min(y), max(y)), pch=16,
	axes=F, xlab=xname, ylab=yname, type="n", xlim=c(0.5, nlevels(x)+0.5))

	axis(2, las=1)
	axis(1, xlevels, levels(x))
	box()

	arrows(xlevels, means-s.dev, xlevels, means+s.dev, angle=90, code=3,
	length =  0.25 , col = "grey", lwd = 7.5 )

	points(xlevels, means, pch = 19, col="grey", cex = 6 )
	points(xlevels, means, pch = 19, cex = 1.5 )
	points(xlevels, means, pch = 8, cex = 2.5 )
	points(xlevels, means, pch = 1, cex = 4 )

  
}


