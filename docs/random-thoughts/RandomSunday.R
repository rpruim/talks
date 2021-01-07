require(lattice)
require(mosaic)
trellis.par.set(theme=col.mosaic())

set.seed(123456)

toward <- function( a, b, ratio=1/2 ) {
  return ( a + ratio * ( b - a ) )
}

art1 <- function( n, x=c(0,1,.5), y=c(0,0,sqrt(3)/2), r=resample(1:length(x),n), ratio=1/2 , ...){
  xout <- rep(NA,n)
  yout <- rep(NA,n)
  xout[1] <- x[1]
  yout[1] <- y[1]
  r <- resample( 1:length(x), n )
  for (i in 2:n) {
    xout[i] <- toward( xout[i-1], x[r[i]], ratio )
    yout[i] <- toward( yout[i-1], y[r[i]], ratio )
  }
  print( xyplot( yout ~ xout, xlab="", ylab="", scales=list(draw=FALSE), ...) )
  invisible(data.frame(x=xout, y=yout))
}

d1 <- art1(50000)
d2 <- art1(50000)
pdf('sierpinski.pdf',width=6,height=6)
trellis.par.set(theme=col.mosaic())
print(xyplot(y~x, d1, cex=.1, scales=list(draw=FALSE), xlab="", ylab=""))
print(xyplot(y~x, d2, cex=.1, scales=list(draw=FALSE), xlab="", ylab=""))
dev.off()

pdf('sierpinski-start.pdf', width=6, height=6)
trellis.par.set(theme=col.mosaic())
print(xyplot(y~x, d1, cex=.1, scales=list(draw=FALSE), xlab="", ylab="",alpha=.2,
      panel=function(x,y,...) { 
        panel.xyplot(x,y,...)
        panel.xyplot(x[1:14],y[1:14],type=c('p','l'))
      }
      ))

print(xyplot(y~x, d2, cex=.1, scales=list(draw=FALSE), xlab="", ylab="",alpha=.2,
      panel=function(x,y,...) { 
        panel.xyplot(x,y,...)
        panel.xyplot(x[1:14],y[1:14],type=c('p','l'))
      }
      ))
dev.off()

write.csv(d1,file="d1.csv", row.names=FALSE)
write.csv(d2,file="d2.csv", row.names=FALSE)

