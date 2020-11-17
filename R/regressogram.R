regressogram <- function(x,y,nbins=10,show.bins=TRUE,show.means=TRUE,show.lines=TRUE,
                         x.lab="X",y.lab="Y",main="TITLE"){
  xy <- data.frame(x=x,y=y)
  xy <- xy[order(xy$x),]
  z <- cut(xy$x,breaks=seq(min(xy$x),max(xy$x),length=nbins+1),
           labels=1:nbins,include.lowest=TRUE)
  xyz <- data.frame(xy,z=z)
  MEANS <- c(by(xyz$y,xyz$z,FUN=mean))
  x.seq <- seq(min(x),max(x),length=nbins+1)
  midpts <- (x.seq[-1]+x.seq[-(nbins+1)])/2
  d2 <- data.frame(midpts=midpts,MEANS=MEANS)
  p <- ggplot(xyz, aes(x,y)) + geom_point() + ggtitle(main) + xlab(x.lab) + 
    ylab(y.lab) + theme(text = element_text(size = 20))
  if(show.bins) p <- p + geom_vline(xintercept=x.seq[-c(1,nbins+1)],linetype="dashed",color="blue")
  if(show.means) p <- p + geom_point(data=d2, aes(x=midpts, y=MEANS), color="red", shape=18, size=5) 
  if(show.lines) p <- p + geom_line(data=d2, aes(x=midpts, y=MEANS), color="red") 
  return(p)
}
