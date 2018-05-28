n <- 100 # smoothness


setup <- function(n=100){
  x <- seq(from=-3,to=3,len=n)
  plot(x,dnorm(x),axes=FALSE,xlab='',ylab='',lwd=3,type='n',asp=4,xlim=c(-3,3))
}
  
outline <- function(n=100){
  x <- seq(from=-3,to=3,len=n)
  points(x,dnorm(x),type='l',lwd=3)
  abline(0,0)
}

shading <- function(a,b,f=dnorm){
  x <- seq(from=a,to=b,len=n)
  polygon(x=c(x,rev(x)),y=c(rep(0,n),f(rev(x))),col='gray',border=NA)
}

pdf(file="gaussian_diagram1.pdf",width=5,height=3)
{
  setup()
  shading(-3,1)
  outline()
  par(xpd=TRUE)
  text(1,-0.1,'z')
}
dev.off()

pdf(file="gaussian_diagram2.pdf",width=5,height=3)
{
  setup()
  shading(-3,-1)
  outline()
  par(xpd=TRUE)
  text(-1,-0.1,'z')
}
dev.off()

pdf(file="gaussian_diagram3.pdf",width=5,height=3)
{
  setup()
  shading(-3,-1)
  shading(+1,+3)
  outline()
  par(xpd=TRUE)
  text(-1,-0.1,'K')
  text(+1,-0.1,'K')
}
dev.off()

pdf(file="student_diagram.pdf",width=5,height=3)
{
  x <- seq(from=-2,to=2,len=n)
  plot(x,dnorm(x),axes=FALSE,xlab='',ylab='',lwd=3,type='n',xlim=c(-2,2),ylim=c(0,0.35))
  x <- seq(from=-2,to=2,len=n)
  shading(-2,+1,f=function(x){dt(x,df=1)})
  points(x,dt(x,df=1),type='l',lwd=3)
  abline(0,0)
  par(xpd=TRUE)
  text(+1,-0.1,'z')
}
dev.off()
