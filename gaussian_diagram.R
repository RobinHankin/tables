pdf(file="gaussian_diagram.pdf",width=5,height=3)
n <- 100 # smoothness
x <- seq(from=-3,to=3,len=n)
plot(x,dnorm(x),axes=FALSE,xlab='',ylab='',lwd=3,type='n',asp=4,xlim=c(-3,3))

x <- seq(from=-3,to=1,len=n)
polygon(x=c(x,rev(x)),y=c(rep(0,n),dnorm(rev(x))),col='gray',border=NA)

x <- seq(from=-3,to=3,len=n)
points(x,dnorm(x),type='l',lwd=3)
abline(0,0)
par(xpd=TRUE)
text(1,-0.1,'z')

dev.off()
