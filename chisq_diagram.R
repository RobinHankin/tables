pdf(file="chisq_diagram.pdf",width=5,height=3)
df <- 4
n <- 100 # smoothness
zval <- 6
xmax <- 10  # extent of horizontal axis
par(lend=1)
par(mar=c(2,0,0,0))
x <- seq(from=0,to=xmax,len=n)
plot(x,dchisq(x,df=df),axes=FALSE,xlab='',ylab='',lwd=3,type='n',asp=33)

x <- seq(from=0,to=zval,len=n)
polygon(x=c(x,rev(x)),y=c(rep(0,n),dchisq(rev(x),df=df)),col='gray',border=NA)

x <- seq(from=0,to=xmax,len=n)
points(x,dchisq(x,df=df),type='l',lwd=3)
segments(x0=0,y0=0,x1=xmax)
par(xpd=TRUE)
text(zval,-0.015,'z')

dev.off()
