pdf(file="fisher_diagram.pdf",width=5,height=3)
df1 <- 8
df2 <- 8
n <- 500 # smoothness
zval <- qf(0.95,df1,df2)
xmax <- 5  # extent of horizontal axis
par(lend=1)
par(mar=c(2,0,0,0))
x <- seq(from=0,to=xmax,len=n)
plot(x,df(x,df1=df1,df2=df2),axes=FALSE,xlab='',ylab='',type='n',asp=3)

x <- seq(from=0,to=zval,len=n)
polygon(x=c(x,rev(x)),y=c(rep(0,n),df(rev(x),df1=df1,df2=df2)),col='gray',border=NA)

x <- seq(from=0,to=xmax,len=n)
points(x,df(x,df1=df1,df2=df2),type='l',lwd=3)
segments(x0=0,y0=0,x1=xmax)
par(xpd=TRUE)
text(zval,-0.045,'z')

dev.off()
