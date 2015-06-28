# A simple function to reproduce Figres 5 to 15 of the paper Computation of the Mittag-Leffler
#           function and its derivatives. Fract. Calc. Appl. Anal. 5(2002), 491-518 
#           by R.Gorenflo, J.Loutchko, and Yu. Luchko

test_mlfv <- function(fig=5)
{
  
fig=as.integer(fig)

if (!is.numeric(fig))
    stop('fig is an integer between 5 and 15.')
  
if ((fig < 5) || (fig > 15))
    stop('fig is an integer between 5 and 15.')

require(mittagleffler);

if (fig == 5)
{
  t=seq(0,10,0.1)
  alph=0.25
  y=mittagleffler(-t,alph)
  plot(t,y[,1],type="l",xlab="Time",ylab="Amplitude",main=sprintf("alpha=%g and z=-t",alph),lwd=2.5,ylim=c(0,1));
  lines(t,y[,2],type="l",col="red",lwd=1.0)
  grid(6,5,lty="dotted")
  legend("topright",c("MittagLeffler","Derivative"),lwd=c(2,2),col=c("black","red"),cex=0.6,bty="n",y.intersp=0.6)
}
  
if (fig == 6)
{
  t=seq(0,50,0.1)
  alph=1.75
  y=mittagleffler(-t,alph)
  plot(t,y[,1],type="l",xlab="Time",ylab="Amplitude",main=sprintf("alpha=%g and z=-t",alph),lwd=2.5,ylim=c(-0.6,1));
  lines(t,y[,2],type="l",col="red",lwd=1.0)
  grid(6,5,lty="dotted")
  legend("topright",c("MittagLeffler","Derivative"),lwd=c(2,2),col=c("black","red"),cex=0.6,bty="n",y.intersp=0.6)
}

if (fig == 7)
  {
  t=seq(0,100,0.1)
  alph=2.25
  y=mittagleffler(-t,alph)
  plot(t,y[,1],type="l",xlab="Time",ylab="Amplitude",main=sprintf("alpha=%g and z=-t",alph),lwd=2.5,ylim=c(-1.8,2.7));
  lines(t,y[,2],type="l",col="red",lwd=1.0)
  grid(6,5,lty="dotted")
  legend("topleft",c("MittagLeffler","Derivative"),lwd=c(2,2),col=c("black","red"),cex=0.6,bty="n",y.intersp=0.6)
}
  
if (fig == 8)
{
  alph=0.75
  t=seq(0,5,0.1)
  y=abs(mittagleffler(t*exp(sqrt(as.complex(-1))*alph*pi/4),alph))
  plot(t,y[,1],type="l",xlab="Time",ylab="Absolute Value",main=sprintf("alpha=%g and arg=i*alpha*pi/4",alph));
  lines(t,y[,2],type="l",col="red")
  grid(6,5,lty="dotted")
  legend("topleft",c("MittagLeffler","Derivative"),lwd=c(2,2),col=c("black","red"),cex=0.6,bty="n",y.intersp=0.6)
}

if (fig == 9)
{
  alph=0.75
  t=seq(0,50,0.1)
  z=t*exp(sqrt(as.complex(-1))*alph*pi/2)
  y=abs(mittagleffler(t*exp(sqrt(as.complex(-1))*alph*pi/2),alph))
  plot(t,y[,1],type="l",xlab="Time",ylab="Absolute Value",main=sprintf("alpha=%g and arg=i*alpha*pi/2",alph));
  grid(6,5,lty="dotted")
  abline(h=1/alph, col="purple")
}

if (fig == 10)
{
  alph=0.75
  t=seq(0,50,0.1)
  y=abs(mittagleffler(t*exp(sqrt(as.complex(-1))*alph*3*pi/4),alph))
  plot(t,y[,1],type="l",xlab="Time",ylab="Absolute Value",main=sprintf("alpha=%g and arg=i*3*alpha*pi/4",alph));grid(6,5,lty="dotted")
  lines(t,y[,2],type="l",col="red")
  grid(6,5,lty="dotted")
  legend("topright",c("MittagLeffler","Derivative"),lwd=c(2,2),col=c("black","red"),cex=0.6,bty="n",y.intersp=0.6)
}
  
if (fig == 11)
{
  alph=0.75
  t=seq(0,50,0.1)
  y=mittagleffler(t*exp(sqrt(as.complex(-1))*pi),alph)
  plot(t,Re(y[,1]),type="l",xlab="Time",ylab="Amplitude",main=sprintf("alpha=%g and arg=i*pi/4",alph));grid(6,5,lty="dotted")
  lines(t,Re(y[,2]),type="l",col="red")
  grid(6,5,lty="dotted")
  legend("topright",c("MittagLeffler","Derivative"),lwd=c(2,2),col=c("black","red"),cex=0.6,bty="n",y.intersp=0.6)
}
# alpha = 1.25

if (fig == 12)
{  
  alph=1.25
  t=seq(0,5,0.1)
  y=abs(mittagleffler(t*exp(sqrt(as.complex(-1))*alph*pi/4),alph))
  plot(t,y[,1],type="l",xlab="Time",ylab="Absolute Value",main=sprintf("alpha=%g and arg=i*alpha*pi/4",alph));
  lines(t,y[,2],type="l",col="red")
  grid(6,5,lty="dotted")
  legend("topleft",c("MittagLeffler","Derivative"),lwd=c(2,2),col=c("black","red"),cex=0.6,bty="n",y.intersp=0.6)
}

if (fig == 13)
{
  alph=1.25
  t=seq(0,50,0.1)
  z=t*exp(sqrt(as.complex(-1))*alph*pi/2)
  y=abs(mittagleffler(t*exp(sqrt(as.complex(-1))*alph*pi/2),alph))
  plot(t,y[,1],type="l",xlab="Time",ylab="Absolute Value",main=sprintf("alpha=%g and arg=i*alpha*pi/2",alph),ylim=c(0.25,1));
  grid(6,5,lty="dotted")
  abline(h=1/alph, col="purple")
  lines(t,y[,2],type="l",col="red")
  grid(6,5,lty="dotted")
  legend("topright",c("MittagLeffler","Derivative"),lwd=c(2,2),col=c("black","red"),cex=0.6,bty="n",y.intersp=0.6)
}

if (fig == 14)
{
  alph=1.25
  t=seq(0,50,0.1)
  y=abs(mittagleffler(t*exp(sqrt(as.complex(-1))*alph*3*pi/4),alph))
  plot(t,y[,1],type="l",xlab="Time",ylab="Absolute Value",main=sprintf("alpha=%g and arg=i*3*alpha*pi/4",alph));grid(6,5,lty="dotted")
  lines(t,y[,2],type="l",col="red")
  grid(6,5,lty="dotted")
  legend("topright",c("MittagLeffler","Derivative"),lwd=c(2,2),col=c("black","red"),cex=0.6,bty="n",y.intersp=0.6)
}
  
if (fig == 15)
{
  alph=1.25
  t=seq(0,50,0.1)
  y=mittagleffler(t*exp(sqrt(as.complex(-1))*pi),alph)
  plot(t,Re(y[,1]),type="l",xlab="Time",ylab="Amplitude",main=sprintf("alpha=%g and arg=i*pi/4",alph));grid(6,5,lty="dotted")
  lines(t,Re(y[,2]),type="l",col="red")
  grid(6,5,lty="dotted")
  legend("topright",c("MittagLeffler","Derivative"),lwd=c(2,2),col=c("black","red"),cex=0.6,bty="n",y.intersp=0.6)
}
  
test_mlfv=sprintf("Figure %g Done",fig)

}
