# mittagleffler
Mittagleffler is a package for R that implements the Mittag-Leffler function. It uses a f90 code. The original fortran code by Davide Verotta was modified to deal with complex numbers.

## How to install

### The package

Install the devtools package if you haven't already. Then run:

        library(devtools)
        install_github("emammendes/mittagleffler")

So far the package is only available as source code, so you'll need a functional R build environment (Rtools on Windows, XCode on OS X). 

## Getting started 

        library(mittagleffler)
        lambda <- -1; alpha <- 0.2; 
        t <- (-1)*seq(0,5,0.01)^alpha
        res <- mittagleffler(t,alpha)
        plot(seq(0,5,0.01),res,type="b",xlab="Time",ylab="Amplitude")

A help file is available.

## On the fortran code

The original fortran code is provided by Davide Verotta.  I have simply modified and updated to accommodate the free form of Fortran 90, to deal with complex numbers and to be part of a R-package.   I thank Davide for allowing me to make the code publicly available.

The original fortran code is based on a Matlab function developed by Igor Podlubny and Martin Kacena which was a slightly modified version of Luchko's Mathematica-code and uses the algorithms presented in the paper Computation of the Mittag-Leffler function and its derivatives. Fract. Calc. Appl. Anal. 5(2002), 491-518 by  R.Gorenflo,  J.Loutchko, and Yu. Luchko

## How to reproduce Figure 15 of the paper entitled Computation of the Mittag-Leffler function and its derivatives

        library(mittagleffler)
        alph <- 1.25; 
        t=seq(0,50,0.1)
        y=mittagleffler(t*exp(sqrt(as.complex(-1))*pi),alph)
        plot(t,Re(y[,1]),type="l",xlab="Time",ylab="Amplitude",main=sprintf("alpha=%g and arg=i*pi/4",alph));
        lines(t,Re(y[,2]),type="l",col="red")
        grid(6,5,lty="dotted")
        legend("topright",c("MittagLeffler","Derivative"),lwd=c(2,2),col=c("black","red"),cex=0.6,bty="n",y.intersp=0.6)
