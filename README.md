# mittagleffler
Mittagleffler is a package for R that implements the Mittag-Leffler function. It uses a f90 code. The original fortran code by Davide Verotta was modified to deal with complex numbers.

## How to install

### The package

Install the devtools package if you haven't already. Then run:

        library(devtools)
        install_github("emammendes/mittagleffler")

So far the package is only available as source code, so you'll need a functional R build environment (Rtools on Windows, XCode on OS X). 

## Getting started 

        lambda <- -1; alpha <- 0.2; 
        t=(-1)*seq(0,5,0.01)^alpha
        res=mittagleffler(t,alpha)
        plot(seq(0,5,0.01),res,type="b",xlab="Time",ylab="Amplitude")

A help file is available.
