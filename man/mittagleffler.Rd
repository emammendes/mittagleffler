\name{mittagleffler}
\alias{mittagleffler}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function mittagleffler}
\description{
This function implements the Mittag-Leffler function E\{alpha,beta\}(z) 
           evaluated with accuracy 10^(-fi) for z. 
           alpha and beta are real scalars, fi is integer and z is complex.}
\usage{
mittagleffler(zvector,a,b=1,fi=6) } 
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{zvector}{a scalar or vector - it will be treated as a complex number}
  \item{a}{alpha parameter in E\{alpha,beta\}(z) }
  \item{b}{beta parameter in E\{alpha,beta\}(z)}
  \item{fi}{precision}
}
\details{
No previous data is used.
}
\value{
Returns a \code{\link[base]{complex}} object }
\references{Please see Computation of the Mittag-Leffler function and its derivatives. 
Fract. Calc. Appl. Anal. 5(2002), 491-518 by  R.Gorenflo,  J.Loutchko, and Yu. Luchko}
\author{Eduardo M. A. M. Mendes}
\examples{
lambda=-1
alpha=0.2
t=0.5
z=lambda*(t^alpha)
res=mittagleffler(z,alpha)
t=(-1)*seq(0,5,0.01)^alpha
res=mittagleffler(t,alpha)
plot(seq(0,5,0.01),res[,1],type="b",xlab="Time",ylab="Amplitude")
}
\keyword{methods}% at least one, from doc/KEYWORDS
