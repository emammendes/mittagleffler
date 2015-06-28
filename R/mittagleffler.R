mittagleffler <- function (zvector, a, b=1.0, fi=6)
{
  if (!is.numeric(a))
    stop('a is numeric')
  if (!is.numeric(b))
    stop('b is numeric')  
  if (!is.numeric(fi))
    stop('fi is numeric')
    
  if ((a <= 0) || (a > 5))
    stop('a should be greater than zero and less than 5.')
    
  if (b > 5)
    stop('b should be less than 5.')
    
  fi=ceiling(fi)
  
  if (fi < 1) 
    stop('fi is an integer greater than 0.')
    
  if (fi > 8)
    stop('fi is a positive integer no greater than 8.')
    
  auxvector=as.vector(zvector)
  leng=length(auxvector)  
  resvector=auxvector
  resderivvector=auxvector
    
  res <- 0
  resderiv <- 0
  
  for (i in seq(1,leng))
  {
  	out <- .Fortran("call_mlfv", 
                    res = as.complex(res), 
                    resderiv = as.complex(resderiv),
                    alf = as.numeric(a), 
                    bet = as.numeric(b), 
                    z = as.complex(auxvector[i]),
                    fi = as.integer(fi));
  	resvector[i]=out$res
  	resderivvector[i]=out$resderiv
  }
  if (is.complex(zvector))
  	{
  	return(cbind(resvector,resderivvector));
  	}
  else
  	{
  	return(cbind(Re(resvector),Re(resderivvector)))
  	}
}
