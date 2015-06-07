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
    
  if (fi > 20)
    stop('fi is a positive integer no greater than 20.')
    
  auxvector=as.vector(zvector)
  leng=length(auxvector)  
  resvector=auxvector
    
  res <- 0
  
  for (i in seq(1,leng))
  {
  	out <- .Fortran("call_mlfv", 
                    res = as.complex(res), 
                    alf = as.numeric(a), 
                    bet = as.numeric(b), 
                    z = as.complex(auxvector[i]),
                    fi = as.integer(fi));
  	resvector[i]=out$res
  }
  if (is.complex(zvector))
  	{
  	return(resvector);
  	}
  else
  	{
  	return(Re(resvector))
  	}
}
