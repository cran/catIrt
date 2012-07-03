lder2.brm <-
function(xu, theta){  # xu is a vector of length 4 -- a, b, c, u (response)
    
# Dividing into response and parameters
  if( is.null( dim(xu) ) )
    { u <- xu[ length(xu) ]; x <- xu[ -length(xu) ] }

  else
    { u <- xu[ , dim(xu)[2] ]; x <- xu[ , -dim(xu)[2] ] }
    
# Calculating the two parts of the second derivative and returning it: 
  lder2.1 <- ( -1 / p.brm(x, theta)^2 ) * pder1.brm(x, theta)^2 + ( 1 / p.brm(x, theta) * pder2.brm(x, theta) )
  lder2.2 <- (  1 / q.brm(x, theta)^2 ) * pder1.brm(x, theta)^2 + ( 1 / q.brm(x, theta) * pder2.brm(x, theta) )

  return( u * lder2.1 + -(1 - u) * lder2.2 )
    
} # END lder2.brm FUNCTION

