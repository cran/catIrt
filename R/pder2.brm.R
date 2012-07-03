# p'' = (1 - c)*a^2*((exp - exp^2)/(1 + exp)^3) = a*[1 - exp(a*(thet - b))]*q*pder1
# (or alternatively: a^2*[p*q^2 - p^2*q])

pder2.brm <-
function(x, theta){
  
# If x is a vector: a, b, and c are elements of that vector
  if( is.null( dim(x) ) )    
    { a <- x[ 1 ]; b <- x[ 2 ]; c <- x[ 3 ] }
    
# If x is a matrix: a, b, and c are columns of that matrix
  else
    { a <- x[ , 1]; b <- x[ , 2]; c <- x[ , 3] }
    
  ex <- exp( a * ( theta - b ) )
    
  return( (1 - c) * a^2 * ( ( ex - ex^2 ) / ( 1 + ex )^3 ) )
    
} # END pder2.brm FUNCTION

