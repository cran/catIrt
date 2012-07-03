# p' = (1 - c)*a*p*q

pder1.brm <-
function(x, theta){
  
# If x is a vector: a, b, and c are elements of that vector
  if( is.null( dim(x) ) )    
    { a <- x[1]; b <- x[2]; c <- x[3] }
    
# If x is a matrix: a, b, and c are columns of that matrix
  else
    { a <- x[ , 1]; b <- x[ , 2]; c <- x[ , 3] }
    
  ex <- exp( a * ( theta - b ) )
    
  return( (1 - c) * a * ex / ( 1 + ex )^2 )
    
} # END pder1.brm FUNCTION
