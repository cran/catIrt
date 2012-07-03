# q = 1 - p = (1 - c)[1 - ilogit[exp(a*(thet - b))]]  

q.brm <-
function(x, theta){
    
# If x is a vector: a, b, and c are elements of that vector
  if( is.null( dim(x) ) )    
    { a <- x[1]; b <- x[2]; c <- x[3] }
    
# If x is a matrix: a, b, and c are columns of that matrix
  else
    { a <- x[ , 1]; b <- x[ , 2]; c <- x[ , 3] }
    
  return( (1 - c) * exp( -a * ( theta - b )) / ( 1 + exp( -a * ( theta - b ) ) ) )
    
} # END q.brm FUNCTION
