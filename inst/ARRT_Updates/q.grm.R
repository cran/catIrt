# q = 1 - p = ilogit[exp(a*(thet - b.k))]]  

q.grm <-
function(x, theta){
    
# If x is a vector: a and b.k are elements of that vector
  if( is.null( dim(x) ) )    
    { a <- x[1]; b.k <- x[2] }
    
# If x is a matrix: a, b, and c are columns of that matrix
  else
    { a <- x[ , 1]; b.k <- x[ , 2] }
    
  return( exp( -a * ( theta - b.k ) ) / ( 1 + exp( -a * ( theta - b.k ) ) ) )
    
} # END q.grm FUNCTION
