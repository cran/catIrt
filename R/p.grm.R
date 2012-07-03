# p = ilogit[exp(a*(thet - b.k))]

p.grm <-
function(x, theta){
  
# If x is a vector: a and b.k are elements of that vector
  if( is.null( dim(x) ) )    
    { a <- x[1]; b.k <- x[2] }

# If x is a matrix: a, b, and c are columns of that matrix
 else
    { a <- x[ , 1]; b.k <- x[ , 2] }
    
  return( 1 / ( 1 + exp( - a * (theta - b.k) ) ) )

} # END p.grm FUNCTION
