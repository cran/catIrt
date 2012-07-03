# p'' = a^2*((exp - exp^2)/(1 + exp)^3) = a*[1 - exp(a*(thet - b))]*q*pder1
# (or alternatively: a^2*[p*q^2 - p^2*q])

pder2.grm <-
function(x, theta){
  
# If x is a vector: a and b.k are elements of that vector
  if( is.null( dim(x) ) )    
    { a <- x[ 1 ]; b.k <- x[ 2 ] }
    
# If x is a matrix: a and b are columns of that matrix
  else
    { a <- x[ , 1]; b.k <- x[ , 2] }
    
 return( a * ( 1 - exp( a * (theta - b.k) ) ) * q.grm(x, theta) * pder1.grm(x, theta) )
    
} # END pder2.grm FUNCTION

