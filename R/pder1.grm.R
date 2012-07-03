# p' = a*p

pder1.grm <-
function(x, theta){
  
# If x is a vector: a and b are elements of that vector
  if( is.null( dim(x) ) )    
    { a <- x[1]; b.k <- x[2] }
    
# If x is a matrix: a, b, and c are columns of that matrix
  else
    { a <- x[ , 1]; b.k <- x[ , 2] }
    
  return( a * p.grm(x, theta) * q.grm(x, theta) )
    
} # END pder1.grm FUNCTION

