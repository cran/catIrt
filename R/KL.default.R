KL.default <-
function( params,        # parameters over which to calculate
          theta,         # value of theta
          delta = .1 )   # the indifference region specification
{
  
# Turn params into a matrix:
  params <- rbind(params)
  
# Then find the number of people:
  N <- length(theta)
     
#~~~~~~~~~~~~~~~~~#
# Argument Checks #
#~~~~~~~~~~~~~~~~~#
## 1 ## (Make sure that params, thet, resp are ALL numeric)
  if( mode(params) != "numeric" | mode(theta) != "numeric" )
    stop( "params and theta need to be numeric" )
    
#~~~~~~~~~~~~~~~~#
# KL Information #
#~~~~~~~~~~~~~~~~#

  if( N == 1 ){
    p0 <- p.brm(params, theta - delta); p1 <- p.brm(params, theta + delta)
    q0 <- q.brm(params, theta - delta); q1 <- q.brm(params, theta + delta)
  } else{
    p0 <- apply(params, MARGIN = 1, FUN = p.brm, theta = theta - delta)
    p1 <- apply(params, MARGIN = 1, FUN = p.brm, theta = theta + delta)
 
    q0 <- apply(params, MARGIN = 1, FUN = q.brm, theta = theta - delta)
    q1 <- apply(params, MARGIN = 1, FUN = q.brm, theta = theta + delta)
  } # END ifelse STATEMENT
  
# To prevent computation problems, work with logs and not probabilities:
  info <- p1 * ( log(p1) - log(p0) ) + q1 * ( log(q1) - log(q0) )
  
# If theta is a scalar, item information is a vector and test information is a scalar
# If theta is a vector, item information is a matrix and test information is a vector
  
  if( N == 1 ){
  
    i.info <- info
    t.info <- sum(info)
    
  } else{
  	
    i.info <- t(info)
    t.info <- colSums(i.info)
    
  } # END ifelse STATEMENT
                  
                  
  return( list(item = i.info, test = t.info) )
  
} # END KL.default FUNCTION