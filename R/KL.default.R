KL.default <-
function( params,        # parameters over which to calculate
          theta,         # value of theta
          delta = .1 )   # the indifference region specification
{
  
# Turn params into a matrix:
  if( is.null( dim(params) ) )           # if it's a vector ... -->
    params <- t(params)                  # ... --> turn it into a matrix
    
#~~~~~~~~~~~~~~~~~#
# Argument Checks #
#~~~~~~~~~~~~~~~~~#
## 1 ## (Make sure that params, thet, resp are ALL numeric)
  if( mode(params) != "numeric" | mode(theta) != "numeric" )
    stop( "params and theta need to be numeric" )
    
#~~~~~~~~~~~~~~~~#
# KL Information #
#~~~~~~~~~~~~~~~~#

  if( length(theta) == 1 ){
    p0 <- p.brm(params, theta - delta); p1 <- p.brm(params, theta + delta)
    q0 <- q.brm(params, theta - delta); q1 <- q.brm(params, theta + delta)
  }
  
  else{
    p0 <- apply(params, MARGIN = 1, FUN = p.brm, theta = theta - delta)
    p1 <- apply(params, MARGIN = 1, FUN = p.brm, theta = theta + delta)
 
    q0 <- apply(params, MARGIN = 1, FUN = q.brm, theta = theta - delta)
    q1 <- apply(params, MARGIN = 1, FUN = q.brm, theta = theta + delta)
  }
  
# To prevent computation problems, work with logs and not probabilities:
  info <- p1 * ( log(p1) - log(p0) ) + q1 * ( log(q1) - log(q0) )
  
# If theta is a scalar, item information is a vector and test information is a scalar
# If theta is a vector, item information is a matrix and test information is a vector
  
  i.info <- { if( length(theta) == 1 )   info
              else                     t(info) }
                  
  t.info <- { if( length(theta) == 1 ) sum(info)
              else                     apply(i.info, MARGIN = 2, FUN = sum) }
                  
                  
  return( list(item = i.info, test = t.info) )
  
} # END KL.default FUNCTION
