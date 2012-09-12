KL.grm <-
function( params,       # parameters over which to calculate
          theta,        # value(s) of theta
          delta = .1)   # the indifference region specification
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

# To make calling things simpler:
a <- params[ , 1, drop = FALSE]; b <- params[ , -1, drop = FALSE]

# If there is only ONE theta:
  if( length(theta) == 1 & length(delta) == 1 ){
        
    info <- 0
        
    for( k in 1:( dim(b)[2] + 1 ) ){
        
      if( k == 1 ){
        p1.large <- 1
        p1.small <- p.grm(x = cbind(a, b[ , k] ), theta = theta + delta)
       
        p0.large <- 1
        p0.small <- p.grm(x = cbind(a, b[ , k] ), theta = theta - delta)
      }
     
# For the last boundary:
      if( k == ( dim(b)[2] + 1 ) ){
        p1.large <- p.grm(x = cbind(a, b[ , k - 1] ), theta = theta + delta)
        p1.small <- 0
       
        p0.large <- p.grm(x = cbind(a, b[ , k - 1] ), theta = theta - delta)
        p0.small <- 0
      }
     
# Otherwise:
      if( ( k != 1 ) & ( k != ( dim(b)[2] + 1 ) ) ){
        p1.large <- p.grm(x = cbind(a, b[ , k - 1] ), theta = theta + delta)
        p1.small <- p.grm(x = cbind(a, b[ , k] ), theta = theta + delta)
      
        p0.large <- p.grm(x = cbind(a, b[ , k - 1] ), theta = theta - delta)
        p0.small <- p.grm(x = cbind(a, b[ , k] ), theta = theta - delta)
      }
      
      info <- info + ( p1.large - p1.small ) * ( log( p1.large - p1.small ) - log( p0.large - p0.small ) )
        
    } # END for LOOP
        
  } # END if STATEMENT
  
# If there are many thetas
  else{
      
    info <- 0
      
    for( k in 1:( dim(b)[2] + 1 ) ){

# For the first boundary:
      if( k == 1 ){
        p1.large <- 1
        p1.small <- apply(cbind(a, b[ , k]), MARGIN = 1, FUN = p.grm, theta = theta + delta)
          
        p0.large <- 1
        p0.small <- apply(cbind(a, b[ , k]), MARGIN = 1, FUN = p.grm, theta = theta - delta)
      }
   
# For the last boundary:
      if( k == ( dim(b)[2] + 1 ) ){
        p1.large <- apply(cbind(a, b[ , k - 1]), MARGIN = 1, FUN = p.grm, theta = theta + delta)
        p1.small <- 0
          
        p0.large <- apply(cbind(a, b[ , k - 1]), MARGIN = 1, FUN = p.grm, theta = theta - delta)
        p0.small <- 0
      }
   
# Otherwise:
      if( ( k != 1 ) & ( k != ( dim(b)[2] + 1 ) ) ){
        p1.large <- apply(cbind(a, b[ , k - 1]), MARGIN = 1, FUN = p.grm, theta = theta + delta)
        p1.small <- apply(cbind(a, b[ , k]), MARGIN = 1, FUN = p.grm, theta = theta + delta)
          
        p0.large <- apply(cbind(a, b[ , k - 1]), MARGIN = 1, FUN = p.grm, theta = theta - delta)
        p0.small <- apply(cbind(a, b[ , k]), MARGIN = 1, FUN = p.grm, theta = theta - delta)
      }
                     
      info <- info + ( p1.large - p1.small ) * ( log( p1.large - p1.small ) - log( p0.large - p0.small ) )
        
    } # END for LOOP
      
  } # END else STATEMENT
 
 
# If theta is a scalar, item information is a vector and test information is a scalar
# If theta is a vector, item information is a matrix and test information is a vector
  
  i.info <- { if( length(theta) == 1 )   info
              else                     t(info) }
                  
  t.info <- { if( length(theta) == 1 ) sum(info)
              else                     apply(i.info, MARGIN = 2, FUN = sum) }
                  
                  
  return( list(item = i.info, test = t.info) )
  
} # END KL.grm FUNCTION

