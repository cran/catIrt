KL.grm <-
function( params,       # parameters over which to calculate
          theta,        # value(s) of theta
          delta = .1)   # the indifference region specification
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

# To make calling things simpler:
  a <- params[ , 1, drop = FALSE]; b <- params[ , -1, drop = FALSE]

# If there is only ONE theta:
  if( N == 1 ){
        
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
        
  } else{
      
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
      
  } # END ifelse STATEMENT
 
 
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
  
} # END KL.grm FUNCTION

