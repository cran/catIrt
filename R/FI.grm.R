FI.grm <-
function( params,                            # parameters over which to calculate
          theta,                             # values/estimates of theta
          type = c("expected", "observed"),  # which information to calculate
          resp = NULL )                      # a response vector/matrix            
{
  
# First, make sure that resp is NULL if type is "expected"
  if( type == "expected" )
    resp <- NULL
  
# Then turn params into a matrix:
  params <- rbind(params)
      
# And turn response into a matrix:
  resp <- { if( dim(params)[1] > 1 ) rbind(resp)   # ... --> turn it into a multi-column matrix,
            else                     cbind(resp) } # ... --> or a 1-column matrix

# Then find the number of items and people:
  N <- length(theta)
  J <- dim(params)[1]
  
# And find the number of responses:
  n.resp <- ifelse(test = !is.null(resp), yes = dim(resp)[1], no = 0)


#~~~~~~~~~~~~~~~~~#
# Argument Checks #
#~~~~~~~~~~~~~~~~~#

# Then make sure that the arguments are OK:

## 1 ## (Make sure that resp exists if we are calculating observed information)
  if( is.null(resp) & type == "observed" )
    stop( "need response scalar/vector to calculate observed information" )
    
## 2 ## (Make sure that params, theta, resp are ALL numeric)
  if( mode(params) != "numeric" | mode(theta) != "numeric" )
    stop( "params and thet need to be numeric" )
    
  if( !is.null(resp) & mode(resp) != "numeric" )
    stop( "resp needs to be numeric" )

## 3 ## (Make sure that the dimensions of params and response are equal)
  if( !is.null(resp) & ( n.resp != N ) )
    stop( "number of params does not match the length of resp" )
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Expected Fisher Information #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Expected Fisher Information:
# -- sum[P'^2/P]
# -- P is prob of scoring in category k and P' is deriv of P
# -- p is prob of scoring above category k

  if( type == "expected" ){
    
# To make calling things simpler:
    a <- params[ , 1, drop = FALSE]; b <- params[ , -1, drop = FALSE]
     
# If there is only ONE theta:
    if( N == 1 ){
      
      info <- 0
      
      for( k in 1:( dim(b)[2] + 1 ) ){
      
        if( k == 1 ){
          p.large <- 1
          p.small <- p.grm(x = cbind(a, b[ , k] ), theta = theta)
     
          pder1.large <- 0
          pder1.small <- pder1.grm(x = cbind(a, b[ , k] ), theta = theta)
        }
   
# For the last boundary:
        if( k == ( dim(b)[2] + 1 ) ){
          p.large <- p.grm(x = cbind(a, b[ , k - 1] ), theta = theta)
          p.small <- 0
     
          pder1.large <- pder1.grm(x = cbind(a, b[ , k - 1] ), theta = theta)
          pder1.small <- 0
        }
   
# Otherwise:
        if( ( k != 1 ) & ( k != ( dim(b)[2] + 1 ) ) ){
          p.large <- p.grm(x = cbind(a, b[ , k - 1] ), theta = theta)
          p.small <- p.grm(x = cbind(a, b[ , k] ), theta = theta)
    
          pder1.large <- pder1.grm(x = cbind(a, b[ , k - 1] ), theta = theta)
          pder1.small <- pder1.grm(x = cbind(a, b[ , k] ), theta = theta)
        }
    
        info <- info + ( pder1.large - pder1.small )^2 / ( p.large - p.small )
      
      } # END for LOOP
      
    } else{
      
      info <- 0
      
      for( k in 1:( dim(b)[2] + 1 ) ){

# For the first boundary:
        if( k == 1 ){
          p.large <- 1
          p.small <- apply(cbind(a, b[ , k]), MARGIN = 1, FUN = p.grm, theta = theta)
          
          pder1.large <- 0
          pder1.small <- apply(cbind(a, b[ , k]), MARGIN = 1, FUN = pder1.grm, theta = theta)
        }
   
# For the last boundary:
        if( k == ( dim(b)[2] + 1 ) ){
          p.large <- apply(cbind(a, b[ , k - 1]), MARGIN = 1, FUN = p.grm, theta = theta)
          p.small <- 0
          
          pder1.large <- apply(cbind(a, b[ , k - 1]), MARGIN = 1, FUN = pder1.grm, theta = theta)
          pder1.small <- 0
        }
   
# Otherwise:
        if( ( k != 1 ) & ( k != ( dim(b)[2] + 1 ) ) ){
          p.large <- apply(cbind(a, b[ , k - 1]), MARGIN = 1, FUN = p.grm, theta = theta)
          p.small <- apply(cbind(a, b[ , k]), MARGIN = 1, FUN = p.grm, theta = theta)
          
          pder1.large <- apply(cbind(a, b[ , k - 1]), MARGIN = 1, FUN = pder1.grm, theta = theta)
          pder1.small <- apply(cbind(a, b[ , k]), MARGIN = 1, FUN = pder1.grm, theta = theta)
        }
                     
      info <- info + ( pder1.large - pder1.small )^2 / ( p.large - p.small )
        
      } # END for LOOP
      
    } # END ifelse STATEMENT
    
  } # END if STATEMENT
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Observed Fisher Information #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

  if( type == "observed" ){
    
    if( N == 1 ){
    	
      info <- -lder2.grm(xu = cbind(params, c(resp)), theta = theta)
    
    } else{
      
      info <- NULL
      for( i in seq_along(theta) )
        info <- rbind(info, -lder2.grm(xu = cbind(params, c(resp[i , ])), theta = theta[i]) )
        
    } # END ifelse STATEMENT
    
  } # END if STATEMENT

# If theta is a scalar, item information is a vector and test information is a scalar
# If theta is a vector, item information is a matrix and test information is a vector

  if( N == 1 ){
  
    i.info <- info
    t.info <- sum(info)
    
  } else{
  	
    i.info <- t(info)
    t.info <- colSums(i.info)
    
  } # END ifelse STATEMENT
     
  sem <- ifelse( test = signif(t.info) > 0, yes = sqrt( 1 / t.info ), no = NA )
                
                
  return( list(item = i.info, test = t.info, sem = sem, type = type) )
    
} # END FI.grm FUNCTION
