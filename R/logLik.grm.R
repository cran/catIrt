logLik.grm <-
function(u, x, theta,
         type = c("MLE", "BME"),
         ddist = dnorm, ... ) # ddist and ... are prior distribution stuff
{

# u is the response, and x are the parameters.

  if( missing(type) )
    type <- "MLE"

# Breaking up the parameters:
  a <- x[ , 1, drop = FALSE]; b <- x[ , -1, drop = FALSE]

# If theta is a vector, we can calculate directly:
  if( length(theta) == 1 ){
    
## Calculating the loglikelihood without the Bayesian part: ##
    
    logLik <- 0
  
    for( k in 1:( dim(b)[2] + 1 ) ){
      
      y <- as.numeric(u == k)

# For the first boundary:
      if( k == 1 ){
        p.large <- 1
        p.small <- p.grm(x = cbind(a, b[ , k] ), theta = theta)
      }
   
# For the last boundary:
      if( k == ( dim(b)[2] + 1 ) ){
        p.large <- p.grm(x = cbind(a, b[ , k - 1] ), theta = theta)
        p.small <- 0
      }
   
# Otherwise:
      if( ( k != 1 ) & ( k != ( dim(b)[2] + 1 ) ) ){
        p.large <- p.grm(x = cbind(a, b[ , k - 1] ), theta = theta)
        p.small <- p.grm(x = cbind(a, b[ , k] ), theta = theta)
      }
  
      logLik <- logLik + y * log( p.large - p.small )
  
    } # END for LOOP
    
    logLik <- sum(logLik)
    
  } else{
    
    logLik <- 0
    
# The following is the only way to get the responses to line up:
    if( is.null( dim(u) ) )
      u <- matrix(u, nrow = length(theta), ncol = dim(b)[1], byrow = TRUE )
    
    for( k in 1:( dim(b)[2] + 1 ) ){
    
      y <- u == k
    
# For the first boundary:
      if( k == 1 ){
        p.large <- 1
        p.small <- apply( cbind(a, b[ , k]), MARGIN = 1, FUN = p.grm, theta = theta )
      }

# For the last boundary:
      if( k == ( dim(b)[2] + 1 ) ){
        p.large <- apply( cbind(a, b[ , k - 1]), MARGIN = 1, FUN = p.grm, theta = theta )
        p.small <- 0
      }
       
# Otherwise:
      if( ( k != 1 ) & ( k != ( dim(b)[2] + 1 ) ) ){
        p.large <- apply( cbind(a, b[ , k - 1]), MARGIN = 1, FUN = p.grm, theta = theta )
        p.small <- apply( cbind(a, b[ , k]), MARGIN = 1, FUN = p.grm, theta = theta )
      }
      
      logLik <- logLik + y * log( p.large - p.small )
      
    } # END for LOOP
            
    logLik <- rowSums(logLik)
    
  } # END ifelse STATEMENT


## Now, the Bayesian part: ##

  if( type == "MLE" )
    bme <- 1
  if( type == "BME" )
    bme <- ddist(x = theta, ... )
  
# if there is a silly prior, set it to something very small
  bme <- ifelse(test = bme <= 0, yes = bme <- 1e-15 , no = bme)
  
  return( logLik + log(bme) )
  
} # END logLik.grm FUNCTION