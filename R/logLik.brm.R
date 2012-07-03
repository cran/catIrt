# l = sum[ u*log(p) + (1 - u)*log(1 - p) ] #

logLik.brm <-
function(u, x, theta,
         type  = c("MLE", "BME"),
         ddist = dnorm, ... ) # ddist and ... are prior distribution stuff
{
  
# u is the response, and x are the parameters.

  if( missing(type) )
    type <- "MLE"

## Calculating the loglikelihood without the Bayesian part: ##

# If thet is a scalar, direct calculation is the quickest.
  if( length(theta) == 1 ){
  	
  	logLik <- sum( u * log( p.brm(x, theta) ) + (1 - u) * log( q.brm(x, theta) ) )
  	
  } else{

# If theta is a matrix, we need to use the apply command.
  	
# The following is the only way to get the responses to line up:
  	if( is.null( dim(u) ) )
      u <- matrix(u, nrow = length(theta), ncol = dim(x)[1], byrow = TRUE )
      
    logLik <- {      u  * log( apply( x, MARGIN = 1, FUN = p.brm, theta = theta) ) +
                (1 - u) * log( apply( x, MARGIN = 1, FUN = q.brm, theta = theta) ) }            
    logLik <- apply(logLik, MARGIN = 1, FUN = sum)
  } # END else STATEMENT


## Now, the Bayesian part: ##
  
  if( type == "MLE" )
    bme <- 1
  if( type == "BME" )
    bme <- ddist(x = theta, ... )
  
# if there is a silly prior, set it to something very small
  bme <- ifelse(test = bme <= 0, yes = bme <- 1e-15 , no = bme)
  
  return( logLik + log(bme) )
  
} # END logLik.brm FUNCTION