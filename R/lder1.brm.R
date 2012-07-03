# l' = sum[ (u - p)*p'/(p*q) ] #

lder1.brm <-
function(u, x, theta,
         type = c("MLE", "WLE")  ) # WLE gives weighted maximum likelihood score fct
{  

# u is the response, and x are the parameters.
  
# Calculating the probability of response:
  p <- p.brm(x, theta)
  q <- q.brm(x, theta)
  
# Calculating the first and second derivatives:
  pder1 <- pder1.brm(x, theta)
  pder2 <- pder2.brm(x, theta)

# Calculating Warm correction:
  I <- sum( pder1^2 / (p * q) )
  H <- sum( (pder1 * pder2)  / (p * q) )
  
  if( type == "MLE" )
    return( sum( (u - p) * pder1 / (p * q) ) )
    
  if( type == "WLE" )
    return( sum( (u - p) * pder1 / (p * q) ) + H / (2 * I) )
  
} # END lder1.brm FUNCTION

