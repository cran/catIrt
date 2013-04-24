lder1.grm <-
function(u, x, theta,
         type = c("MLE", "WLE") ) # WLE gives weighted maximum likelihood score fct
{

# u is the response, and x are the parameters.
  if( is.null( dim(x) ) )
    x <- t(x)

    a <- x[ , 1, drop = FALSE]; b <- x[ , -1, drop = FALSE]
  
  if( is.null( dim(b) ) )
    b <- t(b)
        
# I IS FISHER INFORMATION:
  I <- FI.grm(params = x, theta = theta, type = "expected")$test
  
## H IS WARM CORRECTION, lder1 IS REGULAR SCORE FUNCTION ##
  H <- 0
  lder1 <- 0
  
# Setting this up algorithmically for all of the boundaries:
  for( k in 1:( dim(b)[2] + 1 ) ){
   
     y <- as.numeric(u == k)
   
# For the first boundary:
     if( k == 1 ){
       p.large <- 1
       p.small <- p.grm(x = cbind(a, b[ , k] ), theta = theta)
     
       pder1.large <- 0
       pder1.small <- pder1.grm(x = cbind(a, b[ , k] ), theta = theta)
     
       pder2.large <- 0
       pder2.small <- pder2.grm(x = cbind(a, b[ , k] ), theta = theta)
     }
   
# For the last boundary:
     if( k == ( dim(b)[2] + 1 ) ){
       p.large <- p.grm(x = cbind(a, b[ , k - 1] ), theta = theta)
       p.small <- 0
     
       pder1.large <- pder1.grm(x = cbind(a, b[ , k - 1] ), theta = theta)
       pder1.small <- 0
     
       pder2.large <- pder2.grm(x = cbind(a, b[ , k - 1] ), theta = theta)
       pder2.small <- 0
     }
   
# Otherwise:
    if( ( k != 1 ) & ( k != ( dim(b)[2] + 1 ) ) ){
      p.large <- p.grm(x = cbind(a, b[ , k - 1] ), theta = theta)
      p.small <- p.grm(x = cbind(a, b[ , k] ), theta = theta)
    
      pder1.large <- pder1.grm(x = cbind(a, b[ , k - 1] ), theta = theta)
      pder1.small <- pder1.grm(x = cbind(a, b[ , k] ), theta = theta)
    
      pder2.large <- pder2.grm(x = cbind(a, b[ , k - 1] ), theta = theta)
      pder2.small <- pder2.grm(x = cbind(a, b[ , k] ), theta = theta)
    }
    
    lder1 <- lder1 + ( y * ( pder1.large - pder1.small ) ) / ( p.large - p.small )
  
    if( type == "WLE" )
      H   <- H + ( ( pder1.large - pder1.small ) * ( pder2.large - pder2.small ) ) / ( p.large - p.small )
  
  } # END for k LOOP
  
  if( type == "MLE" ){
  	
    return( sum(lder1) )
    
  } else if( type == "WLE" ){
  
    return( sum(lder1) + sum(H) / (2 * I) )
    
  } # END ifelse STATEMENT
  
} # END lder1.grm FUNCTION

