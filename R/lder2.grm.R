lder2.grm <-
function(xu, theta){
    
# Dividing into response and parameters
  xu <- rbind(xu)

  u <- xu[ , dim(xu)[2] ]; a <- xu[ , 1, drop = FALSE]; b <- xu[ , -c(1, dim(xu)[2]), drop = FALSE]
  
  lder2 <- 0
  
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
      pder2.small <- pder2.grm(x = cbind(a, b[ , k] ), theta = theta )
    }
  
    lder2.1 <-  ( -1 / ( p.large - p.small )^2 ) * ( ( pder1.large - pder1.small )^2 )
    lder2.2 <-  (  1 / ( p.large - p.small )   ) * ( ( pder2.large - pder2.small ) )
    
    lder2 <- lder2 + ( y * ( lder2.1 + lder2.2 ) )

  } # END for k LOOP
  
# Note: The middle boundaries are exactly the same as the others.
#       Only, we have a composite term (p_{ij(k - 1)} - p_{ijk}) rather than a single term.

  return( lder2 )
    
} # END lder2.grm FUNCTION

