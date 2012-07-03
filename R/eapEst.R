eapEst <-
function(resp,                             # The vector of responses
         params,                           # The item parameters
         int = c(-6, 6),                   # The limits of integration
         mod = c("brm", "grm"),            # The model                        
         ddist = dnorm, quad = 33, ... ){  # The prior distribution stuff:

  require(sfsmisc)
  
# First turn params into a matrix:
  if( is.null( dim(params) ) )                       # if it's a vector ... -->
    params <- t(params)                              # ... --> turn it into a matrix

# And turn response into a matrix:
  if( !is.null(resp) & is.null( dim(resp) ) )                    # if it's a vector ... -->
    resp <- { if( dim(params)[1] > 1 ) matrix( resp, nrow = 1 )  # ... --> turn it into a multi-column matrix,
              else                     matrix( resp, ncol = 1) } # ... --> or a 1-column matrix

  class(params) <- c(mod, "matrix")

#~~~~~~~~~~~~~~~~~#
# Argument Checks #
#~~~~~~~~~~~~~~~~~#

# Make sure that the arguments are OK:

## 1 ## (Make sure that params and resp are ALL numeric)
  if( mode(params) != "numeric" )
    stop( "params need to be numeric" )
    
  if( !is.null(resp) & mode(resp) != "numeric" )
    stop( "resp needs to be numeric" )

## 2 ## (Make sure that the dimensions of params and response are equal)
  if( !is.null(resp) & ( dim(resp)[2] != dim(params)[1] ) )
    stop( "number of params does not match the length of resp" )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Expected A Posteriori Est #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Indicate the lower/upper boundary of integration:
  if( is.null(int) )
    int <- c(-6, 6)

  l <- int[1]; u <- int[2]
  
# Make sure that the lower/upper points of integration is within the legal range:
  tmp.x <- seq(l, u, by = .01)
  tmp.y <- ddist(x = tmp.x, ... )
  
# Usually, if the density doesn't exist, it prints "0", but it might print NA:
  l <- min( tmp.x[signif(tmp.y) != 0 & !is.na(tmp.y)] )
  u <- max( tmp.x[signif(tmp.y) != 0 & !is.na(tmp.y)] )
  
  est <- NULL

# The Likelihood function used in the EAP integration:
  LikFun <- function( ... )
               exp( get(paste("logLik.", mod, sep = "") )( ... ) )

# For each person:
  for( i in 1:dim(resp)[1] ){
    
# Set up the numerator and denominator of integral:
    eap.num <- function( theta ) theta * LikFun(theta = theta, u = resp[i, ], x = params) * ddist(x = theta, ... )
    eap.den <- function( theta )         LikFun(theta = theta, u = resp[i, ], x = params) * ddist(x = theta, ... )
    
# Set up the bounds of integration:
    X  <- seq(l, u, length = quad)
    Y1 <- eap.num( X )
    Y2 <- eap.den( X )
    
# And integrate:
    est[i] <- integrate.xy(X, Y1) / integrate.xy(X, Y2)
  
  } # END FOR LOOP
  
# Round the estimated value to three/four? decimal places:          
  est <- round(est, digits = 4)
  
# And pull out the information as well as the SEM:
  info <- get(paste("FI.", mod, sep = ""))(params = params,
                                           theta = est,
                                           type = "observed",
                                           resp = resp)
  
# NOTE: NEED TO ADD THE ACTUAL INFORMATION/SEM CORRESPONDING TO EAP?
  
  list(theta = est, info = info$test, sem = info$sem)
  
} # END eapEst FUNCTION