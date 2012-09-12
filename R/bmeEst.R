bmeEst <-
function(resp,                         # The vector of responses
         params,                       # The item parameters
         range = c(-6, 6),             # The integer to maximize over
         mod = c("brm", "grm"),        # The model
         ddist = dnorm, ... ){         # The prior distribution stuff:

# First turn params into a matrix:
  if( is.null( dim(params) ) )                       # if it's a vector ... -->
    params <- t(params)                              # ... --> turn it into a matrix
    
# And turn response into a matrix:
# And turn response into a matrix:
  if( !is.null(resp) & is.null( dim(resp) ) )                    # if it's a vector ... -->
    resp <- { if( dim(params)[1] > 1 ) matrix( resp, nrow = 1 )  # ... --> turn it into a multi-column matrix,
              else                     matrix( resp, ncol = 1) } # ... --> or a 1-column matrix

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
  if( !is.null(resp) & ( dim(resp)[ 2 ] != dim(params)[ 1 ] ) )
    stop( "number of params does not match the length of resp" )


#~~~~~~~~~~~~~~~~~~~~#
# Bayesian Modal Est #
#~~~~~~~~~~~~~~~~~~~~#

# Indicate the lower/upper boundary of the search:
  if( is.null(range) )
    range <- c(-6, 6)

  l <- range[1]; u <- range[2]
  
  est <- NULL # a vector for estimates
  hes <- NULL # a vector for the hessian of the prior
  
# Then, maximize the loglikelihood function over that interval for each person:
  for( i in 1:dim(resp)[1] ){
    likFun <- paste("logLik.", mod, sep = "")
    est[i] <- optimize( get(likFun), lower = l, upper = u, maximum = TRUE,
                        x = params, u = resp[i, ], type = "BME",
                        ddist = ddist, ... )$max                    
    hes[i] <- hessian(func = function(x, ... ) log( ddist(x, ... ) ), x = est[i], method = "Richardson", ... )
  }

# Note --> it's finding local maxima --> how do I get away from that?

# Round the estimated value to three/four? decimal places:          
  est <- round(est, digits = 4)

# And pull out the information as well as find the SEM:
  info <- get(paste("FI.", mod, sep = ""))(params = params,
                                           theta = est,
                                           type = "observed",
                                           resp = resp)$test
  sem  <- (info - hes)^(-1/2) # see Keller (p. 10)
  
# NOTE: NEED TO ADD THE ACTUAL INFORMATION/SEM CORRESPONDING TO BME?
  
  list(theta = est, info = info, sem = sem)
  
} # END bmeEst FUNCTION

