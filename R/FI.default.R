FI.default <-
function( params,                              # parameters over which to calculate
          theta,                               # values/estimates of theta
          type = c("expected", "observed"),    # which information to calculate
          resp = NULL)                         # a response vector/matrix
{
  
# First, make sure that resp is NULL if type is "expected"
  if( type == "expected" )
    resp <- NULL
  
# Then turn params into a matrix:
  if( is.null( dim(params) ) )                             # if it's a vector ... -->
    params <- t(params)                                    # ... --> turn it into a matrix
    
# And turn response into a matrix:
  if( !is.null(resp) )                                          # if it exists ... -->
    resp <- { if( length(theta) == 1 ) matrix( resp, ncol = 1 )  # ... --> turn it into a 1-column matrix,
              else                     t(resp) }                 # ... --> or a multi-column matrix

# Then find the number of items and people:
  n.ppl <- length(theta)
  n.it <- dim(params)[1]
  
# And find the number of responses:
  n.resp <- ifelse(test = !is.null(resp), yes = dim(resp)[1], no = 0)


#~~~~~~~~~~~~~~~~~#
# Argument Checks #
#~~~~~~~~~~~~~~~~~#

# Then make sure that the arguments are OK:

## 1 ## (Make sure that resp exists if we are calculating observed information)
  if( is.null(resp) & type == "observed" )
    stop( "need response scalar/vector to calculate observed information" )
    
## 2 ## (Make sure that params, thet, resp are ALL numeric)
  if( mode(params) != "numeric" | mode(theta) != "numeric" )
    stop( "params and theta need to be numeric" )
    
  if( !is.null(resp) & mode(resp) != "numeric" )
    stop( "resp needs to be numeric" )

## 3 ## (Make sure that the dimensions of params and response are equal)
  if( !is.null(resp) & ( n.resp != n.it ) )
    stop( "number of params does not match the length of resp" )
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Expected Fisher Information #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Expected Fisher Information: p'^2/(p*q)
  if( type == "expected" ){
    
    if( length(theta) == 1 ){
      info <- pder1.brm(params, theta)^2 / ( p.brm(params, theta) * q.brm(params, theta) )
    } # END if STATEMENT
    
    else{
      p <- apply(params, MARGIN = 1, FUN = p.brm, theta = theta)
      q <- apply(params, MARGIN = 1, FUN = q.brm, theta = theta)
    
      pder1 <- apply(params, MARGIN = 1, FUN = pder1.brm, theta = theta)
    
      info <- pder1^2 / ( p * q )
    } # END else STATEMENT
    
  } # END if STATEMENT
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Observed Fisher Information #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

  if( type == "observed" ){
    
    if( length(theta) == 1 ){
      info <- -lder2.brm(xu = cbind(params, resp), theta = theta)
    } # END if STATEMENT
    
    else{
      info <- NULL
      for( i in seq_along(theta) )
        info <- rbind( info, -lder2.brm(xu = cbind(params, resp[ , i]), theta = theta[i]) )
    } # END else STATEMENT
    
  } # END if STATEMENT
 

# If theta is a scalar, item information is a vector and test information is a scalar
# If theta is a vector, item information is a matrix and test information is a vector

  i.info <- { if( length(theta) == 1 ) info
              else                     t(info) }
                
  t.info <- { if( length(theta) == 1 ) sum(info)
              else                     apply(i.info, MARGIN = 2, FUN = sum) }
              
    
  sem <- ifelse(test = signif(t.info) > 0, yes = sqrt( 1 / t.info ), no = NA)
               
                
  return( list(item = i.info, test = t.info, sem = sem, type = type) )
    
} # END FI.default FUNCTION