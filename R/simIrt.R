simIrt <-
function( thetas = seq(-3, 3, by = .1), # a scalar/vector of theta values
          params,                       # the item parameters
          mod = c("brm", "grm") )       # the model (binary response model or graded response model for now)
{

#~~~~~~~~~~~~~~~~~#
# Argument Checks #
#~~~~~~~~~~~~~~~~~#

# We need to make sure that the arguments are OK:

## 1 ## (Make sure that theta is a numeric vector)
  if( !is.null( dim(thetas) ) | mode(thetas) != "numeric" )
    stop( "thetas must be a numeric vector of person/simulee parameters" )
    
## 2 ## (Make sure that params is a numeric matrix)
  if( mode(params) != "numeric" & !inherits(params, "matrix") )
    stop( "params must be a numeric matrix of item parameters" )
    
## 3 ## (Make sure that the parameters match the model)
  if( mod == "brm" ){               # for the binary response model:
  
    if( dim(params)[2] != 3 )
      stop( "for a binary response model, there must be three parameters" )
      
    if( any( params[ , 3] >= 1 ) | any( params[ , 3] < 0 ) )
      stop( "the third parameter (guessing) must be between 0 and 1" )
      
    if( any( params[ , 1] < 0 ) )
      warning( "there is at least one negative first parameter (discrimination)" )
      
  } # END if STATEMENTS
  
  if( mod == "grm" ){               # for the graded response model:
  
    if( any( params[ , 1] < 0 ) )
      warning( "there is at least one negative first parameter (discrimination)" )
      
  } # END if STATEMENTS
  

#~~~~~~~~~~~~~~~~~~~~#
# Sorting Parameters #
#~~~~~~~~~~~~~~~~~~~~#

# If we are using the graded response model, the boundaries need to be ordered:
  if( mod == "grm" & inherits(params[ , -1], "matrix") ){
  	
    params           <- cbind(params[ , 1], t(apply(params[ , 2:dim(params)[2] ], MARGIN = 1, FUN = sort)))
    colnames(params) <- c("a", paste("b", 1:(dim(params)[2] - 1), sep = ""))
  
  } # END if STATEMENT
    
    
#~~~~~~~~~~~~~~~~~~~~~~#
# Simulating Responses #
#~~~~~~~~~~~~~~~~~~~~~~#

# Call C to simulate responses to the model ("brm" or "grm" or others)
  sim <- .Call(mod, thetas, params)                    
    
# Set the class according to the model:
#  a) We want the prime/first class to be "brm", "grm", or whatever model.
#  b) We want the second class to be "matrix" so that it will still ACT like a matrix
#     most of the time except when the model class co-opts.
  class(sim)    <- c(mod, "matrix")
  class(params) <- c(mod, "matrix")

  ret <- list(resp = sim, params = params, thetas = thetas)
  
# The above two lines make the code somewhat easy to generalize the model.

  if(mod == "brm" & length(thetas) == 1){
    cat("\nBinary response model simulation:\n   ",
         length(thetas), " simulee, ",  nrow(params), " items\n\n")
  } else if(mod == "brm"){
     cat("\nBinary response model simulation:\n   ",
         length(thetas), " simulees, ", nrow(params), " items\n\n")
  } else if(mod == "grm" & length(thetas) == 1){
    cat("\nGraded response model simulation:\n   ",
         length(thetas), " simulee, ",  nrow(params), " items, ", ncol(params), " levels per item\n\n")
  } else if(mod == "grm"){
    cat("\nGraded response model simulation:\n   ",
         length(thetas), " simulees, ", nrow(params), " items, ", ncol(params), " levels per item\n\n")
  } # END ifelse STATEMENTS
                     
  return(ret)
  
} # END simIrt FUNCTION
