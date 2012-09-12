#####
# 1 # (EXTRACTING)
#####

# Making sure that selecting elements does not change the class:
"[.brm" <- function(x, ...){
  r <- NextMethod("[")
  class(r) <- class(x)
  return(r)
} # END [.brm FUNCTION

"[.grm" <- function(x, ...){
  r <- NextMethod("[")
  class(r) <- class(x)
  return(r)
} # END [.grm FUNCTION


#####
# 2 # (REPLACING)
#####

# Making sure that assignment elements does not change the class.
"[<-.brm" <- function(x, ..., value){
  r <- NextMethod("[<-")
  class(r) <- class(x)
  return(r)
} # END [<-.brm FUNCTION

"[<-.grm" <- function(x, ..., value){
  r <- NextMethod("[<-")
  class(r) <- class(x)
  return(r)
} # END [<-.grm FUNCTION	                        