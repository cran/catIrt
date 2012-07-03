stpRule <-
function(method = c("fixed", "var", "class"),
         min.it, it.left,
         it.crit, se.crit,
         n.it, se.obs,                        
         categ.est )
{
  
  stp  <- 0   # default to continue (stp <- 1 means stop)
  term <- NA  # default to no termination

# - If the number of items is less than the minimum: continue.
# - If all of the items are used up: stop.
  if( (n.it < min.it) | (it.left == 0) ){
    if( it.left == 0 ){
      stp <- 1
      term <- "total"
    }
  } else{

# 1) If we have reached the maximum number of items, stop:
    if( any(method == "fixed") ){
      if( !is.null( it.crit ) ){
        if( n.it >= it.crit ){
          stp <- 1
          term <- "fixed"
        }
      }
    } # END if 1) STATEMENT
  
# 2) If we have gone below the variable threshold, stop:
    if( any(method == "var") ){
      if( !is.na(se.obs) & !is.null(se.crit) ){
        if( se.obs <= se.crit ){
          stp <- 1
          term <- "var"
        }
      }
    } # END if 2) STATEMENT
  
# 3) If we have decided on a class, stop:
    if( any( method == "class" ) ){
      if( !is.na(categ.est) ){
        stp <- 1
        term <- "class"
      }
    } # END if 3) STATEMENT

  } # END ifelse STATEMENTS
  
  return( list(stp = stp, term = term) )
  
} # END stpRule FUNCTION
