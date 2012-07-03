itChoose <- function( left_par,                     # parameters remaining + index of items
                      cat_theta,                    # current estimate(s) of theta
                      numb = 1,                     # the number of items to choose (ultimately)
                      catMiddle,                    # choosing items: select (how to choose items)
                      catTerm )                     # choosing items: bounds, delta (categories)
{

# Note: "at" will be either "theta" or "bounds" depending if the user wants
#        to pick maximum information at "theta" or at one of the classification
#        bounds.

# Note 2: Maybe add in content later?  How??

# First, turn params into a matrix:
  if( is.null( dim(left_par) ) )   # if it's a vector ... -->
    left_par <- t(left_par)        # ... --> turn it into a matrix

#####
# 1 # (WHERE TO SELECT)
#####
  
# Next, we need to choose the bound at which we're selecting the item:
  if( is.null(catMiddle$at) | catMiddle$at == "theta" ){
  
    theta <- cat_theta
    
  } else if(catMiddle$at == "bounds" ){
  	
# Figuring out the closest bound to current theta, and sampling lest there are 2:
  	bounds <- catTerm$c.term$bounds
    theta  <- sample(bounds[ which.min( abs( cat_theta - bounds ) ) ], size = 1 )
  
  } # END if STATEMENTS
  
# Note: We might want more sophsticated methods of choosing proximate theta?

#####
# 2 # (FIGURING OUT INFO)
#####

## FOR FISHER INFORMATION ##
#  -- we need expected info for remaining params at current theta:
#  -- we need to figure out which boundary we should be using
#  -- we need to figure out Fisher Info at that boundary
  if( catMiddle$select == "FI" ){
  
    info <- FI(params = left_par[ , -1], theta = theta, type = "expected")$item
    
  } # END FI if STATEMENT


## FOR KL INFORMATION ##
#  -- we need to figure out which boundary we should be using,
#  -- we need to find KL Info at that boundary point using delt
  if( catMiddle$select == "KL" ){
      
# And, using the bounds, finding KL Information
    info <- KL(params = left_par[ , -1], theta = theta, delta = catTerm$c.term$delta)$item
    
  } # END KL if STATEMENT
  
## FOR random INFORMATION ##
#  -- everything should have the same information
#  -- or at least, probabilistically, everything should have the same information :)
#  -- the runif is to make sure that the we don't just select items at the beginning.
  if( catMiddle$select == "random" ){

    info <- runif( nrow(left_par) )
  
  } # END random if STATEMENT

#####
# 3 # (SORTING ITEMS)
#####

# Making sure that we have items to choose between:
  n_select <- ifelse(test = is.null(catMiddle$n.select), yes = 1, no = catMiddle$n.select)
  
# In case there are fewer items left than those to choose from:
  n_select <- min(n_select, length(info))[1]  # for the items to choose between
  numb     <- min(numb, length(info))[1]      # for the number of items to select

# Next we want to find the "n" items with largest info (making sure to keep everything as a matrix):
  par_select <- left_par[order(info, decreasing = TRUE), , drop = FALSE][1:max(numb, n_select)[1], , drop = FALSE]
  
#####
# 4 # (CHOOSING ITEMS)
#####

# If we are only selecting one item, randomly select that item.
  if( numb == 1 ){

# (doubling the number of items is a trick to make sure it selects at least one):
    it_next <- sample(c( par_select[ , 1], par_select[ , 1] ), size = 1)
    return(par_select[par_select[ , 1] == it_next, , drop = FALSE])
    
  } else if( (numb > 1) & (numb <= n_select) ){
  
# If we are selecting more than one item, randomly select those items:
    it_next <- sample( c(par_select[ , 1] ), size = numb)
    return( par_select[par_select[ , 1] %in% it_next, , drop = FALSE] )
    
  } else if( numb >= n_select ){
  
# If we are selecting all of the items we have, select all of them:
    return( par_select )
    
  } # END if STATEMENTS
  
} # END itChoose FUNCTION