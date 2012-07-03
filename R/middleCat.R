middleCat <-
function( params, resp, mod,
          it_flags,
          cat_par, cat_resp, cat_theta,
          cat_info, cat_sem,
          catStart,
          catMiddle = list( select = c("random", "FI", "KL"),
                            at = c("theta", "bounds"),
                            n.select = 5,
                            score = c("MLE", "WLE", "BME", "EAP"), int = c(-6, 6),
                            expose = c("none", "SH") ),
          catTerm,
          ddist = dnorm, ... )
{
          	
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
# Arguments in catMiddle:                                 #
#  - select: how items will be selected                   #
#  - at: where the items should be selected               #
#  - n.select: number of items to select b/w              #
#  - score: how to score theta after each item            #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

  S        <- NULL            # a vector to store selection rates
  j        <- nrow(cat_par)   # the number of items administered
  
  j <- j + 1                  # so that we select the next item
    
  stp <- 0                    # for the S-H iterature item selection

#######################
## I. SELECT AN ITEM ##
#######################

  while(!stp){
    
    it_select <- itChoose( left_par = params[!it_flags, -ncol(params)],
                           cat_theta = cat_theta,
                           numb = catMiddle$n.select,
                           catMiddle = catMiddle,
                           catTerm   = catTerm )[ , 1]
                             
# Pick the particular item (using a trick in case we only have one left):
    cat_it.i[j]    <<- sample(c(it_select, it_select), size = 1)
    
# Mark the item (getting rid of it), and save the location of the item:
    it_flags[ pl <- which(params[ , 1] == cat_it.i[j]) ] <<- 1
                                              
# IF S-H ITEM EXPOSURE:
#  - a) find the item exposure prob (k),
#  - b) add the item to the total list of selected items,
#  - c) pick a uniform number (u) between 0 and 1
#  - d) If u < k, administer the item, otherwise repeat!
    if(catMiddle$expos == "SH"){
    
      k <- params[pl, ncol(params)]
      S <- c(S, cat_it.i[j])

# If u < k...    	
      if(runif(1) < k){

# ... administer the item and save the parameters:
    	  cat_resp.i[j]  <<- resp[pl]
    	  cat_par.i[j, ] <<- params[pl, ]
    	  stp            <- 1
    	  
    	} # END if STATEMENT
    	
      } else{
      	
# IF NO S-H ITEM EXPOSURE, JUST SAVE THE ITEM:
        S <- c(S, cat_it.i[j])
        cat_resp.i[j]  <<- resp[pl]
        cat_par.i[j, ] <<- params[pl, ]
        stp            <- 1
        
      } # END ifelse STATEMENTS
      
    } # END while STATEMENT
    
#####################
## II. SCORE THETA ##
#####################

## NON-MIXED RESPONSE PATTERN AND MLE ##
  if( { catMiddle$score == "MLE" & 
  	    ( { all(cat_resp.i[1:j] == min(get("resp", envir = environment(middleCat)))) |
  	    	    all(cat_resp.i[1:j] == max(get("resp", envir = environment(middleCat)))) } ) } ){
  	    
  	    	
# Repeat the same scoring as in startCat:
    if(catStart$score == "WLE" | catStart$score == "BME" | catStart$score == "EAP"){
    	
# If we are using WLE, EAP, or BME as our method of scoring:
# --> a) Build a character string for the WLE/EAP/BME estimation function,
      scoreFun <- paste(tolower(catStart$score), "Est", sep = "")
      
# --> b) Call the estimation function on stuff to this point,
      x <- get(scoreFun)(resp = cat_resp.i[1:j],
                         params = cat_par.i[1:j, -c(1, ncol(cat_par.i))],
                         int = catMiddle$int, mod = mod, ddist = ddist, ... )
                        
# --> c) Pull out important information.
      cat_theta.i[j + 1] <<- x$theta
      cat_info.i[j]      <<- x$info
      cat_sem.i[j]       <<- x$sem
      
    } else{
    
# If we are doing one of the fixed/random CAT procedures:
# --> a) Figure out which procedure we will use (random/step/fixed),
      cat_theta.i[j + 1] <<- switch(catMiddle$score,
                                    random = runif(n = 1,
                                                   min = min(catMiddle$int)[1],
                                                   max = max(catMiddle$int)[1]),
                                          
                                    step   = { function(){
                                                if( cat_resp.i[j] == min(get("resp", envir = environment(middleCat))) ){
        	                                          max(cat_thet[j] - step.size, min(catMiddle$int)[1])
                                                } else if( cat_resp.i[j] == max(get("resp", envir = environment(middleCat))) ){
                                                  min(cat_thet[j] + step.size, max(catMiddle$int)[1])
                                                } else{
                                                  cat_thet[j]
                                                } # END if STATEMENTS
                                              } }( ),
                                
                                    fixed  = catStart$init.thet, catStart$init.thet)
                                
    } # END ifelse STATEMENTS 
    
    
# After we have given an item and scored it:
# --> BREAK THE FUNCTION AND RETURN STUFF  
  ret <- list(S = S)
  return(ret)

  } else{
  
# --> a) Build a character string for the WLE/EAP/BME estimation function,
    scoreFun <- paste(tolower(catMiddle$score), "Est", sep = "")
      
# --> b) Call the estimation function on stuff to this point,
    x <- get(scoreFun)(resp = cat_resp.i[1:j],
                       params = cat_par.i[1:j, -c(1, ncol(cat_par.i))],
                       int = catMiddle$int, mod = mod, ddist = ddist, ...)
                        
# --> c) Pull out important information.
    cat_theta.i[j + 1] <<- x$theta
    cat_info.i[j]      <<- x$info
    cat_sem.i[j]       <<- x$sem
      
# After we have given an item and scored it:
# --> BREAK THE FUNCTION AND RETURN STUFF  
    ret <- list(S = S)
    return(ret)
      
  } # END ifelse STATEMENTS
  
} # END FUNCTION
