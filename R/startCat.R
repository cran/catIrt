startCat <-
function( params, resp, mod,
          it_flags = it_flags,
          catStart = list( init.thet = 0, step.size = 3,
                           select = c("random", "FI", "KL"),
                           at = c("theta", "bounds"),
                           n.select = 5,
                           score = c("fixed", "step", "random", "WLE", "BME", "EAP"),
                           n.it = 5, leave.after.MLE = FALSE ),
          catMiddle,
          catTerm,
          ddist = dnorm, ... ){
          	
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
# Arguments in catStart:                                  #
#  - init.thet: theta to select item 1                    #
#  - step.size: number of thetas to move if non-mixed     #
#  - select: how items will be selected                   #
#  - at: where the items should be selected               #
#  - n.select: number of items to select b/w              #
#  - n.it: number of items administered in catStart stage #
#  - score: how to score theta after each item            #
#    -- "fixed" (same place for each item)                #
#    -- "step" (change by same amount)                    #
#    -- "random" (random theta within the bounds)         #
#  - leave.after.MLE: go to catMiddle after mixed resp    #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# A vector to store selection rates (if needed):
  S        <- NULL
  
## FOR EACH ITEM IN STARTCAT ##
  for(j in 1:catStart$n.it){
  	
    stp <- 0  # for the S-H iterature item selection
  	
#######################
## I. SELECT AN ITEM ##
#######################

    while(!stp){
    	
      it_select <- itChoose( left_par = params[!it_flags, -ncol(params)],
                             cat_theta = cat_theta.i[j],
                             numb = catStart$n.select,
                             catMiddle = list(select   = catStart$select,
                                              at       = catStart$at,
                                              n.select = catMiddle$n.select),
                             catTerm   = catTerm )[ , 1]
                             
                             
# Pick the particular item (using a trick in case we only have one left):
      cat_it.i[j]  <<- sample(c(it_select, it_select), size = 1)
      
    
# Mark the item (getting rid of it), and save the location of the item:
      it_flags[ pl <- which(params[ , 1] == cat_it.i[j]) ] <<- 1 # global saving
      it_flags[ pl ]                                       <-  1 # local (to function) saving
      
                                              
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
 
########################################### 
## II. SCORE THETA AND/OR LEAVE STARTCAT ##
###########################################

## NON-MIXED RESPONSE PATTERN AND MLE ##
    if( { catStart$leave.after.MLE & 
  	      !( all(cat_resp.i[1:j] == min(get("resp", envir = environment(startCat)))) | all(cat_resp.i[1:j] == max(get("resp", envir = environment(startCat)))) ) } ){
    	
# If we want to leave after an MLE is obtained & we have a mixed response pattern:
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
      
# --> BREAK THE FUNCTION AND RETURN STUFF 
      ret <- list(S = S)
      return(ret)
    
    } else if(catStart$score == "WLE" | catStart$score == "BME" | catStart$score == "EAP"){
      
# If we are using WLE, EAP, or BME as our method of scoring:
# --> a) Build a character string for the WLE/EAP/BME estimation function,
      scoreFun    <- paste(tolower(catStart$score), "Est", sep = "")

      
# --> b) Call the estimation function on stuff to this point,
      x <- get(scoreFun)(resp = cat_resp.i[1:j],
                         params = cat_par.i[1:j, -c(1, ncol(cat_par.i))],
                         int = catMiddle$int, mod = mod, ddist = ddist, ...)
                         
                        
# --> c) Pull out important information.
      cat_theta.i[j + 1] <<- x$theta
      cat_info.i[j]      <<- x$info
      cat_sem.i[j]       <<- x$sem
      
    } else{
        
# If we are doing one of the fixed/random CAT procedures:
# --> a) Figure out which procedure we will use (random/step/fixed),
# ----> If random, then randomly generate theta between int[1] and int[2], 
      cat_theta.i[j + 1] <<- switch(catStart$score,
                                    random = runif(n = 1,
                                                   min = min(catMiddle$int)[1],
                                                   max = max(catMiddle$int)[1]),

# ----> If step, then subtract or add based on last response,                                             
                                    step   = { function(){
                                   	             if( cat_resp.i[j] == min(get("resp", envir = environment(startCat))) ){
        	                                       max(cat_theta.i[j] - catStart$step.size, min(catMiddle$int)[1])
                                                 } else if( cat_resp.i[j] == max(get("resp", envir = environment(startCat))) ){
                                                   min(cat_theta.i[j] + catStart$step.size, max(catMiddle$int)[1])
                                                 } else{
                                                   cat_theta.i[j]
                                                 } # END if STATEMENTS
                                              } }( ),
        
# ----> If fixed (or anything else), then assign the fixed value.                          
                                    fixed  = catStart$init.thet, catStart$init.thet)
                                
    } # END ifelse STATEMENTS
    
  } # END for j STATEMENT
    	
# After we have given all of the required items:
# --> BREAK THE FUNCTION AND RETURN STUFF  
  ret <- list(S = S)
  return(ret)
  
} # END startCat FUNCTION
