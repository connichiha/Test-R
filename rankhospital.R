rankhospital <- function(state, outcome, num = "best") { 
    ## Read outcome data 
    D_raw <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
    L_raw <- split(D_raw,D_raw$State)
    L_St <- L_raw[names(L_raw) == state]
    
    col <- NULL
    if(outcome == "heart attack")
    {
        col <- 11
    }
    if(outcome == "heart failure")
    {
        col <- 17
    }
    if(outcome == "pneumonia")
    {
        col <- 23
    }
    ## Check that state and outcome are valid
    if(length(L_St) == 0 )
    {
        stop("invalid state")        
    }
    if(is.null(col))
    {
        stop("invalid outcome")
    }      
    ## Return hospital name in that state with the given rank 
    D_St <- L_St[[1]]
    D_St[, col] <- as.numeric(D_St[, col])
    D_tmp <- D_St[!is.na(D_St[,col]),] ## kick out NA lines, this should be done after as.numeric, 
                                        ## because before numeric, NA is "Not Available"
    orders <- order(D_tmp[,col],D_tmp$Hospital.Name) 
    if (num == "best")
    {
        seq <- 1 
    }
    if (num == "worst")
    {
        seq <- length(orders)
    }
    else
    {
        seq <- num
    }
    print(D_tmp[orders[seq],][1,2])
    ## 30-day death rate 
}
