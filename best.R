best <- function(state, outcome) {
    ## Read outcome data
    D_raw <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
    L_raw <- split(D_raw,D_raw$State)
    L_St <- L_raw[names(L_raw) == state]

    col <- NULL
    if(outcome == "heart attack")
    {
        col = 11
    }
    if(outcome == "heart failure")
    {
        col = 17
    }
    if(outcome == "pneumonia")
    {
        col = 23
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
    ## Return hospital name in that state with lowest 30-day death
    D_St <- L_St[[1]]
    D_St[, col] <- as.numeric(D_St[, col])
    orders <- order(D_St[,col]) ##, decreasing = TRUE
    print(D_St[orders,][1,2])
    ## rate
}