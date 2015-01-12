rankall <- function(outcome, num = "best") { 
    ## Read outcome data 
    D_raw <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
    L_raw <- split(D_raw,D_raw$State)
    L_Sname <- names(L_raw)
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
    seq <- NULL

    ## Check that outcome is valid 
    if(is.null(col))
    {
        stop("invalid outcome")
    }  
    ## For each state, find the hospital of the given rank 
    L_Hname <- vector("list",length(L_raw)) ## blank list for hostipal names 
    for (i in 1:length(L_raw))
    {
        D_St <- L_raw[[i]] ## get the data of a state  type is data.frame
        D_St[, col] <- as.numeric(D_St[, col])
        D_tmp <- D_St[!is.na(D_St[,col]),]
        orders <- order(D_tmp[,col], D_tmp$Hospital.Name) 
        if (num == "best")
        {
            seq <- 1 
        }
        else
            if (num == "worst")
            {
                seq <- length(orders)
            }
            else
            {
                seq <- num
            }
        L_Hname[i] <- D_tmp[orders[seq],][1,2]   
    }
    hospital <- as.character(L_Hname)
    state <- L_Sname
    ## Return a data frame with the hospital names and the 
    info <- data.frame(hospital,state, stringsAsFactors = F)
    ## (abbreviated) state name 
}
