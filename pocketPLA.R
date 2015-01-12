## raw <- read.table("https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_18_train.dat ")
## verify <- read.table("https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_18_test.dat ")

pocket <- function()
{
    raw <- read.table("https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_18_train.dat ")
    verify <- read.table("https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_18_test.dat ")
    ## verify part, run N times
    N <- 2000
    j <- 0
    sum_e <- 0   
    ## J repeat N times using 
    while(j < N)
    {
        j <- j + 1
        
        ## PLA with pocket to find the best W
        ## initialize 
        M <- 100 # pla interate M times
        num <- nrow(raw)
        info <- matrix(data = NA,nrow = M,ncol = 6) # store error and w in PLA
        # update <- matrix(data = NA,nrow = N,ncol = 6) # pocket that stores smallest error and w 
        i <- 0
        t <- 0
        
        #set.seed(j)
        random <- sample(num) # we want to PLA run 50 times randomly, so we just need random[1:50]
        
        X <- as.matrix(cbind(rep(1,length(raw[,1])),raw[random,1:4])) # X is reorganized randomly
        Y <- as.matrix(raw[random,5])
        W <- X[1,] # initialize w
        
        while(i < M)
        {
            i <- i + 1
            h <- sign(X %*% W)
            h[h == 0] <- -1
            ## find a mistake and correct
            index <- which(h != Y )
            ##info[i,] <- as.matrix(cbind(length(index),t(W)))  ## in this position is wrong.
            if (length(index) != 0)
            {
                W <- W + Y[index[1]]*X[index[1],]
                t <- t + 1
            }
            else
                break
            info[i,] <- as.matrix(cbind(length(index),t(W)))
        }
        # update[j,] <- info[order(info[,1])[1],]
        best_W <- info[order(info[,1])[1],2:6]
        #W50 <- info[M,2:6]
        #print(best_W)
        ## PLA with pocket finished
        
        vX <- as.matrix(cbind(rep(1,length(verify[,1])),verify[,1:4]))
        vY <- as.matrix(verify[,5])
        vh <- sign(vX %*% best_W)
        #vh <- sign(vX %*% W50)
        index <- which(vh != vY)
        error <- length(index)
        sum_e <- sum_e + error
    }
    rate <- sum_e/(N*nrow(verify))
    rate
}
