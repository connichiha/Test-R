## raw <- read.table("https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_15_train.dat")
randompla <- function (raw)
{
    ## initialize 
    N <- 2000
    j <- 0
    num <- nrow(raw)
    update <- c()
    #X <- as.matrix(c(1,raw[,1:4]))
    #Y <- as.matrix(raw[,5])
    sum <- 0
    while(j < N)
    {
        j <- j + 1
        t <- 0
        i <- 0
        w_4 <- w_3 <- w_2 <- w_1 <- w_0 <- 0
        L_w <- c(w_0,w_1,w_2,w_3,w_4)
        
        set.seed(j)
        random <- sample(num)
        ## This random is not same as the question.
        ## This is totally random, like 30, 200, 1000. 
        
        correct <- 0
        total <- 0
        t <- 0
        while(correct != nrow(raw))
        {
            correct <- 0
            for(i in 1:nrow(raw))
            {
                if (i == 1)
                {
                    total <- total + t
                    t <- 0
                }
                L_x <- as.numeric(c(1,raw[random[i],1:4]))
                y <- raw[random[i],5]
                
                
                h <- sign(L_x %*% L_w) ## x and w should be the same type. numeric or matrix
                h[h == 0] <- -1
                ## find a mistake and correct
                if(h != y)
                {
                    L_w <- L_w + y*L_x
                    t <- t + 1
                }
                else
                {
                    correct <- correct + 1   
                }
            }
        }
        #print(total)
        #print(L_w)
        sum <- sum + total 
    }
    ave <- sum/N
}
#########################################################################
## This function is the same as the rule of question and with matrix.
frandom <- function(raw)
{
    N <- 10
    j <- 0
    num <- nrow(raw)
    sum <- 0
    while(j < N)
    {
        j <- j + 1
        set.seed(j)
        random <- sample(num)    
        X <- as.matrix(cbind(rep(1,length(raw[,1])),raw[random,1:4])) # X is reorganized randomly
        Y <- as.matrix(raw[random,5])
        W <- c(0,0,0,0,0)
        t <- 0
        info <- fpla(X,Y,W,t)
        W <- info[[1]]
        t <- info[[2]]
        #print(t)
        sum <- sum + t
    }
    ave <- sum/N
}




fpla <- function(x,y,w,flag = 0)
{
    while(1)
    {
        num <- nrow(data)
        
        h <- sign(x %*% w)
        h[h == 0] <- -1
        ## find a mistake and correct
        index <- which(h != y )
        if (length(index) != 0)
        {
            w <- w + y[index[1]]*x[index[1],]
            flag <- flag + 1
        }
        else
        {
            break            
        }
        x <- x[index,]
        y <- y[index] 
    } 
    info <- list(w,flag)
}

#########################################################
## this fun uses matrix to acc., but the loop method is not the same as question.
frandompla <- function (raw)
{
    ## initialize 
    j <- 0
    num <- nrow(raw)
    update <- 0
    X <- as.matrix(cbind(rep(1,length(raw[,1])),raw[,1:4]))
    Y <- as.matrix(raw[,5])
    
    ## J repeat N times using 
    while(j < 2000)
    {
        j <- j + 1
        t <- 0
        
        set.seed(j)
        random <- sample(num)[1]
        W <- Y[random]*X[random,]
        while(1)
        {            
            h <- sign(X %*% W)
            h[h == 0] <- -1
            ## find a mistake and correct
            index <- which(h != Y )
            if (length(index) != 0)
            {
                W <- W + 0.5*Y[index[1]]*X[index[1],]
                t <- t + 1
            }
            else
                break
        }
        update <- update + t
    }
    print(W)
    print(update/2000)
}
## 1.0: eve_update: 55.24   0.5: eve_update: 52.33  but the first method(totally random ) have no difference between results.

