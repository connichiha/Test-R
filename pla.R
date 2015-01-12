## raw <- read.table("https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_15_train.dat")

## Loop
pla <- function (raw)
{
    ## initialize 
    #w_4 <- w_3 <- w_2 <- w_1 <- w_0 <- 0
    #L_w <- c(w_0,w_1,w_2,w_3,w_4)
    L_w <- c(0,0,0,0,0)
    info <- c(0,0)

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
            x_1 <- raw[i,1]
            x_2 <- raw[i,2]
            x_3 <- raw[i,3]
            x_4 <- raw[i,4]
            L_x <- c(1, x_1, x_2, x_3, x_4)
            #L_x <- as.numeric(c(1,raw[i,1:4]))
            y <- raw[i,5]
            
            #f <- sum(L_w*L_x) # w_0+w_1*x_1+w_2*x_2+w_3*x_3+w_4*x_4
            #if(f == 0)
            #{
            #    h <- -1
            #}
            #else
            #{
            #    h <- sign(f)
            #}
            h <- sign(L_x %*% L_w) ## x and w should be the same type. numeric or matrix
            h[h == 0] <- -1
            ## find a mistake and correct
            if(h != y)
            {
                L_w <- L_w + 0.5*y*L_x
                #i <- 0     ##   according to the hw1, we should continue to find next error, but not to restart from the first of index
                t <- t + 1
            }
            else
            {
                correct <- correct + 1   
            }
        }
    }
    print(total)
    print(t)
    print(L_w)
    print(correct)
    
    ## check part
    X <- as.matrix(cbind(rep(1,length(raw[,1])),raw[,1:4]))
    Y <- as.matrix(raw[,5])
    h <- sign(X %*% L_w)
    index <- which(h != Y )
    if (length(index) != 0)
    {
        print("not all")
    }
    else
        print("correct")
}


## matrix : This circle is not the same as the question's rule
plahw <- function ()
{
    raw <- read.table("https://d396qusza40orc.cloudfront.net/ntumlone%2Fhw1%2Fhw1_15_train.dat")
    ## initialize
    num <- nrow(raw)
    X <- as.matrix(cbind(rep(1,length(raw[,1])),raw[,1:4]))
    Y <- as.matrix(raw[,5])
    W <- X[1,] # initialize w
    t <- 0
    while(1)
    {
        h <- sign(X %*% W)
        h[h == 0] <- -1
        ## find a mistake and correct
        index <- which(h != Y )
        if (length(index) != 0)
        {
            W <- W + Y[index[1]]*X[index[1],]
            t <- t + 1
        }
        else
            break
    }
    print(W)
    print(t)
    
    ## check part
    X <- as.matrix(cbind(rep(1,length(raw[,1])),raw[,1:4]))
    Y <- as.matrix(raw[,5])
    h <- sign(X %*% W)
    index <- which(h != Y )
    if (length(index) != 0)
    {
        print("not all")
    }
    else
        print("correct")
}
