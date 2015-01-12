## Put comments here that give an overall description of what your
## functions do

makeVector <- function(x = numeric()) {  ## 定义x，默认值为numeric()
    
    m <- NULL                 ## 定义m，初始值为NULL；
    
    set <- function(y) {      ## 定义函数set，
        x <<- y           ## set函数能把x的值修改为y，
        m <<- NULL        ## 同时把m重置成NULL；
    }
    
    get <- function() {       ## 定义函数get，
        x                 ## get函数能返回x；
    }                ## x为自由变量，因此会从get函数被定义的环境中查找x的值
    
    setmean <- function(mean) { ## 定义函数setmean，
        m <<- mean          ## setmean函数能把m的值修改为mean；
    }
    
    getmean <- function() {   ## 定义函数getmean，
        m                 ## getmean能函数返回m；
    }                ## m为自由变量，因此会从getmean函数被定义的环境中查找m的值
    
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)   ## makeVector函数返回一个list
} 


cachemean <- function(x, ...) {    ## 读入makeVector函数生成的list
    
    m <- x$getmean()      ## 尝试读取缓存的平均值
    
    if(!is.null(m)) {                      ## 如果缓存值不会空
        message("getting cached data")
        ##return(m)                      ## 直接返回缓存值
    }
    else{
        ## 如果代码执行到这里，说明之前的if语句被跳过，缓存值为空
        data <- x$get()       ## 读取缓存的vector/matrix
        m <- mean(data, ...)  ## 求vector/matrix的平均值
        x$setmean(m)          ## 把平均值缓存到x的环境中        
    }
    m                     ## 返回平均值
}

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                 
    
    set <- function(y) {      ## 定义函数set，
        x <<- y           ## set函数能把x的值修改为y，
        inv <<- NULL       
    }
    
    get <- function() {       ## 定义函数get，
        x                 ## get函数能返回x；
    }                ## x为自由变量，因此会从get函数被定义的环境中查找x的值
    
    setinv <- function(inverse) { ## 定义函数setinv
        inv <<- inverse          ##setinv可以修改inv的值
    }
    
    getinv <- function() {   ## 定义函数getminv，
        inv                 
    } 
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)   ## 函数返回一个list
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()      ## 尝试读取缓存的inv
    
    if(!is.null(inv)) {                      ## 如果缓存值不会空
        message("getting cached data")
    }
    else{
        ## 如果代码执行到这里，说明之前的if语句被跳过，缓存值为空
        data <- x$get()       ## 读取缓存的vector/matrix
        inv <- solve(data)  ## 求vector/matrix的平均值
        x$setinv(inv)          ## 把平均值缓存到x的环境中        
    }
    inv                     ## 返回平均值
}
