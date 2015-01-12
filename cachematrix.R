## Put comments here that give an overall description of what your
## functions do

makeVector <- function(x = numeric()) {  ## ����x��Ĭ��ֵΪnumeric()
    
    m <- NULL                 ## ����m����ʼֵΪNULL��
    
    set <- function(y) {      ## ���庯��set��
        x <<- y           ## set�����ܰ�x��ֵ�޸�Ϊy��
        m <<- NULL        ## ͬʱ��m���ó�NULL��
    }
    
    get <- function() {       ## ���庯��get��
        x                 ## get�����ܷ���x��
    }                ## xΪ���ɱ�������˻��get����������Ļ����в���x��ֵ
    
    setmean <- function(mean) { ## ���庯��setmean��
        m <<- mean          ## setmean�����ܰ�m��ֵ�޸�Ϊmean��
    }
    
    getmean <- function() {   ## ���庯��getmean��
        m                 ## getmean�ܺ�������m��
    }                ## mΪ���ɱ�������˻��getmean����������Ļ����в���m��ֵ
    
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)   ## makeVector��������һ��list
} 


cachemean <- function(x, ...) {    ## ����makeVector�������ɵ�list
    
    m <- x$getmean()      ## ���Զ�ȡ�����ƽ��ֵ
    
    if(!is.null(m)) {                      ## �������ֵ�����
        message("getting cached data")
        ##return(m)                      ## ֱ�ӷ��ػ���ֵ
    }
    else{
        ## �������ִ�е����˵��֮ǰ��if��䱻����������ֵΪ��
        data <- x$get()       ## ��ȡ�����vector/matrix
        m <- mean(data, ...)  ## ��vector/matrix��ƽ��ֵ
        x$setmean(m)          ## ��ƽ��ֵ���浽x�Ļ�����        
    }
    m                     ## ����ƽ��ֵ
}

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                 
    
    set <- function(y) {      ## ���庯��set��
        x <<- y           ## set�����ܰ�x��ֵ�޸�Ϊy��
        inv <<- NULL       
    }
    
    get <- function() {       ## ���庯��get��
        x                 ## get�����ܷ���x��
    }                ## xΪ���ɱ�������˻��get����������Ļ����в���x��ֵ
    
    setinv <- function(inverse) { ## ���庯��setinv
        inv <<- inverse          ##setinv�����޸�inv��ֵ
    }
    
    getinv <- function() {   ## ���庯��getminv��
        inv                 
    } 
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)   ## ��������һ��list
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()      ## ���Զ�ȡ�����inv
    
    if(!is.null(inv)) {                      ## �������ֵ�����
        message("getting cached data")
    }
    else{
        ## �������ִ�е����˵��֮ǰ��if��䱻����������ֵΪ��
        data <- x$get()       ## ��ȡ�����vector/matrix
        inv <- solve(data)  ## ��vector/matrix��ƽ��ֵ
        x$setinv(inv)          ## ��ƽ��ֵ���浽x�Ļ�����        
    }
    inv                     ## ����ƽ��ֵ
}