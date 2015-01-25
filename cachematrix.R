## Functions that cache, inverse of a matrix and a vector

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    start_time <- Sys.time()
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        end_time <- Sys.time()
        
        time_duration <- end_time - start_time
        
        print(time_duration)
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    
    end_time <- Sys.time()
    
    time_duration <- end_time - start_time
    
    print(time_duration)
    
    m
}


## Example: Caching the Mean of a Vector
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}
cachemean <- function(x, ...) {
    start_time <- Sys.time()
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        end_time <- Sys.time()
        
        time_duration <- end_time - start_time
        
        print(time_duration)
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    
    end_time <- Sys.time()
    
    time_duration <- end_time - start_time
    
    print(time_duration)
    
    m
}

