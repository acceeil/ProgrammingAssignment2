## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {  # input x is a vector
        m <- NULL                            # will be our 'inverse' and it's reset to NULL every time makeCacheMatrix is called
        set <- function(y) {                 
                x <<- y
                m <<- NULL
        }
        get <- function() x                  # returns the value of the original vector
        setinverse <- function(inverse) m <<- inverse   # called by cacheSolve during the first cacheSolve() access and 
                                                        # it will store the value using superassignment
        getinverse <- function() m           # this will return the cached value to cacheSolve() on subsequent accesses
       
        list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)  #returned with the newly created object.
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {   #the input is an object created by makeVector
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {                        #if inverse was already cached (not NULL)
                message("getting cached data")  # send this message to the console
                return(m)                       #and return the inverse
        }
        data <- x$get()                         #reach this code only if x$getinverse() returned NULL
        m <- solve(data, ...)                   #if m was NULL then we have to calculate the inverse
        x$setinverse(m)                         #store the calculated inverse matrix in x 
        m
}

