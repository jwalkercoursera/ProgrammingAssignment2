## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function sets the value of the matrix,
## gets the matrix, and set/gets the of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list( set = set, get = get,
              setsolve = setsolve,
              getsolve = getsolve)
}


## Write a short comment describing this function
## The cacheSolve function inverses the matrix by 
## calling the solve() function, as in the cachemean
## function, it checks to see if the inverse matrix is NULL
## if it is, the solve function is called and the inverse matrix is returned, otherwise
## the cached inversed matrix is returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
