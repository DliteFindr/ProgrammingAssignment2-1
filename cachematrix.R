## These functions take advantage of scoping rules
## where results are kept in the function environment

## This function takes a matrix object to be cached

makeCacheMatrix <- function(x = matrix()) {       ## function params
        m <- NULL                                 ## resetting vari "m"
        set <- function(y) {                      ## Undefinde function
                x <<- y
                m <<- NULL
        }
        get <- function() x                       ## Calling & setting 
        setmatrix <- function(matrix) m <<- matrix   ## variables in cache
        getmatrix <- function() m
        
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)               ## resetting vaiables as needed

}


## This function computes the inverse of a matrix from makeCacheMatrix

cacheSolve <- function(x, ...) {         ## setting function param
        m <- x$getmatrix()               ## retieving matrix
        if(!is.null(m)) {                ## checking to see if there's a matrix
                message("getting cached data")
                return(m)
        }
        data <- x$get()                  ## getting matrix
        m <- solve(data, ...)            ## calc inverse of matrix
        x$setmatrix(m)                   
        m                                ## output
}
