# Gina Cannarozzi 26 July 2014
# This pair of funcitons will create the inverse of a square, invertible 
# matrix. The function cacheSolve will get the inverse from the cache if 
# it exists, else it will compute the inverse. makeCacheMatrix is a data
# structure that will get and set the matrix and get and set the inverse
# of the matrix.
# 

# this funciton creates a list containing 
# a function to set the value of the matrix, get the value of the matrix,
# set the value of the inverse and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(inverse) m <<- inverse
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## this function calculates the inverse of the data structure above if it is not
# already cached.  If it is already cached, it retrieves the cached value. 

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setmatrix(m)
        m
}
