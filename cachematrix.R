## Put comments here that give an overall description of what your
## functions do

## Creates a special "vector" which is a list of functions allowing storing (setting and retrieving) 
##   matrixes and their inverse
##   Arguments required:
##      the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Returns the inverse of a matrix optimizing solve() by caching the result
##   Arguments required:
##      the matrix obtained via "makeCacheMatrix"
##      the extra arguments for solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
