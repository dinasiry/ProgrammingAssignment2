## The first function caches the invertible matrix that has been
## inputted. It will check to see if the matrix is invertible, if not,
## it will output a message stating that matrix is not invertible.
##
## The second function takes the cached matrix and inverts it and
## caches the inverted matrix.

## This function will cache matrix using the list of functions

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL                                     #set Solve to empty
        if (det(x) != 0) {
            set <- function (sm) {                    #set value of special matrix sm
                x <<- matrix(sm)
                s <<- NULL
          }
        }
        else {
            message("matrix is not invertible, enter new matrix")
            return(x)
        }
        get <- function() x                           #get value of matrix x
        setsolve <- function(solve) s <<- solve       #set value of solve
        getsolve <- function() s                      #get the value of the solve inversion
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The function cacheSolve will invert the matrix and cache the
## newly inverted matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        s <- x$getsolve()                           #check to see if inverse of matrix is already returned
        if(!is.null(s)) {                           #if it's already inverted
                message("getting cached data")      #get the matrix inversion from cache
                return(s)
                s
        }
        specialmatrix <- x$get()                    #if it's not calculated get the special matrix
        s <- solve(specialmatrix, ...)              #and solve for the inversion
        x$setsolve(s)                               #then set the solve
        s
}
