## The first function caches the invertible matrix that has been
## inputted. The matrix must be invertible and square (meaning same
## number of rows as to columns in matrix) It will check to see if
## the matrix is invertible, if not, it will output a message stating
## that matrix is not invertible.
##
## The second function takes the cached matrix and inverts it with the
## SOLVE function and caches the inverted matrix.

## This function will cache matrix using the list of functions

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL                                     #set Solve (inverted matrix) to empty
        if (det(x) != 0) {                            #determines if matrix is invertible
            set <- function (sm) {                    #set value of special matrix sm
                x <<- matrix(sm)
                s <<- NULL
          }
        }
        else {                                        #if matrix is not invertible, show message below
            message("matrix is not invertible, enter new matrix")
            return(x)
        }
        get <- function() x                           #get value of matrix x
        setsolve <- function(solve) s <<- solve       #set value of solve
        getsolve <- function() s                      #get the value of the solve inversion
        list(set = set, get = get,                    #list of functions to set and get matrix
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
                s                                   #output the inverted matrix
        }
        specialmatrix <- x$get()                    #if it's not calculated get the special matrix
        s <- solve(specialmatrix, ...)              #and solve for the inversion (invert matrix)
        x$setsolve(s)                               #then set the solve
        s                                           #output the inverted matrix
}
