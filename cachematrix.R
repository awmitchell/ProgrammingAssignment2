## The two functions below will increase code efficiency where your code needs to repetitively
## calculate the inverse of a matrix. Using Solve to calculate the inverse of a matrix can be
## computationally expensive and doing this in a loop over a large data set can slow code down.
## These two functions (makeCacheMatrix) and cacheSolve work together to increase the speed of
## excution in such circumstances.

## NON-TYPICAL EXAMPLE OF USE FOR DEMONSTRATION PURPOSES:
##
##> source("cachematrix.R")                     ## Load the functions. Ensure your path is correct.
##> a = martix(rnorm(25), nrow = 5, ncol = 5)   ## Create some random data for demonstration.
##> a
##            [,1]       [,2]       [,3]       [,4]       [,5]
##[1,] -0.10612452  1.3048697  0.6359504 -0.3066386 -0.4304691
##[2,]  1.51152200  2.2866454 -0.2842529 -1.7813084 -0.2572694
##[3,] -0.09465904 -1.3888607 -2.6564554 -0.1719174 -1.7631631
##[4,]  2.01842371 -0.2787888 -2.4404669  1.2146747  0.4600974
##[5,] -0.06271410 -0.1333213  1.3201133  1.8951935 -0.6399949
##> b <- makeCacheMatrix()                      ## Call makeCacheMatrix to create methods on b.
##> b                                           ## Not normally required. For demonstration only.
##$set
##function (y) 
##{
##    x <<- y
##    m <<- NULL
##}
##<environment: 0x0000000012e83550>
##
##$get
##function () 
##x
##<environment: 0x0000000012e83550>
##
##$setinverse
##function (solve) 
##m <<- solve
##<environment: 0x0000000012e83550>
##
##$getinverse
##function () 
##m
##<environment: 0x0000000012e83550>
##
##> b$set(a)                                    ## Set the cache function with matrix 'a'
##> b$get()                                     ## Get the matrix to confirm that b$set worked.
##            [,1]       [,2]       [,3]       [,4]       [,5]
##[1,] -0.10612452  1.3048697  0.6359504 -0.3066386 -0.4304691
##[2,]  1.51152200  2.2866454 -0.2842529 -1.7813084 -0.2572694
##[3,] -0.09465904 -1.3888607 -2.6564554 -0.1719174 -1.7631631
##[4,]  2.01842371 -0.2787888 -2.4404669  1.2146747  0.4600974
##[5,] -0.06271410 -0.1333213  1.3201133  1.8951935 -0.6399949
##> b$setinverse(m)                             ## Stores the inverse of 'a' once calculated.
##> b$getinverse()                              ## Returns the stored inverse. Result not shown.

## MORE TYPICAL EXAMPLE OF USE FOR DEMONSTRATION PURPOSES:
##> source("cachematrix.R")			## Load the functions. (Ensure correct path.)
##> a = martix(rnorm(25), nrow = 5, ncol = 5)   ## Create some random data for demonstration.
##> b <- makeCacheMatrix(a)                     ## Call makeCacheMatrix to create $methods on b.
##> > cacheSolve(b)                             ## Call cacheSolve to solve, cache & return inverse.
##           [,1]       [,2]        [,3]        [,4]        [,5]
##[1,] -1.2598782  0.7261407 -0.02018182 -0.09874974  0.54012031
##[2,]  1.2875624 -0.3270038 -0.08019734  0.29954905 -0.29829160
##[3,] -0.8100261  0.3872557 -0.09985974 -0.32386876  0.43144102
##[4,]  0.6830956 -0.3794372 -0.05399490  0.31975759  0.07169967
##[5,]  0.2072277 -0.3278590 -0.34718913  0.22612015 -0.45104850

## When assigned to a vector ('b' in the examples above) makeCacheMatrix creates and returns
## a list of functions $set, $get, $setinverse and $getinverse against the assigned vector.
## These methods are used by cacheSolve to efficiently return the inverse matix.

makeCacheMatrix <- function(x = matrix()) {
        ## Creates a special vector of functions to be used by cacheSolve
        m <- NULL
        set <- function(y) {                    ## Stores the matrix.
                x <<- y
                m <<- NULL
        }
        get <- function() x			## Returns the matrix.
        setinverse <- function(s) m <<- s	## Stores the solved (inverse) matrix.
        getinverse <- function() m		## Returns the inverse matrix.
        list(set = set, get = get,		## Returns the vector of functions.
                setinverse = setinverse,
                getinverse = getinverse)
}

## When passed a special vector (created by makeCacheMatrix) cacheSolve returns the
## inverse of the matrix. First it checks to see if the inverse matrix has been 
## created and stored. If it has, it is returned. If not, then it is calculated and
## stored.

cacheSolve <- function(x, ...) {
        ## Uses functions created by makeCacheMatrix to return a matrix that is the inverse of 'x'.
        ## Assumes 'x' is invertable.
        m <- x$getinverse()			## Gets the inverse (if it already exists)
        if(!is.null(m)) {			## Returns the inverse if it exists
                return(m)
        }
        data <- x$get()				## Gets the matrix
        m <- solve(data, ...)		        ## Solves for the inverse
        x$setinverse(m)				## Stores the inverse
        m					## Returns the inverse
}