## Functions to optimize the (re)use of inverse matrices

## Wrap-up function to store the original matrix along with functions to 
#store and retrieve it's inverse.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL # 'i' for inverse matrix
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     SetInverse <- function(inverse) i <<- inverse
     GetInverse <- function() i
     list(set = set, get = get,
          SetInverse = SetInverse,
          GetInverse = GetInverse)

}


## Function to calculate a inverse matrix or retrieve a previously cached one.
# It's meant to have as input on a object created with the 
#'makeCacheMatrix' function.

cacheSolve <- function(x, ...) {
     i <- x$GetInverse()
     if(!is.null(i)) {                    ## If a previously computed inverse
                                          # matrix 'i' exists...  
          message("getting cached data")
          return(i)                       # return it. If not...
     }
     data <- x$get()                      # get the original matrix.
     i <- solve(data, ...)                # Calculate its inverse.
     x$SetInverse(i)                      # Store it inside the object.
     i                                    # And return it.
}

#x <- matrix(c(2,3,1,5,4,1,6,8,1),3,byrow = T)