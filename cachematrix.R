# creates a special_matrix with additional functions
makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL                     #sets the inversed matrix to NULL
        # sets the special_matrix and the inverse variable in the parent environment to NULL
        set <- function(y) {                    
                x <<- y
                inverse <<- NULL
        }
        
        # implements several functions like set special_matrix, calculates the inverse etc
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        # implements a list of functions to the special matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
#calculates the inverse matrix and checks if there is an inversed matrix already in the cache
        # if there is an cached matrix the message will tell and returns the cached inversed matrix
        inv <- x$getinverse()
        if(!is.null(inv)) {                     
                message("getting cached data")
                return(inv)
        }
        
        # if there is no cache, gets the data out of the special_matrix and returns the inversed matrix
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

# Test the functions from above
# Set.seed for transparency

set.seed(234)
data_for_matrix <- rnorm(25)
matrix <- matrix (data = data_for_matrix, nrow = 5, ncol = 5) # create random square matrix

special_matrix <- makeCacheMatrix(matrix) # creates special_matrix for caching

cacheSolve(special_matrix) # returns the inverse matrix
