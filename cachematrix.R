## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        # Initialize the inverse property
        inv <- NULL
        
        # Method to set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Method the get the matrix
        get <- function() {
                x
        }
        
        # Method to set the inverse of the matrix
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        
        # Method to get the inverse of the matrix
        getInverse <- function() {
                inv
        }
        
        # Return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        # Just return the inverse if it is already set
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Get the matrix from our object
        data <- x$get()
        
        # Calculate the inverse using matrix multiplication
        inv <- solve(data, ...)
        
        # Set the inverse to the object
        x$setInverse(inv)
        
        # Return the matrix
        inv
}


# 1. Create a 2x2 matrix
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))

# 2. Get the matrix
my_matrix$get()
# Output:
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4

# 3. Calculate the inverse (First run - no cache)
cacheSolve(my_matrix)
# Output:
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# 4. Calculate the inverse again (Second run - retrieves from cache)
cacheSolve(my_matrix)
# Output:
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5