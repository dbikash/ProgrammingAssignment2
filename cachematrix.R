## Calculates inverse of a square non-singular matrix, and returns result from cache
## if available, using scoping rules of R (and <<- operator)
## Example usage:
## > mat <- matrix(c(2,5,3,9), 2, 2)
## > x <- makeCacheMatrix(mat)
## > cacheSolve(x)
## > cacheSolve(x) ## returns cached inverse


## Construct a 'special matrix' object, that is actually a list containing functions to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of its inverse
## 4. Get the value of its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## 1. Set function: store matrix in parent environment
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    ## 2. Get function: retrieve matrix from parent environment
    get <- function() x
    
    ## 3. Set inverse in parent environment
    setinverse <- function(inverse) inv <<- inverse
    
    ## 4. Get inverse from parent environment
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }


## Returns the inverse of the 'special matrix' object created with the help of the
## above function. If present in cache, it returns inverse from cache. Else it
## calculates the inverse and stores in cache. Matrix is assumed to be invertible, so
## no error checking is done
cacheSolve <- function(x, ...) {
  
  ## Return cached inverse if available
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Else, calculate inverse and cache
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  
  inv
}
