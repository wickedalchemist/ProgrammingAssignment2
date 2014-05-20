## These fuctions together will cashe the inverse of a matrix, 
## and check, before performing a solve(matrix), 
## if the matrix has changed. If not, the inverse will not be 
## recomputed and will instead be pulled from memory.

##Example useage: 
  # b=matrix(1:4,2,2)
  # a<-makeCacheMatrix(b)
  # cacheSolve(a)

## Make a matrix that can cashe its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## initiates empty array, with NULL internal variable, 
  ## that can be used with subsequently defined subfunctions
  
    m <- NULL
    ## takes input vector (numeric type) and sets to x.
    ## clears cache
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    ## reterns vector set in functon set()
    get <- function() x
    ## stores array to cache
    setinverse <- function(solve) m <<- solve
    ## returns cache value
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }


## cacheSolve find the inverse of the matrix, as defined in makeCacheMatrix.
## If the inverse has already been calculated, and the matrix has not changed,
## then cacheSolve retrieves the previously computed inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  #check if the inverse has been cached:
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ## it has, so return cached value
  }
  ## inverse has not been cached, so get the matrix from the function
  ## above and find the inverse
  data <- x$get()
  m <- solve(data, ...)
  ## save the calculated inverse into the cache
  x$setinverse(m)
  m
}