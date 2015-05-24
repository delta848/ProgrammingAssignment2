## These two functions, makeCacheMatrix and cacheSolve, calculate the 
## inverse of a matrix. The matrix is first passed to makeCacheMatrix
## then cacheSolve is called on it's output, to return the inverse.
## These functions should be very performant, as the result will be
## retrieved from the cache if possible, reducing processing time.

## This function takes in a list of the kind returned by makeCacheMatrix
## and returns the solved matrix, either from the cache, if available or
## from calculating it directly

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
    ## caches the solved matrix into m
    setcm <- function(solved) m <<- solved
    ## returns the cached matrix m
    getcm <- function() m
    ## returns the matrix x
    getm <- function() x
  list (setcm = setcm, getcm = getcm, getm = getm)

}

## This function takes in a list of the kind returned by makeCacheMatrix
## and returns the solved matrix, either from the cache, if available or
## from calculating it directly

cacheSolve <- function(x, ...) {
  m <- x$getcm()   
  ## if m is not null then the data is in the cache
  if(!is.null(m)) { message("getting cached matrix")
                    return (m)}
  ## gets the matrix 
  matrix <- x$getm()
  ## solves the matrix - calculates the inverse
  m <- solve(matrix)
  ## caches the result
  x$setcm(m)
  m
}
