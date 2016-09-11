## cachematrix.R
## Functions that can create a special matrix to cache its inverse and avoid
## several calculations if contents are the same.

## makeCacheMatrix function
## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(current_matrix = matrix()) {
  # initialize cache with NULL
  cached_matrix <- NULL

  # set function assigns the matrix and clears the cached matrix
  set <- function(matrix){
    current_matrix <<- matrix
    cached_matrix <<- NULL
  }

  # get function returns the current matrix if any
  get <- function() current_matrix

  # setInverse functon sets the cache with the solved matrix
  setInverse <- function(solved_matrix) cached_matrix <<- solved_matrix

  # getInverse function returns the cached result if any
  getInverse <- function() cached_matrix

  # last line returns the special matrix object
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function
## Returns the inverse of a matrix, the computation is skipped if a cached
## result exists

cacheSolve <- function(x, ...) {
  # Get the result if it exists
  result <- x$getInverse()
  # If the result is cached return
  if(!is.null(result)) {
    message("getting cached matrix")
    return(result)
  }
  # Else compute inverse matrix and save in cache
  data <- x$get()
  result <- solve(data, ...)
  x$setInverse(result)
  # Return computed result
  result
}
