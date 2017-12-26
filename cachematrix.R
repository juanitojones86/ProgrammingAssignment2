## This function creates a special matrix object that can cache its inverse.
##It will try to reduce the computing time.
  makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
  ##introducing <<-
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
  }
  
  # This function computes the inverse of the special "matrix" created by 
  ## makeCacheMatrix above.


cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
   ##message in spanish
    message("Buscando datos en caché")
    return(inv)
  }
  ##if the inverse has´n been calculated..
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv

}
