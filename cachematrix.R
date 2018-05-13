## makeCacheMatrix creates a special matrix, which is really a list containing a function to:

## 1.set the value of the vector
## 2.get the value of the vector
## 3.set the value of the mean
## 4.get the value of the mean



makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL 
	}
	get <- function() x
      setInv <- function(inv) m <<- inv
      getInv <- function() m
      list(set=set, get=get,setInv=setInv, getInv=getInv)
}


## cacheSolve function calculates the inverse of the special matrix returned by the function makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	m <- x$getInv()
      if(is.null(m)==F) { 
		message("getting cached data")
            return(m) 
	}
      m <- solve(x$get(), ...)
      x$setInv(m)
      m
}
